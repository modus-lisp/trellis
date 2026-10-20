;;;; alg.lisp -- org.eclipse.elk.core.alg: AlgorithmAssembler,
;;;; LayoutProcessorConfiguration, EnumBasedFactoryComparator.
;;;;
;;;; Both list-accessor classes extend core.util.AbstractRandomListAccessor in
;;;; Java; its behaviour is reproduced by the %RLA- helpers over a LIST slot.

(in-package #:elk)

;;; ---------------------------------------------------------------- AbstractRandomListAccessor

(defgeneric %rla-provide-default (accessor))

(defun %rla-ensure-list-size (this size)
  (loop for i from (length (.list this)) below size
        do (jl-add (.list this) (%rla-provide-default this))))

(defun %rla-get-list-item (this index)
  (when (< index 0)
    (jthrow 'index-out-of-bounds-exception "Invalid index: ~D" index))
  (%rla-ensure-list-size this (1+ index))
  (jl-get (.list this) index))

(defun %rla-set-list-item (this index value)
  (when (< index 0)
    (jthrow 'index-out-of-bounds-exception "Invalid index: ~D" index))
  (if (< index (length (.list this)))
      (jl-set (.list this) index value)
      (progn
        ;; Avoid creating a new default value which will be immediately overwritten anyway
        (%rla-ensure-list-size this index)
        (jl-add (.list this) value))))

(defun %rla-get-list-size (this) (length (.list this)))
(defun %rla-clear-list (this) (jl-clear (.list this)))

;;; ---------------------------------------------------------------- EnumBasedFactoryComparator

(defjmethod compare ((this enum-based-factory-comparator) factory1 factory2)
  ;; If the two factories are of the same type, and if that type is an enumeration, we know how
  ;; to compare them
  (unless (typep factory1 'jenum)
    (jthrow 'unsupported-operation-exception
            "This comparator can only compare enumeration constants that are part of the same enumeration."))
  (unless (eq (class-of factory1) (class-of factory2))
    (jthrow 'class-cast-exception "~A cannot be compared to ~A" factory1 factory2))
  (compare-to factory1 factory2))

(defun %enum-based-factory-comparator-fn ()
  (let ((c (new-enum-based-factory-comparator)))
    (lambda (a b) (compare c a b))))

;;; ---------------------------------------------------------------- LayoutProcessorConfiguration

(defjstatic layout-processor-configuration.create ()
  (let ((this (make-instance 'layout-processor-configuration)))
    ;; Le private constructor.
    (clear this)
    this))

(defjstatic layout-processor-configuration.create-from (source)
  (add-all (layout-processor-configuration.create) source))

(defmethod %rla-provide-default ((this layout-processor-configuration))
  (new-jset 'eq))

(defjmethod clear ((this layout-processor-configuration))
  (%rla-clear-list this)
  (setf (.current-index this) -1)
  this)

(defjmethod before ((this layout-processor-configuration) phase)
  (setf (.current-index this) (ordinal phase))
  this)

(defjmethod after ((this layout-processor-configuration) phase)
  (setf (.current-index this) (1+ (ordinal phase)))
  this)

(defjmethod add ((this layout-processor-configuration) processor)
  (when (< (.current-index this) 0)
    (jthrow 'illegal-state-exception "Did not call before(...) or after(...) before calling add(...)."))
  (%lpc-do-add this (.current-index this) processor)
  this)

(defjmethod add-before ((this layout-processor-configuration) phase processor)
  ;; Calls to add(...) after this method without calling before(...) or after(...) will usually not do
  ;; what people want them to, so make sure that we have no valid current index
  (setf (.current-index this) -1)
  (%lpc-do-add this (ordinal phase) processor)
  this)

(defjmethod add-after ((this layout-processor-configuration) phase processor)
  (setf (.current-index this) -1)
  (%lpc-do-add this (1+ (ordinal phase)) processor)
  this)

(defun %lpc-do-add (this index processor)
  (js-add (%rla-get-list-item this index) processor))

(defjmethod add-all ((this layout-processor-configuration) configuration)
  (loop for i from 0 below (%rla-get-list-size configuration)
        do (js-add-all (%rla-get-list-item this i) (%rla-get-list-item configuration i)))
  this)

(defjmethod processors-before ((this layout-processor-configuration) phase)
  (%lpc-processors this (ordinal phase)))

(defjmethod processors-after ((this layout-processor-configuration) phase)
  (%lpc-processors this (1+ (ordinal phase))))

(defun %lpc-processors (this index)
  (jset-from (%rla-get-list-item this index) 'eq))

;;; ---------------------------------------------------------------- AlgorithmAssembler

(defjstatic algorithm-assembler.create (phase-enum)
  "PHASE-ENUM is the symbol naming the phase enumeration, e.g. 'layered-phases."
  (let* ((this (make-instance 'algorithm-assembler))
         (constants (enum-values-of phase-enum)))
    (setf (.processor-comparator this) (%enum-based-factory-comparator-fn))
    (setf (.phases-enum-class this) phase-enum)
    (setf (.number-of-phases this) (length constants))
    (when (zerop (.number-of-phases this))
      (jthrow 'illegal-argument-exception "There must be at least one phase in the phase enumeration."))
    (setf (.configured-phases this) (enum-set.none-of phase-enum))
    (setf (.additional-processors this) (layout-processor-configuration.create))
    (setf (.cache this) (make-hash-table :test 'eq))
    this))

(defmethod %rla-provide-default ((this algorithm-assembler)) nil)

;;; Configuration

(defjmethod with-caching ((this algorithm-assembler) enabled)
  (setf (.enable-caching this) enabled)
  this)

(defjmethod with-fail-on-missing-phase ((this algorithm-assembler) fail)
  (setf (.fail-on-missing-phase this) fail)
  this)

(defjmethod with-processor-comparator ((this algorithm-assembler) comparator)
  "COMPARATOR: a function (a b) -> int, or an object with a compare/2 method."
  (setf (.processor-comparator this)
        (if (functionp comparator) comparator (lambda (a b) (compare comparator a b))))
  this)

;;; Phase Assembly

(defjmethod clear-cache ((this algorithm-assembler))
  (clrhash (.cache this))
  this)

(defjmethod reset ((this algorithm-assembler))
  (%rla-clear-list this)
  (clear (.configured-phases this))
  (clear (.additional-processors this))
  this)

(defjmethod set-phase ((this algorithm-assembler) phase phase-factory)
  (%rla-set-list-item this (ordinal phase) phase-factory)
  (add (.configured-phases this) phase)
  this)

(defjmethod add-processor-configuration ((this algorithm-assembler) config)
  (add-all (.additional-processors this) config)
  this)

;;; Algorithm Building

(defjmethod build ((this algorithm-assembler) graph)
  ;; Check if there are enough phases
  (when (and (.fail-on-missing-phase this)
             (< (size (.configured-phases this)) (.number-of-phases this)))
    (jthrow 'illegal-state-exception "Expected ~D phases to be configured; only found ~D"
            (.number-of-phases this) (size (.configured-phases this))))
  ;; All the phases that could potentially have a phase implementation
  (let ((phase-enum-constants (enum-values-of (.phases-enum-class this)))
        (phase-implementations (new-jlist (.number-of-phases this)))
        (processor-configuration (layout-processor-configuration.create))
        (algorithm (new-jlist)))
    ;; Instantiate all configured phases
    (loop for phase across phase-enum-constants
          do (let ((phase-factory (%rla-get-list-item this (ordinal phase))))
               (jl-add phase-implementations
                       (if phase-factory (%aa-retrieve-phase this phase-factory) nil))))
    ;; Assemble a definitive processor configuration
    (jdo (phase phase-implementations)
      (when phase
        (let ((config (get-layout-processor-configuration phase graph)))
          (when config
            (add-all processor-configuration config)))))
    (add-all processor-configuration (.additional-processors this))
    ;; Add processors and phases to the algorithm
    (loop for phase across phase-enum-constants
          do ;; Add processors
             (jl-add-all algorithm (%aa-retrieve-processors this (processors-before processor-configuration phase)))
             ;; Add the phase itself, if it exists
             (let ((phase-implementation (jl-get phase-implementations (ordinal phase))))
               (when phase-implementation
                 (jl-add algorithm phase-implementation))))
    ;; Add processors after the last phase
    (jl-add-all algorithm
                (%aa-retrieve-processors
                 this (processors-after processor-configuration
                                        (aref phase-enum-constants (1- (length phase-enum-constants))))))
    algorithm))

;;; Utilities

(defun %aa-retrieve-processors (this factories)
  (let ((sorted (jl-sort (jlist-from factories) (.processor-comparator this)))
        (processors (new-jlist (jsize factories))))
    (jdo (factory sorted)
      (jl-add processors (%aa-retrieve-processor this factory)))
    processors))

(defun %aa-retrieve-phase (this factory)
  (%aa-retrieve-processor this factory))

(defun %aa-retrieve-processor (this factory)
  (if (.enable-caching this)
      (multiple-value-bind (processor found) (gethash factory (.cache this))
        (if found
            processor
            (setf (gethash factory (.cache this)) (create factory))))
      (create factory)))
