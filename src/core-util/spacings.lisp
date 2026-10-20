;;;; spacings.lisp -- IndividualSpacings, ElkSpacings, PropertyConstantsDelegator

(in-package #:elk)

;;; ------------------------------------------------------------ IndividualSpacings

(defjctor individual-spacings (other)
  (copy-properties this other))

(defjmethod jclone-individual-spacings ((this individual-spacings))
  (new-individual-spacings this))

(defmethod jclone ((x individual-spacings)) (new-individual-spacings x))

(defjstatic individual-spacings.get-individual-or-inherited (node property)
  (let ((result nil))
    (when (has-property node core-options.spacing-individual)
      (let ((individual-spacings (get-property node core-options.spacing-individual)))
        (when (has-property individual-spacings property)
          (setf result (get-property individual-spacings property)))))
    (if (typep node 'node-adapter)
        (progn
          (when (and (null result) (get-graph node))
            (setf result (get-property (get-graph node) property)))
          (when (null result)
            (setf result (get-default property))))
        (when (and (null result) (get-parent node))
          (setf result (get-property (get-parent node) property))))
    result))

(defjconst individual-spacings.serialized-option-separator ";,;")

(defjmethod to-string ((this individual-spacings))
  (let ((parts '()))
    (dolist (cell (property-value-cells this))
      (push (jstr (get-id (car cell)) ":" (%cu-to-string (cdr cell))) parts))
    (format nil (concatenate 'string "~{~A~^" individual-spacings.serialized-option-separator "~}")
            (nreverse parts))))

(defun %cu-split (string separator)
  "String.split with a literal separator (trailing empty strings dropped)."
  (let ((parts '()) (start 0) (n (length separator)))
    (loop for pos = (search separator string :start2 start)
          while pos
          do (push (subseq string start pos) parts)
             (setf start (+ pos n)))
    (push (subseq string start) parts)
    (let ((list (nreverse parts)))
      (loop while (and list (string= (car (last list)) "")) do (setf list (butlast list)))
      list)))

(defjmethod parse ((this individual-spacings) string)
  (when (%cu-null-or-empty string)
    (return-from parse/1 nil))
  (handler-case
      (dolist (option-string (%cu-split string individual-spacings.serialized-option-separator))
        (let* ((option (%cu-split option-string ":"))
               (option-data (get-option-data-by-suffix (layout-meta-data-service.get-instance)
                                                       (first option))))
          (when (null option-data)
            (jthrow 'illegal-argument-exception "Invalid option id: ~A" (first option)))
          (let ((value (parse-value option-data (second option))))
            (when (null value)
              (jthrow 'illegal-argument-exception "Invalid option value: ~A" (second option)))
            (set-property this option-data value))))
    (error ()
      (jthrow 'illegal-argument-exception
              "The given string does not match the expected format for individual spacings.")))
  nil)

;;; ------------------------------------------------------------ ElkSpacings

(defjstatic elk-spacings.with-base-value (base-spacing)
  (new-elk-core-spacings-builder base-spacing))

(defjconst abstract-spacings-builder.double-eq-epsilon 10e-5)

(defun %cu-option-filter-accept (filter holder property)
  (if (functionp filter) (funcall filter holder property) (accept filter holder property)))

(defjconst abstract-spacings-builder.elk-option-target-filter
  (lambda (holder property)
    ;; As in ELK, the PROPERTY (not the holder) is tested for being a graph
    ;; element, so this filter accepts everything.
    (if (typep property 'elk-graph-element)
        (%cu-option-filter-accept layout-configurator.option-target-filter holder property)
        t)))

(defjgeneric get-base-spacing-option 0)
(defjgeneric get-dependend-spacing-options 0)
(defjgeneric this-t 0)

(defjctor abstract-spacings-builder (the-base-spacing)
  (setf (.base-spacing this) (jd the-base-spacing)
        (.filters this) (jlist abstract-spacings-builder.elk-option-target-filter))
  (let ((base (get-base-spacing-option this)))
    (setf (.factor-map this) (list (cons base 1.0)))
    (jdo (option (get-dependend-spacing-options this))
      (let ((factor (if (and (get-default base) (get-default option))
                        (with-java-floats (jd (/ (get-default option) (get-default base))))
                        1.0)))
        (%cu-factor-put this option factor)))))

(defun %cu-factor-cell (this option)
  (assoc (get-id option) (.factor-map this) :key #'get-id/0 :test #'string=))

(defun %cu-factor-put (this option factor)
  (let ((cell (%cu-factor-cell this option)))
    (if cell
        (setf (cdr cell) factor)
        (setf (.factor-map this) (append (.factor-map this) (list (cons option factor)))))))

(defjmethod get-filters ((this abstract-spacings-builder)) (.filters this))

(defjmethod compute-factor ((this abstract-spacings-builder) value)
  (with-java-floats (/ value (.base-spacing this))))

(defjmethod get-factors ((this abstract-spacings-builder))
  (let ((m (make-hash-table :test 'eq)))
    (loop for (p . f) in (.factor-map this) do (setf (gethash p m) f))
    m))

(defun %cu-check-spacing-option (this spacing-option what amount)
  (unless (%cu-factor-cell this spacing-option)
    (jthrow 'illegal-argument-exception "'~A' is not a configurable spacing option." (get-id spacing-option)))
  (when (< amount 0.0)
    (jthrow 'illegal-argument-exception "The ~A for '~A' must not be negative (~A)."
            what (get-id spacing-option) (jstr amount)))
  (when (string= (get-id spacing-option) (get-id (get-base-spacing-option this)))
    (jthrow 'illegal-argument-exception
            "'~A' is the base spacing option not allowed to use with '~A'."
            (get-id spacing-option) (if (string= what "factor") "withFactor" "withValue"))))

(defjmethod with-factor ((this abstract-spacings-builder) spacing-option factor)
  (%cu-check-spacing-option this spacing-option "factor" factor)
  (%cu-factor-put this spacing-option (jd factor))
  (this-t this))

(defjmethod with-value ((this abstract-spacings-builder) spacing-option value)
  (%cu-check-spacing-option this spacing-option "value" value)
  (%cu-factor-put this spacing-option (compute-factor this value))
  (this-t this))

(defjmethod with-overwrite ((this abstract-spacings-builder) shall-overwrite)
  (setf (.overwrite this) shall-overwrite)
  (this-t this))

(defun %cu-fuzzy-equals (a b tolerance)
  (or (<= (abs (- a b)) tolerance) (= a b)))

(defjmethod build ((this abstract-spacings-builder))
  "A Consumer<IPropertyHolder>: a function of one holder."
  (unless (.overwrite this)
    (jl-add (.filters this) layout-configurator.no-overwrite-holder))
  (if (and (not (.overwrite this))
           (%cu-fuzzy-equals (.base-spacing this) (get-default (get-base-spacing-option this))
                             abstract-spacings-builder.double-eq-epsilon))
      (lambda (e) (declare (ignore e)) nil)
      (let ((factor-map (.factor-map this))
            (filters (.filters this))
            (base-spacing (.base-spacing this)))
        (lambda (element)
          (loop for (p . factor) in factor-map
                when (every (lambda (filter) (%cu-option-filter-accept filter element p)) filters)
                  do (set-property element p (* factor base-spacing)))))))

(defjmethod j-apply ((this abstract-spacings-builder) holder)
  (funcall (build this) holder)
  nil)

(defjmethod to-visitor ((this abstract-spacings-builder))
  (let ((consumer (build this)))
    (lambda (e) (funcall consumer e))))

;; ElkCoreSpacingsBuilder

(defjconst elk-core-spacings-builder.base-spacing-option core-options.spacing-node-node)

(defjconst elk-core-spacings-builder.dependent-spacing-options
  (jlist core-options.spacing-component-component
         core-options.spacing-edge-edge
         core-options.spacing-edge-label
         core-options.spacing-edge-node
         core-options.spacing-label-label
         core-options.spacing-label-node
         core-options.spacing-label-port-horizontal
         core-options.spacing-label-port-vertical
         core-options.spacing-node-self-loop
         core-options.spacing-port-port))

(defjctor elk-core-spacings-builder (base-spacing)
  (super-ctor abstract-spacings-builder this base-spacing))

(defjmethod this-t ((this elk-core-spacings-builder)) this)
(defjmethod get-base-spacing-option ((this elk-core-spacings-builder))
  elk-core-spacings-builder.base-spacing-option)
(defjmethod get-dependend-spacing-options ((this elk-core-spacings-builder))
  elk-core-spacings-builder.dependent-spacing-options)

;;; ------------------------------------------------------------ PropertyConstantsDelegator

(defjstatic property-constants-delegator.create-empty () (new-property-constants-delegator))

(defjstatic property-constants-delegator.create-for-layout-algorithm-data (algorithm-data)
  (let ((delegator (new-property-constants-delegator)))
    (jdo (option-id (get-known-option-ids algorithm-data))
      (add-delegate delegator (new-property option-id (get-default-value algorithm-data option-id))))
    delegator))

(defjstatic property-constants-delegator.create-for-layout-algorithm-with-id (algorithm-id)
  (let ((algorithm-data (get-algorithm-data (layout-meta-data-service.get-instance) algorithm-id)))
    (if algorithm-data
        (property-constants-delegator.create-for-layout-algorithm-data algorithm-data)
        (property-constants-delegator.create-empty))))

(defjmethod add-delegate ((this property-constants-delegator) delegate)
  (setf (gethash (get-id delegate) (.property-delegates this)) delegate)
  this)

(defjmethod get-property-or-delegate ((this property-constants-delegator) property)
  (multiple-value-bind (d found) (gethash (get-id property) (.property-delegates this))
    (if found d property)))

(defjmethod get-property ((this property-constants-delegator) holder property)
  (get-property holder (get-property-or-delegate this property)))

(defjmethod get-individual-or-inherited-property ((this property-constants-delegator) node property)
  (individual-spacings.get-individual-or-inherited node (get-property-or-delegate this property)))
