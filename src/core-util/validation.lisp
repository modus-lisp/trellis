;;;; validation.lisp -- org.eclipse.elk.core.validation: GraphIssue (with its
;;;; Severity enum), GraphValidator and LayoutOptionValidator.

(in-package #:elk)

;;; ------------------------------------------------------------ GraphIssue

(defjctor graph-issue (element message severity)
  (setf (.element this) element
        (.message this) message
        (.severity this) severity))

(defjmethod get-element ((this graph-issue)) (.element this))
(defjmethod get-message ((this graph-issue)) (.message this))
(defjmethod get-severity ((this graph-issue)) (.severity this))

;;; GraphIssue.Severity carries a user-readable string (its constructor argument).
(defjmethod get-user-string ((this severity))
  (cond ((eq this severity.error) "Error")
        (t "Warning")))

;;; ------------------------------------------------------------ GraphValidator

(defjmethod visit ((this graph-validator) element)
  (when (typep element 'elk-edge)
    (check-edge this element))
  (let ((parent (elk-graph-util.containing-graph element)))
    (when parent
      (run-algorithm-specific-checks this element parent))
    (when (and (typep element 'elk-node)
               (or (null parent)
                   (not (eq (get-property parent core-options.resolved-algorithm)
                            (get-property element core-options.resolved-algorithm)))))
      (run-algorithm-specific-checks this element element)))
  nil)

(defjmethod check-edge ((this graph-validator) edge)
  (cond ((not (is-connected edge))
         (jl-add (.issues this) (new-graph-issue/3 edge "Edge is not connected." severity.error)))
        (t
         (let ((best-container (elk-graph-util.find-best-edge-containment edge)))
           (when (and best-container (not (eq best-container (get-containing-node edge))))
             (let ((message (new-string-builder "Edge should be contained in ")))
               (elk-util.print-element-path best-container message)
               (jl-add (.issues this)
                       (new-graph-issue/3 edge (coerce message 'simple-string) severity.warning)))))))
  (jdo (edge-section (get-sections edge))
    (let ((incoming-shape (get-incoming-shape edge-section)))
      (when incoming-shape
        (unless (contains (get-sources edge) incoming-shape)
          (jl-add (.issues this)
                  (new-graph-issue/3 edge
                                     (jstr (%cu-elk-class-name incoming-shape)
                                           " declared as incoming shape is not a source of this edge.")
                                     severity.error)))
        (unless (is-empty (get-incoming-sections edge-section))
          (jl-add (.issues this)
                  (new-graph-issue/3 edge
                                     (jstr "An edge section cannot be connected to an "
                                           (%cu-elk-class-name incoming-shape)
                                           " and other sections at the same time.")
                                     severity.error)))))
    (let ((outgoing-shape (get-outgoing-shape edge-section)))
      (when outgoing-shape
        (unless (contains (get-targets edge) outgoing-shape)
          (jl-add (.issues this)
                  (new-graph-issue/3 edge
                                     (jstr (%cu-elk-class-name outgoing-shape)
                                           " declared as outgoing shape is not a target of this edge.")
                                     severity.error)))
        (unless (is-empty (get-outgoing-sections edge-section))
          (jl-add (.issues this)
                  (new-graph-issue/3 edge
                                     (jstr "An edge section cannot be connected to an "
                                           (%cu-elk-class-name outgoing-shape)
                                           " and other sections at the same time.")
                                     severity.error))))))
  nil)

(defjmethod run-algorithm-specific-checks ((this graph-validator) element parent)
  (let ((algo-data (get-property parent core-options.resolved-algorithm)))
    (when algo-data
      (let ((validator (get-validator this algo-data)))
        (when validator
          (visit validator element)))))
  nil)

(defjmethod get-validator ((this graph-validator) algo-data)
  (let ((cached (gethash algo-data (.algorithm-specific-validators this))))
    (when cached
      (return-from get-validator cached)))
  (let ((validator-class (get-validator-class algo-data)))
    (when (null validator-class)
      (return-from get-validator nil))
    ;; validatorClass.getConstructor().newInstance(): the stored class is a
    ;; symbol naming a defjclass (its NEW-<name>/0 constructor) or a constructor function.
    (let ((validator (cond ((functionp validator-class) (funcall validator-class))
                           ((symbolp validator-class)
                            (funcall (intern (format nil "NEW-~A/0" (symbol-name validator-class))
                                             :elk)))
                           (t validator-class))))
      (setf (gethash algo-data (.algorithm-specific-validators this)) validator)
      validator)))

(defjmethod get-issues ((this graph-validator))
  (if (zerop (hash-table-count (.algorithm-specific-validators this)))
      (.issues this)
      (let ((result (new-jlist)))
        (jl-add-all result (.issues this))
        (maphash (lambda (algo-data validator)
                   (declare (ignore algo-data))
                   (jl-add-all result (get-issues validator)))
                 (.algorithm-specific-validators this))
        result)))

;;; ------------------------------------------------------------ LayoutOptionValidator

(defun %lov-compare-bound (bound value)
  "bound.compareTo(value) where a bound may be an infinity keyword."
  (cond ((eq bound :negative-infinity) -1)
        ((eq bound :positive-infinity) 1)
        (t (jcompare bound value))))

(defun %lov-simple-name (option-class)
  "Class.getSimpleName() for the Lisp representation of an option class."
  (cond ((consp option-class) (princ-to-string (second option-class)))
        ((symbolp option-class) (string-downcase (symbol-name option-class)))
        (t (princ-to-string option-class))))

(defun %lov-option-class-instance-p (option-data value)
  "optionData.getOptionClass().isInstance(value).  Enum option classes are
checked against the enum's class; other classes are accepted."
  (let ((option-class (get-option-class option-data)))
    (cond ((and (consp option-class) (eq (first option-class) :enum) (symbolp (second option-class))
                (find-class (second option-class) nil))
           (typep value (second option-class)))
          (t t))))

(defjmethod is-valid-type ((this layout-option-validator) option-data value)
  (let ((type (get-type option-data)))
    (cond ((eq type core-data-type.string) (stringp value))
          ((eq type core-data-type.boolean) (eq value t))
          ((eq type core-data-type.int) (integerp value))
          ((eq type core-data-type.double) (typep value 'double-float))
          ((eq type core-data-type.enumset) (typep value 'enum-set))
          (t (%lov-option-class-instance-p option-data value)))))

(defjmethod check-property ((this layout-option-validator) property value element)
  (let ((option-name nil))
    (when (typep property 'layout-option-data)
      (setf option-name (get-name property))
      (when (and value (not (is-valid-type this property value)))
        (let ((message (jstr "The assigned value " (princ-to-string value)
                             " of the option '" option-name
                             "' does not match the type "
                             (%lov-simple-name (get-option-class property)) ".")))
          (return-from check-property
            (jlist (new-graph-issue/3 element message severity.error))))))
    (when value
      (let ((lower (get-lower-bound property))
            (upper (get-upper-bound property)))
        (cond ((> (%lov-compare-bound lower value) 0)
               (unless option-name (setf option-name (get-id property)))
               (return-from check-property
                 (jlist (new-graph-issue/3 element
                                           (jstr "The assigned value " (princ-to-string value)
                                                 " of the option '" option-name
                                                 "' is less than the lower bound "
                                                 (princ-to-string lower) ".")
                                           severity.error))))
              ((< (%lov-compare-bound upper value) 0)
               (unless option-name (setf option-name (get-id property)))
               (return-from check-property
                 (jlist (new-graph-issue/3 element
                                           (jstr "The assigned value " (princ-to-string value)
                                                 " of the option '" option-name
                                                 "' is greater than the upper bound "
                                                 (princ-to-string upper) ".")
                                           severity.error)))))))
    (new-jlist)))

(defjmethod visit ((this layout-option-validator) element)
  (let ((map (.property-map element)))
    (when map
      (loop for cell in (loop for c being the hash-values of map collect c)
            do (let ((property (car cell))
                     (value (cdr cell)))
                 (when property
                   (when (typep value 'property-value-proxy)
                     (let ((resolved (resolve-proxy value property)))
                       (setf value resolved)
                       (when resolved
                         (setf (cdr cell) resolved))))
                   (jl-add-all (.issues this) (check-property this property value element)))))))
  nil)

(defjmethod get-issues ((this layout-option-validator))
  (.issues this))
