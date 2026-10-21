;;;; elk-reflect-test.lisp -- port of org.eclipse.elk.shared.test.ElkReflectTest.
;;;; testSimpleProperty walks Class.getDeclaredFields() of Java classes and is
;;;; NOT ported (no Java reflection in this port; see BLOCKERS.md).
;;;; testLayoutOptionData is ported with one approximation, marked below.

(in-package #:elk)

(defun %ert-check-property-default (property)
  (let ((o (get-default property)))
    (when (and o (cloneable-p o))
      (assert-true (not (eq o (get-default property)))))))

(defcore-class elk-reflect-test
  ("testLayoutOptionData"
   (jdo (ld (get-option-data (layout-meta-data-service.get-instance)))
     (%ert-check-property-default ld)
     (when (and (eq (get-type ld) core-data-type.object) (can-parse-value ld))
       ;; Java first tries ld.getClass().getConstructor().newInstance() and
       ;; keeps ld.getDefault() when that throws.  Only the default path is
       ;; ported (no reflective constructor lookup).
       (let ((o (get-default ld)))
         (when o
           (assert-true (typep o 'i-data-object))
           (let* ((serialized (to-string o))
                  (parsed (parse-value ld serialized)))
             (assert-equals serialized (to-string parsed)))))))))
