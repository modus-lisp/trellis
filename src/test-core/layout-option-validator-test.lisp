;;;; layout-option-validator-test.lisp -- port of
;;;; org.eclipse.elk.core.validation.LayoutOptionValidatorTest.

(in-package #:elk)

(defun %lov-issues-string (validator)
  (jstr "[" (format nil "~{~A~^, ~}" (jlist-to-list (get-issues validator))) "]"))

(defcore-class layout-option-validator-test
  ("testInvalidType"
   (let* ((graph (elk-graph-util.create-graph))
          (aspect-ratio (get-option-data (layout-meta-data-service.get-instance)
                                         (get-id core-options.aspect-ratio)))
          (validator (new-layout-option-validator)))
     (set-property graph aspect-ratio "foo")
     (elk-util.apply-visitors graph (vector validator))
     (assert-equals "[ERROR: The assigned value foo of the option 'Aspect Ratio' does not match the type Double. (Root Node)]"
                    (%lov-issues-string validator))))
  ("testExclusiveLowerBound"
   (let ((graph (elk-graph-util.create-graph))
         (validator (new-layout-option-validator)))
     (set-property graph core-options.aspect-ratio 0d0)
     (elk-util.apply-visitors graph (vector validator))
     (assert-equals "[ERROR: The assigned value 0.0 of the option 'org.eclipse.elk.aspectRatio' is less than the lower bound 0.0 (exclusive). (Root Node)]"
                    (%lov-issues-string validator)))))
