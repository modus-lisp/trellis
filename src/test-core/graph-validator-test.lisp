;;;; graph-validator-test.lisp -- port of
;;;; org.eclipse.elk.core.validation.GraphValidatorTest.

(in-package #:elk)

(defun %gv-issues-string (validator)
  "Java's List.toString() of the issues."
  (jstr "[" (format nil "~{~A~^, ~}" (jlist-to-list (get-issues validator))) "]"))

(defcore-class graph-validator-test
  ("testUnconnectedEdge"
   (let ((graph (elk-graph-util.create-graph))
         (validator (new-graph-validator)))
     (elk-graph-util.create-edge graph)
     (elk-util.apply-visitors graph (vector validator))
     (assert-equals "[ERROR: Edge is not connected. (Root Node > Edge)]"
                    (%gv-issues-string validator)))))
;; testCustomValidation is not ported (BLOCKERS.md): it needs LayoutAlgorithmData.Builder
;; and a registry lambda.
