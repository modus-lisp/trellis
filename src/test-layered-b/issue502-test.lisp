;;;; issue502-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue502Test.

(in-package #:elk)

(defun %b-502-test-large-enough-node (node)
  (let ((required (new-elk-rectangle)))
    (jdo (child (get-children node))
      (j-union required (new-elk-rectangle (get-x child) (get-y child)
                                           (get-width child) (get-height child))))
    (jdo (edge (get-contained-edges node))
      (jdo (section (get-sections edge))
        (jdo (bend-point (get-bend-points section))
          (setf (.width required) (max (.width required) (get-x bend-point)))
          (setf (.height required) (max (.height required) (get-y bend-point))))))
    (assert-true (format nil "Node not large enough for content. Size required: (~A, ~A). Actual size: (~A, ~A)"
                         (.width required) (.height required) (get-width node) (get-height node))
                 (and (>= (get-width node) (.width required))
                      (>= (get-height node) (.height required))))))

(deflayouttest issue502-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/502_collapsingCompoundNode.elkt")))
  (:test test-large-enough-nodes () (graph)
    (jdo (node (get-children graph))
      (%b-502-test-large-enough-node node))))
