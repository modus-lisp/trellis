;;;; edge-and-layer-constraint-edge-reverser-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.EdgeAndLayerConstraintEdgeReverserTest.

(in-package #:elk)

(deflayouttest edge-and-layer-constraint-edge-reverser-test
  (:algorithm "org.eclipse.elk.layered")
  (:graph-resources (list (model-resource-path "tests/layered/**/")))
  (:after-processor test-layer-constraints
    (:processors (edge-and-layer-constraint-edge-reverser)) (graph)
    (jdo (node (get-layerless-nodes graph))
      (let ((constr (get-property node layered-options.layering-layer-constraint)))
        (cond
          ((eq constr layer-constraint.first)
           ;; either no incoming edges or only from FIRST_SEPARATE nodes
           (jdo (inc (get-incoming-edges node))
             (let ((src (get-node (get-source inc))))
               (assert-true (eq (get-property src layered-options.layering-layer-constraint)
                                layer-constraint.first-separate)))))
          ((eq constr layer-constraint.last)
           ;; either no outgoing edges or only to LAST_SEPARATE nodes
           (jdo (inc (get-outgoing-edges node))
             (let ((src (get-node (get-target inc))))
               (assert-true (eq (get-property src layered-options.layering-layer-constraint)
                                layer-constraint.last-separate)))))
          ((eq constr layer-constraint.first-separate)
           (assert-true (is-empty (get-incoming-edges node))))
          ((eq constr layer-constraint.last-separate)
           (assert-true (is-empty (get-outgoing-edges node)))))))))
