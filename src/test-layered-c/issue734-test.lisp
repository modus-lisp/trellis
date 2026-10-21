;;;; issue734-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue734Test.

(in-package #:elk)

(defun %issue734-node-span (nodes)
  "computeNodeCoordinateSpan: min x and max x+width over NODES (0 when empty)."
  (let ((min double.positive-infinity) (max double.negative-infinity))
    (jdo (node nodes)
      (setf min (math.min min (get-x node)))
      (setf max (math.max max (+ (get-x node) (get-width node)))))
    (if (is-empty nodes) (values 0d0 0d0) (values min max))))

(defun %issue734-edge-span (edges)
  "computeEdgeCoordinateSpan: min and max x over all bend/section vectors."
  (let ((min double.positive-infinity) (max double.negative-infinity))
    (jdo (edge edges)
      (jdo (section (get-sections edge))
        (jdo (vec (elk-util.create-vector-chain section))
          (setf min (math.min min (.x vec)))
          (setf max (math.max max (.x vec))))))
    (values min max)))

(deflayouttest issue734-test
  (:algorithm "org.eclipse.elk.layered")
  (:graph-resources (list (model-resource-path "tickets/layered/734_nodeEdgeSpacingFlat.elkt")
                          (model-resource-path "tickets/layered/734_nodeEdgeSpacingFlatMoreComplex.elkt")
                          (model-resource-path "tickets/layered/734_nodeEdgeSpacingHierarchical.elkt")))
  (:test test-orthogonal-edges () (graph)
    ;; If the edge is routed properly, the edge span should have the same distance to the node span on both
    ;; sides. In other words, their respective centres should be aligned
    (multiple-value-bind (node-min node-max) (%issue734-node-span (get-children graph))
      (multiple-value-bind (edge-min edge-max) (%issue734-edge-span (get-contained-edges graph))
        (tlc-assert-equals-delta "node and edge span centres differ"
                                 (/ (+ node-min node-max) 2)
                                 (/ (+ edge-min edge-max) 2)
                                 0.5d0)))))
