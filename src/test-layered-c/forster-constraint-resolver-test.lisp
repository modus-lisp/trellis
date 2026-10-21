;;;; forster-constraint-resolver-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p3order.ForsterConstraintResolverTest.

(in-package #:elk)

(deflayouttest forster-constraint-resolver-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/528_nodeEdgeOverlap.elkt")
                          (model-resource-path "tickets/layered/528_violatedNodeOrder.elkt")))
  ;; Test whether the order of nodes respects each node's IN_LAYER_SUCCESSOR_CONSTRAINTS.
  (:after-processor test-successor-constraints (:processors (layer-sweep-crossing-minimizer)) (graph)
    (jdo (layer graph)
      (let ((encountered-nodes (new-jset)))
        (jdo (node layer)
          (when (has-property node layered-internal-properties.in-layer-successor-constraints)
            (let ((violation (some (lambda (n) (contains encountered-nodes n))
                                   (coerce (jlist-to-list
                                            (get-property node layered-internal-properties.in-layer-successor-constraints))
                                           'list))))
              (assert-false (format nil "Successor constraints are not respected for ~A" node)
                            violation)))
          (js-add encountered-nodes node)))))
  ;; Nodes may be part of layout units, which must be adhered to even after in-layer constraints are resolved.
  (:after-processor test-non-overlapping-layout-units (:processors (layer-sweep-crossing-minimizer)) (graph)
    (jdo (layer graph)
      (let ((encountered-layout-units (new-jset))
            (current-layout-unit nil))
        (jdo (node layer)
          (let ((new-layout-unit (get-property node layered-internal-properties.in-layer-layout-unit)))
            (when (and new-layout-unit (not (eq new-layout-unit current-layout-unit)))
              (assert-false (format nil "Node ~A is overlapping with the current layout unit" new-layout-unit)
                            (contains encountered-layout-units new-layout-unit))
              (when current-layout-unit
                (js-add encountered-layout-units current-layout-unit))
              (setf current-layout-unit new-layout-unit))))))))
