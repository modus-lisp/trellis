;;;; model-order-barycenter-heuristic-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p3order.ModelOrderBarycenterHeuristicTest.

(in-package #:elk)

(deflayouttest model-order-barycenter-heuristic-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:configurator-provider prefer-edges-weighted-configurator ()
    (let ((config (new-layout-configurator)))
      (set-property (configure config 'elk-node) layered-options.crossing-minimization-strategy
                    crossing-minimization-strategy.layer-sweep)
      (set-property (configure config 'elk-node) layered-options.consider-model-order-strategy
                    ordering-strategy.nodes-and-edges)
      (set-property (configure config 'elk-node) layered-options.crossing-minimization-force-node-model-order
                    t)
      (set-property (configure config 'elk-node) layered-options.crossing-minimization-greedy-switch-type
                    greedy-switch-type.off)
      config))
  ;; Just check for errors that might occur
  (:after-processor test-model-order-respected (:processors (layer-sweep-crossing-minimizer)) (graph)
    (jdo (layer graph)
      ;; We iterate over the layer's nodes check whether the real nodes (the one with a model order)
      ;; are correctly ordered.
      (let ((model-order -1))
        (jdo (node layer)
          (when (has-property node layered-internal-properties.model-order)
            (let ((new-model-order (get-property node layered-internal-properties.model-order)))
              (assert-true (format nil "Node has model order ~A, which is smaller than the previous model order of ~A"
                                   new-model-order model-order)
                           (> new-model-order model-order))
              (setf model-order new-model-order))))))))
