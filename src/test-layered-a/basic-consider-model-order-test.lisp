;;;; basic-consider-model-order-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.BasicConsiderModelOrderTest.
;;;; Only checks that layout with each model-order configuration runs without
;;;; errors (the after-processor body is assert(true) in Java).

(in-package #:elk)

(defun %bcmo-configurator (&rest settings)
  "A LayoutConfigurator with CROSSING_MINIMIZATION_STRATEGY = LAYER_SWEEP on ElkNode
plus the given (property value) pairs, all on ElkNode."
  (let ((config (new-layout-configurator)))
    (set-property (configure config 'elk-node)
                  layered-options.crossing-minimization-strategy
                  crossing-minimization-strategy.layer-sweep)
    (loop for (property value) on settings by #'cddr
          do (set-property (configure config 'elk-node) property value))
    config))

(defparameter *bcmo-weighted* '(0.001d0 0.001d0))

(deflayouttest basic-consider-model-order-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:configurator-provider nodes-and-edges-configurator ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.nodes-and-edges))
  (:configurator-provider nodes-and-edges-weighted-configurator ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.nodes-and-edges
                        layered-options.consider-model-order-crossing-counter-node-influence 0.001d0
                        layered-options.consider-model-order-crossing-counter-port-influence 0.001d0))
  (:configurator-provider prefer-edges-configurator ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.prefer-edges))
  (:configurator-provider prefer-edges-weighted-configurator ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.prefer-edges
                        layered-options.consider-model-order-crossing-counter-node-influence 0.001d0
                        layered-options.consider-model-order-crossing-counter-port-influence 0.001d0))
  (:configurator-provider prefer-nodes-configurator ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.prefer-nodes))
  (:configurator-provider prefer-nodes-weighted-configurator ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.prefer-nodes
                        layered-options.consider-model-order-crossing-counter-node-influence 0.001d0
                        layered-options.consider-model-order-crossing-counter-port-influence 0.001d0))
  (:configurator-provider port-model-order-prefer-edges ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.prefer-edges
                        layered-options.consider-model-order-port-model-order t))
  (:configurator-provider port-model-order-nodes-and-edges ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.nodes-and-edges
                        layered-options.consider-model-order-port-model-order t))
  (:configurator-provider port-model-order-prefer-nodes ()
    (%bcmo-configurator layered-options.consider-model-order-strategy ordering-strategy.prefer-nodes
                        layered-options.consider-model-order-port-model-order t))
  ;; Just check for errors that might occur
  (:after-processor test
    (:processors (sort-by-input-model-processor)) (graph)
    (assert-true t)))
