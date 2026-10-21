;;;; node-promotion-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.NodePromotionTest.
;;;; The three after-processors delegate to BasicLayerAssignmentTest's test methods
;;;; (new BasicLayerAssignmentTest().testX(graph)) as in Java.

(in-package #:elk)

(defun %npt-configurator-for (strategy)
  (let ((config (new-layout-configurator)))
    (set-property (configure config 'elk-node) layered-options.layering-node-promotion-strategy strategy)
    config))

(deflayouttest node-promotion-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:configurator-provider nikolov-configurator ()
    (%npt-configurator-for node-promotion-strategy.nikolov))
  (:configurator-provider nikolov-improved-configurator ()
    (%npt-configurator-for node-promotion-strategy.nikolov-improved))
  (:configurator-provider nikolov-pixel-configurator ()
    (%npt-configurator-for node-promotion-strategy.nikolov-pixel))
  (:configurator-provider nikolov-improved-pixel-configurator ()
    (%npt-configurator-for node-promotion-strategy.nikolov-improved-pixel))
  (:configurator-provider no-boundary-configurator ()
    (%npt-configurator-for node-promotion-strategy.no-boundary))
  (:configurator-provider model-order-left-to-right ()
    (let ((config (%npt-configurator-for node-promotion-strategy.model-order-left-to-right)))
      (set-property (configure config 'elk-node) layered-options.cycle-breaking-strategy
                    cycle-breaking-strategy.model-order)
      (set-property (configure config 'elk-node) layered-options.layering-strategy
                    layering-strategy.longest-path-source)
      (set-property (configure config 'elk-node) layered-options.consider-model-order-strategy
                    ordering-strategy.prefer-edges)
      config))
  (:configurator-provider model-order-right-to-left ()
    (let ((config (%npt-configurator-for node-promotion-strategy.model-order-right-to-left)))
      (set-property (configure config 'elk-node) layered-options.cycle-breaking-strategy
                    cycle-breaking-strategy.model-order)
      (set-property (configure config 'elk-node) layered-options.layering-strategy
                    layering-strategy.longest-path)
      (set-property (configure config 'elk-node) layered-options.consider-model-order-strategy
                    ordering-strategy.prefer-edges)
      config))
  (:after-processor test-no-layerless-nodes
    (:processors (node-promotion)) (graph)
    (test-no-layerless-nodes (new-basic-layer-assignment-test) graph))
  (:after-processor test-no-empty-layers
    (:processors (node-promotion)) (graph)
    (test-no-empty-layers (new-basic-layer-assignment-test) graph))
  (:after-processor test-edges-point-towards-next-layers
    (:processors (node-promotion)) (graph)
    (test-edges-point-towards-next-layers (new-basic-layer-assignment-test) graph)))
