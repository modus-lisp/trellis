;;;; basic-model-order-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p2layers.BasicModelOrderTest.  Just checks for
;;;; errors that might occur with the model-order cycle breaker and layerers.

(in-package #:elk)

(deflayouttest basic-model-order-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:configurator-provider bf-configuration ()
    (let ((config (new-layout-configurator)))
      (set-property (configure config 'elk-node) layered-options.cycle-breaking-strategy
                    cycle-breaking-strategy.model-order)
      (set-property (configure config 'elk-node) layered-options.layering-strategy
                    layering-strategy.bf-model-order)
      config))
  (:configurator-provider df-configuration ()
    (let ((config (new-layout-configurator)))
      (set-property (configure config 'elk-node) layered-options.cycle-breaking-strategy
                    cycle-breaking-strategy.model-order)
      (set-property (configure config 'elk-node) layered-options.layering-strategy
                    layering-strategy.df-model-order)
      config))
  ;; Just check for errors that might occur.
  (:test test () (graph)
    (assert-true t)))
