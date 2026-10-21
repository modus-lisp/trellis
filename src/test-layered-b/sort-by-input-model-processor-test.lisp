;;;; sort-by-input-model-processor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.SortByInputModelProcessorTest.

(in-package #:elk)

(deflayouttest sort-by-input-model-processor-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tests/layered/preserve_order/**")))
  (:configurator-provider preserve-order-configurator ()
    (let ((config (new-layout-configurator)))
      (set-property (configure config 'elk-node)
                    layered-options.crossing-minimization-strategy
                    crossing-minimization-strategy.layer-sweep)
      (set-property (configure config 'elk-node)
                    layered-options.consider-model-order-strategy
                    ordering-strategy.nodes-and-edges)
      config))
  (:after-processor preserve-node-and-edge-order
    (:processors (layer-sweep-crossing-minimizer)) (graph)
    (let ((layer-index 0))
      (jdo (layer graph)
        (let ((node-index 0))
          (jdo (node (get-nodes layer))
            (when (eq (get-type node) node-type.normal)
              (assert-equals "A node is on an unexpected position."
                             (format nil "n_l~Dp~D" layer-index node-index)
                             (to-string node)))
            (incf node-index)))
        (incf layer-index)))))
