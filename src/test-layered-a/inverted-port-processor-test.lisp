;;;; inverted-port-processor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.InvertedPortProcessorTest.

(in-package #:elk)

(deflayouttest inverted-port-processor-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:configurator-provider configurator ()
    (let ((config (new-layout-configurator)))
      (set-property (configure config 'elk-node) layered-options.edge-routing edge-routing.orthogonal)
      config))
  (:after-processor test-valid-node-order
    (:processors (inverted-port-processor)) (graph)
    (jdo (layer graph)
      (jdo (node layer)
        ;; outgoing edges left
        (jdo (edge (get-outgoing-edges node))
          (when (eq (get-side (get-source edge)) port-side.west)
            (let ((target (get-node (get-target edge))))
              ;; dummy in same layer
              (assert-true (eq (get-type target) node-type.long-edge))
              (assert-true (= (get-index (get-layer target)) (get-index layer))))))
        ;; incoming edges right
        (jdo (edge (get-incoming-edges node))
          (when (eq (get-side (get-target edge)) port-side.east)
            (let ((source (get-node (get-source edge))))
              ;; dummy in same layer
              (assert-true (eq (get-type source) node-type.long-edge))
              (assert-true (= (get-index (get-layer source)) (get-index layer))))))))))
