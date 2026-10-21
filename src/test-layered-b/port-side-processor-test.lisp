;;;; port-side-processor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.PortSideProcessorTest.

(in-package #:elk)

(deflayouttest port-side-processor-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%b-with-ext (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:after-processor test-node-constraints
    (:processors (port-side-processor)) (graph)
    (jdo (layer graph)
      (jdo (node layer)
        (assert-true (is-side-fixed (get-property node layered-options.port-constraints))))))
  (:after-processor test-port-sides
    (:processors (port-side-processor)) (graph)
    (jdo (layer graph)
      (jdo (node layer)
        (jdo (port (get-ports node))
          (assert-true (not (eq (get-side port) port-side.undefined))))))))
