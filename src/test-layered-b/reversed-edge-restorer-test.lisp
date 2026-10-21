;;;; reversed-edge-restorer-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.ReversedEdgeRestorerTest.

(in-package #:elk)

(deflayouttest reversed-edge-restorer-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%b-with-ext (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:after-processor test-removed-nodes
    (:processors (reversed-edge-restorer)) (graph)
    (jdo (layer graph)
      (jdo (node layer)
        (jdo (edge (get-outgoing-edges node))
          (assert-false (get-property edge layered-internal-properties.reversed)))))))
