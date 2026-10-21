;;;; long-edge-joiner-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.LongEdgeJoinerTest.

(in-package #:elk)

(deflayouttest long-edge-joiner-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:after-processor test-removed-nodes
    (:processors (long-edge-joiner)) (graph)
    (jdo (layer graph)
      (jdo (node layer)
        (assert-true (not (eq (get-type node) node-type.long-edge)))))))
