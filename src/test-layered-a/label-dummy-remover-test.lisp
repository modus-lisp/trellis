;;;; label-dummy-remover-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.LabelDummyRemoverTest.

(in-package #:elk)

(deflayouttest label-dummy-remover-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration :edge-labels t)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:after-processor test-removed-nodes
    (:processors (label-dummy-remover)) (graph)
    (jdo (layer graph)
      (jdo (node (get-nodes layer))
        (assert-true (not (eq (get-type node) node-type.label)))))))
