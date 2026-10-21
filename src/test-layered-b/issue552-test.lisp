;;;; issue552-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue552Test.

(in-package #:elk)

(deflayouttest issue552-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/552_hierarchySelfLoopPorts.elkt")))
  (:test test-self-loop-port-placement () (graph)
    (jdo (node (get-children graph))
      (jdo (port (get-ports node))
        (assert-true (> (get-y port) 0))))))
