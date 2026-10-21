;;;; issue463-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue463Test.
;;;; The Java test body is empty.

(in-package #:elk)

(deflayouttest issue463-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/463_aioobe_with_self_loops.elkt")))
  (:test test-node-label-alignment () (graph)
    nil))
