;;;; issue548-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue548Test.
;;;; The Java test body is empty.

(in-package #:elk)

(deflayouttest issue548-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/548_NPEWithInsideSelfLoops.elkt")))
  (:test test-large-enough-nodes () (graph)
    nil))
