;;;; issue870-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue870Test.
;;;; The Java test body is empty: the test passes when the layout does not fail.

(in-package #:elk)

(deflayouttest issue870-test
  (:algorithm "org.eclipse.elk.layered")
  (:graph-resources (list (model-resource-path "tickets/layered/870_networkSimplexNodePlacer.elkt")))
  (:test ensure-layout-does-not-fail () (graph)
    nil))
