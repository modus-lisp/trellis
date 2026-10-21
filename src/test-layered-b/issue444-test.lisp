;;;; issue444-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue444Test.
;;;; The Java test body is empty: it passes if layout of the graph completes.

(in-package #:elk)

(deflayouttest issue444-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/444_selfLoopsNPE.elkt")))
  (:test test-proper-label-coordinates () (graph)
    nil))
