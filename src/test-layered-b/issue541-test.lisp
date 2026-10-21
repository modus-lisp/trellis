;;;; issue541-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue541Test.
;;;; The Java test body is empty.

(in-package #:elk)

(deflayouttest issue541-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%b-with-name-filter (model-resource-path "tickets/layered/**")
                                               (lambda (name) (%b-name-matches-p name "541" "")))))
  (:test test-large-enough-nodes () (graph)
    nil))
