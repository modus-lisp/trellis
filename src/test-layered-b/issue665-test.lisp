;;;; issue665-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue665Test.
;;;; The Java test body is empty.

(in-package #:elk)

(deflayouttest issue665-test
  (:default-configuration)
  (:graph-resources (list (%b-with-name-filter (model-resource-path "tickets/layered/**")
                                               (lambda (name) (%b-name-matches-p name "665" ".elkt")))))
  (:test test-no-exception () (graph)
    nil))
