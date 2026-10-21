;;;; issue587-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue587Test.

(in-package #:elk)

(deflayouttest issue587-test
  (:default-configuration)
  (:graph-resources (list (%b-with-name-filter (model-resource-path "tickets/layered/**")
                                               (lambda (name) (%b-name-matches-p name "587" ".elkt")))))
  (:configurator configure-orthogonal-edge-routing (graph)
    (set-property graph layered-options.edge-routing edge-routing.orthogonal))
  (:configurator configure-polyline-edge-routing (graph)
    (set-property graph layered-options.edge-routing edge-routing.polyline))
  (:configurator configure-spline-edge-routing (graph)
    (set-property graph layered-options.edge-routing edge-routing.splines))
  (:test test-no-exception () (graph)
    nil))
