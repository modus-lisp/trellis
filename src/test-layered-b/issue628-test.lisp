;;;; issue628-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue628Test.

(in-package #:elk)

(deflayouttest issue628-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration :nodes nil)
  (:graph-resources (list (%b-with-name-filter (model-resource-path "tickets/layered/**")
                                               (lambda (name) (%b-name-matches-p name "628" ".elkt")))))
  (:configurator configure-greedy-switch-hierarchical (graph)
    (set-property graph layered-options.crossing-minimization-greedy-switch-hierarchical-type
                  greedy-switch-type.two-sided))
  (:configurator unconfigure-greedy-switch-hierarchical (graph)
    (set-property graph layered-options.crossing-minimization-greedy-switch-hierarchical-type nil))
  (:configurator configure-greedy-switch (graph)
    (set-property graph layered-options.crossing-minimization-greedy-switch-type
                  greedy-switch-type.two-sided))
  (:configurator unconfigure-greedy-switch (graph)
    (set-property graph layered-options.crossing-minimization-greedy-switch-type nil))
  (:test test-no-label-node-overlaps () (graph)
    (let* ((l1 (j-get (get-children graph) 0))
           (l2-1 (%b-element-with-id (get-children l1) "l2_1"))
           (l2-2 (%b-element-with-id (get-children l1) "l2_2")))
      (assert-true "Y-coordinates not equal."
                   (%b-fuzzy-equals (get-y l2-1) (get-y l2-2) 1d-5))
      (%b-assert-equals-delta "Spacing between the nodes not as expected."
                              10d0
                              (- (get-x l2-2) (+ (get-x l2-1) (get-width l2-1)))
                              1d-5))))
