;;;; issue596-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue596Test.

(in-package #:elk)

(defparameter %b-596-tolerance 0.5d0)

(deflayouttest issue596-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/596_outsideHierarchicalPortLabels.elkt")))
  (:test test-edge-starts-at-port () (graph)
    (let* ((outer (j-get (get-children graph) 0))
           (inner (j-get (get-children outer) 0)))
      (%b-assert-equals-delta nil
                              (/ (- (get-width outer) (get-width inner)) 2d0)
                              (get-x inner)
                              %b-596-tolerance))))
