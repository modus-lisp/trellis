;;;; issue905-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue905Test.

(in-package #:elk)

(defun %issue905-test-begin-label (edge)
  "Labels of the edge must sit at the same location after layout, whatever their
location was before it (only the tail label was wrong in issue 905)."
  (let ((expected-x-tail 10) (expected-y-tail 92)
        (expected-x-center 61) (expected-y-center 91)
        (expected-x-head 124) (expected-y-head 92))
    (jdo (label (get-labels edge))
      (let ((placement (get-property label layered-options.edge-labels-placement)))
        (cond ((eq placement edge-label-placement.tail)
               (tlc-assert-equals-delta "The x location of the tail label is wrong after layout." expected-x-tail (get-x label) 0)
               (tlc-assert-equals-delta "The y location of the tail label is wrong after layout." expected-y-tail (get-y label) 0))
              ((eq placement edge-label-placement.center)
               (tlc-assert-equals-delta "The x location of the center label is wrong after layout." expected-x-center (get-x label) 0)
               (tlc-assert-equals-delta "The y location of the center label is wrong after layout." expected-y-center (get-y label) 0))
              ((eq placement edge-label-placement.head)
               (tlc-assert-equals-delta "The x location of the head label is wrong after layout." expected-x-head (get-x label) 0)
               (tlc-assert-equals-delta "The y location of the head label is wrong after layout." expected-y-head (get-y label) 0)))))))

(deflayouttest issue905-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  ;; 2 identical graph except the location of the labels of the edge
  (:graph-resources (list (model-resource-path "tickets/layered/905_beginLabelAboveEdge.elkt")
                          (model-resource-path "tickets/layered/905_beginLableBellowEdge.elkt")))
  (:test test-edge-label-alignment () (graph)
    (assert-equals "Wrong input graph: The graph must contained one node." 1 (size (get-children graph)))
    (let ((node (j-get (get-children graph) 0)))
      (assert-equals "Wrong input graph: The node of the graph must contained one edge."
                     1 (size (get-contained-edges node)))
      (%issue905-test-begin-label (j-get (get-contained-edges node) 0)))))
