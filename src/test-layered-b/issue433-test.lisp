;;;; issue433-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue433Test.

(in-package #:elk)

(deflayouttest issue433-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/433_graphSizeWithSelfLoopLabel.elkt")))
  (:test test-proper-label-coordinates () (graph)
    (let* ((graph-rect (list 0d0 0d0 (get-width graph) (get-height graph)))
           (label-rect (block found
                         (jdo (edge (get-contained-edges graph))
                           (jdo (label (get-labels edge))
                             (return-from found
                               (list (get-x label) (get-y label)
                                     (get-width label) (get-height label)))))
                         nil)))
      (unless label-rect
        (error "NoSuchElementException: no contained edge label"))
      (assert-true (format nil "Self loop label is not entirely inside the graph.~%Graph rect: ~A~%Label rect: ~A"
                           graph-rect label-rect)
                   (%b-rect2d-contains-rect-p graph-rect label-rect)))))
