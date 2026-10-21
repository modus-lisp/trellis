;;;; issue680-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue680Test.
;;;;
;;;; Java builds the graph in testGraph() and lays it out with
;;;; RecursiveGraphLayoutEngine; here the graph provider builds it and the
;;;; framework lays it out, then the test body reads the nodes back: the
;;;; parent is the root's only child and the child is the parent's only child.

(in-package #:elk)

(defun %issue680-test-graph ()
  (let* ((graph (elk-graph-util.create-graph))
         (parent (elk-graph-util.create-node graph)))
    (set-property graph core-options.algorithm layered-options.algorithm-id)
    (set-property graph layered-options.edge-routing edge-routing.orthogonal)
    (set-property graph layered-options.direction core-direction.down)
    (set-property parent core-options.algorithm layered-options.algorithm-id)
    (set-property parent layered-options.edge-routing edge-routing.orthogonal)
    (set-property parent layered-options.direction core-direction.down)
    (let ((p1 (elk-graph-util.create-port parent))
          (p2 (elk-graph-util.create-port parent)))
      (set-width p1 15)
      (set-height p1 165)
      (set-property p1 layered-options.port-border-offset -20.0d0)
      (set-width p2 15)
      (set-height p2 166)
      (set-property p2 layered-options.port-border-offset -22.0d0)
      (let ((child (elk-graph-util.create-node parent)))
        (set-width child 40.265625d0)
        (set-height child 75.5d0)
        (let ((child-p1 (elk-graph-util.create-port child))
              (child-p2 (elk-graph-util.create-port child)))
          (set-width child-p1 15)
          (set-height child-p1 33)
          (set-property child-p1 layered-options.port-border-offset -8.0d0)
          (set-width child-p2 15)
          (set-height child-p2 34)
          (set-property child-p2 layered-options.port-border-offset -8.0d0)
          (elk-graph-util.create-simple-edge p1 child-p1)
          (elk-graph-util.create-simple-edge child-p2 p2))))
    graph))

(deflayouttest issue680-test
  (:algorithm "org.eclipse.elk.layered")
  (:graph test-graph () (%issue680-test-graph))
  (:test test-positions () (graph)
    (let* ((parent (j-get (get-children graph) 0))
           (child (j-get (get-children parent) 0)))
      (tlc-assert-equals-delta nil 157.0d0 (get-y parent) 1d-5)
      (tlc-assert-equals-delta nil 57.0d0 (get-y child) 1d-5))))
