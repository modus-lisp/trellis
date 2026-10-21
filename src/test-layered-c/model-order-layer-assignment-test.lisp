;;;; model-order-layer-assignment-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p2layers.ModelOrderLayerAssignmentTest.
;;;;
;;;; Plain JUnit.  Each Java test builds the same shape of graph (n square
;;;; 30x30 nodes with labels, a fixed set of edges, LEFT-to-RIGHT by default),
;;;; lays it out with LayeredLayoutProvider and checks every node's coordinates.
;;;; The graph description and the expected coordinates are passed as data to
;;;; %MOLAT-CHECK; each Java test is one call to it.

(in-package #:elk)

(defun %molat-check (name-count layering promotion edges expected)
  "Build NAME-COUNT nodes (labelled n1..nN) with EDGES, a list of (from to)
1-based index pairs, lay the graph out with LAYERING and PROMOTION, and check
each node's (x y) against EXPECTED, a list of (x y) in node order."
  (let* ((parent (elk-graph-util.create-graph))
         (nodes (loop repeat name-count
                      collect (let ((n (elk-graph-util.create-node parent)))
                                (set-dimensions n 30 30)
                                n))))
    (dolist (n nodes)
      (elk-graph-util.create-label (format nil "n~D" (1+ (position n nodes))) n))
    (set-property parent core-options.algorithm layered-options.algorithm-id)
    (set-property parent core-options.direction core-direction.right)
    (set-property parent layered-options.consider-model-order-strategy ordering-strategy.prefer-edges)
    (set-property parent layered-options.layering-strategy layering)
    (set-property parent layered-options.layering-node-promotion-strategy promotion)
    (set-property parent core-options.padding (new-elk-padding 0.0d0))
    (set-property parent core-options.spacing-node-node 10.0d0)
    (set-property parent layered-options.spacing-node-node-between-layers 20.0d0)
    (dolist (e edges)
      (elk-graph-util.create-simple-edge (nth (1- (first e)) nodes) (nth (1- (second e)) nodes)))
    (with-java-floats (layout (new-layered-layout-provider) parent (new-basic-progress-monitor)))
    (loop for n in nodes
          for (x y) in expected
          for i from 1
          do (tlc-assert-equals-delta (format nil "n~D on wrong x coordinate" i) x (get-x n) 0.1d0)
             (tlc-assert-equals-delta (format nil "n~D on wrong y coordinate" i) y (get-y n) 0.1d0))))

(defmacro %molat-test (name &rest args)
  `(defplaintest model-order-layer-assignment-test ,name ()
     (%molat-check ,@args)))

(%molat-test test-forward-promotion-disregards-edge-length
  5 layering-strategy.longest-path-source node-promotion-strategy.model-order-left-to-right
  '((1 2) (1 4) (1 4) (2 3) (3 5) (4 5))
  '((0 7.5d0) (60 0) (110 0) (110 40) (160 5)))

(%molat-test test-backward-promotion-disregards-edge-length
  5 layering-strategy.longest-path node-promotion-strategy.model-order-right-to-left
  '((1 2) (1 3) (2 5) (2 5) (3 4) (4 5))
  '((0 5) (50 0) (50 40) (100 40) (150 6)))

(%molat-test test-forward-promotion-more-nodes
  6 layering-strategy.longest-path-source node-promotion-strategy.model-order-left-to-right
  '((1 2) (1 4) (1 4) (1 5) (2 3) (3 6) (4 6) (5 6))
  '((0 9) (70 0) (120 0) (120 40) (120 80) (170 40)))

(%molat-test test-backward-promotion-more-nodes
  6 layering-strategy.longest-path node-promotion-strategy.model-order-right-to-left
  '((1 2) (1 3) (1 4) (2 6) (2 6) (3 6) (4 5) (5 6))
  '((0 7.5d0) (60 0) (60 40) (60 80) (110 80) (170 9)))

(%molat-test test-forward-promotion-no-end-node
  4 layering-strategy.longest-path-source node-promotion-strategy.model-order-left-to-right
  '((1 2) (1 4) (1 4) (2 3))
  '((0 7.5d0) (60 0) (110 0) (110 40)))

(%molat-test test-backward-promotion-no-end-node
  4 layering-strategy.longest-path node-promotion-strategy.model-order-right-to-left
  '((1 4) (1 4) (2 3) (3 4))
  '((0 0) (0 40) (50 40) (100 6)))

(%molat-test test-backward-promotion-no-end-node2
  4 layering-strategy.longest-path node-promotion-strategy.model-order-right-to-left
  '((1 2) (1 3) (3 4))
  '((0 5) (50 0) (50 40) (100 40)))
