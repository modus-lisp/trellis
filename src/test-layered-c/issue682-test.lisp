;;;; issue682-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue682Test.
;;;;
;;;; JUnit's Parameterized runner (one run per direction) is four graph
;;;; providers, one per direction.  The Java testGraph() is the graph provider;
;;;; the parent is the root's only child and the label the parent's only label.

(in-package #:elk)

(defun %issue682-test-graph (direction)
  (let ((graph (elk-graph-util.create-graph)))
    (set-property graph core-options.algorithm layered-options.algorithm-id)
    (set-property graph layered-options.edge-routing edge-routing.orthogonal)
    (set-property graph layered-options.direction direction)
    (set-property graph layered-options.node-labels-padding (new-elk-padding 21 32 43 54))
    (let ((parent (elk-graph-util.create-node graph)))
      (set-property parent layered-options.node-size-constraints
                    (enum-set.of size-constraint.node-labels))
      (set-property parent layered-options.node-labels-placement (node-label-placement.inside-top-center))
      (let ((label (elk-graph-util.create-label "foobar" parent)))
        (set-width label 23)  ; Arbitrary
        (set-height label 22)))
    graph))

(deflayouttest issue682-test
  (:algorithm "org.eclipse.elk.layered")
  (:graph right () (%issue682-test-graph core-direction.right))
  (:graph down () (%issue682-test-graph core-direction.down))
  (:graph up () (%issue682-test-graph core-direction.up))
  (:graph left () (%issue682-test-graph core-direction.left))
  (:test test () (graph)
    (let* ((parent (j-get (get-children graph) 0))
           (label (j-get (get-labels parent) 0)))
      ;; Label position
      (tlc-assert-equals-delta nil 54.0d0 (get-x label) 1d-5)
      (tlc-assert-equals-delta nil 21.0d0 (get-y label) 1d-5)
      ;; Node dimension
      (tlc-assert-equals-delta nil (+ (get-x label) (get-width label) 32) (get-width parent) 1d-5)
      ;; TODO the following line fails, it looks like the label's height is applied twice.
      ;; (tlc-assert-equals-delta nil (+ (get-y label) (get-height label) 43) (get-height parent) 1d-5)
      )))
