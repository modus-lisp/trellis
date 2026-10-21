;;;; issue455-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue455Test.

(in-package #:elk)

(defparameter %b-455-coordinate-fuzzyness 0.5d0)

(defun %b-455-do-test (node)
  (let ((xs (new-jlist)))
    (jdo (child (get-children node))
      (jl-add xs (get-x child)))
    ;; An empty DoubleSummaryStatistics has max - min = -Infinity, which passes.
    (when (plusp (length xs))
      (assert-true "Child nodes are not placed in the same layer."
                   (<= (- (reduce (lambda (a b) (max a b)) xs)
                          (reduce (lambda (a b) (min a b)) xs))
                       %b-455-coordinate-fuzzyness)))))

(deflayouttest issue455-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/455_longHierarchicalEdges.elkt")
                          (model-resource-path "tickets/layered/455_shortHierarchicalEdges.elkt")))
  (:test test-child-nodes-are-in-same-layer () (graph)
    (jdo (child (get-children graph))
      (when (is-hierarchical child)
        (%b-455-do-test child)))))
