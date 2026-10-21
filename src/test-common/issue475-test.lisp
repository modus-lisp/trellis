;;;; issue475-test.lisp -- port of org.eclipse.elk.alg.common.issues.Issue475Test.

(in-package #:elk)

(defparameter *issue475-size* 20d0
  "Default compound node size.")

(defun %issue475-do-test-compound-node-size (node)
  (if (get-property node fixed-layouter-options.node-size-fixed-graph-size)
      (progn
        ;; The node's size should not have changed
        (%assert-equals-delta *issue475-size* (get-width node) 0d0)
        (%assert-equals-delta *issue475-size* (get-height node) 0d0))
      (let ((max-x 0d0) (max-y 0d0))
        (jdo (child (get-children node))
          (setf max-x (math.max max-x (+ (get-x child) (get-width child))))
          (setf max-y (math.max max-y (+ (get-y child) (get-height child)))))
        (assert-true (>= (get-width node) max-x))
        (assert-true (>= (get-height node) max-y)))))

(deflayouttest issue475-test
  (:algorithm "org.eclipse.elk.fixed")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/core/475_fixedLayoutWithLongLabels.elkt")))
  (:configurator configure-fixed-graph-size (graph)
    (jdo (child (get-children graph))
      ;; Make sure we know the size the node shall have
      (set-property child fixed-layouter-options.node-size-fixed-graph-size t)
      (set-height child *issue475-size*)
      (set-width child *issue475-size*)))
  (:configurator configure-no-fixed-graph-size (graph)
    (jdo (node (get-children graph))
      (set-property node fixed-layouter-options.node-size-fixed-graph-size nil)))
  (:test test-compound-node-size () (graph)
    (jdo (node (get-children graph))
      (%issue475-do-test-compound-node-size node))))
