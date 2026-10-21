;;;; issue-457-test.lisp -- port of org.eclipse.elk.core.issues.Issue457Test.

(in-package #:elk)

(defparameter *issue-457-min-size* 60)

(deflayouttest issue-457-test
  (:algorithm "org.eclipse.elk.box")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/core/457_boxLayouterIgnoresMinimumSize.elkt")))
  (:configurator configure-min-size (graph)
    (jdo (node (get-children graph))
      (set-property node box-layouter-options.node-size-constraints (size-constraint.minimum-size))
      (set-property node box-layouter-options.node-size-minimum
                    (new-k-vector *issue-457-min-size* *issue-457-min-size*))
      (jdo (child (get-children node))
        (set-dimensions child 5 5))))
  (:test test-proper-label-coordinates () (graph)
    (jdo (node (get-children graph))
      (assert-near *issue-457-min-size* (get-width node) 1)
      (assert-near *issue-457-min-size* (get-height node) 1))))
