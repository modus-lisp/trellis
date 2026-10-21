;;;; issue-489-test.lisp -- port of org.eclipse.elk.core.issues.Issue489Test.

(in-package #:elk)

(defparameter *issue-489-padding* 5)
(defparameter *issue-489-tolerance* 0.1d0)

(deflayouttest issue-489-test
  (:algorithm "org.eclipse.elk.box")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/core/489_paddingAndMinSize_Small.elkt")
                          (model-resource-path "tickets/core/489_paddingAndMinSize_Large.elkt")))
  (:configurator configure-padding (graph)
    (jdo (node (get-children graph))
      (set-property node box-layouter-options.padding (new-elk-padding *issue-489-padding*))))
  (:test test-proper-child-placement () (graph)
    (let ((pad *issue-489-padding*) (tol *issue-489-tolerance*))
      (jdo (node (get-children graph))
        (assert-equals 1 (size (get-children node)))
        (let ((child (j-get (get-children node) 0)))
          (assert-near pad (get-x child) tol)
          (assert-near pad (get-y child) tol)
          (assert-near (- (get-width node) (* 2 pad)) (get-width child) tol)
          (assert-near (- (get-height node) (* 2 pad)) (get-height child) tol))))))
