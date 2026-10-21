;;;; issue476-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue476Test.

(in-package #:elk)

(defparameter %b-476-coordinate-fuzzyness 0.5d0)

(defun %b-476-test-labels (node)
  (let ((node-width (get-width node)))
    (jdo (label (get-labels node))
      (%b-assert-equals-delta (format nil "Label \"~A\" is not centered." (get-text label))
                              (/ node-width 2d0)
                              (+ (get-x label) (/ (get-width label) 2d0))
                              %b-476-coordinate-fuzzyness))))

(deflayouttest issue476-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/476_multiLabelInVerticalLayout.elkt")
                          (model-resource-path "tickets/layered/476_multiLabelInHorizontalLayout.elkt")))
  (:test test-node-label-alignment () (graph)
    (jdo (node (get-children graph))
      (%b-476-test-labels node))))
