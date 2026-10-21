;;;; issue471-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue471Test.

(in-package #:elk)

(defparameter %b-471-coordinate-fuzzyness 0.5d0)

(defun %b-471-test-labels (edge)
  (let ((min-x double.max-value)
        (max-width 0d0))
    (jdo (label (get-labels edge))
      (setf min-x (min min-x (get-x label))
            max-width (max max-width (get-width label))))
    (jdo (label (get-labels edge))
      (%b-assert-equals-delta nil
                              (+ min-x (/ (- max-width (get-width label)) 2d0))
                              (get-x label)
                              %b-471-coordinate-fuzzyness))))

(deflayouttest issue471-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/471_verticalInlineLabelAlignment.elkt")))
  (:test test-edge-label-alignment () (graph)
    (jdo (edge (get-contained-edges graph))
      (%b-471-test-labels edge))))
