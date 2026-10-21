;;;; issue445-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue445Test.

(in-package #:elk)

(deflayouttest issue445-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/445_feedbackEdgeTailLabel.elkt")))
  (:configurator configure-padding (graph)
    (set-property graph layered-options.padding (new-elk-padding 0)))
  (:test test-proper-label-coordinates () (graph)
    (let ((labels (new-jlist)))
      (jdo (edge (get-contained-edges graph))
        (jdo (label (get-labels edge))
          (jl-add labels label)))
      (let ((misplaced-labels (count-if (lambda (label)
                                          (and (= (get-x label) 0) (= (get-y label) 0)))
                                        labels)))
        (assert-true (format nil "Found ~D label(s) placed at (0, 0) instead of a proper position."
                             misplaced-labels)
                     (= misplaced-labels 0))))))
