;;;; node-margin-calculator-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.NodeMarginCalculatorTest.

(in-package #:elk)

(defun %nmct-test-node-margins (graph)
  (jdo (layer graph)
    (jdo (node layer)
      (assert-true (not (null (get-margin node))))
      (assert-true (>= (.top (get-margin node)) 0))
      (assert-true (>= (.right (get-margin node)) 0))
      (assert-true (>= (.bottom (get-margin node)) 0))
      (assert-true (>= (.left (get-margin node)) 0)))))

(deflayouttest node-margin-calculator-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:after-processor test-node-margins-after-innermost-node-margin-calculator
    (:processors (innermost-node-margin-calculator)) (graph)
    (%nmct-test-node-margins graph))
  (:after-processor test-node-margins-after-self-loop-router
    (:processors (self-loop-router)) (graph)
    (%nmct-test-node-margins graph))
  (:after-processor test-node-margins-after-comment-node-margin-calculator
    (:processors (comment-node-margin-calculator)) (graph)
    (%nmct-test-node-margins graph))
  (:after-processor test-node-margins-after-end-label-preprocessor
    (:processors (end-label-preprocessor)) (graph)
    (%nmct-test-node-margins graph)))
