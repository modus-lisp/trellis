;;;; layer-size-and-graph-heigth-calculator-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.LayerSizeAndGraphHeigthCalculatorTest.

(in-package #:elk)

(deflayouttest layer-size-and-graph-heigth-calculator-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:after-processor test-all-nodes-contained
    (:processors (layer-size-and-graph-height-calculator)) (graph)
    (let* ((tolerance 0.1d0)
           (top (- 0d0 (.y (get-offset graph))))
           (bottom (- (.y (get-size graph)) (.y (get-offset graph)))))
      (jdo (layer graph)
        (jdo (node layer)
          (let ((node-top (- (.y (get-position node)) (.top (get-margin node)))))
            (assert-true (or (> node-top top) (< (abs (- node-top top)) tolerance))))
          (let ((node-bottom (+ (.y (get-position node)) (.y (get-size node))
                                (.bottom (get-margin node)))))
            (assert-true (or (< node-bottom bottom) (< (abs (- node-bottom bottom)) tolerance)))))))))
