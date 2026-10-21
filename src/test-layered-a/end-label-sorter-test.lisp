;;;; end-label-sorter-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.EndLabelSorterTest.

(in-package #:elk)

(defun %els-fail-if-unsorted (strings)
  "Asserts that each string is <= its successor (String.compareTo)."
  (when (is-empty strings)
    (return-from %els-fail-if-unsorted nil))
  (let ((items (coerce (jiterable-items strings) 'list)))
    (loop for (prev curr) on items
          while curr
          do (assert-true (not (string> prev curr))))))

(defun %els-label-texts (labels)
  (mapcar (jfun get-text 0) (coerce (jiterable-items labels) 'list)))

(defun %els-test-node (node)
  (let ((type (get-type node)))
    (cond
      ((eq type node-type.normal)
       ;; Build a list of label strings that we can check
       (when (has-property node layered-internal-properties.end-labels)
         (jdo (label-cell (jm-values (get-property node layered-internal-properties.end-labels)))
           (%els-fail-if-unsorted (%els-label-texts (get-labels label-cell))))))
      ((eq type node-type.label)
       (let ((labels (get-property node layered-internal-properties.represented-labels)))
         (assert-not-null labels)
         (%els-fail-if-unsorted (mapcar (jfun get-text 0) (coerce (jiterable-items labels) 'list))))))))

(deflayouttest end-label-sorter-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tests/layered/labels_ordering/**/")))
  (:after-processor test-correct-order
    (:processors (end-label-sorter)) (graph)
    (jdo (layer (get-layers graph))
      (jdo (node (get-nodes layer))
        (%els-test-node node)))))
