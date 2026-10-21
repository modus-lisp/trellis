;;;; layer-constraint-processor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.LayerConstraintProcessorTest.

(in-package #:elk)

;;; Guava HashMultimap: a set of values per key.  A key with no values is absent,
;;; and get() on it is the empty collection.

(defun %lcpt-multimap-put (map key value)
  (let ((values (gethash key map)))
    (unless (member value values)
      (setf (gethash key map) (append values (list value))))))

(defun %lcpt-multimap-get (map key)
  (gethash key map))

(defun %lcpt-multimap-contains-key (map key)
  (and (nth-value 1 (gethash key map)) (not (null (gethash key map)))))

(defun %lcpt-contains-only (values element)
  "firstSep.size() == 1 && firstSep.contains(element)"
  (and (= (length values) 1) (member element values)))

(deflayouttest layer-constraint-processor-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tests/layered/layer_constraints/**/")))
  (:after-processor test-layer-constraints
    (:processors (layer-constraint-postprocessor)) (graph)
    (let ((constraint-map (make-hash-table :test 'eql))
          (next-layer-id 0))
      (jdo (layer graph)
        (setf (.id layer) next-layer-id)
        (incf next-layer-id)
        (jdo (node layer)
          (%lcpt-multimap-put constraint-map
                              (get-property node layered-options.layering-layer-constraint)
                              (.id layer))))
      (flet ((get-c (c) (%lcpt-multimap-get constraint-map c)))
        (if (%lcpt-multimap-contains-key constraint-map layer-constraint.first-separate)
            (let ((first-sep (get-c layer-constraint.first-separate)))
              (assert-true (%lcpt-contains-only first-sep 0))
              (assert-true (not (member 0 (get-c layer-constraint.none))))
              (let ((first (get-c layer-constraint.first)))
                (when first
                  (assert-true (%lcpt-contains-only first 1)))))
            (let ((first (get-c layer-constraint.first)))
              (when first
                (assert-true (%lcpt-contains-only first 0)))))
        (let ((last-layer (- (length (get-layers graph)) 1)))
          (if (%lcpt-multimap-contains-key constraint-map layer-constraint.last-separate)
              (let ((last-sep (get-c layer-constraint.last-separate)))
                (assert-true (%lcpt-contains-only last-sep last-layer))
                (assert-true (not (member last-layer (get-c layer-constraint.none))))
                (let ((last (get-c layer-constraint.last)))
                  (when last
                    (assert-true (%lcpt-contains-only last (- last-layer 1))))))
              (let ((last (get-c layer-constraint.last)))
                (when last
                  (assert-true (%lcpt-contains-only last last-layer)))))))))

  (:after-processor no-empty-layers
    (:processors (layer-constraint-postprocessor)) (graph)
    (jdo (layer (get-layers graph))
      (assert-true (not (is-empty (get-nodes layer)))))))

