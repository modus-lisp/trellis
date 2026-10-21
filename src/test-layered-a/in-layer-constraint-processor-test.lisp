;;;; in-layer-constraint-processor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.InLayerConstraintProcessorTest.

(in-package #:elk)

(defparameter *in-layer-constraint-random-seed* 0
  "Fixed seed to keep tests reproducible.")

(defun %ilcpt-configure-random-in-layer-constraints (graph)
  (let ((node-queue (jlist-from (get-children graph)))
        (rand (new-random *in-layer-constraint-random-seed*))
        (head 0))
    ;; Deque<ElkNode> used as a FIFO: poll from the front, addAll at the back.
    (loop while (< head (length node-queue))
          do (let ((node (aref node-queue head)))
               (incf head)
               (jdo (child (get-children node)) (jl-add node-queue child))
               (let ((r (next-double rand)))
                 (set-property node layered-internal-properties.in-layer-constraint
                               (cond ((< r 0.2d0) in-layer-constraint.top)
                                     ((< r 0.8d0) in-layer-constraint.none)
                                     (t in-layer-constraint.bottom))))))))

(deflayouttest in-layer-constraint-processor-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:configurator configure-random-in-layer-constraints (graph)
    (%ilcpt-configure-random-in-layer-constraints graph))
  (:after-processor test-valid-node-order
    (:processors (in-layer-constraint-processor)) (graph)
    (jdo (layer graph)
      (let ((last-constraint nil))
        (jdo (node layer)
          (let ((current-constraint (get-property node layered-internal-properties.in-layer-constraint)))
            (when (and last-constraint (not (eq current-constraint last-constraint)))
              ;; if the value changes check valid transitions
              (let ((error (format nil "Invalid constraint transition: ~A -> ~A"
                                   (name last-constraint) (name current-constraint))))
                (cond ((eq current-constraint in-layer-constraint.none)
                       (assert-true/1 error (eq last-constraint in-layer-constraint.top)))
                      ((eq current-constraint in-layer-constraint.bottom)
                       (assert-true/1 error (or (eq last-constraint in-layer-constraint.top)
                                                (eq last-constraint in-layer-constraint.none)))))))
            (setf last-constraint current-constraint)))))))
