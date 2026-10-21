;;;; label-side-selector-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.LabelSideSelectorTest.

(in-package #:elk)

(defparameter *label-side-selector-random-seed* 0
  "Fixed random seed to keep tests reproducible.")

(defun %lsst-configurator (graph)
  (let ((node-queue (new-jlist))
        (rand (new-random *label-side-selector-random-seed*))
        (head 0))
    (jl-add node-queue graph)
    (loop while (< head (length node-queue))
          do (let ((node (aref node-queue head)))
               (incf head)
               (jdo (edge (get-contained-edges node))
                 (when (is-empty (get-labels edge))
                   (elk-graph-util.create-label (get-identifier edge) edge))
                 (let* ((label (j-get (get-labels edge) 0))
                        (r (next-double rand)))
                   (set-property label layered-options.edge-labels-placement
                                 (cond ((< r 0.3d0) edge-label-placement.head)
                                       ((< r 0.7d0) edge-label-placement.center)
                                       (t edge-label-placement.tail)))))))))

(deflayouttest label-side-selector-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")
                          (model-resource-path "tickets/layered/599_unimportantPortIndex.elkt")))
  (:configurator configurator (graph)
    (%lsst-configurator graph))
  (:after-processor test-removed-nodes
    (:processors (label-side-selector)) (graph)
    (jdo (layer (get-layers graph))
      (jdo (node (get-nodes layer))
        (jdo (edge (get-outgoing-edges node))
          (jdo (label (get-labels edge))
            (assert-true (not (eq (get-property label layered-internal-properties.label-side)
                                  label-side.unknown)))))))))
