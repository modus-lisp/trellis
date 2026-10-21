;;;; basic-crossing-minimization-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p3order.BasicCrossingMinimizationTest.

(in-package #:elk)

;; Java: Map<LGraph, Multimap<Layer, LNode>> layerAssignments (an instance field,
;; so per test instance).  Here, one table for the class: each graph is removed
;; again when it is verified.
(defvar *bcm-layer-assignments* (make-hash-table :test 'eq))

(defun %bcm-record-layer-assignment (lgraph)
  (let ((assignment (make-hash-table :test 'eq)))
    (jdo (layer lgraph)
      (jdo (node layer)
        (push node (gethash layer assignment))))
    (setf (gethash lgraph *bcm-layer-assignments*) assignment)))

(defun %bcm-verify-layer-assignment (lgraph)
  (let ((assignment (gethash lgraph *bcm-layer-assignments*)))
    (remhash lgraph *bcm-layer-assignments*)
    (jdo (layer lgraph)
      (jdo (node layer)
        ;; node in the same layer?
        (assert-true (member node (gethash layer assignment)))))))

(deflayouttest basic-crossing-minimization-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tests/**/")))
  (:configurator-provider layer-sweep-configurator ()
    (let ((config (new-layout-configurator)))
      (set-property (configure config 'elk-node) layered-options.crossing-minimization-strategy
                    crossing-minimization-strategy.layer-sweep)
      config))
  (:before-processor ensure-layer-assignment-unchanged (:processors (layer-sweep-crossing-minimizer)) (graph)
    (if (gethash graph *bcm-layer-assignments*)
        (%bcm-verify-layer-assignment graph)
        (%bcm-record-layer-assignment graph)))
  (:after-processor ensure-layer-assignment-unchanged (:processors (layer-sweep-crossing-minimizer)) (graph)
    (if (gethash graph *bcm-layer-assignments*)
        (%bcm-verify-layer-assignment graph)
        (%bcm-record-layer-assignment graph))))
