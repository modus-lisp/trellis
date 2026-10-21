;;;; partition-post-processor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.PartitionPostProcessorTest.

(in-package #:elk)

(deflayouttest partition-post-processor-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tests/layered/partitioning/**/")
                          (model-resource-path "tickets/layered/656_brokenLayoutPartitions.elkt")))
  (:after-processor no-empty-layer-test
    (:processors (partition-postprocessor)) (graph)
    (jdo (layer graph)
      (assert-false (is-empty (get-nodes layer)))))
  (:after-processor test-partition-order
    (:processors (partition-postprocessor)) (graph)
    (let ((last-partition -1))
      (jdo (layer graph)
        ;; Set to invalid partition
        (let ((current-partition -1))
          (jdo (node layer)
            (when (and (eq (get-type node) node-type.normal)
                       (has-property node layered-options.partitioning-partition))
              (let ((node-partition (get-property node layered-options.partitioning-partition)))
                (if (= current-partition -1)
                    (setf current-partition node-partition)
                    (assert-equals current-partition node-partition)))))
          (assert-true (<= last-partition current-partition))
          (setf last-partition current-partition))))))
