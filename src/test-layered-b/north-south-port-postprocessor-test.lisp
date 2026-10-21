;;;; north-south-port-postprocessor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.NorthSouthPortPostprocessorTest.

(in-package #:elk)

(deflayouttest north-south-port-postprocessor-test
  (:algorithm "org.eclipse.elk.layered")
  (:graph-resources (list (model-resource-path "tests/layered/north_south_ports/**")))
  (:after-processor test-all-north-south-port-dummies-removed
    (:processors (north-south-port-postprocessor)) (graph)
    (jdo (layer graph)
      (jdo (node layer)
        (when (eq (get-type node) node-type.north-south-port)
          (fail nil))))))
