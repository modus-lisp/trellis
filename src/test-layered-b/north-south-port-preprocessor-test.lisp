;;;; north-south-port-preprocessor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.NorthSouthPortPreprocessorTest.

(in-package #:elk)

(defun %b-fail-on-north-south-port-connections (l-node)
  (jdo (l-port (get-ports l-node))
    (when (and (contains port-side.sides-north-south (get-side l-port))
               (not (is-empty (get-connected-edges l-port))))
      (fail nil))))

(deflayouttest north-south-port-preprocessor-test
  (:algorithm "org.eclipse.elk.layered")
  (:graph-resources (list (model-resource-path "tests/layered/north_south_ports/**")))
  (:after-processor test-isolated-north-south-ports
    (:processors (north-south-port-preprocessor)) (graph)
    (jdo (layer graph)
      (jdo (node layer)
        (when (is-side-fixed (get-property node layered-options.port-constraints))
          (%b-fail-on-north-south-port-connections node))))))
