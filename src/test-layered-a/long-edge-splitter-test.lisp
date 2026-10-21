;;;; long-edge-splitter-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.LongEdgeSplitterTest.
;;;; NOTE: src/test-layered-pilot/long-edge-splitter-test.lisp already ports this
;;;; class under the same name (the pilot ran first); this file replaces it on load
;;;; with the same test.

(in-package #:elk)

(deflayouttest long-edge-splitter-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter
                           (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:after-processor test-edges-between-adjacent-layers
    (:processors (long-edge-splitter)) (graph)
    (let ((next-layer-id 0))
      (jdo (layer graph)
        (setf (.id layer) next-layer-id)
        (incf next-layer-id)))
    (jdo (layer graph)
      (jdo (node layer)
        (jdo (edge (get-outgoing-edges node))
          (let ((target-layer (get-layer (get-node (get-target edge)))))
            (assert-true (< (.id layer) (.id target-layer)))
            (assert-true (= (+ (.id layer) 1) (.id target-layer)))))))))
