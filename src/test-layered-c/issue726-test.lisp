;;;; issue726-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue726Test
;;;; (extends TestGraphCreator in Java; it uses none of its members, so the
;;;; creator is not instantiated here).

(in-package #:elk)

(deflayouttest issue726-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/726_edgesMissingBendPoints.elkt")))
  (:after-processor test-orthogonal-edges (:processors (orthogonal-edge-router)) (graph)
    (jdo (layer (get-layers graph))
      (jdo (node (get-nodes layer))
        (jdo (edge (get-outgoing-edges node))
          (%issue726-test-orthogonal-edge edge)))))
  )

(defun %issue726-test-orthogonal-edge (edge)
  ;; Assemble edge route
  (let ((route (new-k-vector-chain/0)))
    (add route (get-absolute-anchor (get-source edge)))
    (jdo (point (get-bend-points edge)) (add route point))
    (add route (get-absolute-anchor (get-target edge)))
    (basic-edge-router-test.check-edge-route-is-orthogonal edge route)))
