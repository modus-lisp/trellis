;;;; issue447-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue447Test.

(in-package #:elk)

(defparameter %b-447-port-size 10d0)
(defparameter %b-447-port-anchor 5d0)
(defparameter %b-447-coordinate-fuzzyness 0.5d0)

(defun %b-447-test-edge-end-point (containing-node port x y)
  (let* ((port-anchor (new-k-vector (+ (get-x port) %b-447-port-anchor)
                                    (+ (get-y port) %b-447-port-anchor)))
         (absolute-port-anchor (elk-util.to-absolute port-anchor (get-parent port)))
         (end-point (new-k-vector x y))
         (absolute-end-point (elk-util.to-absolute end-point containing-node)))
    (%b-assert-equals-delta "X coordinate of edge does not match X coordinate of port anchor"
                            (.x absolute-port-anchor) (.x absolute-end-point)
                            %b-447-coordinate-fuzzyness)
    (%b-assert-equals-delta "Y coordinate of edge does not match Y coordinate of port anchor"
                            (.y absolute-port-anchor) (.y absolute-end-point)
                            %b-447-coordinate-fuzzyness)))

(defun %b-447-test-edge (edge)
  (jdo (section (get-sections edge))
    (when (typep (get-incoming-shape section) 'elk-port)
      (%b-447-test-edge-end-point (get-containing-node edge) (get-incoming-shape section)
                                  (get-start-x section) (get-start-y section)))
    (when (typep (get-outgoing-shape section) 'elk-port)
      (%b-447-test-edge-end-point (get-containing-node edge) (get-outgoing-shape section)
                                  (get-end-x section) (get-end-y section)))))

(deflayouttest issue447-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/447_hierarchicalPortAnchors.elkt")))
  (:configurator configure-port-sizes-and-anchors (graph)
    (jdo (node (graph-test-utils.all-nodes graph nil))
      (jdo (port (get-ports node))
        (set-width port %b-447-port-size)
        (set-height port %b-447-port-size)
        (set-property port layered-options.port-anchor
                      (new-k-vector %b-447-port-anchor %b-447-port-anchor)))))
  (:test test-edges-connect-to-anchors () (graph)
    (jdo (node (graph-test-utils.all-nodes graph t))
      (jdo (edge (get-contained-edges node))
        (%b-447-test-edge edge)))))
