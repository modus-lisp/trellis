;;;; basic-edge-router-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p5edges.BasicEdgeRouterTest (extends
;;;; TestGraphCreator in Java; none of its members are used by the tests here).

(in-package #:elk)

(defconstant +ber-tolerance+ 0.05d0)

(defun %ber-configurator-for (routing)
  (let ((config (new-layout-configurator)))
    (set-property (configure config 'elk-node) layered-options.edge-routing routing)
    config))

(defun %ber-is-horizontal (p1 p2)
  "Checks whether the line from p1 to p2 is horizontal."
  (< (abs (- (.y p1) (.y p2))) +ber-tolerance+))

(defjstatic basic-edge-router-test.check-edge-route-is-orthogonal (edge route)
  "Ensures that the given list of points describe an orthogonal route through the
diagram.  Utility method that can be used by other tests as well."
  ;; The route should contain at least two points
  (when (< (size route) 2)
    (fail (format nil "~A: Route with less than 2 points" edge)))
  (let* ((horizontal (%ber-is-horizontal (j-get route 0) (j-get route 1)))
         (prev (j-get route 0)))
    (loop for i from 1 below (size route)
          for curr = (j-get route i)
          for error = (format nil "~A: horizontal = ~A prev = ~A curr = ~A" edge horizontal prev curr)
          do (if horizontal
                 (tlc-assert-equals-delta error (.y prev) (.y curr) +ber-tolerance+)
                 (tlc-assert-equals-delta error (.x prev) (.x curr) +ber-tolerance+))
             (setf horizontal (not horizontal))
             (setf prev curr))))

(deflayouttest basic-edge-router-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:configurator-provider orthogonal-configurator ()
    (%ber-configurator-for edge-routing.orthogonal))
  ;; (polyline and spline configurators are commented out in the Java source)
  (:test test-orthogonal-edges () (graph)
    ;; Iterate over all of the graph's edges, through the whole hierarchy
    (let ((node-queue (new-jlist)))
      (jl-add node-queue graph)
      (loop until (is-empty node-queue)
            do (let ((node (jl-poll-first node-queue)))
                 (jdo (child (get-children node)) (jl-add node-queue child))
                 (jdo (edge (get-contained-edges node))
                   (jdo (section (get-sections edge))
                     (basic-edge-router-test.check-edge-route-is-orthogonal
                      edge (elk-util.create-vector-chain section)))))))))
