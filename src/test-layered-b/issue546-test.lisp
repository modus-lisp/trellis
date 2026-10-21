;;;; issue546-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue546Test.

(in-package #:elk)

(defparameter %b-546-tolerance 0.5d0)

(deflayouttest issue546-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/546_borderGaps.elkt")))
  (:test test-edge-starts-at-port () (graph)
    (let ((node-queue (new-jlist)))
      (jl-add node-queue graph)
      (loop until (is-empty node-queue)
            do (let ((curr-node (jl-poll-first node-queue)))
                 (jdo (child (get-children curr-node))
                   (jl-add node-queue child))
                 (jdo (edge (get-contained-edges curr-node))
                   (let* ((src-port (elk-graph-util.connectable-shape-to-port (j-get (get-sources edge) 0)))
                          (src-port-pos (elk-util.absolute-position src-port))
                          (src-port-rect (list (- (.x src-port-pos) %b-546-tolerance)
                                               (- (.y src-port-pos) %b-546-tolerance)
                                               (+ (get-width src-port) (* 2 %b-546-tolerance))
                                               (+ (get-height src-port) (* 2 %b-546-tolerance))))
                          (section (j-get (get-sections edge) 0))
                          (src-point (new-k-vector (get-start-x section) (get-start-y section))))
                     (elk-util.to-absolute src-point curr-node)
                     (assert-true (format nil "Source point ~A is not close enough to port ~A"
                                          src-point src-port-rect)
                                  (%b-rect2d-contains-point-p src-port-rect (.x src-point) (.y src-point))))))))))
