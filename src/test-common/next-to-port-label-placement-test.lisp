;;;; next-to-port-label-placement-test.lisp -- port of
;;;; org.eclipse.elk.alg.common.nodespacing.NextToPortLabelPlacementTest.

(in-package #:elk)

(defun %next-to-should-be-placed-next-to-port-p (port inside-label-placement)
  (when (and (is-empty (get-incoming-edges port))
             (is-empty (get-outgoing-edges port)))
    (return-from %next-to-should-be-placed-next-to-port-p t))
  (let ((edges-to-insides nil)
        (edges-to-somewhere-else nil))
    (jdo (out-edge (get-outgoing-edges port))
      (let ((inside-edge (elk-graph-util.is-descendant
                          (elk-graph-util.connectable-shape-to-node
                           (j-get (get-targets out-edge) 0))
                          (get-parent port))))
        (setf edges-to-insides (or edges-to-insides inside-edge))
        (setf edges-to-somewhere-else (or edges-to-somewhere-else (not inside-edge)))))
    (jdo (in-edge (get-incoming-edges port))
      (let ((inside-edge (elk-graph-util.is-descendant
                          (elk-graph-util.connectable-shape-to-node
                           (j-get (get-sources in-edge) 0))
                          (get-parent port))))
        (setf edges-to-insides (or edges-to-insides inside-edge))
        (setf edges-to-somewhere-else (or edges-to-somewhere-else (not inside-edge)))))
    (or (and inside-label-placement (not edges-to-insides))
        (and (not inside-label-placement) (not edges-to-somewhere-else)))))

(defun %next-to-test-port-label (port inside-label-placement)
  (let ((label (j-get (get-labels port) 0)))
    (if (%next-to-should-be-placed-next-to-port-p port inside-label-placement)
        (assert-true (and (>= (+ (get-y label) (get-height label)) 0)
                          (<= (get-y label) (get-height port))))
        (assert-true (or (<= (+ (get-y label) (get-height label)) 0)
                         (>= (get-y label) (get-height port)))))))

(deflayouttest next-to-port-label-placement-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration :edges nil :nodes nil :ports t)
  (:graph-resources (list (model-resource-path "tests/core/label_placement/port_labels/next_to_port_if_possible_inside.elkt")
                          (model-resource-path "tests/core/label_placement/port_labels/next_to_port_if_possible_outside.elkt")))
  (:test test-next-to-port-labels () (graph)
    (let ((node-queue (jlist-from (get-children graph))))
      (loop until (is-empty node-queue)
            do (let ((current-node (jl-poll-first node-queue)))
                 (jl-add-all node-queue (get-children current-node))
                 (let ((inside-label-placement
                         (contains (get-property current-node core-options.port-labels-placement)
                                   port-label-placement.inside)))
                   (jdo (port (get-ports current-node))
                     (%next-to-test-port-label port inside-label-placement))))))))
