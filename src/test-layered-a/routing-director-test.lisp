;;;; routing-director-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.loops.routing.RoutingDirectorTest.
;;;; Plain JUnit class; run with (routing-director-test.run-all).

(in-package #:elk)

(defun routing-director-test.test-opposing-two-side-loop-uses-deterministic-side-order ()
  (let* ((graph (new-l-graph))
         (node (self-loop-test-graph-creator.node graph)))
    (self-loop-test-graph-creator.ports node 1 1 1 1)
    (let* ((east-port (j-get (get-ports node) 1))
           (west-port (j-get (get-ports node) 3)))
      (self-loop-test-graph-creator.edge east-port west-port)
      (let* ((sl-holder (self-loop-holder.install node))
             (sl-loop (j-get (get-sl-hyper-loops sl-holder) 0)))
        (compute-ports-per-side sl-loop)
        ;; ListMultimap<PortSide, SelfLoopPort> reversedSides: WEST then EAST.  Putting
        ;; an empty list adds no key (as Multimap.putAll does), so only non-empty sides go in.
        (let ((reversed-sides (new-jmap)))
          (dolist (side (list port-side.west port-side.east))
            (let ((ports (get-sl-ports-by-side sl-loop side)))
              (unless (is-empty ports)
                (jm-put reversed-sides side ports))))
          ;; setPortsBySide: field.set(slLoop, portsBySide) on the private slPortsBySide.
          (setf (.sl-ports-by-side sl-loop) reversed-sides))
        (determine-loop-routes (new-routing-director) sl-holder)
        (assert-true (eq east-port (get-l-port (get-leftmost-port sl-loop))))
        (assert-true (eq west-port (get-l-port (get-rightmost-port sl-loop))))
        (assert-equals (enum-set.of port-side.east port-side.south port-side.west)
                       (get-occupied-port-sides sl-loop))))))

(defun routing-director-test.run-all ()
  (routing-director-test.test-opposing-two-side-loop-uses-deterministic-side-order))
