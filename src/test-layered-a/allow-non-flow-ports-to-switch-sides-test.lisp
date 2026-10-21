;;;; allow-non-flow-ports-to-switch-sides-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.AllowNonFlowPortsToSwitchSidesTest.
;;;; A @RunWith(Parameterized) JUnit class, so the parameter table is a list and
;;;; the test runs once per row: (allow-non-flow-ports-to-switch-sides-test.run-all).

(in-package #:elk)

(defparameter *anf-data*
  ;; port constraints, node1MaySwitchPortSides, node2MaySwitchPortSides, edgesIntersect
  (list (list port-constraints.fixed-side nil nil t)
        (list port-constraints.fixed-pos t t t)
        ;; As soon as one port is allowed to switch sides, the edge crossing can be prevented
        (list port-constraints.fixed-side t nil nil)
        (list port-constraints.fixed-side nil t nil)
        (list port-constraints.fixed-side t t nil)
        ;; TODO The behavior for FIXED_ORDER is not clearly defined, currently ports will switch if allowed
        (list port-constraints.fixed-order t t nil)))

(defun %anf-create-simple-graph (port-constraints node1-may-switch node2-may-switch)
  (let* ((graph (elk-graph-util.create-graph))
         (node1 (elk-graph-util.create-node graph)))
    (set-width node1 30d0)
    (set-height node1 30d0)
    (set-property node1 layered-options.port-constraints port-constraints)
    (let ((port-north1 (elk-graph-util.create-port node1))
          (port-south1 (elk-graph-util.create-port node1)))
      (set-x port-north1 15d0)
      (set-y port-north1 -1d0)
      (set-property port-north1 layered-options.port-side port-side.north)
      (set-property port-north1 layered-options.allow-non-flow-ports-to-switch-sides node1-may-switch)
      (set-x port-south1 15d0)
      (set-y port-south1 30d0)
      (set-property port-south1 layered-options.port-side port-side.south)
      (set-property port-south1 layered-options.allow-non-flow-ports-to-switch-sides node1-may-switch)
      (let* ((node2 (elk-graph-util.create-node graph))
             (port-north2 nil) (port-south2 nil))
        (set-width node2 30d0)
        (set-height node2 30d0)
        (set-property node2 layered-options.port-constraints port-constraints)
        (setf port-north2 (elk-graph-util.create-port node2))
        (set-x port-north2 15d0)
        (set-y port-north2 -1d0)
        (set-property port-north2 layered-options.port-side port-side.north)
        (set-property port-north2 layered-options.allow-non-flow-ports-to-switch-sides node2-may-switch)
        (setf port-south2 (elk-graph-util.create-port node2))
        (set-x port-south2 15d0)
        (set-y port-south2 30d0)
        (set-property port-south2 layered-options.port-side port-side.south)
        (set-property port-south2 layered-options.allow-non-flow-ports-to-switch-sides node2-may-switch)
        ;; Two edges that, with fixed port sides, will have to cross.
        (elk-graph-util.create-simple-edge port-north1 port-south2)
        (elk-graph-util.create-simple-edge port-south1 port-north2)))
    graph))

(defun %anf-edge-to-segments (edge)
  (let* ((chain (elk-util.create-vector-chain (elk-graph-util.first-edge-section edge nil nil)))
         (items (coerce (jiterable-items chain) 'vector))
         (segments (new-jlist)))
    (loop for i from 1 below (length items)
          do (let ((prev (aref items (1- i))) (cur (aref items i)))
               (jl-add segments (list (.x prev) (.y prev) (.x cur) (.y cur)))))
    segments))

(defun %anf-segments-intersect-p (s1 s2)
  "Line2D.intersectsLine for two (x1 y1 x2 y2) segments (proper and touching cases)."
  (destructuring-bind (x1 y1 x2 y2) s1
    (destructuring-bind (x3 y3 x4 y4) s2
      (flet ((ccw (ax ay bx by cx cy)
               (let ((v (- (* (- cy ay) (- bx ax)) (* (- by ay) (- cx ax)))))
                 (cond ((> v 0) 1) ((< v 0) -1) (t 0)))))
        (and (<= (min x1 x2) (max x3 x4)) (<= (min x3 x4) (max x1 x2))
             (<= (min y1 y2) (max y3 y4)) (<= (min y3 y4) (max y1 y2))
             (<= (* (ccw x1 y1 x2 y2 x3 y3) (ccw x1 y1 x2 y2 x4 y4)) 0)
             (<= (* (ccw x3 y3 x4 y4 x1 y1) (ccw x3 y3 x4 y4 x2 y2)) 0))))))

(defun %anf-intersect-p (e1 e2)
  "AllowNonFlowPortsToSwitchSidesTest.intersect: do the two straight-segment paths cross?"
  (let ((segments1 (%anf-edge-to-segments e1))
        (segments2 (%anf-edge-to-segments e2)))
    (jdo (s1 segments1)
      (jdo (s2 segments2)
        (when (%anf-segments-intersect-p s1 s2)
          (return-from %anf-intersect-p t))))
    nil))

(defun allow-non-flow-ports-to-switch-sides-test.test-only-switch-port-sides-if-permitted
    (port-constraints node1-may-switch node2-may-switch edges-intersect)
  (initialize-plain-java-layout)
  (let* ((layered-layout (new-layered-layout-provider))
         (graph (%anf-create-simple-graph port-constraints node1-may-switch node2-may-switch))
         (e1 (j-get (get-contained-edges graph) 0))
         (e2 (j-get (get-contained-edges graph) 1)))
    (layout layered-layout graph (new-null-elk-progress-monitor))
    (assert-equals edges-intersect (%anf-intersect-p e1 e2))))

(defun allow-non-flow-ports-to-switch-sides-test.run-all ()
  (dolist (row *anf-data*)
    (apply #'allow-non-flow-ports-to-switch-sides-test.test-only-switch-port-sides-if-permitted row)))
