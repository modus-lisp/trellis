;;;; port-placement-calculation-test.lisp -- port of
;;;; org.eclipse.elk.alg.topdown.layered.test.PortPlacementCalculationTest.

(in-package #:elk)

(defalgtest port-placement-calculation-test test-bottom-up
  (algs-init)
  (let* ((graph (elk-graph-util.create-graph))
         (port-spacing 10.0d0)
         (node (elk-graph-util.create-node graph)))
    (set-property node core-options.node-size-constraints
                  (enum-set.of size-constraint.port-labels size-constraint.ports))
    (set-property node core-options.spacing-port-port port-spacing)
    (set-dimensions node 20 60)
    (let ((port1 (elk-graph-util.create-port node))
          (port2 (elk-graph-util.create-port node))
          (port3 (elk-graph-util.create-port node)))
      (algs-layout graph)
      (assert-near port-spacing (abs (- (get-y port1) (get-y port2))) 0.0001)
      (assert-near port-spacing (abs (- (get-y port2) (get-y port3))) 0.0001))))

(defalgtest port-placement-calculation-test test-top-down
  (algs-init)
  (let* ((graph (elk-graph-util.create-graph))
         (port-spacing 10.0d0)
         (fixed-node-height 60.0d0))
    (set-property graph core-options.topdown-layout t)
    (set-property graph core-options.topdown-node-type topdown-node-types.root-node)
    (let ((node (elk-graph-util.create-node graph)))
      (set-property node core-options.topdown-layout t)
      (set-property node core-options.topdown-node-type topdown-node-types.hierarchical-node)
      (set-property node core-options.node-size-fixed-graph-size t)
      (set-property node core-options.node-size-constraints
                    (enum-set.of size-constraint.port-labels size-constraint.ports))
      (set-property node core-options.spacing-port-port port-spacing)
      (set-dimensions node 20 fixed-node-height)
      (let ((port1 (elk-graph-util.create-port node))
            (port2 (elk-graph-util.create-port node))
            (port3 (elk-graph-util.create-port node)))
        (algs-layout graph)
        (let ((expected (/ fixed-node-height 4)))
          (assert-near expected (abs (- (get-y port1) (get-y port2))) 0.0001)
          (assert-near expected (abs (- (get-y port2) (get-y port3))) 0.0001))))))
