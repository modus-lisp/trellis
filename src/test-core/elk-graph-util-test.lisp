;;;; elk-graph-util-test.lisp -- port of org.eclipse.elk.graph.util.ElkGraphUtilTest.

(in-package #:elk)

(defun %egu-create-edge-without-containment (source target)
  (let ((edge (elk-graph-util.create-edge nil)))
    (when source (add (get-sources edge) source))
    (when target (add (get-targets edge) target))
    edge))

(defcore-class elk-graph-util-test
  ("testConnectableShapeToNode"
   (let* ((node (elk-graph-util.create-node nil))
          (port (elk-graph-util.create-port node)))
     (assert-equals node (elk-graph-util.connectable-shape-to-node node))
     (assert-equals node (elk-graph-util.connectable-shape-to-node port))))
  ("testConnectableShapeToNodeWithNullPointer"
   (assert-throws error (elk-graph-util.connectable-shape-to-node nil)))
  ("testConnectableShapeToPort"
   (let* ((node (elk-graph-util.create-node nil))
          (port (elk-graph-util.create-port node)))
     (assert-null (elk-graph-util.connectable-shape-to-port node))
     (assert-equals port (elk-graph-util.connectable-shape-to-port port))))
  ("testConnectableShapeToPortWithNullPointer"
   (assert-throws error (elk-graph-util.connectable-shape-to-port nil)))
  ("testFindBestEdgeContainment"
   (let* ((graph1 (elk-graph-util.create-graph))
          (node1 (elk-graph-util.create-node graph1))
          (node2 (elk-graph-util.create-node graph1))
          (node3 (elk-graph-util.create-node graph1))
          (node3-1 (elk-graph-util.create-node node3))
          (node3-1-1 (elk-graph-util.create-node node3-1))
          (node4 (elk-graph-util.create-node graph1))
          (node4-1 (elk-graph-util.create-node node4))
          (graph2 (elk-graph-util.create-graph))
          (node-a (elk-graph-util.create-node graph2)))
     (flet ((best (e) (elk-graph-util.find-best-edge-containment e))
            (edge (s tg) (%egu-create-edge-without-containment s tg)))
       (assert-equals graph1 (best (edge node1 node2)))
       (assert-equals graph1 (best (edge node1 node1)))
       (assert-equals graph1 (best (edge node1 node3-1)))
       (assert-equals graph1 (best (edge node3-1 node2)))
       (assert-equals node3 (best (edge node3 node3-1)))
       (assert-equals node3 (best (edge node3-1 node3)))
       (assert-equals node3 (best (edge node3 node3-1-1)))
       (assert-equals node3 (best (edge node3-1-1 node3)))
       (assert-equals graph1 (best (edge node3-1 node4-1)))
       (assert-null (best (edge node1 node-a)))
       (assert-equals graph1 (best (edge nil node1)))
       (assert-equals graph1 (best (edge node1 nil))))))
  ("testFindBestEdgeContainmentWithUnconnectedEdge"
   (assert-throws error
     (elk-graph-util.find-best-edge-containment (elk-graph-util.create-edge nil))))
  ("testFindBestEdgeContainmentWithNullPointer"
   (assert-throws error (elk-graph-util.find-best-edge-containment nil))))
