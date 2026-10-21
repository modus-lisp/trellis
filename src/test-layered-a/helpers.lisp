;;;; helpers.lisp -- small shared helpers for the layered-a test ports.  These
;;;; stand for repeated Java statements (node setup, the ElkGraphUtil calls, the
;;;; @Before of the plain JUnit classes); each one does exactly what the Java
;;;; lines it replaces do, nothing more.

(in-package #:elk)

(defun %la-setup ()
  "@Before: PlainJavaInitialization.initializePlainJavaLayout(); new LayeredLayoutProvider()."
  (initialize-plain-java-layout)
  (new-layered-layout-provider))

(defun %la-node (graph)
  "ElkGraphUtil.createNode(graph) followed by setWidth(30) and setHeight(30)."
  (let ((node (elk-graph-util.create-node graph)))
    (set-width node 30d0)
    (set-height node 30d0)
    node))

(defun %la-edge (source target)
  "ElkGraphUtil.createSimpleEdge(source, target)."
  (elk-graph-util.create-simple-edge source target))

(defun %la-unzip-graph (graph)
  "The graph-level options every AlternatingLayerUnzipperTest case sets."
  (set-property graph layered-options.layer-unzipping-strategy layer-unzipping-strategy.alternating)
  (set-property graph layered-options.consider-model-order-strategy ordering-strategy.prefer-edges)
  graph)

(defun %la-assert-equals-delta (message expected actual delta)
  "org.junit.Assert.assertEquals(String, double, double, double): |expected - actual| <= delta."
  (unless (<= (abs (- expected actual)) delta)
    (fail (format nil "~A expected:<~A> but was:<~A>" message expected actual)))
  t)

;;; BasicLayerAssignmentTest: the layer-assignment checks that NodePromotionTest and
;;; the layered-c BasicLayerAssignmentTest port both run on a laid-out graph.  Java
;;; calls them as `new BasicLayerAssignmentTest().testNoLayerlessNodes(graph)`.

(defjclass basic-layer-assignment-test ()
  ())

(defjctor basic-layer-assignment-test ())

(defjmethod test-no-layerless-nodes ((this basic-layer-assignment-test) graph)
  (assert-true (is-empty (get-layerless-nodes graph))))

(defjmethod test-no-empty-layers ((this basic-layer-assignment-test) graph)
  (jdo (layer graph)
    (assert-false (is-empty (get-nodes layer)))))

(defjmethod test-edges-point-towards-next-layers ((this basic-layer-assignment-test) graph)
  ;; Assign increasing IDs to the layers
  (let ((next-layer-id 0))
    (jdo (layer graph)
      (setf (.id layer) next-layer-id)
      (incf next-layer-id)))
  (jdo (layer graph)
    (jdo (node layer)
      (let ((node-layer-id (.id (get-layer node))))
        (jdo (edge (get-outgoing-edges node))
          (assert-true (< node-layer-id (.id (get-layer (get-node (get-target edge)))))))))))
