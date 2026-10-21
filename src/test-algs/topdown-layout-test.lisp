;;;; topdown-layout-test.lisp -- port of org.eclipse.elk.alg.topdown.test.TopdownLayoutTest.

(in-package #:elk)

(defun %tdl-scaled-graph (&key (width 20.0d0) (aspect 0.4d0) cap (padding (new-elk-padding))
                             (spacing 0.0d0))
  "The shared graph of the four scaling tests: a root, one hierarchical toplevel
node with the given topdown width/aspect (and scale cap), and two fixed-size children.
Returns (values graph toplevel)."
  (let* ((graph (elk-graph-util.create-graph))
         (toplevel (elk-graph-util.create-node graph)))
    (set-property graph core-options.topdown-layout t)
    (set-property graph core-options.topdown-node-type topdown-node-types.root-node)
    (set-property toplevel core-options.topdown-layout t)
    (set-property toplevel core-options.topdown-node-type topdown-node-types.hierarchical-node)
    (set-property toplevel core-options.node-size-fixed-graph-size t)
    (when cap (set-property toplevel core-options.topdown-scale-cap cap))
    (set-property toplevel core-options.algorithm "org.eclipse.elk.fixed")
    (set-property toplevel core-options.padding padding)
    (set-property toplevel core-options.spacing-node-node spacing)
    (set-property toplevel core-options.topdown-hierarchical-node-width width)
    (set-property toplevel core-options.topdown-hierarchical-node-aspect-ratio aspect)
    (let ((child1 (elk-graph-util.create-node toplevel))
          (child2 (elk-graph-util.create-node toplevel)))
      (set-property child1 core-options.topdown-layout t)
      (set-property child1 core-options.topdown-node-type topdown-node-types.hierarchical-node)
      (set-property child1 core-options.node-size-fixed-graph-size t)
      (set-x child1 0)
      (set-y child1 0)
      (set-dimensions child1 30 30)
      (set-property child2 core-options.topdown-layout t)
      (set-property child2 core-options.topdown-node-type topdown-node-types.hierarchical-node)
      (set-property child2 core-options.node-size-fixed-graph-size t)
      (set-x child2 0)
      (set-y child2 40)
      (set-dimensions child2 30 30))
    (values graph toplevel)))

(defalgtest topdown-layout-test test-two-level-layout-horizontal-scaling
  (multiple-value-bind (graph toplevel)
      (%tdl-scaled-graph :width 20.0d0 :aspect 0.4d0)
    (algs-layout graph)
    (assert-near (/ 20.0d0 30.0d0) (get-property toplevel core-options.topdown-scale-factor) 0.00001)))

(defalgtest topdown-layout-test test-two-level-layout-vertical-scaling
  (multiple-value-bind (graph toplevel)
      (%tdl-scaled-graph :width 40.0d0 :aspect 1.33333d0)
    (algs-layout graph)
    (assert-near (/ 30.0d0 70.0d0) (get-property toplevel core-options.topdown-scale-factor) 0.00001)))

(defalgtest topdown-layout-test test-scale-cap
  (multiple-value-bind (graph toplevel)
      (%tdl-scaled-graph :width 300.0d0 :aspect 1.0d0 :cap 100.0d0)
    (algs-layout graph)
    (assert-near (/ 300.0d0 70.0d0) (get-property toplevel core-options.topdown-scale-factor) 0.00001)))

(defalgtest topdown-layout-test test-scale-cap-bounded
  (multiple-value-bind (graph toplevel)
      (%tdl-scaled-graph :width 300.0d0 :aspect 1.0d0 :cap 3.0d0)
    (algs-layout graph)
    (assert-near 3.0d0 (get-property toplevel core-options.topdown-scale-factor) 0.00001)))

(defalgtest topdown-layout-test test-child-dimension-calculation
  ;; The toplevel graph has no topdown fixed size of its own; its children are sized
  ;; by the fallback case, which depends on the padding and the topdown size.
  (let* ((padding (new-elk-padding 10))
         (graph (elk-graph-util.create-graph)))
    (set-property graph core-options.topdown-layout t)
    (set-property graph core-options.topdown-node-type topdown-node-types.root-node)
    (let ((toplevel (elk-graph-util.create-node graph)))
      (set-property toplevel core-options.topdown-layout t)
      (set-property toplevel core-options.topdown-node-type topdown-node-types.hierarchical-node)
      (set-property toplevel core-options.node-size-fixed-graph-size t)
      (set-property toplevel core-options.algorithm "org.eclipse.elk.layered")
      (set-property toplevel core-options.topdown-hierarchical-node-width 20.0d0)
      (set-property toplevel core-options.topdown-hierarchical-node-aspect-ratio 1.0d0)
      (set-property toplevel core-options.padding padding)
      (let ((child1 (elk-graph-util.create-node toplevel)))
        (set-property child1 core-options.topdown-layout t)
        (set-property child1 core-options.topdown-node-type topdown-node-types.hierarchical-node)
        (set-property child1 core-options.node-size-fixed-graph-size t)
        (set-x child1 0)
        (set-y child1 0)
        (set-property child1 core-options.topdown-hierarchical-node-width 20.0d0)
        (set-property child1 core-options.topdown-hierarchical-node-aspect-ratio 1.0d0)
        (algs-layout graph)
        (assert-near (+ 20.0d0 (get-horizontal padding)) (get-width toplevel) 0.00001)
        (assert-near (+ 20.0d0 (get-vertical padding)) (get-height toplevel) 0.00001)))))
