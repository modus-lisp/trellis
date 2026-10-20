;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered.p1cycles and .p2layers.

(in-package #:elk)

;;; ------------------------------------------------------------ p1cycles

(defjclass greedy-cycle-breaker (i-layout-phase)
  ((indeg nil)
   (outdeg nil)
   (mark nil)
   (sources (new-jlist))
   (sinks (new-jlist))
   (layered-graph nil)
   (random nil)))

(defjclass greedy-model-order-cycle-breaker (greedy-cycle-breaker) ())

(defjclass depth-first-cycle-breaker (i-layout-phase)
  ((sources nil)
   (visited nil)
   (active nil)
   (edges-to-be-reversed nil)))

(defjclass interactive-cycle-breaker (i-layout-phase) ())

(defjclass model-order-cycle-breaker (i-layout-phase) ())

(defjclass bfs-node-order-cycle-breaker (i-layout-phase)
  ((sources nil)
   (sinks nil)
   (visited nil)
   (bfs-queue nil)
   (edges-to-be-reversed nil)
   (graph nil)))

(defjclass dfs-node-order-cycle-breaker (i-layout-phase)
  ((sources nil)
   (visited nil)
   (active nil)
   (edges-to-be-reversed nil)
   (graph nil)))

;; abstract
(defjclass scc-model-order-cycle-breaker (i-layout-phase)
  ((strongly-connected-components (new-jlist))
   (node-to-sccid (new-jmap))
   (rev-edges (new-jlist))
   (graph nil)))

(defjclass scc-node-type-cycle-breaker (scc-model-order-cycle-breaker) ())

(defjclass sc-connectivity (scc-model-order-cycle-breaker) ())

(defjclass group-model-order-calculator ()
  ((first-separate-nodes 0)
   (last-separate-nodes 0)))

;;; ------------------------------------------------------------ p2layers

(defjclass network-simplex-layerer (i-layout-phase)
  ((layered-graph nil)
   (component-nodes nil)
   (node-visited nil)))

(defjclass longest-path-layerer (i-layout-phase)
  ((layered-graph nil)
   (node-heights nil)))

(defjclass longest-path-source-layerer (i-layout-phase)
  ((layered-graph nil)
   (node-heights nil)))

(defjclass interactive-layerer (i-layout-phase) ())

;; InteractiveLayerer.LayerSpan
(defjclass layer-span ()
  ((start 0d0)
   (end 0d0)
   (nodes (new-jlist))))

(defjclass coffman-graham-layerer (i-layout-phase)
  ((node-mark nil)
   (edge-mark nil)
   (in-deg nil)
   (out-deg nil)
   (topo-ord nil)
   ;; ListMultimap<LNode, Integer>: node -> jlist of integers
   (in-topo (new-jmap))))

(defjclass breadth-first-model-order-layerer (i-layout-phase)
  ((layered-graph nil)))

(defjclass depth-first-model-order-layerer (i-layout-phase)
  ((layered-graph nil)
   (current-layer nil)
   (current-layer-id 0)
   (current-dummy-layer nil)
   (nodes-to-place nil)
   (max-to-place 0)))

(defjclass min-width-layerer (i-layout-phase)
  ((dummy-size 0d0)
   (minimum-node-size 0d0)
   (norm-size nil)
   (avg-size 0d0)
   (in-degree nil)
   (out-degree nil)))

(defjclass stretch-width-layerer (i-layout-phase)
  ((width-current 0d0)
   (width-up 0d0)
   (max-width 0d0)
   (current-graph nil)
   (upper-layer-influence 0d0)
   (sorted-layerless-nodes nil)
   (already-placed-nodes (new-jset))
   (already-placed-in-other-layers (new-jset))
   (temp-layerless-nodes nil)
   (successors nil)
   (out-degree nil)
   (remaining-out-going nil)
   (in-degree nil)
   (selected-node nil)
   (minimum-node-size 0d0)
   (maximum-node-size 0d0)
   (norm-size nil)
   (dummy-size 0d0)))

;;; ------------------------------------------------------------ helpers

(defun %p1p2-first (coll)
  "coll.iterator().next() (NIL when empty)."
  (block found (jdo (x coll) (return-from found x)) nil))

(defun %p1p2-int-array (n &optional (init 0))
  (make-array n :initial-element init))

(defun %p1p2-cycle-config ()
  "The configuration shared by the cycle breakers: REVERSED_EDGE_RESTORER after P5."
  (add-after (layout-processor-configuration.create)
             layered-phases.p5-edge-routing
             layered-intermediate-processor-strategy.reversed-edge-restorer))

(defun %p1p2-layering-config ()
  "The baseline configuration shared by most layerers."
  (add-before
   (add-before
    (add-before (layout-processor-configuration.create)
                layered-phases.p1-cycle-breaking
                layered-intermediate-processor-strategy.edge-and-layer-constraint-edge-reverser)
    layered-phases.p2-layering
    layered-intermediate-processor-strategy.layer-constraint-preprocessor)
   layered-phases.p3-node-ordering
   layered-intermediate-processor-strategy.layer-constraint-postprocessor))

(defun %p1p2-enforce-group-model-order-p (graph)
  (eq (get-property graph layered-options.consider-model-order-group-model-order-cb-group-order-strategy)
      group-order-strategy.enforced))

(defun %p1p2-boxed-int-eq (a b)
  "Java == on two Integer objects: identity, which for autoboxed values holds only inside the
Integer cache (-128..127)."
  (if (and (integerp a) (integerp b))
      (and (= a b) (<= -128 a 127))
      (eq a b)))
