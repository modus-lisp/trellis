;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered.p3order and .p3order.counting

(in-package #:elk)

;;; ---------------------------------------------------------------- counting

(defjinterface i-initializable)

(defjclass all-crossings-counter (i-initializable)
  ((crossing-counter nil)
   (has-hyper-edges-east-of-index nil)
   (hyperedge-crossings-counter nil)
   (in-layer-edge-counts nil)
   (has-north-south-ports nil)
   (n-ports 0)))

(defjclass binary-indexed-tree ()
  ((binary-sums nil)
   (nums-per-index nil)
   (size 0)
   (max-num 0)))

(defjclass crossings-counter ()
  ((port-positions nil)
   (index-tree nil)
   (ends (new-jlist))
   (node-cardinalities nil)))

(defjclass cross-min-util () ())

(defjclass hyperedge-crossings-counter ()
  ((port-pos nil)))

(defvar *%p3-hyperedge-counter* 0
  "Stand-in for Java's identity hashCode() of Hyperedge (creation order).")

(defjclass hyperedge ()
  ((edges (new-jlist))
   (ports (new-jlist))
   (upper-left 0)
   (lower-left 0)
   (upper-right 0)
   (lower-right 0)
   (hash (incf *%p3-hyperedge-counter*))))

(defjclass hyperedge-corner ()
  ((hyperedge nil)
   (position 0)
   (opposite-position 0)
   (type nil)))

;;; ---------------------------------------------------------------- p3order

(defjinterface i-crossing-minimization-heuristic (i-initializable))
(defjinterface i-sweep-port-distributor (i-initializable))

(defjclass graph-info-holder (i-initializable)
  ((l-graph nil)
   (current-node-order nil)
   (currently-best-node-and-port-order nil)
   (best-node-and-port-order nil)
   (port-positions nil)
   (use-bottom-up nil)
   (child-graphs nil)
   (has-external-ports nil)
   (has-parent nil)
   (parent-graph-data nil)
   (parent nil)
   (layer-sweep-type-decider nil)
   (cross-minimizer nil)
   (port-distributor nil)
   (crossings-counter nil)
   (n-ports 0)))

(defjclass sweep-copy ()
  ((node-order nil)
   (port-orders nil)))

(defjclass abstract-barycenter-port-distributor (i-sweep-port-distributor)
  ((port-ranks nil)
   (min-barycenter 0f0)
   (max-barycenter 0f0)
   (node-positions nil)
   (port-barycenter nil)
   (in-layer-ports nil)
   (n-ports 0)))

(defjclass layer-total-port-distributor (abstract-barycenter-port-distributor) ())
(defjclass node-relative-port-distributor (abstract-barycenter-port-distributor) ())

(defjclass greedy-port-distributor (i-sweep-port-distributor)
  ((crossings-counter nil)
   (n-ports 0)
   (port-pos nil)
   (hierarchical-crossings-counter nil)))

(defjclass barycenter-heuristic (i-crossing-minimization-heuristic)
  ((port-ranks nil)
   (random nil)
   (constraint-resolver nil)
   (barycenter-state nil)
   (port-distributor nil)
   (barycenter-state-comparator nil)))

(defjclass barycenter-state ()
  ((node nil)
   (summed-weight 0d0)
   (degree 0)
   (barycenter nil)
   (visited nil)))

(defjclass model-order-barycenter-heuristic (barycenter-heuristic)
  ((bigger-than (new-jmap))
   (smaller-than (new-jmap))))

(defjclass median-heuristic (i-crossing-minimization-heuristic)
  ((random nil)
   (weight-comparator nil)))

(defjclass forster-constraint-resolver (i-initializable)
  ((constraints-between-non-dummies nil)
   (layout-units nil)
   (barycenter-states nil)
   (constraint-groups nil)))

(defjclass constraint-group ()
  ((outer nil)                          ; the enclosing ForsterConstraintResolver
   (summed-weight 0d0)
   (degree 0)
   (nodes nil)
   (outgoing-constraints nil)
   (incoming-constraints nil)
   (incoming-constraints-count 0)))

(defjclass layer-sweep-type-decider (i-initializable)
  ((node-info nil)
   (graph-data nil)))

(defjclass node-info ()
  ((connected-edges 0)
   (hierarchical-influence 0)
   (random-influence 0)))

(defjclass layer-sweep-crossing-minimizer (i-layout-phase i-hierarchy-aware-layout-processor)
  ((graph-info-holders nil)
   (graphs-whose-node-order-changed nil)
   (random nil)
   (random-seed 0)
   (cross-min-type nil)))

(defjclass interactive-crossing-minimizer (i-layout-phase) ())
(defjclass no-crossing-minimizer (i-layout-phase) ())
