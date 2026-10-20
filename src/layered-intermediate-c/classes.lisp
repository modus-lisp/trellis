;;;; classes.lisp -- class definitions of org.eclipse.elk.alg.layered.intermediate
;;;; .loops (+ .ordering, .routing), .greedyswitch, .preserveorder, .unzipping.

(in-package #:elk)

;;; ------------------------------------------------------------ loops

(defjclass self-loop-port ()
  ((l-port nil)
   (had-only-self-loops nil)
   (incoming-sl-edges (new-jlist))
   (outgoing-sl-edges (new-jlist))
   (is-hidden nil)))

(defjclass self-loop-edge ()
  ((l-edge nil)
   (sl-hyper-loop nil)
   (sl-source nil)
   (sl-target nil)))

(defjclass self-hyper-loop ()
  ((sl-holder nil)
   (sl-ports (new-jlist))
   ;; HashSet in Java; insertion-ordered here
   (sl-edges (new-jset))
   (sl-labels nil)
   (self-loop-type nil)
   ;; ListMultimap<PortSide, SelfLoopPort>: hash table side -> jlist
   (sl-ports-by-side nil)
   (leftmost-port nil)
   (rightmost-port nil)
   (occupied-port-sides (enum-set.none-of 'port-side))
   (routing-slot (make-array (length (port-side.values)) :initial-element 0))))

(defjclass self-loop-holder ()
  ((l-node nil)
   (sl-hyper-loops (new-jlist))
   ;; LinkedHashMap<LPort, SelfLoopPort>
   (sl-ports (new-jmap))
   (are-ports-hidden nil)
   (routing-slot-count (make-array (length (port-side.values)) :initial-element 0))))

(defjclass self-hyper-loop-labels ()
  ((id 0)
   (l-labels (new-jlist))
   (size (new-k-vector))
   (position (new-k-vector))
   (layout-direction nil)
   (label-label-spacing 0.0)
   (side nil)
   (alignment nil)
   (alignment-reference-sl-port nil)))

;;; loops.ordering

(defjclass port-side-assigner () ())

(defjclass port-restorer ()
  ((sl-loops-by-type nil)
   (target-areas nil)))

;;; loops.routing

(defjclass abstract-self-loop-router () ())
(defjclass orthogonal-self-loop-router (abstract-self-loop-router) ())
(defjclass polyline-self-loop-router (orthogonal-self-loop-router) ())
(defjclass spline-self-loop-router (orthogonal-self-loop-router) ())

(defjclass layered-label-placer () ())

(defjclass routing-director ()
  ((port-penalties nil)))

(defjclass routing-slot-assigner ()
  ((hyper-edge-segments nil)
   (sl-loop-to-segment-map nil)
   (sl-loop-activity-over-ports nil)))

;;; ------------------------------------------------------------ greedyswitch

(defjclass between-layer-edge-two-node-crossings-counter ()
  ((upper-lower-crossings 0)
   (lower-upper-crossings 0)
   (upper-adjacencies nil)
   (lower-adjacencies nil)
   (current-node-order nil)
   (free-layer-index 0)
   (port-positions (new-jmap))
   (eastern-adjacencies (new-jmap))
   (western-adjacencies (new-jmap))))

;; inner class BetweenLayerEdgeTwoNodeCrossingsCounter.AdjacencyList
(defjclass adjacency-list ()
  ((outer nil)
   (node nil)
   (adjacency-list (new-jlist))
   (side nil)
   (size 0)
   (current-size 0)
   (current-index 0)))

;; inner class AdjacencyList.Adjacency
(defjclass adjacency ()
  ((position 0)
   (cardinality 1)
   (current-cardinality 1)))

(defjclass crossing-matrix-filler ()
  ((is-crossing-matrix-filled nil)
   (crossing-matrix nil)
   (in-between-layer-crossing-counter nil)
   (direction nil)
   (one-sided nil)))

(defjclass greedy-switch-heuristic (i-crossing-minimization-heuristic)
  ((greedy-switch-type nil)
   (current-node-order nil)
   (switch-decider nil)
   (port-positions nil)
   (graph-data nil)
   (n-ports 0)))

(defjclass north-south-edge-neighbouring-node-crossings-counter ()
  ((upper-lower-crossings 0)
   (lower-upper-crossings 0)
   (port-positions (new-jmap))
   (layer nil)))

(defjclass switch-decider ()
  ((free-layer nil)
   (left-in-layer-counter nil)
   (right-in-layer-counter nil)
   (north-south-counter nil)
   (crossing-matrix-filler nil)
   (graph-data nil)
   (parent-cross-counter nil)
   (count-crossings-caused-by-port-switch nil)))

;;; ------------------------------------------------------------ preserveorder

(defjclass cm-group-model-order-calculator () ())

;; Comparators are funcallable instances: (funcall cmp a b) == (compare cmp a b).
(defclass model-order-node-comparator (sb-mop:funcallable-standard-object jobject)
  ((previous-layer :accessor .previous-layer :initform nil)
   (graph :accessor .graph :initform nil)
   (ordering-strategy :accessor .ordering-strategy :initform nil)
   (group-order-strategy :accessor .group-order-strategy :initform nil)
   (bigger-than :accessor .bigger-than :initform (new-jmap))
   (smaller-than :accessor .smaller-than :initform (new-jmap))
   (long-edge-node-order :accessor .long-edge-node-order :initform long-edge-ordering-strategy.equal)
   (before-ports :accessor .before-ports :initform nil))
  (:metaclass sb-mop:funcallable-standard-class))

(defclass model-order-port-comparator (sb-mop:funcallable-standard-object jobject)
  ((target-node-model-order :accessor .target-node-model-order :initform nil)
   (port-model-order :accessor .port-model-order :initform nil)
   (previous-layer :accessor .previous-layer :initform nil)
   (graph :accessor .graph :initform nil)
   (strategy :accessor .strategy :initform nil)
   (bigger-than :accessor .bigger-than :initform (new-jmap))
   (smaller-than :accessor .smaller-than :initform (new-jmap)))
  (:metaclass sb-mop:funcallable-standard-class))

(defjclass port-side-comparator () ())

;;; ------------------------------------------------------------ unzipping

(defjclass alternating-layer-unzipper (i-layout-processor) ())
