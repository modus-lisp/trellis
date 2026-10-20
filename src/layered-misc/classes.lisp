;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered.{compaction,components,compound}

(in-package #:elk)

;;; ------------------------------------------------------------ compaction.oned

(defjclass layered-c-graph ()
  ((c-nodes (new-jlist))
   (c-groups (new-jlist))
   (supported-directions nil)))

;; cNodes is a LinkedHashSet in Java; it is a jlist here (insertion order is
;; what matters, and add/remove/re-add must move an element to the end).
(defjclass layered-c-group ()
  ((start-pos double.negative-infinity)
   (delta 0d0)
   (delta-normalized 0d0)
   (c-nodes (new-jlist))
   (incoming-constraints (new-jlist))
   (out-degree 0)
   (reference nil)
   (id 0)
   (reposition t)))

(defjclass layered-quadruplet ()
  ((left nil) (right nil) (up nil) (down nil)))

;; abstract class CNode
(defjclass layered-c-node ()
  ((c-group nil)
   (parent-node nil)
   (constraints (new-jlist))
   (hitbox nil)
   (c-group-offset (new-k-vector))
   (start-pos double.negative-infinity)
   (reposition t)
   (lock (new-layered-quadruplet))
   (spacing-ignore (new-layered-quadruplet))
   (id 0)))

(defjinterface layered-i-spacings-handler)
(defjclass %lm-default-spacing-handler (layered-i-spacings-handler) ())

(defjinterface ic-graph-transformer)

(defjclass layered-one-dimensional-compactor ()
  ((compaction-algorithm nil)
   (constraint-algorithm nil)
   (c-graph nil)
   (direction core-direction.undefined)
   (locking-strategy nil)
   (finished nil)
   (spacings-handler nil)))

;;; ------------------------------------------------------------ compaction.oned.algs

(defjinterface layered-i-compaction-algorithm)
(defjinterface layered-i-constraint-calculation-algorithm)

(defjclass layered-longest-path-compaction (layered-i-compaction-algorithm) ())
(defjclass layered-quadratic-constraint-calculation (layered-i-constraint-calculation-algorithm) ())

(defjclass layered-scanline-constraint-calculator (layered-i-constraint-calculation-algorithm)
  ((compactor nil)
   (constraints-scanline-comparator nil)
   (constraints-scanline-handler nil)))

(defjclass layered-timestamp ()
  ((low nil) (node nil)))

;;; ------------------------------------------------------------ compaction.recthull

(defjinterface layered-event-handler)

(defjclass layered-point ()
  ((x 0d0) (y 0d0) (quadrant nil) (convex t)))

(defjclass layered-scanline ()
  ((points nil) (comparator nil) (event-handlers nil)))

(defjclass layered-rectilinear-convex-hull ()
  ((hull (new-jlist))
   (x-max1 nil) (x-max2 nil)
   (x-min1 nil) (x-min2 nil)
   (y-max1 nil) (y-max2 nil)
   (y-min1 nil) (y-min2 nil)))

(defjclass layered-maximal-elements-event-handler (layered-event-handler)
  ((quadrant nil) (points (new-jlist)) (maximal-y 0d0) (compare nil)))

(defjclass layered-rectangle-event-handler (layered-event-handler)
  ((outer nil)
   (rects (new-jlist))
   (min-y nil) (max-y nil)
   (last-x 0d0)
   (queued nil) (queued-pnt nil)))

(defjclass layered-constraints-scanline-handler (layered-event-handler)
  ((outer nil)
   ;; TreeSet ordered by hitbox center; a sorted jlist (see %lm-ts-*)
   (intervals (new-jlist))
   (cand nil)))

;;; ------------------------------------------------------------ compaction.components

(defjinterface i-component)
(defjinterface i-connected-components)
(defjinterface i-external-extension)

(defjclass components-to-c-graph-transformer (ic-graph-transformer)
  ((c-graph nil)
   (spacing 0d0)
   (old-position (new-jmap))
   (offsets (new-jmap))
   (global-offset nil)
   (graph-size nil)
   (external-extensions (new-jmap))
   ;; HashMultimap<Direction, Pair>: hash table direction -> jlist
   (external-placeholder (new-jmap))))

(defjclass c-rect-node (layered-c-node)
  ((outer nil) (rect nil) (individual-spacing nil)))

(defjclass %lm-components-spacing-handler (layered-i-spacings-handler) ())

(defjclass one-dimensional-components-compaction ()
  ((compaction-graph nil)
   (transformer nil)
   (vertical-external-extensions nil)
   (horizontal-external-extensions nil)
   (compactor nil)
   (top-left nil)
   (bottom-right nil)))

;;; ------------------------------------------------------------ components

(defjclass abstract-graph-placer () ())
(defjclass simple-row-graph-placer (abstract-graph-placer) ())
(defjclass model-order-row-graph-placer (simple-row-graph-placer) ())
(defjclass component-group-graph-placer (abstract-graph-placer)
  ((component-groups (new-jlist))))
(defjclass component-group-model-order-graph-placer (component-group-graph-placer) ())

;; ArrayListMultimap<Set<PortSide>, LGraph>: an EQUAL hash table keyed by the
;; sorted list of side ordinals, value (original-set . jlist-of-graphs).
(defjclass component-group ()
  ((components (make-hash-table :test 'equal))))
(defjclass model-order-component-group (component-group)
  ((component-order (new-jlist))))

(defjclass layered-components-processor ()
  ((component-group-graph-placer (new-component-group-graph-placer))
   (component-group-model-order-graph-placer (new-component-group-model-order-graph-placer))
   (model-order-row-graph-placer (new-model-order-row-graph-placer))
   (simple-row-graph-placer (new-simple-row-graph-placer))
   (graph-placer nil)))

(defjclass components-compactor ()
  ((compactor nil)
   (yet-another-offset nil)
   (compacted-graph-size nil)
   (graph-top-left nil)
   (graph-bottom-right nil)))

(defjclass internal-connected-components (i-connected-components)
  ((components (new-jlist))
   (contains-external-ports nil)))

(defjclass internal-component (i-component)
  ((graph nil)
   (contains-regular-nodes nil)
   (rectilinear-convex-hull nil)
   (external-extensions (new-jlist))))

(defjclass internal-union-external-extension (i-external-extension)
  ((edges (new-jlist)) (side nil) (extension nil) (placeholder nil)))

(defjclass internal-external-extension (i-external-extension)
  ((edge nil) (external-port nil) (external-port-side nil)
   (external-extension nil) (parent nil)))

;; extends ArrayList<Point>
(defjclass hullpoints ()
  ((items (new-jlist))
   (top-left (new-k-vector double.positive-infinity double.positive-infinity))
   (bottom-right (new-k-vector double.negative-infinity double.negative-infinity))))

(defjclass segments ()
  ((inner-segments (new-jlist)) (outer-segment nil)))

(defjclass outer-segments ()
  ((min (make-array 5 :initial-element double.positive-infinity))
   (max (make-array 5 :initial-element double.negative-infinity))
   (extent (make-array 5 :initial-element double.negative-infinity))))

;;; ------------------------------------------------------------ compound

(defjclass cross-hierarchy-edge ()
  ((new-edge nil) (graph nil) (type nil)))

(defjclass cross-hierarchy-edge-comparator ()
  ((graph nil)))

(defjclass compound-graph-postprocessor (i-layout-processor) ())

(defjclass compound-graph-preprocessor (i-layout-processor)
  ((cross-hierarchy-map nil)
   (dummy-node-map (new-jmap))))

(defjclass external-port ()
  ((orig-edges (new-jlist))
   (new-edge nil) (dummy-node nil) (dummy-port nil)
   (type port-type.undefined)
   (exported nil)))
