;;;; compaction-classes.lisp -- org.eclipse.elk.alg.common.compaction classes
;;;; (Scanline, EventHandler, the oned/ classes and interfaces, options are generated enums)

(in-package #:elk)

(defjinterface common-event-handler)
(defjgeneric handle 1)

(defjinterface common-i-compaction-algorithm)
(defjgeneric compact 1)

(defjinterface common-i-constraint-calculation-algorithm)
(defjgeneric calculate-constraints 1)

(defjinterface i-lock-function)
(defjgeneric is-locked 2)

(defjinterface common-i-spacings-handler)
(defjgeneric get-horizontal-spacing 2)
(defjgeneric get-vertical-spacing 2)

;; ISpacingsHandler.DEFAULT_SPACING_HANDLER (an anonymous class returning 0).
(defjclass common-default-spacings-handler (common-i-spacings-handler) ())

(defjclass common-c-graph ()
  ((c-nodes (new-jlist))
   (c-groups (new-jlist))
   (supported-directions nil)
   (predefined-horizontal-constraints (new-jlist))
   (predefined-vertical-constraints (new-jlist))))

(defjclass common-c-group ()
  ((id 0)
   (master nil)
   ;; LinkedHashSet<CNode>: insertion-ordered, duplicates rejected (see %cg-add-cnode)
   (c-nodes (new-jlist))
   (start-pos double.negative-infinity)
   ;; HashSet<CNode>: iteration order is unspecified in Java, so a jset here
   (incoming-constraints (new-jset))
   (out-degree 0)
   (out-degree-real 0)
   (reference nil)
   (delta 0d0)
   (delta-normalized 0d0)))

(defjclass common-c-group-builder ()
  ((group (new-common-c-group))))

(defjclass common-c-node ()
  ((id 0)
   (origin nil)
   (type nil)
   (to-string-delegate nil)
   (c-group nil)
   (c-group-offset (new-k-vector))
   (hitbox-pre-compaction nil)
   (hitbox nil)
   (constraints (new-jlist))
   (start-pos double.negative-infinity)))

(defjclass c-node-builder ()
  ((node (new-common-c-node))
   (group-parent nil)))

(defjclass common-compare-fuzzy () ())

(defjclass common-quadruplet ()
  ((left nil)
   (right nil)
   (up nil)
   (down nil)))

(defjclass common-scanline-constraint-calculator (common-i-constraint-calculation-algorithm)
  ((compactor nil)
   (constraints-scanline-comparator #'%sc-scanline-compare)
   (constraints-scanline-handler (new-common-constraints-scanline-handler))))

(defjclass common-oned-timestamp ()
  ((low nil)
   (node nil)))

(defjclass common-constraints-scanline-handler (common-event-handler)
  ((intervals (new-jlist))
   (cand nil)
   (outer nil)))

(defjclass common-longest-path-compaction (common-i-compaction-algorithm) ())

(defjclass common-quadratic-constraint-calculation (common-i-constraint-calculation-algorithm) ())

(defjclass common-one-dimensional-compactor ()
  ((compaction-algorithm nil)
   (constraint-algorithm nil)
   (c-graph nil)
   (lock-fun nil)
   (spacings-handler nil)
   (direction nil)
   (finished nil)))
