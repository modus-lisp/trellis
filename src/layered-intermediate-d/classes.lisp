;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered.intermediate.wrapping
;;;; and org.eclipse.elk.alg.layered.intermediate.compaction.

(in-package #:elk)

;;; ------------------------------------------------------------ wrapping

(defjinterface i-cut-index-calculator)
(defjgeneric get-cut-indexes 2)
(defjgeneric guarantee-valid 0)

;; ICutIndexCalculator.ManualCutIndexCalculator
(defjclass manual-cut-index-calculator (i-cut-index-calculator) ())
(defjclass ard-cut-index-heuristic (i-cut-index-calculator) ())
(defjclass msd-cut-index-heuristic (i-cut-index-calculator) ())

(defjclass graph-stats ()
  ((graph nil)
   (spacing 0.0)
   (in-layer-spacing 0.0)
   (dar 0.0)
   (longest-path 0)
   ;; computed on demand (Java Double, null = not yet computed)
   (max-width nil)
   (max-height nil)
   (sum-width nil)
   (widths nil)
   (heights nil)
   (cuts-allowed nil)))

(defjclass breaking-point-inserter (i-layout-processor) ())

;; BreakingPointInserter.BPInfo
(defjclass bp-info ()
  ((start nil)
   (end nil)
   (node-start-edge nil)
   (start-end-edge nil)
   (original-edge nil)
   (start-in-layer-dummy nil)
   (start-in-layer-edge nil)
   (end-in-layer-dummy nil)
   (end-in-layer-edge nil)
   (prev nil)
   (next nil)))

;; BreakingPointInserter.Cut (private)
(defjclass cut ()
  ((index 0)
   (new-index 0)
   (prev nil)
   (suc nil)
   (assigned nil)))

(defjclass breaking-point-processor (i-layout-processor) ())
(defjclass breaking-point-remover (i-layout-processor)
  ((edge-routing nil)))
(defjclass single-edge-graph-wrapper (i-layout-processor) ())

;;; ------------------------------------------------------------ compaction

(defjclass vertical-segment ()
  ((potential-group-parents (new-jlist))
   (represented-l-edges (new-jlist))
   (affected-bends (new-jlist))
   (affected-bounding-boxes (new-jlist))
   (hitbox (new-elk-rectangle))
   (junction-points (new-k-vector-chain))
   (ignore-spacing (new-common-quadruplet))
   (constraints (new-jlist))
   (a-port nil)
   (joined (new-jlist))))

(defjclass horizontal-graph-compactor (i-layout-processor)
  ((l-graph nil)
   ;; the anonymous ISpacingsHandler, created in the constructor (it needs THIS)
   (special-spacings-handler nil)))

;; The anonymous ISpacingsHandler of HorizontalGraphCompactor; OUTER is the compactor
;; (its lGraph field is read at call time).
(defjclass %hgc-special-spacings-handler (common-i-spacings-handler)
  ((outer nil)))

(defjclass l-graph-to-c-graph-transformer ()
  ((c-graph nil)
   (layered-graph nil)
   (edge-routing nil)
   (comment-offsets (new-jmap))
   (nodes-map (new-jmap))
   (vertical-segments-map (new-jmap))
   (lock-map (new-jmap))))

(defjclass network-simplex-compaction (common-i-compaction-algorithm)
  ((compactor nil)
   (network-simplex-graph nil)
   (n-nodes nil)
   (index 0)))

(defjclass edge-aware-scanline-constraint-calculation (common-scanline-constraint-calculator)
  ((vertical-edge-edge-spacing 0.0)
   (edge-routing nil)))
