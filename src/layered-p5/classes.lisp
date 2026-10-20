;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered.p5edges.**

(in-package #:elk)

;;; ------------------------------------------------------------ p5edges

(defjclass edge-router-factory (i-layout-phase-factory)
  ((edge-routing-strategy nil)))

(defjclass orthogonal-edge-router (i-layout-phase) ())

(defjclass polyline-edge-router (i-layout-phase)
  ;; Set<KVector> with KVector value equality: kept as a list searched with EQUALS.
  ((created-junction-points (new-jlist))))

;;; ------------------------------------------------------------ orthogonal.direction

(defjclass base-routing-direction-strategy ()
  ((created-junction-points (new-jlist))))

(defjclass west-to-east-routing-strategy (base-routing-direction-strategy) ())
(defjclass north-to-south-routing-strategy (base-routing-direction-strategy) ())
(defjclass south-to-north-routing-strategy (base-routing-direction-strategy) ())

;;; ------------------------------------------------------------ orthogonal

(defjclass hyper-edge-segment ()
  ((routing-strategy nil)
   (ports (new-jlist))
   (mark 0)
   (routing-slot 0)
   (start-position double.nan)
   (end-position double.nan)
   (incoming-connection-coordinates (new-jlist))
   (outgoing-connection-coordinates (new-jlist))
   (outgoing-segment-dependencies (new-jlist))
   (out-dep-weight 0)
   (critical-out-dep-weight 0)
   (incoming-segment-dependencies (new-jlist))
   (in-dep-weight 0)
   (critical-in-dep-weight 0)
   (split-partner nil)
   (split-by nil)))

(defjclass hyper-edge-segment-dependency ()
  ((type nil)
   (source nil)
   (target nil)
   (weight 0)))

(defjclass hyper-edge-cycle-detector () ())

(defjclass hyper-edge-segment-splitter ()
  ((routing-generator nil)))

(defjclass free-area ()
  ((start-position 0d0)
   (end-position 0d0)
   (size 0d0)))

(defjclass area-rating ()
  ((dependencies 0)
   (crossings 0)))

(defjclass orthogonal-routing-generator ()
  ((segment-splitter nil)
   (routing-strategy nil)
   (edge-spacing 0d0)
   (conflict-threshold 0d0)
   (critical-conflict-threshold 0d0)
   (debug-prefix nil)))

;;; ------------------------------------------------------------ splines

(defjclass splines-math () ())

(defjclass rectangle ()
  ((top double.max-value)
   (bottom (- double.max-value))
   (left double.max-value)
   (right (- double.max-value))))

(defjclass edge-information ()
  ((start-y 0d0)
   (end-y 0d0)
   (normal-source-node nil)
   (normal-target-node nil)
   (inverted-left nil)
   (inverted-right nil)))

(defjclass spline-segment ()
  ((handled nil)
   (left-ports (new-jset))
   (right-ports (new-jset))
   (outgoing (new-jlist))
   (incoming (new-jlist))
   (mark 0)
   (inweight 0)
   (outweight 0)
   (rank 0)
   (edges (new-jset))
   (is-straight nil)
   (bounding-box (new-elk-rectangle))
   (is-west-of-initial-layer nil)
   (x-delta 0d0)
   (source-port nil)
   (target-port nil)
   (initial-segment nil)
   (last-segment nil)
   (source-node nil)
   (target-node nil)
   (inverse-order nil)
   (hyper-edge-top-y-pos 0d0)
   (hyper-edge-bottom-y-pos 0d0)
   (center-control-point-y 0d0)
   (edge-information (new-jmap))))

;; SplineEdgeRouter.Dependency
(defjclass dependency ()
  ((source nil)
   (target nil)
   (weight 0)))

(defjclass spline-edge-router (i-layout-phase)
  ((edges-remaining-layer (new-jlist))
   (spline-segments-layer (new-jlist))
   (left-ports-layer (new-jset))
   (right-ports-layer (new-jset))
   (self-loops-layer (new-jset))
   (l-graph nil)
   (start-edges (new-jlist))
   (all-spline-segments (new-jlist))
   (edge-to-segment-map (new-jmap))
   (successing-edge (new-jmap))))

(defjclass polar-cp ()
  ((cp nil)
   (polar-coordinate nil)))

(defjclass nub-spline ()
  ((knot-vector (new-jlist))
   (control-points (new-jlist))
   (dim-nubs 0)
   (is-uniform nil)
   (is-clamped nil)
   (outer-box nil)
   (is-bezier nil)
   (min-knot 0d0)
   (max-knot 0d0)))

(defjclass nubs-self-loop (nub-spline)
  ((first-label-position (new-k-vector))))
