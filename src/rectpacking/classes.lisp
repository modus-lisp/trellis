;;;; classes.lisp -- every class of org.eclipse.elk.alg.rectpacking (the enums
;;;; and option holders are generated).  Java's Block is J-BLOCK (see
;;;; util-blocks.lisp).

(in-package #:elk)

;;; util
(defjclass j-block ()
  ((smallest-rect-width double.positive-infinity)
   (min-width 0d0)
   (width 0d0)
   (min-height 0d0)
   (smallest-rect-height double.positive-infinity)
   (average-height 0d0)
   (max-height 0d0)
   (height 0d0)
   (children (new-jlist))
   (rows (new-jlist))
   (x 0d0)
   (y 0d0)
   (parent-row nil)
   (stack nil)
   (node-node-spacing 0d0)
   (fixed nil)
   (position-fixed nil)))

(defjclass block-row ()
  ((x 0d0)
   (y 0d0)
   (width 0d0)
   (height 0d0)
   (node-node-spacing 0d0)
   (rects (new-jlist))))

(defjclass block-stack ()
  ((blocks nil)
   (x 0d0)
   (y 0d0)
   (width 0d0)
   (height 0d0)
   (node-node-spacing 0d0)))

(defjclass rect-row ()
  ((height 0d0)
   (width 0d0)
   (y 0d0)
   (node-node-spacing 0d0)
   (children (new-jlist))
   (stacks (new-jlist))
   (potential-additional-width-to-get-last-block 0d0)))

(defjclass drawing-data ()
  ((scale-measure 0d0)
   (drawing-width 0d0)
   (drawing-height 0d0)
   (area 0d0)
   (aspect-ratio 0d0)
   (dar 0d0)
   (placement-option nil)
   (next-x-coordinate 0d0)
   (next-y-coordinate 0d0)))

;;; p1widthapproximation
(defjclass area-approximation ()
  ((aspect-ratio 0d0)
   (goal nil)
   (lp-shift nil)))

(defjinterface best-candidate-filter)
(defjclass area-filter (best-candidate-filter) ())
(defjclass aspect-ratio-filter (best-candidate-filter) ())
(defjclass scale-measure-filter (best-candidate-filter) ())
(defjclass greedy-width-approximator (i-layout-phase) ())
(defjclass target-width-width-approximator (i-layout-phase) ())

;;; p2packing
(defjclass compaction () ())
(defjclass compactor (i-layout-phase) ())
(defjclass initial-placement () ())
(defjclass no-placement (i-layout-phase) ())
(defjclass simple-placement (i-layout-phase) ())
(defjclass row-filling-and-compaction ()
  ((aspect-ratio 0d0)
   (node-node-spacing 0d0)
   (potential-row-width-increase-min double.positive-infinity)
   (potential-row-width-increase-max double.positive-infinity)
   (potential-row-width-decrease-min double.positive-infinity)
   (potential-row-width-decrease-max double.positive-infinity)
   (rows-to-fill nil)))

;;; p3whitespaceelimination
(defjclass equal-whitespace-eliminator (i-layout-phase) ())
(defjclass rectangle-expansion () ())
(defjclass to-aspectratio-node-expander () ())

;;; intermediate
(defjinterface i-node-size-comparator)
(defjclass node-size-comparator () ())
(defjclass node-size-reorderer (i-layout-processor) ())
(defjclass interactive-node-reorderer (i-layout-processor) ())
(defjclass min-size-pre-processor (i-layout-processor) ())
(defjclass min-size-post-processor (i-layout-processor) ())

;;; top level
(defjclass rect-packing-layout-provider (abstract-layout-provider) ())
(defjclass interactive-rect-packing-graph-visitor () ())
