;;;; disco/classes.lisp -- org.eclipse.elk.alg.disco classes.

(in-package #:elk)

(defjinterface disco-i-compactor)
(defjinterface disco-i-graph-transformer)

(defjclass dis-co-layout-provider (abstract-layout-provider)
  ((result nil)
   (component-spacing 0.0)))

(defjclass dis-co-polyomino-compactor (disco-i-compactor)
  ((grid-cell-size-x 0.0)
   (grid-cell-size-y 0.0)
   (upper-bound 100.0)
   (cmp-graph nil)
   (polys nil)
   (grid nil)
   (aspect-ratio nil)
   (fill nil)))

;; graph
(defjclass dc-component ()
  ((offset nil)
   (shapes nil)
   (changed t)
   (bounds nil)
   (min-corner-of-bounding-rectangle nil)
   (id -1)))

(defjclass dc-extension ()
  ((parent nil)
   (direction nil)
   (offset nil)
   (width 0.0)))

(defjclass dc-graph (map-property-holder)
  ((components (new-jset))      ; LinkedHashSet
   (dimensions nil)
   (inset 0.0)))

(defjclass dc-element (map-property-holder)
  ((shape nil)
   (bounds nil)
   (cp nil)
   (coords nil)
   (parent-coords nil)
   (extensions (new-jlist))))

;; structures
(defjclass dc-polyomino (polyomino)
  ((representee nil)
   (p-width 0)
   (p-height 0)
   (cell-size-x 0.0)
   (cell-size-y 0.0)))

;; transform
(defjclass disco-elk-graph-transformer (disco-i-graph-transformer)
  ((parent nil)
   (element-mapping (new-jmap))
   (incoming-extensions-mapping (new-jmap))
   (outgoing-extensions-mapping (new-jmap))
   (transformed-graph nil)
   (component-spacing 0.0)))

;; ElkGraphTransformer.OffsetApplier and its inner OffSetToChainApplier
(defjclass offset-applier ()
  ((outer nil)
   (offset nil)))
(defjclass off-set-to-chain-applier ()
  ((outer nil)))
