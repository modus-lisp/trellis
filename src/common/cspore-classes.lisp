;;;; cspore-classes.lisp -- org.eclipse.elk.alg.common.spore classes

(in-package #:elk)

(defjinterface i-overlap-handler)
(defjgeneric handle 2)

(defjclass node ()
  ((original-vertex nil)
   (vertex nil)
   (rect nil)))

(defjclass scanline-overlap-check ()
  ((overlap-handler nil)
   (overlaps-scanline-comparator nil)))

;; Inner classes of ScanlineOverlapCheck (Java: private class Timestamp, and
;; OverlapsScanlineHandler implementing EventHandler<Timestamp>).
(defjclass common-spore-timestamp ()
  ((low nil)
   (node nil)))

(defjclass overlaps-scanline-handler (common-event-handler)
  ((intervals-sort-left (new-jlist))
   (outer nil)))
