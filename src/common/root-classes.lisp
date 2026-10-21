;;;; common/root-classes.lisp -- org.eclipse.elk.alg.common (see tools/classnames.tsv)
;;;;
;;;; Classes of the ROOT package: Point (+ its Quadrant enum, which lives in
;;;; gen-enums as common-quadrant), TEdge, TTriangle, Tree, ICostFunction, the
;;;; NodeMicroLayout / RectilinearConvexHull state, and the two EventHandler
;;;; classes nested in RectilinearConvexHull.  Constructors and methods are in
;;;; root.lisp.
;;;;
;;;; The two event handlers extend JOBJECT rather than the compaction module's
;;;; EventHandler interface, because this file loads before compaction does.
;;;; Dispatch is by the method HANDLE/1, which is all Scanline needs.

(in-package #:elk)

(defjclass common-point ()
  ((x 0d0)
   (y 0d0)
   (quadrant nil)
   (convex t)))

(defjclass t-edge ()
  ((u nil)
   (v nil)))

(defjclass t-triangle ()
  ((a nil)
   (b nil)
   (c nil)
   (t-edges nil)
   (vertices nil)
   (circumcenter nil)))

(defjclass tree ()
  ((node nil)
   (children nil)))

(defjinterface i-cost-function)

(defjclass node-micro-layout ()
  ((adapter nil)))

(defjclass common-rectilinear-convex-hull ()
  ((hull (new-jlist))
   (x-max1 nil)
   (x-max2 nil)
   (x-min1 nil)
   (x-min2 nil)
   (y-max1 nil)
   (y-max2 nil)
   (y-min1 nil)
   (y-min2 nil)))

(defjclass common-maximal-elements-event-handler ()
  ((quadrant nil)
   (points (new-jlist))
   (maximal-y 0d0)
   (compare nil)))

(defjclass common-rectangle-event-handler ()
  ((outer nil)
   (rects (new-jlist))
   (min-y nil)
   (max-y nil)
   (last-x 0d0)
   (queued nil)
   (queued-pnt nil)))
