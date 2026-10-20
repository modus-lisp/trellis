;;;; core-math classes -- org.eclipse.elk.core.math

(in-package #:elk)

;;; org.eclipse.elk.graph.properties.IDataObject (parse/1 + toString).  Not
;;; defined by the graph module; defined here so the value types can carry it.
(defjinterface i-data-object)
(defjgeneric parse 1)

;;; KVector: public double fields x, y.  Kept minimal -- hot loops everywhere.
(defjclass k-vector (i-data-object jcloneable)
  ((x 0.0)
   (y 0.0)))

;;; KVectorChain extends LinkedList<KVector>: the elements live in ITEMS.
(defjclass k-vector-chain (i-data-object jcloneable)
  ((items (new-jlist))))

;;; Spacing (abstract) and its two subclasses.
(defjclass spacing (i-data-object jcloneable)
  ((top 0.0)
   (bottom 0.0)
   (left 0.0)
   (right 0.0)))

(defjclass elk-padding (spacing) ())
(defjclass elk-margin (spacing) ())

(defjclass elk-rectangle ()
  ((x 0.0)
   (y 0.0)
   (width 0.0)
   (height 0.0)))

(defjclass bezier-spline ()
  ((curves (new-jlist))))

;;; BezierSpline.BezierCurve (static nested class)
(defjclass bezier-curve ()
  ((start nil)
   (fst-control-pnt nil)
   (snd-control-pnt nil)
   (end nil)))

(defjinterface i-spline-interpolator)
(defjgeneric interpolate-points 1)
(defjgeneric interpolate-points 4)

(defjclass cubic-spline-interpolator (i-spline-interpolator) ())
