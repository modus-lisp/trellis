;;;; util-drawing.lisp -- org.eclipse.elk.alg.rectpacking.util: DrawingData,
;;;; DrawingUtil.  (DrawingDataDescriptor is generated in gen-enums.)

(in-package #:elk)

(defjctor drawing-data (dar drawing-width drawing-height placement-option)
  (this-ctor drawing-data this dar drawing-width drawing-height placement-option 0d0 0d0))

(defjctor drawing-data (dar drawing-width drawing-height placement-option next-x next-y)
  (setf (.dar this) dar
        (.drawing-width this) drawing-width
        (.drawing-height this) drawing-height
        (.placement-option this) placement-option
        (.next-x-coordinate this) next-x
        (.next-y-coordinate this) next-y)
  (%rp-drawing-data-calc this))

(defun %rp-drawing-data-calc (this)
  "calcAreaAspectRatioScaleMeasure: only for a positive drawing; otherwise the old values stay."
  (when (and (> (.drawing-width this) 0) (> (.drawing-height this) 0))
    (setf (.area this) (* (.drawing-width this) (.drawing-height this)))
    (setf (.aspect-ratio this) (/ (.drawing-width this) (.drawing-height this)))
    (setf (.scale-measure this)
          (drawing-util.compute-scale-measure/3 (.drawing-width this) (.drawing-height this) (.dar this)))))

(defjmethod get-drawing-width ((this drawing-data)) (.drawing-width this))
(defjmethod set-drawing-width ((this drawing-data) w)
  (setf (.drawing-width this) w)
  (%rp-drawing-data-calc this)
  nil)
(defjmethod get-drawing-height ((this drawing-data)) (.drawing-height this))
(defjmethod set-drawing-height ((this drawing-data) h)
  (setf (.drawing-height this) h)
  (%rp-drawing-data-calc this)
  nil)
(defjmethod get-scale-measure ((this drawing-data)) (.scale-measure this))
(defjmethod get-area ((this drawing-data)) (.area this))
(defjmethod get-aspect-ratio ((this drawing-data)) (.aspect-ratio this))
(defjmethod get-placement-option ((this drawing-data)) (.placement-option this))
(defjmethod set-placement-option ((this drawing-data) option)
  (setf (.placement-option this) option)
  nil)
(defjmethod get-next-x-coordinate ((this drawing-data)) (.next-x-coordinate this))
(defjmethod set-next-x-coordinate ((this drawing-data) x)
  (setf (.next-x-coordinate this) x)
  nil)
(defjmethod get-next-y-coordinate ((this drawing-data)) (.next-y-coordinate this))
(defjmethod set-next-y-coordinate ((this drawing-data) y)
  (setf (.next-y-coordinate this) y)
  nil)
(defjmethod get-desired-aspect-ratio ((this drawing-data)) (.dar this))

;;; ------------------------------------------------------------ DrawingUtil

(defjstatic drawing-util.compute-scale-measure (width height dar)
  (math.min (/ dar width) (/ 1d0 height)))

(defjstatic drawing-util.reset-coordinates (graph)
  (jdo (node graph)
    (set-location/2 node 0d0 0d0))
  nil)

(defjstatic drawing-util.calculate-dimensions (rows node-node-spacing)
  (let ((max-width 0d0)
        (new-height 0d0)
        (index 0))
    (jdo (row rows)
      (setf max-width (math.max max-width (get-width/0 row)))
      (incf new-height (+ (get-height/0 row) (if (> index 0) node-node-spacing 0d0)))
      (incf index))
    (new-k-vector/2 max-width new-height)))

(defjstatic drawing-util.calculate-dimensions (rectangles)
  (let ((max-width 0d0)
        (max-height 0d0))
    (jdo (node rectangles)
      (setf max-width (math.max (+ (get-width/0 node) (get-x/0 node)) max-width))
      (setf max-height (math.max (+ (get-height/0 node) (get-y/0 node)) max-height)))
    (new-k-vector/2 max-width max-height)))
