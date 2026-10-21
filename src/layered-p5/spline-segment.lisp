;;;; spline-segment.lisp -- org.eclipse.elk.alg.layered.p5edges.splines.SplineSegment
;;;;
;;;; Java has two three-argument constructors:
;;;;   SplineSegment(LPort singlePort, Set<Pair<SideToProcess, LEdge>>, SideToProcess)
;;;;   SplineSegment(LEdge edge, SideToProcess, SideToProcess)
;;;; They are one defjctor that dispatches on the first argument.  The Java
;;;; Pair<SideToProcess, LEdge> is a cons (side . edge).

(in-package #:elk)

(defjconst spline-segment.hyperedge-pos-outer-rate 0.9d0)
(defjconst spline-segment.hyperedge-pos-mid-rate (- 1 0.9d0))
(defjconst spline-segment.one-half 0.5d0)

(defjctor spline-segment (a b c)
  (if (typep a 'l-port)
      (%ss-init-from-port this a b c)
      (%ss-init-from-edge this a b c)))

(defun %ss-anchor-y (port)
  (if (contains port-side.sides-north-south (get-side port))
      (get-property port layered-internal-properties.spline-ns-port-y-coord)
      (.y (get-absolute-anchor port))))

(defun %ss-add-edge (this edge)
  (js-add (.edges this) edge)
  (let ((ei (new-edge-information)))
    (jm-put (.edge-information this) edge ei)
    (setf (.start-y ei) (%ss-anchor-y (get-source edge))
          (.end-y ei) (%ss-anchor-y (get-target edge))
          (.normal-source-node ei) (spline-edge-router.is-normal-node (get-node (get-source edge)))
          (.normal-target-node ei) (spline-edge-router.is-normal-node (get-node (get-target edge)))
          (.inverted-left ei) (eq (get-side (get-source edge)) port-side.west)
          (.inverted-right ei) (eq (get-side (get-target edge)) port-side.east))
    ei))

(defun %ss-set-relevant-positions (this source-y target-y-min target-y-max)
  (let ((bb (.bounding-box this)))
    (setf (.y bb) (math.min source-y target-y-min))
    (setf (.height bb) (- (math.max source-y target-y-max) (.y bb))))
  (let ((outer spline-segment.hyperedge-pos-outer-rate)
        (mid spline-segment.hyperedge-pos-mid-rate))
    (if (< source-y target-y-min)
        ;; source lays below all target ports
        (let ((center (* spline-segment.one-half (+ source-y target-y-min))))
          (setf (.center-control-point-y this) center
                (.hyper-edge-top-y-pos this) (+ (* mid center) (* outer source-y))
                (.hyper-edge-bottom-y-pos this) (+ (* mid center) (* outer target-y-min))))
        ;; source lays above all target ports
        (let ((center (* spline-segment.one-half (+ source-y target-y-max))))
          (setf (.center-control-point-y this) center
                (.hyper-edge-top-y-pos this) (+ (* mid center) (* outer target-y-max))
                (.hyper-edge-bottom-y-pos this) (+ (* mid center) (* outer source-y)))))))

(defun %ss-init-from-port (this single-port edges source-side)
  (if (eq source-side side-to-process.left)
      (js-add (.left-ports this) single-port)
      (js-add (.right-ports this) single-port))
  (let ((y-min double.positive-infinity)
        (y-max double.negative-infinity))
    (jdo (pair edges)
      (let* ((side (car pair))
             (edge (cdr pair))
             (tgt-port (get-source edge)))
        (when (eq tgt-port single-port)
          (setf tgt-port (get-target edge)))
        (if (eq side side-to-process.left)
            (js-add (.left-ports this) tgt-port)
            (js-add (.right-ports this) tgt-port))
        (let ((y-pos (%ss-anchor-y tgt-port)))
          (setf y-min (math.min y-min y-pos)
                y-max (math.max y-max y-pos)))))
    (%ss-set-relevant-positions this (%ss-anchor-y single-port) y-min y-max)
    (jdo (pair edges)
      (%ss-add-edge this (cdr pair)))
    (setf (.is-straight this) nil)))

(defun %ss-init-from-edge (this edge source-side target-side)
  (if (eq source-side side-to-process.left)
      (js-add (.left-ports this) (get-source edge))
      (js-add (.right-ports this) (get-source edge)))
  (if (eq target-side side-to-process.left)
      (js-add (.left-ports this) (get-target edge))
      (js-add (.right-ports this) (get-target edge)))
  (%ss-add-edge this edge)
  (let ((source-y (%ss-anchor-y (get-source edge)))
        (target-y (%ss-anchor-y (get-target edge))))
    (%ss-set-relevant-positions this source-y target-y target-y)
    (setf (.is-straight this) (spline-edge-router.is-straight source-y target-y))))

(defjmethod is-hyper-edge ((this spline-segment))
  (> (jsize (.edges this)) 1))

(defjmethod compare-to ((this spline-segment) other)
  (- (.mark this) (.mark other)))
