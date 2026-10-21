;;;; final-spline.lisp -- FinalSplineBendpointsCalculator.

(in-package #:elk)

;; Private helpers of this file are called before their defjmethod is compiled.
(define-jcalls :instance
  process
  index-nodes-per-layer
  calculate-control-points
  calculate-control-points-straight
  calculate-control-points-inverted-edge
  calculate-control-points-conservative
  calculate-control-points-sloppy
  node-to-bounding-box
  compute-sloppy-center-y
  calculate-bezier-bend-points
  insert-straightening-control-points
  segment-allows-sloppy-routing
  segment-node-distance-threshold)

(defjconst final-spline-bendpoints-calculator.one-half 0.5d0)
(defjconst final-spline-bendpoints-calculator.node-to-straightening-cp-gap 5d0)
(defjconst final-spline-bendpoints-calculator.sloppy-center-cp-multiplier 0.4d0)

(defun %fsbc-first-element (coll)
  "The first element of a Java Iterable (a LinkedHashSet's iterator().next())."
  (block first-element
    (jdo (x coll)
      (return-from first-element x))
    nil))

(defjmethod process ((this final-spline-bendpoints-calculator) graph progress-monitor)
  (setf (.edge-edge-spacing this) (get-property graph layered-options.spacing-edge-edge-between-layers))
  (setf (.edge-node-spacing this) (get-property graph layered-options.spacing-edge-node-between-layers))
  (setf (.spline-routing-mode this) (get-property graph layered-options.edge-routing-splines-mode))
  (setf (.compaction-strategy this) (get-property graph layered-options.compaction-post-compaction-strategy))
  (index-nodes-per-layer this graph)
  (let ((start-edges (new-jlist)))
    (jdo (layer (get-layers graph))
      (jdo (node (get-nodes layer))
        (jdo (e (get-outgoing-edges node))
          (when (and (not (is-self-loop e))
                     (has-property e layered-internal-properties.spline-route-start))
            (jl-add start-edges e)))))
    (jdo (e start-edges)
      (let ((spline (get-property e layered-internal-properties.spline-route-start)))
        (jdo (s spline)
          (calculate-control-points this s))
        (set-property e layered-internal-properties.spline-route-start nil)))
    (jdo (e start-edges)
      (let ((surviving-edge (get-property e layered-internal-properties.spline-surviving-edge))
            (edge-chain (get-property e layered-internal-properties.spline-edge-chain)))
        (calculate-bezier-bend-points this edge-chain surviving-edge)
        (set-property e layered-internal-properties.spline-edge-chain nil))))
  nil)

(defjmethod index-nodes-per-layer ((this final-spline-bendpoints-calculator) graph)
  (jdo (l (get-layers graph))
    (let ((index 0))
      (jdo (n l)
        (setf (.id n) index)
        (incf index)))))

(defjmethod calculate-control-points ((this final-spline-bendpoints-calculator) segment)
  "Dispatches the calculation of NUB control points for the passed SEGMENT to the
calculate-control-points-* methods based on the desired spline routing mode, whether the segment is
straight, whether it represents an in-layer edge due to inverted ports and whether it represents a
hyperedge."
  (unless (.handled segment)
    (setf (.handled segment) t)
    (jdo (edge (.edges segment))
      (let ((ei (jm-get (.edge-information segment) edge)))
        (cond
          ((and (.is-straight segment) (not (is-hyper-edge segment)))
           (calculate-control-points-straight this segment))
          ((or (.inverted-left ei) (.inverted-right ei))
           (calculate-control-points-inverted-edge this edge segment))
          (t
           (let ((sloppy (and (eq (.spline-routing-mode this) spline-routing-mode.sloppy)
                              (or (.normal-source-node ei) (.normal-target-node ei))
                              (segment-allows-sloppy-routing this segment)
                              (not (is-hyper-edge segment)))))
             (if sloppy
                 (calculate-control-points-sloppy this edge segment)
                 (calculate-control-points-conservative this edge segment)))))))
    (when (.inverse-order segment)
      (jdo (e (.edges segment))
        (jl-reverse (.items (get-bend-points e)))))))

(defjmethod calculate-control-points-straight ((this final-spline-bendpoints-calculator) segment)
  "Adds a single control point to a straight segment, halfway between the source and target layer."
  (let* ((bb (.bounding-box segment))
         (x-start-pos (.x bb))
         (x-end-pos (+ (.x bb) (.width bb)))
         (halfway (new-k-vector/2 (+ x-start-pos (/ (- x-end-pos x-start-pos) 2))
                                  (.center-control-point-y segment))))
    (add (get-bend-points (%fsbc-first-element (.edges segment))) halfway)))

(defjmethod calculate-control-points-inverted-edge ((this final-spline-bendpoints-calculator) edge containing-segment)
  "If EDGE is an inverted edge on the western side, control points are computed as in the ELK
documentation figure (inverted edge on the right side works analogously)."
  (let* ((bb (.bounding-box containing-segment))
         (start-x-pos (.x bb))
         (end-x-pos (+ (.x bb) (.width bb)))
         (ei (jm-get (.edge-information containing-segment) edge))
         (y-source-anchor (.start-y ei))
         (y-target-anchor (.end-y ei))
         (source-straight-cp (if (.inverted-left ei)
                                 (new-k-vector/2 end-x-pos y-source-anchor)
                                 (new-k-vector/2 start-x-pos y-source-anchor)))
         (target-straight-cp (if (.inverted-right ei)
                                 (new-k-vector/2 start-x-pos y-target-anchor)
                                 (new-k-vector/2 end-x-pos y-target-anchor)))
         (center-x-pos start-x-pos))
    (unless (.is-west-of-initial-layer containing-segment)
      (incf center-x-pos (.edge-node-spacing this)))
    (incf center-x-pos (+ (.x-delta containing-segment)
                          (* (+ (.rank containing-segment) 0) (.edge-edge-spacing this))))
    (let* ((source-vertical-cp (new-k-vector/2 center-x-pos y-source-anchor))
           (target-vertical-cp (new-k-vector/2 center-x-pos y-target-anchor))
           (is-hyperedge (> (size (.edges containing-segment)) 1)))
      (add-all (get-bend-points edge) (list source-straight-cp source-vertical-cp))
      (when is-hyperedge
        (add (get-bend-points edge)
             (new-k-vector/2 center-x-pos (.center-control-point-y containing-segment))))
      (add-all (get-bend-points edge) (list target-vertical-cp target-straight-cp)))))

(defjmethod calculate-control-points-conservative ((this final-spline-bendpoints-calculator) edge containing-segment)
  "For a regular upward pointing edge and the conservative routing style, control points are
computed as in the ELK documentation figure (downward pointing edges are handled analogously)."
  (let* ((bb (.bounding-box containing-segment))
         (start-x-pos (.x bb))
         (end-x-pos (+ (.x bb) (.width bb)))
         (ei (jm-get (.edge-information containing-segment) edge))
         (y-source-anchor (.start-y ei))
         (y-target-anchor (.end-y ei))
         (source-straight-cp (new-k-vector/2 start-x-pos y-source-anchor))
         (target-straight-cp (new-k-vector/2 end-x-pos y-target-anchor))
         (center-x-pos start-x-pos))
    (unless (.is-west-of-initial-layer containing-segment)
      (incf center-x-pos (.edge-node-spacing this)))
    (incf center-x-pos (+ (.x-delta containing-segment)
                          (* (+ (.rank containing-segment) 0) (.edge-edge-spacing this))))
    (let* ((source-vertical-cp (new-k-vector/2 center-x-pos y-source-anchor))
           (target-vertical-cp (new-k-vector/2 center-x-pos y-target-anchor))
           (is-hyperedge (> (size (.edges containing-segment)) 1)))
      (add-all (get-bend-points edge) (list source-straight-cp source-vertical-cp))
      (when is-hyperedge
        (add (get-bend-points edge)
             (new-k-vector/2 center-x-pos (.center-control-point-y containing-segment))))
      (add-all (get-bend-points edge) (list target-vertical-cp target-straight-cp)))))

(defjmethod calculate-control-points-sloppy ((this final-spline-bendpoints-calculator) edge containing-segment)
  "As opposed to the conservative strategy to insert control points, the sloppy version tries to
use less control points to make the splines curvier.  The method handles only the case in which one
of the two involved nodes is a normal node.  If all control points could be omitted, a single control
point is added halfway in between the two nodes (see compute-sloppy-center-y).  If no control points
can be omitted, the same control points are used as with the conservative strategy."
  (let* ((ei (jm-get (.edge-information containing-segment) edge))
         (bb (.bounding-box containing-segment))
         (start-x-pos (.x bb))
         (end-x-pos (+ (.x bb) (.width bb)))
         (y-source-anchor (.start-y ei))
         (y-target-anchor (.end-y ei))
         (edge-points-downwards (< y-source-anchor y-target-anchor))
         (source-straight-cp (new-k-vector/2 start-x-pos y-source-anchor))
         (target-straight-cp (new-k-vector/2 end-x-pos y-target-anchor))
         (center-x-pos (/ (+ start-x-pos end-x-pos) 2))
         (source-vertical-cp (new-k-vector/2 center-x-pos y-source-anchor))
         (target-vertical-cp (new-k-vector/2 center-x-pos y-target-anchor))
         (center-y-pos (compute-sloppy-center-y this edge y-source-anchor y-target-anchor))
         (v1 (get-absolute-anchor (.source-port containing-segment)))
         (v2 (new-k-vector/2 center-x-pos center-y-pos))
         (v3 (get-absolute-anchor (.target-port containing-segment)))
         (approx (elk-math.approximate-bezier-segment 2 (list v1 v2 v3)))
         (short-cut-source nil)
         (src (get-node (.source-port containing-segment))))
    (when (and src (get-layer src) (.normal-source-node ei))
      (let ((need-to-check-src (or (and edge-points-downwards
                                        (< (.id src) (1- (size (get-nodes (get-layer src))))))
                                   (and (not edge-points-downwards) (> (.id src) 0)))))
        (if (not need-to-check-src)
            (setf short-cut-source t)
            (let* ((neighbor-index (.id src))
                   (neighbor-index (if edge-points-downwards (1+ neighbor-index) (1- neighbor-index)))
                   (neighbor (jl-get (get-nodes (get-layer src)) neighbor-index))
                   (box (node-to-bounding-box this neighbor)))
              (setf short-cut-source
                    (not (or (elk-math.intersects box v1 (elt approx 0))
                             (elk-math.contains box v1 (elt approx 0)))))))))
    (let ((short-cut-target nil)
          (tgt (get-node (.target-port containing-segment))))
      (when (and tgt (get-layer tgt) (.normal-target-node ei))
        (let ((need-to-check-tgt (or (and edge-points-downwards (> (.id tgt) 0))
                                     (and (not edge-points-downwards)
                                          (< (.id tgt) (1- (size (get-nodes (get-layer tgt)))))))))
          (if (not need-to-check-tgt)
              (setf short-cut-target t)
              (let* ((neighbor-index (.id tgt))
                     (neighbor-index (if edge-points-downwards (1- neighbor-index) (1+ neighbor-index)))
                     (neighbor (jl-get (get-nodes (get-layer tgt)) neighbor-index))
                     (box (node-to-bounding-box this neighbor)))
                (setf short-cut-target
                      (not (or (elk-math.intersects box (elt approx 0) v3)
                               (elk-math.contains box (elt approx 0) v3))))))))
      (when (and short-cut-source short-cut-target)
        (add (get-bend-points edge) v2))
      (unless short-cut-source
        (add-all (get-bend-points edge) (list source-straight-cp source-vertical-cp)))
      (unless short-cut-target
        (add-all (get-bend-points edge) (list target-vertical-cp target-straight-cp))))))

(defjmethod node-to-bounding-box ((this final-spline-bendpoints-calculator) node)
  (let* ((pos (get-position node))
         (size (get-size node))
         (m (get-margin node)))
    (new-elk-rectangle/4 (- (.x pos) (.left m))
                         (- (.y pos) (.top m))
                         (+ (.x size) (get-horizontal m))
                         (+ (.y size) (get-vertical m)))))

(defjmethod compute-sloppy-center-y ((this final-spline-bendpoints-calculator) edge y-source-anchor y-target-anchor)
  (let ((indegree 0)
        (outdegree 0))
    (if (get-source edge)
        (jdo (port (get-ports (get-node (get-target edge))))
          (incf indegree (size (get-incoming-edges port))))
        (setf indegree 1))
    (if (get-target edge)
        (jdo (port (get-ports (get-node (get-source edge))))
          (incf outdegree (size (get-outgoing-edges port))))
        (setf outdegree 1))
    (let* ((degree-diff (to-int (math.signum (- outdegree indegree))))
           (center-y-pos (+ (/ (+ y-target-anchor y-source-anchor) 2)
                            (* (- y-target-anchor y-source-anchor)
                               (* final-spline-bendpoints-calculator.sloppy-center-cp-multiplier
                                  degree-diff)))))
      center-y-pos)))

(defjmethod calculate-bezier-bend-points ((this final-spline-bendpoints-calculator) edge-chain surviving-edge)
  "Collects all NUB control points computed for a spline segment and converts them to bezier
control points.  Note that additional NUB control points may be added before the conversion is
performed."
  (unless (is-empty edge-chain)
    (let* ((all-cp (new-k-vector-chain))
           (edge (if surviving-edge surviving-edge (jl-get edge-chain 0)))
           (source-port (get-source edge)))
      (unless (spline-edge-router.is-qualified-as-starting-node (get-node source-port))
        (jthrow 'illegal-argument-exception
                "The target node of the edge must be a normal node or a northSouthPort."))
      (add-last all-cp (get-absolute-anchor source-port))
      (when (contains port-side.sides-north-south (get-side source-port))
        (let* ((y (get-property source-port layered-internal-properties.spline-ns-port-y-coord))
               (north-south-cp (new-k-vector/2 (.x (get-absolute-anchor source-port)) y)))
          (add-last all-cp north-south-cp)))
      (let ((last-cp nil)
            (add-mid-point nil))
        (jdo (current-edge edge-chain)
          (let ((current-bend-points (get-bend-points current-edge)))
            (unless (is-empty current-bend-points)
              (if add-mid-point
                  (let ((halfway (scale (add last-cp (get-first current-bend-points))
                                        final-spline-bendpoints-calculator.one-half)))
                    (add-last all-cp halfway)
                    (setf add-mid-point nil))
                  (setf add-mid-point t))
              (setf last-cp (clone (get-last current-bend-points)))
              (add-all all-cp current-bend-points)
              (clear current-bend-points)))))
      (let ((target-port (get-target edge)))
        (when (contains port-side.sides-north-south (get-side target-port))
          (let* ((y (get-property target-port layered-internal-properties.spline-ns-port-y-coord))
                 (north-south-cp (new-k-vector/2 (.x (get-absolute-anchor target-port)) y)))
            (add-last all-cp north-south-cp)))
        (add-last all-cp (get-absolute-anchor target-port))
        (when (eq (.spline-routing-mode this) spline-routing-mode.conservative)
          (insert-straightening-control-points this all-cp source-port target-port))
        (let ((nub-spline (new-nub-spline t spline-edge-router.spline-dimension all-cp)))
          (add-all (get-bend-points edge) (get-bezier-cp nub-spline)))))))

(defjmethod insert-straightening-control-points ((this final-spline-bendpoints-calculator) all-cps src-port tgt-port)
  (let* ((cp-first (get-first all-cps))
         (cp-second (j-get all-cps 1))
         (v (new-k-vector/1 (splines-math.port-side-to-direction (get-side src-port))))
         (v2 nil))
    (scale v final-spline-bendpoints-calculator.node-to-straightening-cp-gap)
    (setf v2 (sub (clone cp-second) cp-first))
    (let ((straighten-beginning (new-k-vector/2 (%fsbc-abs-min (.x v) (.x v2))
                                                (%fsbc-abs-min (.y v) (.y v2)))))
      (add straighten-beginning cp-first)
      (add all-cps 1 straighten-beginning))
    (let* ((cp-last (get-last all-cps))
           (second-last (j-get all-cps (1- (size all-cps)))))
      (setf v (new-k-vector/1 (splines-math.port-side-to-direction (get-side tgt-port))))
      (scale v final-spline-bendpoints-calculator.node-to-straightening-cp-gap)
      (setf v2 (sub (clone second-last) cp-last))
      (let ((straighten-ending (new-k-vector/2 (%fsbc-abs-min (.x v) (.x v2))
                                               (%fsbc-abs-min (.y v) (.y v2)))))
        (add straighten-ending cp-last)
        (add all-cps (1- (size all-cps)) straighten-ending)))))

(defun %fsbc-abs-min (d1 d2)
  (if (< (abs d1) (abs d2)) d1 d2))

(defjmethod segment-allows-sloppy-routing ((this final-spline-bendpoints-calculator) segment)
  (block sloppy-routing
  (when (eq (.compaction-strategy this) graph-compaction-strategy.none)
    (return-from sloppy-routing t))
  (let* ((bb (.bounding-box segment))
         (start-x-pos (.x bb))
         (end-x-pos (+ (.x bb) (.width bb))))
    (when (.initial-segment segment)
      (let* ((n (.source-node segment))
             (tt (segment-node-distance-threshold this n))
             (node-segment-distance (- start-x-pos (+ (.x (get-position n)) (.x (get-size n))))))
        (when (> node-segment-distance tt)
          (return-from sloppy-routing nil))))
    (when (.last-segment segment)
      (let* ((n (.target-node segment))
             (tt (segment-node-distance-threshold this n))
             (node-segment-distance (- (.x (get-position n)) end-x-pos)))
        (when (> node-segment-distance tt)
          (return-from sloppy-routing nil))))
    t)))

(defjmethod segment-node-distance-threshold ((this final-spline-bendpoints-calculator) n)
  "A magically determined threshold value."
  (- (.x (get-size (get-layer n))) (/ (.x (get-size n)) 2.0d0)))
