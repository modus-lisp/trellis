;;;; spline-edge-router.lisp -- org.eclipse.elk.alg.layered.p5edges.splines.SplineEdgeRouter
;;;;
;;;; The router runs over pairs of adjacent layers.  Per pair it creates the
;;;; spline segments (single edges and hyper-edges), orders them with a
;;;; dependency graph (cycle breaking + topological numbering), places the
;;;; right layer, and records each edge chain on its start edge.  The control
;;;; points are computed later by FinalSplineBendpointsCalculator.

(in-package #:elk)

;; Private instance methods of SplineEdgeRouter, called before their
;; definitions in this file.
(define-jcalls :instance
  clear-then-fill-mappings
  create-segments-and-compute-ranking
  compute-sloppy-spacing
  find-and-add-successor
  get-edge-chain
  get-spline-path
  create-dependency
  create-spline-segments
  create-spline-segments-for-hyper-edges)

(defjconst spline-edge-router.spline-dimension 3)
(defjconst spline-edge-router.max-vertical-diff-for-straight 0.2d0)
(defjconst spline-edge-router.int-min-value -2147483648)

;;; ------------------------------------------------------------ Dependency

;; SplineEdgeRouter.Dependency: registers itself with both segments.
(defjctor dependency (source target weight)
  (setf (.source this) source
        (.target this) target
        (.weight this) weight)
  (jl-add (.outgoing source) this)
  (jl-add (.incoming target) this))

;;; ------------------------------------------------------------ configuration

(defjmethod get-layout-processor-configuration ((this spline-edge-router) graph)
  (let ((configuration
          (%p5-config
           (list :after layered-phases.p5-edge-routing
                 layered-intermediate-processor-strategy.final-spline-bendpoints-calculator)
           (list :before layered-phases.p3-node-ordering
                 layered-intermediate-processor-strategy.inverted-port-processor)))
        (graph-properties (get-property graph layered-internal-properties.graph-properties)))
    (when (contains graph-properties layered-graph-properties.self-loops)
      (add-all configuration (%p5-self-loop-additions)))
    (when (contains graph-properties layered-graph-properties.center-labels)
      (add-all configuration (%p5-center-label-additions)))
    (when (contains graph-properties layered-graph-properties.north-south-ports)
      ;; the spline router runs the north/south postprocessor BEFORE edge routing,
      ;; unlike the polyline and orthogonal routers (as in SplineEdgeRouter.java)
      (add-all configuration
               (%p5-config
                (list :before layered-phases.p3-node-ordering
                      layered-intermediate-processor-strategy.north-south-port-preprocessor)
                (list :before layered-phases.p5-edge-routing
                      layered-intermediate-processor-strategy.north-south-port-postprocessor))))
    (when (contains graph-properties layered-graph-properties.end-labels)
      (add-all configuration (%p5-end-label-additions)))
    configuration))

;;; ------------------------------------------------------------ process

(defun %ser-first-of (coll)
  (block first-of
    (jdo (x coll) (return-from first-of x))
    nil))

(defun %ser-slot-count (segments)
  "max over the non-straight segments of rank + 1 (0 when there are none)."
  (let ((count 0))
    (jdo (e segments)
      (unless (.is-straight e)
        (setf count (max count (1+ (.rank e))))))
    count))

(defjmethod process ((this spline-edge-router) layered-graph monitor)
  (begin monitor "Spline edge routing" 1)
  (if (zerop (jsize (get-layers layered-graph)))
      (setf (.x (get-size layered-graph)) 0d0)
      (%ser-route this layered-graph))
  (done monitor))

(defun %ser-route (this layered-graph)
  (let* ((node-node-spacing (get-property layered-graph layered-options.spacing-node-node-between-layers))
         (edge-node-spacing (get-property layered-graph layered-options.spacing-edge-node-between-layers))
         (edge-edge-spacing (get-property layered-graph layered-options.spacing-edge-edge-between-layers))
         (sloppy-routing (eq (get-property layered-graph layered-options.edge-routing-splines-mode)
                             spline-routing-mode.sloppy))
         (sloppy-layer-spacing-factor
           (get-property layered-graph layered-options.edge-routing-splines-sloppy-layer-spacing-factor))
         (layers (get-layers layered-graph))
         (layer-count (length layers))
         (first-layer (jl-get layers 0))
         (last-layer (jl-get layers (1- layer-count)))
         ;; check if the first and/or last layer are populated with external port dummies
         (is-left-layer-external (%p5-all-external-p (get-nodes first-layer)))
         (is-right-layer-external (%p5-all-external-p (get-nodes last-layer)))
         (layer-index 0)
         (left-layer nil)
         (xpos 0.0d0))
    (setf (.l-graph this) layered-graph)
    (jl-clear (.start-edges this))
    (jl-clear (.all-spline-segments this))
    (clrhash (.successing-edge this))
    ;; Java: do { ... } while (rightLayer != null), so the last pass sees a nil right layer.
    (loop
      (let ((right-layer (if (< layer-index layer-count)
                             (prog1 (jl-get layers layer-index) (incf layer-index))
                             nil)))
        ;; fresh start for this pair of layers
        (clear-then-fill-mappings this left-layer right-layer)
        (create-segments-and-compute-ranking this)
        (let* ((slot-count (%ser-slot-count (.spline-segments-layer this)))
               (x-segment-delta 0.0d0)
               (right-layer-position xpos)
               (is-special-left-layer (or (null left-layer)
                                          (and is-left-layer-external (eq left-layer first-layer))))
               (is-special-right-layer (or (null right-layer)
                                           (and is-right-layer-external (eq right-layer last-layer)))))
          ;; compute horizontal positions just as for the OrthogonalEdgeRouter
          (if (> slot-count 0)
              (let ((increment 0.0d0))
                (when left-layer
                  (incf increment edge-node-spacing))
                (incf increment (* (1- slot-count) edge-edge-spacing))
                (when right-layer
                  (incf increment edge-node-spacing))
                ;; sloppy routing may want to reserve more space in-between a pair of layers
                (when (and sloppy-routing right-layer)
                  (setf increment
                        (math.max increment
                                  (compute-sloppy-spacing this right-layer edge-edge-spacing
                                                          node-node-spacing sloppy-layer-spacing-factor))))
                ;; if we are between two layers, make sure their minimal spacing is preserved
                (when (and (< increment node-node-spacing)
                           (not is-special-left-layer) (not is-special-right-layer))
                  (setf x-segment-delta (/ (- node-node-spacing increment) 2.0d0))
                  (setf increment node-node-spacing))
                (incf right-layer-position increment))
              (when (and (not is-special-left-layer) (not is-special-right-layer))
                ;; If all edges are straight, use the usual spacing
                (incf right-layer-position node-node-spacing)))
          ;; place right layer's nodes
          (when right-layer
            (l-graph-util.place-nodes-horizontally right-layer right-layer-position))
          ;; tentative start and end points; FinalSplineBendpointsCalculator refines them
          (jdo (segment (.spline-segments-layer this))
            (let ((bb (.bounding-box segment)))
              (setf (.x bb) xpos
                    (.width bb) (- right-layer-position xpos)))
            (setf (.x-delta segment) x-segment-delta
                  (.is-west-of-initial-layer segment) (null left-layer)))
          (jl-add-all (.all-spline-segments this) (.spline-segments-layer this))
          ;; proceed to the next layer
          (setf xpos right-layer-position)
          (when right-layer
            (incf xpos (.x (get-size right-layer))))
          (setf left-layer right-layer)
          (unless right-layer
            (return)))))
    ;; remember the spline paths for the control point calculation of a later processor
    (jdo (edge (.start-edges this))
      (let ((edge-chain (get-edge-chain this edge)))
        (set-property edge layered-internal-properties.spline-edge-chain edge-chain)
        (let ((spline (get-spline-path this edge)))
          (set-property edge layered-internal-properties.spline-route-start spline))))
    ;; assign final width of the layering and thus the overall graph
    (setf (.x (get-size layered-graph)) xpos)
    (setf (.l-graph this) nil)))

(defjmethod clear-then-fill-mappings ((this spline-edge-router) left-layer right-layer)
  (js-clear (.left-ports-layer this))
  (js-clear (.right-ports-layer this))
  (jl-clear (.edges-remaining-layer this))
  (jl-clear (.spline-segments-layer this))
  (js-clear (.self-loops-layer this))
  ;; iterate over all outgoing edges on the left layer
  (when left-layer
    (jdo (node (get-nodes left-layer))
      (jdo (source-port (get-ports node port-side.east))
        (js-add (.left-ports-layer this) source-port)
        (%ser-process-outgoing this source-port left-layer right-layer))))
  (when right-layer
    (jdo (node (get-nodes right-layer))
      ;; handle all self-loops, no matter on which port-side they are
      (jdo (port (get-ports node))
        (jdo (edge (get-outgoing-edges port))
          (when (is-self-loop edge)
            (js-add (.self-loops-layer this) edge))))
      ;; iterate over all outgoing edges on the right layer
      (jdo (source-port (get-ports node port-side.west))
        (js-add (.right-ports-layer this) source-port)
        (%ser-process-outgoing this source-port left-layer right-layer)))))

(defun %ser-process-outgoing (this source-port left-layer right-layer)
  "The edges of one source port, as both halves of clearThenFillMappings do them."
  (jdo (edge (get-outgoing-edges source-port))
    ;; self-loops are handled separately
    (unless (is-self-loop edge)
      ;; add edge to set of all edges and find its successor
      (jl-add (.edges-remaining-layer this) edge)
      (find-and-add-successor this edge)
      ;; check if edge is a starting edge
      (when (spline-edge-router.is-qualified-as-starting-node (get-node (get-source edge)))
        (jl-add (.start-edges this) edge))
      ;; check port-side of target port
      (let* ((target-port (get-target edge))
             (target-layer (get-layer (get-node target-port))))
        (cond ((eq target-layer right-layer)
               (js-add (.right-ports-layer this) target-port))
              ((eq target-layer left-layer)
               (js-add (.left-ports-layer this) target-port))
              (t
               ;; Unhandled situation: incoming and outgoing edges on the same
               ;; port.  This is not supported.
               (jl-remove (.edges-remaining-layer this) edge)))))))

(defjmethod compute-sloppy-spacing ((this spline-edge-router) right-layer edge-edge-spacing
                                    node-node-spacing sloppy-layer-spacing-factor)
  (let ((max-vert-diff 0.0d0))
    ;; Iterate over the layer's nodes
    (jdo (node (get-nodes right-layer))
      (let ((max-curr-input-y-diff 0.0d0))
        (jdo (incoming-edge (get-incoming-edges node))
          (let ((source-pos (.y (get-absolute-anchor (get-source incoming-edge))))
                (target-pos (.y (get-absolute-anchor (get-target incoming-edge)))))
            (setf max-curr-input-y-diff
                  (math.max max-curr-input-y-diff (math.abs (- target-pos source-pos))))))
        (setf max-vert-diff (math.max max-vert-diff max-curr-input-y-diff))))
    ;; the next layer starts based on the maximal vertical span of edges between the two layers
    (* sloppy-layer-spacing-factor
       (math.min 1.0d0 (/ edge-edge-spacing node-node-spacing))
       max-vert-diff)))

(defjmethod find-and-add-successor ((this spline-edge-router) edge)
  (let ((target-node (get-node (get-target edge))))
    ;; if the target node is a normal node there is no successor
    (unless (spline-edge-router.is-normal-node target-node)
      ;; otherwise take the first outgoing edge of the target node
      (jdo (first-outgoing (get-outgoing-edges target-node))
        (setf (gethash edge (.successing-edge this)) first-outgoing)
        (return)))))

(defjmethod create-segments-and-compute-ranking ((this spline-edge-router))
  ;; hyper-edges having their start port on the left side, then on the right side
  (create-spline-segments-for-hyper-edges this (.left-ports-layer this) (.right-ports-layer this)
                                          side-to-process.left t
                                          (.edges-remaining-layer this) (.spline-segments-layer this))
  (create-spline-segments-for-hyper-edges this (.left-ports-layer this) (.right-ports-layer this)
                                          side-to-process.left nil
                                          (.edges-remaining-layer this) (.spline-segments-layer this))
  (create-spline-segments-for-hyper-edges this (.left-ports-layer this) (.right-ports-layer this)
                                          side-to-process.right t
                                          (.edges-remaining-layer this) (.spline-segments-layer this))
  (create-spline-segments-for-hyper-edges this (.left-ports-layer this) (.right-ports-layer this)
                                          side-to-process.right nil
                                          (.edges-remaining-layer this) (.spline-segments-layer this))
  ;; remaining edges are single edges that cannot be combined with others to a hyper-edge
  (create-spline-segments this (.edges-remaining-layer this) (.left-ports-layer this)
                          (.right-ports-layer this) (.spline-segments-layer this))
  ;; creation of the dependencies of the spline segments
  (let* ((segments (.spline-segments-layer this))
         (count (length segments)))
    (loop for i from 0 below count
          do (loop for j from (1+ i) below count
                   do (create-dependency this (jl-get segments i) (jl-get segments j))))
    ;; break cycles, then assign ranks to the hyper-nodes
    (spline-edge-router.break-cycles segments (get-property (.l-graph this) layered-internal-properties.random))
    (spline-edge-router.topological-numbering segments)))

(defjmethod create-spline-segments ((this spline-edge-router) edges left-ports right-ports hyper-edges)
  (jdo (edge edges)
    (let* ((source-port (get-source edge))
           (source-side (cond ((js-contains left-ports source-port) side-to-process.left)
                              ((js-contains right-ports source-port) side-to-process.right)
                              (t (jthrow 'illegal-argument-exception
                                         "Source port must be in one of the port sets."))))
           (target-port (get-target edge))
           (target-side (cond ((js-contains left-ports target-port) side-to-process.left)
                              ((js-contains right-ports target-port) side-to-process.right)
                              (t (jthrow 'illegal-argument-exception
                                         "Target port must be in one of the port sets.")))))
      (let ((seg (new-spline-segment edge source-side target-side)))
        (jm-put (.edge-to-segment-map this) edge seg)
        (jl-add hyper-edges seg)))))

(defun %ser-pair-add (pairs side edge)
  "Set<Pair<SideToProcess, LEdge>>.add: a pair is (side . edge), compared by value."
  (unless (find-if (lambda (p) (and (eq (car p) side) (eq (cdr p) edge))) pairs)
    (jl-add pairs (cons side edge))))

(defjmethod create-spline-segments-for-hyper-edges ((this spline-edge-router) left-ports right-ports
                                                    side-to-process reversed edges-remaining hyper-edges)
  (let ((ports-to-process (if (eq side-to-process side-to-process.left) left-ports right-ports)))
    ;; Iterate through all ports on the side to process.
    (jdo (single-port ports-to-process)
      (let ((single-port-position (.y (get-absolute-anchor single-port)))
            (up-edges (new-jlist))
            (down-edges (new-jlist)))
        ;; Find edges we could construct a hyper-edge from.  An edge that is no
        ;; longer in edges-remaining already belongs to a hyper-edge.
        (jdo (edge (get-connected-edges single-port))
          (let ((edge-reversed (and (get-property edge layered-internal-properties.reversed) t)))
            (when (and (eq edge-reversed (and reversed t))
                       (jl-contains edges-remaining edge))
              ;; find the target port
              (let* ((target-port (if (eq (get-target edge) single-port)
                                      (get-source edge)
                                      (get-target edge)))
                     (target-port-position (.y (get-absolute-anchor target-port)))
                     (side-of-target (if (js-contains left-ports target-port)
                                         side-to-process.left
                                         side-to-process.right)))
                ;; straight edges are not part of a hyper-edge
                (unless (spline-edge-router.is-straight target-port-position single-port-position)
                  (if (< target-port-position single-port-position)
                      (%ser-pair-add up-edges side-of-target edge)
                      (%ser-pair-add down-edges side-of-target edge)))))))
        ;; Create some hyper-edges.  Only hyper-edges with more than one real edge.
        (when (> (jsize up-edges) 1)
          (let ((seg (new-spline-segment single-port up-edges side-to-process)))
            (jdo (pair up-edges)
              (jm-put (.edge-to-segment-map this) (cdr pair) seg))
            (jl-add hyper-edges seg)
            (jdo (pair up-edges)
              (jl-remove edges-remaining (cdr pair)))))
        (when (> (jsize down-edges) 1)
          (let ((seg (new-spline-segment single-port down-edges side-to-process)))
            (jdo (pair down-edges)
              (jm-put (.edge-to-segment-map this) (cdr pair) seg))
            (jl-add hyper-edges seg)
            (jdo (pair down-edges)
              (jl-remove edges-remaining (cdr pair)))))))))

(defjmethod create-dependency ((this spline-edge-router) edge0 edge1)
  (unless (or (> (.hyper-edge-top-y-pos edge0) (.hyper-edge-bottom-y-pos edge1))
              (> (.hyper-edge-top-y-pos edge1) (.hyper-edge-bottom-y-pos edge0)))
    ;; the two hyper-edges share a vertical segment: count the crossings each would cause
    (let ((edge0-counter 0)
          (edge1-counter 0))
      (jdo (port (.right-ports edge0))
        (when (splines-math.is-between (.y (get-absolute-anchor port))
                                       (.hyper-edge-top-y-pos edge1) (.hyper-edge-bottom-y-pos edge1))
          (incf edge0-counter)))
      (jdo (port (.left-ports edge0))
        (when (splines-math.is-between (.y (get-absolute-anchor port))
                                       (.hyper-edge-top-y-pos edge1) (.hyper-edge-bottom-y-pos edge1))
          (decf edge0-counter)))
      (jdo (port (.right-ports edge1))
        (when (splines-math.is-between (.y (get-absolute-anchor port))
                                       (.hyper-edge-top-y-pos edge0) (.hyper-edge-bottom-y-pos edge0))
          (incf edge1-counter)))
      (jdo (port (.left-ports edge1))
        (when (splines-math.is-between (.y (get-absolute-anchor port))
                                       (.hyper-edge-top-y-pos edge0) (.hyper-edge-bottom-y-pos edge0))
          (decf edge1-counter)))
      (cond ((< edge0-counter edge1-counter)
             ;; edge0 should lay left of edge1
             (new-dependency edge0 edge1 (- edge1-counter edge0-counter)))
            ((< edge1-counter edge0-counter)
             ;; edge0 should lay right of edge1
             (new-dependency edge1 edge0 (- edge0-counter edge1-counter)))
            (t
             ;; in either ordering there would be the same number of crossings
             (new-dependency edge1 edge0 0)
             (new-dependency edge0 edge1 0))))))

(defjmethod get-edge-chain ((this spline-edge-router) start)
  (let ((edge-chain (new-jlist))
        (current start))
    (loop
      (jl-add edge-chain current)
      (setf current (gethash current (.successing-edge this)))
      (unless current (return)))
    edge-chain))

(defjmethod get-spline-path ((this spline-edge-router) start)
  (let ((segment-chain (new-jlist))
        (current start))
    (loop
      (let ((segment (gethash current (.edge-to-segment-map this))))
        (setf (.source-port segment) (get-source current)
              (.target-port segment) (get-target current))
        (jl-add segment-chain segment))
      (setf current (gethash current (.successing-edge this)))
      (unless current (return)))
    (let ((initial-segment (jl-get segment-chain 0)))
      (setf (.initial-segment initial-segment) t
            (.source-node initial-segment)
            (get-node (get-source (%ser-first-of (.edges initial-segment))))))
    (let ((final-segment (jl-last segment-chain)))
      (setf (.last-segment final-segment) t
            (.target-node final-segment)
            (get-node (get-target (%ser-first-of (.edges final-segment))))))
    segment-chain))

;;; ------------------------------------------------------------ cycle breaking

(defjstatic spline-edge-router.break-cycles (edges random)
  (let ((sources (new-jlist))
        (sinks (new-jlist))
        (next-mark -1))
    ;; initialize values for the algorithm
    (jdo (edge edges)
      (setf (.mark edge) next-mark)
      (decf next-mark)
      (let ((inweight 0)
            (outweight 0))
        (jdo (dep (.outgoing edge))
          (incf outweight (.weight dep)))
        (jdo (dep (.incoming edge))
          (incf inweight (.weight dep)))
        (setf (.inweight edge) inweight
              (.outweight edge) outweight)
        (cond ((zerop outweight) (jl-add sinks edge))
              ((zerop inweight) (jl-add sources edge)))))
    ;; assign marks to all nodes, ignoring dependencies of weight zero
    (let* ((unprocessed (jset-from edges))
           (mark-base (length edges))
           (next-left (1+ mark-base))
           (next-right (1- mark-base))
           (max-edges (new-jlist)))
      (loop while (plusp (jsize unprocessed))
            do (loop while (plusp (length sinks))
                     do (let ((sink (jl-remove-first sinks)))
                          (js-remove unprocessed sink)
                          (setf (.mark sink) next-right)
                          (decf next-right)
                          (spline-edge-router.update-neighbors sink sources sinks)))
               (loop while (plusp (length sources))
                     do (let ((source (jl-remove-first sources)))
                          (js-remove unprocessed source)
                          (setf (.mark source) next-left)
                          (incf next-left)
                          (spline-edge-router.update-neighbors source sources sinks)))
               (let ((max-outflow spline-edge-router.int-min-value))
                 (jdo (edge unprocessed)
                   (let ((outflow (- (.outweight edge) (.inweight edge))))
                     (when (>= outflow max-outflow)
                       (when (> outflow max-outflow)
                         (jl-clear max-edges)
                         (setf max-outflow outflow))
                       (jl-add max-edges edge))))
                 (when (plusp (length max-edges))
                   ;; with multiple maximal edges, select one at random
                   (let ((max-edge (jl-get max-edges (next-int random (length max-edges)))))
                     (js-remove unprocessed max-edge)
                     (setf (.mark max-edge) next-left)
                     (incf next-left)
                     (spline-edge-router.update-neighbors max-edge sources sinks)
                     (jl-clear max-edges)))))
      ;; shift ranks that are left of the mark base
      (let ((shift-base (1+ (length edges))))
        (jdo (edge edges)
          (when (< (.mark edge) mark-base)
            (incf (.mark edge) shift-base))))
      ;; process edges that point left: remove those of zero weight, reverse the others
      (jdo (source edges)
        (let ((outgoing (.outgoing source))
              (i 0))
          (loop while (< i (length outgoing))
                do (let* ((dep (jl-get outgoing i))
                          (target (.target dep)))
                     (cond ((> (.mark source) (.mark target))
                            (jl-remove-at outgoing i)
                            (jl-remove (.incoming target) dep)
                            (when (> (.weight dep) 0)
                              (setf (.source dep) target)
                              (jl-add (.outgoing target) dep)
                              (setf (.target dep) source)
                              (jl-add (.incoming source) dep)))
                           (t (incf i))))))))))

(defjstatic spline-edge-router.update-neighbors (edge sources sinks)
  ;; process following edges
  (jdo (dep (.outgoing edge))
    (let ((target (.target dep)))
      (when (and (< (.mark target) 0) (> (.weight dep) 0))
        (decf (.inweight target) (.weight dep))
        (when (and (<= (.inweight target) 0) (> (.outweight target) 0))
          (jl-add sources target)))))
  ;; process preceding edges
  (jdo (dep (.incoming edge))
    (let ((source (.source dep)))
      (when (and (< (.mark source) 0) (> (.weight dep) 0))
        (decf (.outweight source) (.weight dep))
        (when (and (<= (.outweight source) 0) (> (.inweight source) 0))
          (jl-add sinks source))))))

;;; ------------------------------------------------------------ topological ordering

(defjstatic spline-edge-router.topological-numbering (edges)
  ;; targets are only added to rightward-targets if they only connect westward
  ;; ports (all their horizontal segments point to the right)
  (let ((sources (new-jlist))
        (rightward-targets (new-jlist))
        (max-rank -1))
    (jdo (edge edges)
      (setf (.rank edge) 0)
      (setf (.inweight edge) (jsize (.incoming edge))
            (.outweight edge) (jsize (.outgoing edge)))
      (when (zerop (.inweight edge))
        (jl-add sources edge))
      (when (and (zerop (.outweight edge)) (zerop (jsize (.left-ports edge))))
        (jl-add rightward-targets edge)))
    ;; assign ranks using topological numbering
    (loop while (plusp (length sources))
          do (let ((edge (jl-remove-first sources)))
               (jdo (dep (.outgoing edge))
                 (let ((target (.target dep)))
                   (setf (.rank target) (max (.rank target) (1+ (.rank edge))))
                   (setf max-rank (max max-rank (.rank target)))
                   (decf (.inweight target))
                   (when (zerop (.inweight target))
                     (jl-add sources target))))))
    ;; hyper nodes without horizontal segments pointing left: move them as far right as possible
    (when (> max-rank -1)
      (jdo (edge rightward-targets)
        (setf (.rank edge) max-rank))
      (loop while (plusp (length rightward-targets))
            do (let ((edge (jl-remove-first rightward-targets)))
                 (jdo (dep (.incoming edge))
                   (let ((source (.source dep)))
                     (when (zerop (jsize (.left-ports source)))
                       (setf (.rank source) (min (.rank source) (1- (.rank edge))))
                       (decf (.outweight source))
                       (when (zerop (.outweight source))
                         (jl-add rightward-targets source)))))))))
  nil)

;;; ------------------------------------------------------------ convenience

(defjstatic spline-edge-router.is-straight (first-y second-y)
  (< (math.abs (- first-y second-y)) spline-edge-router.max-vertical-diff-for-straight))

(defjstatic spline-edge-router.is-normal-node (node)
  (let ((nt (get-type node)))
    (or (eq nt node-type.normal)
        (eq nt node-type.breaking-point))))

(defjstatic spline-edge-router.is-qualified-as-starting-node (node)
  (let ((nt (get-type node)))
    (or (eq nt node-type.normal)
        (eq nt node-type.north-south-port)
        (eq nt node-type.external-port)
        (eq nt node-type.breaking-point))))
