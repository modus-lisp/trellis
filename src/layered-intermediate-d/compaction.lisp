;;;; compaction.lisp -- org.eclipse.elk.alg.layered.intermediate.compaction
;;;;
;;;; Built on org.eclipse.elk.alg.COMMON.compaction.oned (CNode, CGroup, CGraph,
;;;; OneDimensionalCompactor, ...) and alg.common.networksimplex, as in Java.

(in-package #:elk)

(defun %fuzzy-compare (a b tolerance)
  "Guava DoubleMath.fuzzyCompare."
  (cond ((or (<= (abs (- a b)) tolerance) (eql a b)) 0)
        ((< a b) -1)
        ((> a b) 1)
        (t (boolean.compare (nan-p a) (nan-p b)))))

;;; ------------------------------------------------------------ VerticalSegment

(defjctor vertical-segment (bend1 bend2 c-node l-edge)
  (jl-add (.affected-bends this) bend1)
  (jl-add (.affected-bends this) bend2)
  (let ((hitbox (.hitbox this)))
    (setf (.x hitbox) (math.min (.x bend1) (.x bend2))
          (.y hitbox) (math.min (.y bend1) (.y bend2))
          (.width hitbox) (math.abs (- (.x bend1) (.x bend2)))
          (.height hitbox) (math.abs (- (.y bend1) (.y bend2)))))
  ;; adding junction points (this property could be null)
  (let ((in-jps (get-property l-edge layered-options.junction-points)))
    (when in-jps
      (jdo (jp in-jps)
        (when (common-compare-fuzzy.eq (.x jp) (.x bend1))
          (add (.junction-points this) jp)))))
  (when c-node
    (jl-add (.potential-group-parents this) c-node))
  (jl-add (.represented-l-edges this) l-edge))

(defjmethod join-with ((this vertical-segment) other)
  (jl-add-all (.represented-l-edges this) (.represented-l-edges other))
  (jl-add-all (.affected-bends this) (.affected-bends other))
  (jl-add-all (.affected-bounding-boxes this) (.affected-bounding-boxes other))
  (add-all (.junction-points this) (.junction-points other))
  (jl-add-all (.constraints this) (.constraints other))
  (jl-add-all (.potential-group-parents this) (.potential-group-parents other))
  (let* ((hb (.hitbox this)) (ohb (.hitbox other))
         (new-x (math.min (.x hb) (.x ohb)))
         (new-y (math.min (.y hb) (.y ohb)))
         (max-x (math.max (+ (.x hb) (.width hb)) (+ (.x ohb) (.width ohb))))
         (new-w (- max-x new-x))
         (max-y (math.max (+ (.y hb) (.height hb)) (+ (.y ohb) (.height ohb))))
         (new-h (- max-y new-y)))
    (set-rect hb new-x new-y new-w new-h))
  (apply-or (.ignore-spacing this) (.ignore-spacing other))
  (unless (.a-port this)
    (setf (.a-port this) (.a-port other)))
  (jl-add-all (.joined this) (.joined other))
  (jl-add (.joined this) other)
  this)

(defjmethod intersects ((this vertical-segment) o)
  (and (common-compare-fuzzy.eq (.x (.hitbox this)) (.x (.hitbox o)))
       (not (or (common-compare-fuzzy.lt (.y (get-bottom-left (.hitbox this))) (.y (.hitbox o)))
                (common-compare-fuzzy.lt (.y (get-bottom-left (.hitbox o))) (.y (.hitbox this)))))))

(defjmethod compare-to ((this vertical-segment) o)
  (let ((d (%fuzzy-compare (.x (.hitbox this)) (.x (.hitbox o)) common-compare-fuzzy.tolerance)))
    (if (= d 0)
        (double.compare (.y (.hitbox this)) (.y (.hitbox o)))
        d)))

(defjmethod to-string ((this vertical-segment))
  (format nil "VerticalSegment ~A ~{~A~^, ~}" (to-string (.hitbox this))
          (map 'list (lambda (e) (to-string e)) (.represented-l-edges this))))

;;; ------------------------------------------------------------ HorizontalGraphCompactor

;; HorizontalGraphCompactor.NETWORK_SIMPLEX_COMPACTION, created on first use (its
;; superclass lives in the common module).
(defvar horizontal-graph-compactor.network-simplex-compaction nil)

(defun lazy-network-simplex-compaction ()
  (or horizontal-graph-compactor.network-simplex-compaction
      (setf horizontal-graph-compactor.network-simplex-compaction
            (new-network-simplex-compaction))))

(defjctor horizontal-graph-compactor ()
  (setf (.special-spacings-handler this)
        (make-instance '%hgc-special-spacings-handler :outer this)))

(defun %lock-c-group-out-degree-real (node dir)
  (declare (ignore dir))
  (= (.out-degree-real (.c-group node)) 0))

(defjmethod process ((this horizontal-graph-compactor) layered-graph progress-monitor)
  (let ((strategy (get-property layered-graph layered-options.compaction-post-compaction-strategy)))
    (when (eq strategy graph-compaction-strategy.none)
      (return-from process/2 nil))
    (begin progress-monitor "Horizontal Compaction" 1)
    (setf (.l-graph this) layered-graph)
    ;; the layered graph is transformed into a CGraph that is passed to OneDimensionalCompactor
    (let* ((transformer (new-l-graph-to-c-graph-transformer))
           (odc (new-common-one-dimensional-compactor (transform transformer layered-graph))))
      ;; consider special spacing requirements of the lgraph's nodes and edges
      (set-spacings-handler odc (.special-spacings-handler this))
      ;; select constraint algorithm
      (enum-case (get-property layered-graph layered-options.compaction-post-compaction-constraints)
        (constraint-calculation-strategy.scanline
         (set-constraint-algorithm odc (new-edge-aware-scanline-constraint-calculation (.l-graph this))))
        (t (set-constraint-algorithm odc common-one-dimensional-compactor.quadratic-constraints)))
      ;; select compaction strategy
      (enum-case strategy
        (graph-compaction-strategy.left
         (compact odc))
        (graph-compaction-strategy.right
         (compact (change-direction odc direction.right)))
        (graph-compaction-strategy.left-right-constraint-locking
         ;; lock CNodes if they are not constrained
         (compact (set-lock-function (change-direction (compact odc) direction.right)
                                     #'%lock-c-group-out-degree-real)))
        (graph-compaction-strategy.left-right-connection-locking
         ;; compacting left, locking all CNodes that have fewer connections to the right,
         ;; then compacting right to shorten unnecessary long edges
         (compact (set-lock-function (change-direction (compact odc) direction.right)
                                     (lambda (node dir)
                                       (j-get (jm-get (get-lock-map transformer) node) dir)))))
        (graph-compaction-strategy.edge-length
         (compact (set-compaction-algorithm odc (lazy-network-simplex-compaction))))
        (t nil))
      ;; since changeDirection may transform hitboxes, the final direction has to be LEFT again
      (finish odc)
      ;; applying the compacted positions to the LGraph and updating its size and offset
      (apply-layout transformer)
      (done progress-monitor))))

(defjstatic horizontal-graph-compactor.is-vertical-segments-of-same-edge (c-node1 c-node2)
  (let ((v1 (horizontal-graph-compactor.get-vertical-segment-or-null c-node1))
        (v2 (horizontal-graph-compactor.get-vertical-segment-or-null c-node2)))
    (and v1 v2
         ;; !Collections.disjoint
         (some (lambda (e) (find e (.represented-l-edges v2))) (.represented-l-edges v1))
         t)))

(defjstatic horizontal-graph-compactor.get-l-node-or-null (c-node)
  (if (typep (.origin c-node) 'l-node) (.origin c-node) nil))

(defjstatic horizontal-graph-compactor.get-vertical-segment-or-null (c-node)
  (if (typep (.origin c-node) 'vertical-segment) (.origin c-node) nil))

(defjmethod get-horizontal-spacing ((this %hgc-special-spacings-handler) c-node1 c-node2)
  ;; joining north/south segments that belong to the same edge by setting their spacing to 0
  (when (horizontal-graph-compactor.is-vertical-segments-of-same-edge c-node1 c-node2)
    (return-from get-horizontal-spacing/2 0.0))
  ;; get the underlying LNodes
  (let ((node1 (horizontal-graph-compactor.get-l-node-or-null c-node1))
        (node2 (horizontal-graph-compactor.get-l-node-or-null c-node2)))
    ;; if either of the two involved nodes represents an external port, it's ok to move
    ;; the port as close as possible since it will be positioned correctly later on
    (when (or (and node1 (eq (get-type node1) node-type.external-port))
              (and node2 (eq (get-type node2) node-type.external-port)))
      (return-from get-horizontal-spacing/2 0.0))
    ;; default behavior, query the Spacings object
    (let ((spacings (get-property (.l-graph (.outer this)) layered-internal-properties.spacings)))
      (get-horizontal-spacing spacings
                              (if node1 (get-type node1) node-type.long-edge)
                              (if node2 (get-type node2) node-type.long-edge)))))

(defjmethod get-vertical-spacing ((this %hgc-special-spacings-handler) c-node1 c-node2)
  ;; only called when the quadratic constraint calculation is used; vertical segments of the
  ;; same edge get spacing 1, i.e. they overlap in y which results in a constraint
  (when (horizontal-graph-compactor.is-vertical-segments-of-same-edge c-node1 c-node2)
    (return-from get-vertical-spacing/2 1.0))
  (let ((node1 (horizontal-graph-compactor.get-l-node-or-null c-node1))
        (node2 (horizontal-graph-compactor.get-l-node-or-null c-node2))
        (spacings (get-property (.l-graph (.outer this)) layered-internal-properties.spacings)))
    (get-vertical-spacing spacings
                          (if node1 (get-type node1) node-type.long-edge)
                          (if node2 (get-type node2) node-type.long-edge))))

;;; ------------------------------------------------------------ LGraphToCGraphTransformer

;; for debugging
(defun %lgct-node-to-string-delegate (n)
  (to-string (get-property (.origin n) layered-internal-properties.origin)))
(defun %lgct-vs-to-string-delegate (n)
  (to-string (.origin n)))

(defjmethod transform ((this l-graph-to-c-graph-transformer) input-graph)
  (setf (.layered-graph this) input-graph)
  (setf (.edge-routing this) (get-property input-graph layered-options.edge-routing))
  (%lgct-init this)
  (%lgct-transform-nodes this)
  (%lgct-transform-edges this)
  (.c-graph this))

(defjmethod get-lock-map ((this l-graph-to-c-graph-transformer))
  (.lock-map this))

(defun %lgct-init (this)
  ;; checking if the graph has edges and possibly prohibiting vertical compaction
  (let ((has-edges nil) (index 0))
    (jdo (l (get-layers (.layered-graph this)))
      (setf (.id l) index)
      (incf index)
      (jdo (n (get-nodes l))
        (when (and (not has-edges) (not (is-empty (get-connected-edges n))))
          (setf has-edges t))))
    (let ((supported-directions (enum-set.of direction.undefined direction.left direction.right)))
      (unless has-edges
        (add supported-directions direction.up)
        (add supported-directions direction.down))
      ;; initializing members
      (setf (.c-graph this) (new-common-c-graph supported-directions))
      (clrhash (.nodes-map this))
      (clrhash (.comment-offsets this))
      (clrhash (.lock-map this))
      (clrhash (.vertical-segments-map this)))))

(defun %lgct-transform-nodes (this)
  (jdo (layer (get-layers (.layered-graph this)))
    (jdo (node (get-nodes layer))
      (block continue
        ;; comment boxes are part of a node's margins hence we can neglect them here
        (when (get-property node layered-options.comment-box)
          (let ((edges (get-connected-edges node)))
            (unless (is-empty edges)
              (let* ((e (j-get edges 0))
                     (other (get-node (get-source e))))
                (when (eq other node)
                  (setf other (get-node (get-target e))))
                (let ((p (pair.of other (sub (clone (get-position node)) (get-position other)))))
                  (jm-put (.comment-offsets this) node p)
                  (return-from continue))))))
        (let* ((pos (get-position node)) (margin (get-margin node)) (size (get-size node))
               (hitbox (new-elk-rectangle (- (.x pos) (.left margin))
                                          (- (.y pos) (.top margin))
                                          (+ (.x size) (.left margin) (.right margin))
                                          (+ (.y size) (.top margin) (.bottom margin))))
               ;; create the node in the compaction graph
               (c-node (create (to-string-delegate (hitbox (origin (common-c-node.of) node) hitbox)
                                                   #'%lgct-node-to-string-delegate)
                               (.c-graph this))))
          ;; the node lives in its own group
          (create (master (nodes (common-c-group.of) (jlist c-node)) c-node) (.c-graph this))
          (let ((node-lock (new-common-quadruplet)))
            (jm-put (.lock-map this) c-node node-lock)
            ;; locking the node for directions that fewer edges are connected in
            ;; (only used if LEFT_RIGHT_CONNECTION_LOCKING is used)
            (let ((difference (- (size (get-incoming-edges node)) (size (get-outgoing-edges node)))))
              (cond ((< difference 0) (j-set node-lock t direction.left))
                    ((> difference 0) (j-set node-lock t direction.right))))
            ;; excluding external port dummies
            (when (eq (get-type node) node-type.external-port)
              (j-set node-lock nil nil nil nil)))
          (jm-put (.nodes-map this) node c-node))))))

(defun %lgct-transform-edges (this)
  (let* ((style (get-property (.layered-graph this) layered-options.edge-routing))
         (vertical-segments
           (enum-case style
             (edge-routing.orthogonal (%lgct-collect-vertical-segments-orthogonal this))
             (edge-routing.splines (%lgct-collect-vertical-segments-splines this))
             (t (jthrow 'illegal-state-exception "Compaction not supported for ~A edges." style)))))
    ;; merge them
    (%lgct-merge-vertical-segments this vertical-segments)
    ;; create precomputed constraints
    (jdo-map (vs vs-node (.vertical-segments-map this))
      (jdo (other (.constraints vs))
        (let ((other-node (jm-get (.vertical-segments-map this) other)))
          (jl-add (.predefined-horizontal-constraints (.c-graph this)) (pair.of vs-node other-node)))))))

(defun %lgct-collect-vertical-segments-orthogonal (this)
  (let ((vertical-segments (new-jlist))
        (nodes-map (.nodes-map this)))
    (jdo (layer (get-layers (.layered-graph this)))
      (jdo (node (get-nodes layer))
        (let ((c-node (jm-get nodes-map node)))
          ;; add vertical edge segments
          (jdo (edge (get-outgoing-edges node))
            (let* ((bends (to-jvector (get-bend-points edge)))
                   (bpos 0)
                   (first t)
                   (last-segment nil))
              ;; infer vertical segments from positions of bendpoints
              (when (< bpos (length bends))
                (let ((bend1 (aref bends bpos)) (bend2 nil))
                  (incf bpos)
                  ;; get segment of source n/s port of outgoing segment
                  (when (eq (get-side (get-source edge)) port-side.north)
                    (let ((vs (new-vertical-segment bend1 (new-k-vector (.x bend1) (.y (.hitbox c-node)))
                                                    c-node edge)))
                      (setf (.down (.ignore-spacing vs)) t)
                      (setf (.a-port vs) (get-source edge))
                      (jl-add vertical-segments vs)))
                  (when (eq (get-side (get-source edge)) port-side.south)
                    (let ((vs (new-vertical-segment bend1 (new-k-vector (.x bend1)
                                                                        (+ (.y (.hitbox c-node))
                                                                           (.height (.hitbox c-node))))
                                                    c-node edge)))
                      (setf (.up (.ignore-spacing vs)) t)
                      (setf (.a-port vs) (get-source edge))
                      (jl-add vertical-segments vs)))
                  ;; get regular segments
                  (loop while (< bpos (length bends))
                        do (setf bend2 (aref bends bpos))
                           (incf bpos)
                           (unless (common-compare-fuzzy.eq (.y bend1) (.y bend2))
                             (setf last-segment (new-vertical-segment bend1 bend2 nil edge))
                             (jl-add vertical-segments last-segment)
                             ;; the first vertical segment of an outgoing edge
                             (when first
                               (setf first nil)
                               ;; make sure that "invalid" spacing that sometimes is required
                               ;; to route the edges away from the nodes, does not become an issue
                               (cond ((< (.y bend2) (.y (.hitbox c-node)))
                                      (setf (.down (.ignore-spacing last-segment)) t))
                                     ((> (.y bend2) (+ (.y (.hitbox c-node)) (.height (.hitbox c-node))))
                                      (setf (.up (.ignore-spacing last-segment)) t))
                                     (t
                                      ;; completely surrounded
                                      (setf (.up (.ignore-spacing last-segment)) t)
                                      (setf (.down (.ignore-spacing last-segment)) t)))))
                           (when (< bpos (length bends))
                             (setf bend1 bend2)))
                  ;; handle last vertical segment
                  (when last-segment
                    (let ((c-target-node (jm-get nodes-map (get-node (get-target edge)))))
                      (cond ((< (.y bend1) (.y (.hitbox c-target-node)))
                             (setf (.down (.ignore-spacing last-segment)) t))
                            ((> (.y bend1) (+ (.y (.hitbox c-target-node)) (.height (.hitbox c-target-node))))
                             (setf (.up (.ignore-spacing last-segment)) t))
                            (t
                             ;; completely surrounded
                             (setf (.up (.ignore-spacing last-segment)) t)
                             (setf (.down (.ignore-spacing last-segment)) t)))))))))
          ;; same for incoming edges to get north/south segments on target side
          (jdo (edge (get-incoming-edges node))
            (unless (is-empty (get-bend-points edge))
              ;; get segment of target n/s port
              (let ((bend1 (get-last (get-bend-points edge))))
                (when (eq (get-side (get-target edge)) port-side.north)
                  (let ((vs (new-vertical-segment bend1 (new-k-vector (.x bend1) (.y (.hitbox c-node)))
                                                  c-node edge)))
                    (setf (.down (.ignore-spacing vs)) t)
                    (setf (.a-port vs) (get-target edge))
                    (jl-add vertical-segments vs)))
                (when (eq (get-side (get-target edge)) port-side.south)
                  (let ((vs (new-vertical-segment bend1 (new-k-vector (.x bend1)
                                                                      (+ (.y (.hitbox c-node))
                                                                         (.height (.hitbox c-node))))
                                                  c-node edge)))
                    (setf (.up (.ignore-spacing vs)) t)
                    (setf (.a-port vs) (get-target edge))
                    (jl-add vertical-segments vs)))))))))
    vertical-segments))

(defun %lgct-collect-vertical-segments-splines (this)
  (let ((vertical-segments (new-jlist)))
    (jdo (l (get-layers (.layered-graph this)))
      (jdo (n (get-nodes l))
        (jdo (out (get-outgoing-edges n))
          (let ((spline (get-property out layered-internal-properties.spline-route-start)))
            (when spline
              (let ((last-vs nil))
                (jdo (s spline)
                  (unless (.is-straight s)
                    (let* ((left-top (get-top-left (.bounding-box s)))
                           (right-bottom (get-bottom-right (.bounding-box s)))
                           (vs (new-vertical-segment left-top right-bottom nil
                                                     (first (to-cl-list (.edges s))))))
                      (jl-add (.affected-bounding-boxes vs) (.bounding-box s))
                      (jl-add vertical-segments vs)
                      ;; remember that there has to be a constraint between the two
                      ;; (non-straight) segments
                      (when last-vs
                        (jl-add (.constraints last-vs) vs))
                      (setf last-vs vs))))))))))
    vertical-segments))

(defun %lgct-merge-vertical-segments (this vertical-segments)
  (when (zerop (length vertical-segments))
    (return-from %lgct-merge-vertical-segments nil))
  ;; sort the segments such that segments with the same x coordinate are consecutive,
  ;; for equal x coordinates, sort with ascending y coordinate
  (jl-sort vertical-segments (lambda (a b) (compare-to a b)))
  ;; merge segments that intersect
  (let ((survivor (aref vertical-segments 0)))
    (loop for i from 1 below (length vertical-segments)
          do (let ((next (aref vertical-segments i)))
               (if (intersects survivor next)
                   (setf survivor (join-with survivor next))
                   (progn
                     (%lgct-vertical-segment-to-c-node this survivor)
                     ;; start with a new segment
                     (setf survivor next)))))
    ;; transform the last segment as well
    (%lgct-vertical-segment-to-c-node this survivor)))

(defun %lgct-vertical-segment-to-c-node (this vertical-segment)
  ;; create CNode representation for the last segment
  (let ((c-node (create (to-string-delegate
                         (hitbox (j-type (origin (common-c-node.of) vertical-segment) "vs")
                                 (new-elk-rectangle (.hitbox vertical-segment)))
                         #'%lgct-vs-to-string-delegate)
                        (.c-graph this))))
    ;; group the node if requested, currently this is only the case for north/south
    ;; segments of orthogonal edges
    (unless (zerop (length (.potential-group-parents vertical-segment)))
      (add-c-node (.c-group (jl-get (.potential-group-parents vertical-segment) 0)) c-node))
    (let ((vs-lock (new-common-quadruplet))
          (inc (new-jset))
          (out (new-jset)))
      (jm-put (.lock-map this) c-node vs-lock)
      ;; segments belonging to multiple edges should be locked in the direction that
      ;; fewer different ports are connected in
      (jdo (e (.represented-l-edges vertical-segment))
        (js-add inc (get-source e))
        (js-add out (get-target e)))
      (let ((difference (- (size inc) (size out))))
        (cond ((< difference 0)
               (j-set vs-lock t direction.left)
               (j-set vs-lock nil direction.right))
              ((> difference 0)
               (j-set vs-lock nil direction.left)
               (j-set vs-lock t direction.right))))
      (jdo (other (.joined vertical-segment))
        (jm-put (.vertical-segments-map this) other c-node))
      (jm-put (.vertical-segments-map this) vertical-segment c-node))))

(defjmethod apply-layout ((this l-graph-to-c-graph-transformer))
  (let ((c-graph (.c-graph this))
        (nodes-map (.nodes-map this)))
    ;; apply the compacted positions to nodes
    (jdo (c-node (.c-nodes c-graph))
      (when (typep (.origin c-node) 'l-node)
        (let ((l-node (.origin c-node)))
          (setf (.x (get-position l-node)) (+ (.x (.hitbox c-node)) (.left (get-margin l-node)))))))
    ;; adjust comment boxes
    (%lgct-apply-comment-positions this)
    ;; apply new positions to vertical segments
    (jdo (c-node (.c-nodes c-graph))
      (when (typep (.origin c-node) 'vertical-segment)
        (let ((delta-x (- (.x (.hitbox c-node)) (.x (.hitbox-pre-compaction c-node))))
              (vs (.origin c-node)))
          (jdo (b (.affected-bends vs)) (incf (.x b) delta-x))
          (jdo (bb (.affected-bounding-boxes vs)) (incf (.x bb) delta-x))
          (jdo (jp (.junction-points vs)) (incf (.x jp) delta-x)))))
    ;; special treatment of spline edge routes
    ;;  1) self loops have already been routed and are not part of the compaction graph
    ;;  2) control points of straight parts of a spline have to be offset
    (when (eq (.edge-routing this) edge-routing.splines)
      ;; offset selfloops of splines
      (jdo (n (jm-keys nodes-map))
        (jdo (sl (get-outgoing-edges n))
          (when (is-self-loop sl)
            (let* ((l-node (get-node (get-source sl)))
                   (c-node (jm-get nodes-map l-node))
                   (delta-x (- (.x (.hitbox c-node)) (.x (.hitbox-pre-compaction c-node)))))
              (offset (get-bend-points sl) delta-x 0.0)))))
      ;; offset straight segments
      (jdo (l (get-layers (.layered-graph this)))
        (jdo (n (get-nodes l))
          (jdo (e (get-outgoing-edges n))
            (let ((spline (get-property e layered-internal-properties.spline-route-start)))
              (when (and spline (not (is-empty spline)))
                (%lgct-adjust-spline-control-points this spline)))))))
    ;; offset selfloop labels
    (jdo (n (jm-keys nodes-map))
      (jdo (sl (get-outgoing-edges n))
        (when (is-self-loop sl)
          (let* ((l-node (get-node (get-source sl)))
                 (c-node (jm-get nodes-map l-node))
                 (delta-x (- (.x (.hitbox c-node)) (.x (.hitbox-pre-compaction c-node)))))
            (jdo (l (get-labels sl))
              (incf (.x (get-position l)) delta-x))))))
    ;; calculating new graph size and offset
    (let ((top-left (new-k-vector double.positive-infinity double.positive-infinity))
          (bottom-right (new-k-vector double.negative-infinity double.negative-infinity)))
      (jdo (c-node (.c-nodes c-graph))
        (let ((hb (.hitbox c-node)))
          (setf (.x top-left) (math.min (.x top-left) (.x hb))
                (.y top-left) (math.min (.y top-left) (.y hb))
                (.x bottom-right) (math.max (.x bottom-right) (+ (.x hb) (.width hb)))
                (.y bottom-right) (math.max (.y bottom-right) (+ (.y hb) (.height hb))))))
      (add (reset (get-offset (.layered-graph this))) (negate (clone top-left)))
      (add (reset (get-size (.layered-graph this))) (sub (clone bottom-right) top-left))
      ;; external port dummies may have been moved ... put them back
      (%lgct-apply-external-port-positions this top-left bottom-right))
    (%lgct-cleanup this)))

(defun %lgct-adjust-spline-control-points (this spline)
  (when (is-empty spline)
    (return-from %lgct-adjust-spline-control-points nil))
  (let ((last-seg (j-get spline 0)))
    ;; first case: a single segment
    (when (= (size spline) 1)
      ;; the two indexes are selected such that the method's loop runs exactly once
      (%lgct-adjust-control-point-between-segments this last-seg last-seg 1 0 spline)
      (return-from %lgct-adjust-spline-control-points nil))
    ;; ... more than one segment
    (let ((i 1))
      (loop while (< i (size spline))
            do (when (or (.initial-segment last-seg) (not (.is-straight last-seg)))
                 ;; find the next non-straight segment (or the very last segment of this spline)
                 (let ((needle (%lgct-first-non-straight-segment spline i)))
                   (when needle
                     (let ((j (get-first needle))
                           (next-seg (get-second needle)))
                       (%lgct-adjust-control-point-between-segments this last-seg next-seg i j spline)
                       (setf i (+ j 1))
                       (setf last-seg next-seg)))))))))

(defun %lgct-first-non-straight-segment (spline index)
  (when (or (< index 0) (>= index (size spline)))
    (return-from %lgct-first-non-straight-segment nil))
  (loop for i from index below (size spline)
        do (let ((seg (j-get spline i)))
             (when (or (= i (1- (size spline))) (not (.is-straight seg)))
               (return-from %lgct-first-non-straight-segment (pair.of i seg)))))
  nil)

(defun %lgct-adjust-control-point-between-segments (this left right left-idx right-idx spline)
  (let ((start-x 0.0) (idx1 left-idx) (end-x 0.0) (idx2 right-idx))
    ;; check if the initial segment of the spline is a straight one
    (if (and (.initial-segment left) (.is-straight left))
        (let ((n (jm-get (.nodes-map this) (.source-node left))))
          (setf start-x (+ (.x (.hitbox n)) (.width (.hitbox n))))
          (decf idx1))
        (setf start-x (+ (.x (.bounding-box left)) (.width (.bounding-box left)))))
    ;; ... the same for the last segment
    (if (and (.last-segment right) (.is-straight right))
        (let ((n (jm-get (.nodes-map this) (.target-node right))))
          (setf end-x (.x (.hitbox n)))
          (incf idx2))
        (setf end-x (.x (.bounding-box right))))
    ;; divide the available space into equidistant chunks
    (let* ((strip (- end-x start-x))
           (chunks (max 2 (- idx2 idx1)))
           (chunk (/ strip chunks))
           ;; apply new positions to the control points
           (new-pos (+ start-x chunk)))
      (loop for k from idx1 below idx2
            do (let* ((adjust (j-get spline k))
                      (width (.width (.bounding-box adjust))))
                 (setf (.x (.bounding-box adjust)) (- new-pos (/ width 2)))
                 (setf new-pos (+ new-pos chunk)))))))

(defun %lgct-apply-comment-positions (this)
  (jdo-map (comment p (.comment-offsets this))
    (let ((other (get-first p))
          (offset (get-second p)))
      (add (reset (get-position comment)) (add (clone (get-position other)) offset)))))

(defun %lgct-apply-external-port-positions (this top-left bottom-right)
  (jdo (c-node (.c-nodes (.c-graph this)))
    (let ((l-node (horizontal-graph-compactor.get-l-node-or-null c-node)))
      (when (and l-node (eq (get-type l-node) node-type.external-port))
        (enum-case (get-property l-node layered-internal-properties.ext-port-side)
          (port-side.west
           (setf (.x (get-position l-node)) (.x top-left)))
          (port-side.east
           (setf (.x (get-position l-node))
                 (- (.x bottom-right) (+ (.x (get-size l-node)) (.right (get-margin l-node))))))
          (port-side.north
           (setf (.y (get-position l-node)) (.y top-left)))
          (port-side.south
           (setf (.y (get-position l-node))
                 (- (.y bottom-right) (+ (.y (get-size l-node)) (.bottom (get-margin l-node))))))
          (t nil))))))

(defun %lgct-cleanup (this)
  (clrhash (.nodes-map this))
  (clrhash (.comment-offsets this))
  (clrhash (.vertical-segments-map this))
  (clrhash (.lock-map this))
  (jl-clear (.c-groups (.c-graph this)))
  (jl-clear (.c-nodes (.c-graph this)))
  (setf (.c-graph this) nil)
  (setf (.layered-graph this) nil))

;;; ------------------------------------------------------------ NetworkSimplexCompaction

(defjconst network-simplex-compaction.separation-weight 1)
(defjconst network-simplex-compaction.edge-weight 100)

(defjmethod compact ((this network-simplex-compaction) the-compactor)
  (setf (.compactor this) the-compactor)
  (setf (.network-simplex-graph this) (new-n-graph))
  (setf (.n-nodes this) (make-array (length (.c-groups (.c-graph the-compactor))) :initial-element nil))
  (setf (.index this) 0)
  ;; create a node in the network simplex graph for each group
  (jdo (c-group (.c-groups (.c-graph the-compactor)))
    (setf (.id c-group) (.index this))
    (let ((n-node (create (origin (id (n-node.of) (.index this)) c-group) (.network-simplex-graph this))))
      (setf (aref (.n-nodes this) (.index this)) n-node)
      (incf (.index this))))
  ;; #2 transform constraint graph edges
  (%nsc-add-separation-constraints this)
  ;; #3 add the original graph's edges for edge length minimization
  (%nsc-add-edge-constraints this)
  ;; #4 network simplex does not allow disconnected graphs, so we make it connected!
  (%nsc-add-artificial-source-node this)
  ;; #5 execute network simplex
  (execute (network-simplex.for-graph (.network-simplex-graph this)) (new-basic-progress-monitor))
  ;; #6 apply positions
  (jdo (c-node (.c-nodes (.c-graph the-compactor)))
    (setf (.x (.hitbox c-node)) (+ (.layer (aref (.n-nodes this) (.id (.c-group c-node))))
                                   (.x (.c-group-offset c-node))))))

(defun %nsc-nnode (this c-node)
  (aref (.n-nodes this) (.id (.c-group c-node))))

(defun %nsc-add-separation-constraints (this)
  (let ((compactor (.compactor this)))
    (jdo (c-node (.c-nodes (.c-graph compactor)))
      (jdo (inc-node (.constraints c-node))
        ;; no need to add edges between nodes of the same group
        (unless (eq (.c-group c-node) (.c-group inc-node))
          (let* ((spacing (if (is-horizontal (.direction compactor))
                              (get-horizontal-spacing (.spacings-handler compactor) c-node inc-node)
                              (get-vertical-spacing (.spacings-handler compactor) c-node inc-node)))
                 ;; the constraint edges look as follows: cNode <-- incNode
                 (delta (+ (.x (.c-group-offset c-node)) (.width (.hitbox c-node)) spacing
                           (- (.x (.c-group-offset inc-node))))))
            ;; network simplex requires an integer for delta
            (setf delta (math.ceil delta))
            ;; negative values can occur with external ports, it should be ok to fix this here
            (setf delta (math.max 0.0 delta))
            (if (not (horizontal-graph-compactor.is-vertical-segments-of-same-edge c-node inc-node))
                ;; it looks nicer if the initial segment is close to the node
                (let ((weight (jd network-simplex-compaction.separation-weight)))
                  (when (or (and (typep (.origin c-node) 'vertical-segment) (typep (.origin inc-node) 'l-node))
                            (and (typep (.origin inc-node) 'vertical-segment) (typep (.origin c-node) 'l-node)))
                    (setf weight 2.0))
                  ;; add a single edge, i.e. constraint to the network simplex graph
                  (create (target (source (weight (delta (n-edge.of) (to-int delta)) weight)
                                          (%nsc-nnode this c-node))
                                  (%nsc-nnode this inc-node))))
                ;; vertical segments are quite a species ... it may be necessary to change the
                ;; "order" of vertical segments' groups to get a straight edge
                (let* ((helper (create (n-node.of) (.network-simplex-graph this)))
                       (offset-delta (to-int (math.ceil (- (.x (.c-group-offset inc-node))
                                                           (.x (.c-group-offset c-node))))))
                       ;; if ports are involved, move one of them slightly since the computed
                       ;; positions will be integral
                       (adjust (- offset-delta (- (.x (.c-group-offset inc-node))
                                                  (.x (.c-group-offset c-node)))))
                       (port (.a-port (horizontal-graph-compactor.get-vertical-segment-or-null c-node)))
                       (alter-offset c-node))
                  (unless port
                    (setf port (.a-port (horizontal-graph-compactor.get-vertical-segment-or-null inc-node)))
                    (setf adjust (- adjust))
                    (setf alter-offset inc-node))
                  (when port
                    (decf (.x (.c-group-offset alter-offset)) adjust)
                    (decf (.x (get-position port)) adjust))
                  (create (target (source (weight (delta (n-edge.of) (max 0 offset-delta))
                                                  (jd network-simplex-compaction.separation-weight))
                                          helper)
                                  (%nsc-nnode this c-node)))
                  (create (target (source (weight (delta (n-edge.of) (max 0 (- offset-delta)))
                                                  (jd network-simplex-compaction.separation-weight))
                                          helper)
                                  (%nsc-nnode this inc-node)))))))))))

(defun %nsc-add-edge-constraints (this)
  (let ((compactor (.compactor this))
        ;; collect the original edges
        (l-node-map (new-jmap))
        ;; HashMultimap<LEdge, CNode>: per key an insertion-ordered set
        (l-edge-map (new-jmap)))
    (jdo (c-node (.c-nodes (.c-graph compactor)))
      (let ((l-node (horizontal-graph-compactor.get-l-node-or-null c-node)))
        (if l-node
            (jm-put l-node-map l-node c-node)
            (let ((vs (horizontal-graph-compactor.get-vertical-segment-or-null c-node)))
              (when vs
                (jdo (e (.represented-l-edges vs))
                  (let ((l (jm-compute-if-absent l-edge-map e (lambda (k) (declare (ignore k)) (new-jlist)))))
                    (unless (find c-node l) (jl-add l c-node)))))))))
    ;; add network simplex edges
    (jdo (c-node (.c-nodes (.c-graph compactor)))
      (let ((l-node (horizontal-graph-compactor.get-l-node-or-null c-node)))
        (when l-node
          (jdo (l-edge (get-outgoing-edges l-node))
            (block continue
              ;; ignore self-loops
              (when (is-self-loop l-edge) (return-from continue))
              (let ((src-port (get-source l-edge))
                    (tgt-port (get-target l-edge)))
                ;; they may be connected "directly"; if one of them is north or south however,
                ;; we still want to pull the node close
                (when (and (contains port-side.sides-north-south (get-side (get-source l-edge)))
                           (contains port-side.sides-north-south (get-side (get-target l-edge))))
                  (return-from continue))
                (let ((target (jm-get l-node-map (get-node (get-target l-edge)))))
                  (create (target (source (weight (delta (n-edge.of) 0)
                                                  (jd network-simplex-compaction.edge-weight))
                                          (%nsc-nnode this c-node))
                                  (%nsc-nnode this target))))
                ;; keep vertical segments close to the node if they are inverted ports
                (when (and (eq (get-side src-port) port-side.west)
                           (funcall l-port.output-predicate src-port))
                  (jdo (n (jm-get l-edge-map l-edge (new-jlist)))
                    (when (< (.x (.hitbox n)) (.x (.hitbox c-node)))
                      (let ((src (%nsc-nnode this n))
                            (tgt (%nsc-nnode this c-node)))
                        (unless (eq src tgt)
                          (create (target (source (weight (delta (n-edge.of) 1)
                                                          (jd network-simplex-compaction.edge-weight))
                                                  src)
                                          tgt)))))))
                (when (and (eq (get-side tgt-port) port-side.east)
                           (funcall l-port.input-predicate tgt-port))
                  (jdo (n (jm-get l-edge-map l-edge (new-jlist)))
                    (when (> (.x (.hitbox n)) (.x (.hitbox c-node)))
                      (let ((src (%nsc-nnode this c-node))
                            (tgt (%nsc-nnode this n)))
                        (unless (eq src tgt)
                          (create (target (source (weight (delta (n-edge.of) 1)
                                                          (jd network-simplex-compaction.edge-weight))
                                                  src)
                                          tgt)))))))))))))))

(defun %nsc-add-artificial-source-node (this)
  (let ((sources (new-jlist)))
    (jdo (n (.nodes (.network-simplex-graph this)))
      (when (is-empty (get-incoming-edges n))
        (jl-add sources n)))
    (when (> (length sources) 1)
      (let ((dummy-source (create (id (n-node.of) (prog1 (.index this) (incf (.index this))))
                                  (.network-simplex-graph this))))
        (jdo (src sources)
          (create (target (source (weight (delta (n-edge.of) 1) 0.0) dummy-source) src)))))))

;;; ------------------------------------------------------------ EdgeAwareScanlineConstraintCalculation

(defjconst edge-aware-scanline-constraint-calculation.epsilon 0.5)
(defjconst edge-aware-scanline-constraint-calculation.small-epsilon 0.01)

(defjctor edge-aware-scanline-constraint-calculation (graph)
  (setf (.vertical-edge-edge-spacing this) (get-property graph layered-options.spacing-edge-edge))
  (setf (.edge-routing this) (get-property graph layered-options.edge-routing)))

(defjmethod calculate-constraints ((this edge-aware-scanline-constraint-calculation) the-compactor)
  (setf (.compactor this) the-compactor)
  (enum-case (.edge-routing this)
    (edge-routing.orthogonal (%easc-calculate-for-orthogonal this))
    (edge-routing.splines (%easc-calculate-for-spline this))
    (t (jthrow 'unsupported-configuration-exception))))

(defun %easc-min-spacing (this)
  "The minimum spacing over all CNodes (DoubleStream.min().orElse(0))."
  (let ((result nil)
        (epsilon edge-aware-scanline-constraint-calculation.epsilon))
    (jdo (n (.c-nodes (.c-graph (.compactor this))))
      (let ((v (cond
                 ;; ignore external ports since their spacing is internally set to 0
                 ((and (typep (.origin n) 'l-node) (eq (get-type (.origin n)) node-type.external-port))
                  double.positive-infinity)
                 ((horizontal-graph-compactor.get-vertical-segment-or-null n)
                  (math.max 0.0 (- (/ (.vertical-edge-edge-spacing this) 2) epsilon)))
                 ((horizontal-graph-compactor.get-l-node-or-null n)
                  (let ((spacing (spacings.get-individual-or-default
                                  (horizontal-graph-compactor.get-l-node-or-null n)
                                  layered-options.spacing-node-node)))
                    (math.max 0.0 (- (/ spacing 2) epsilon))))
                 (t double.positive-infinity))))
        (setf result (if result (math.min result v) v))))
    (or result 0.0)))

(defun %easc-calculate-for-spline (this)
  (let ((schedule (new-jlist)))
    ;; Vertical Segments: some constraints between subsequent vertical segments of the same
    ;; spline have been precalculated during import; the boxes are not enlarged here
    (sweep this (lambda (n) (typep (.origin n) 'vertical-segment)))
    ;; Everything: find the minimum spacing
    (let ((min-spacing (%easc-min-spacing this)))
      (jdo (n (jlist-from (.c-nodes (.c-graph (.compactor this)))))
        (when (typep (.origin n) 'l-node)
          (let ((n n))
            (%easc-alter-hitbox this n min-spacing 1.0)
            (jl-add schedule (lambda () (%easc-alter-hitbox this n min-spacing -1.0))))))
      (sweep this (lambda (n) (declare (ignore n)) t))
      (jdo (r schedule) (funcall r))
      (jl-clear schedule))))

(defun %easc-calculate-for-orthogonal (this)
  (let ((schedule (new-jlist))
        (epsilon edge-aware-scanline-constraint-calculation.epsilon)
        (c-graph (.c-graph (.compactor this))))
    ;; Vertical Segments
    (jdo (n (jlist-from (.c-nodes c-graph)))
      (when (typep (.origin n) 'vertical-segment)
        (let ((n n)
              (spacing (math.max 0.0 (- (/ (.vertical-edge-edge-spacing this) 2) epsilon))))
          (%easc-alter-hitbox this n spacing 1.0)
          (jl-add schedule (lambda () (%easc-alter-hitbox this n spacing -1.0))))))
    (sweep this (lambda (n) (typep (.origin n) 'vertical-segment)))
    (jdo (r schedule) (funcall r))
    (jl-clear schedule)
    ;; Nodes
    (jdo (n (jlist-from (.c-nodes c-graph)))
      (when (typep (.origin n) 'l-node)
        (let* ((n n)
               (l-node (horizontal-graph-compactor.get-l-node-or-null n))
               ;; One would want to use node-to-node spacing here but if edge-node spacing is
               ;; smaller than node-to-node/2, two nodes could be placed closer than they may
               (spacing (spacings.get-individual-or-default l-node layered-options.spacing-edge-edge))
               (final-spacing (math.max 0.0 (- (/ spacing 2) epsilon))))
          (%easc-alter-hitbox this n final-spacing 1.0)
          (jl-add schedule (lambda () (%easc-alter-hitbox this n final-spacing -1.0))))))
    (sweep this (lambda (n) (typep (.origin n) 'l-node)))
    (jdo (r schedule) (funcall r))
    (jl-clear schedule)
    ;; Everything: find the minimum spacing
    (let ((min-spacing (%easc-min-spacing this)))
      (jdo (g (jlist-from (.c-groups c-graph)))
        (let ((g g))
          (%easc-alter-grouped-hitbox-orthogonal this g min-spacing 1.0)
          (jl-add schedule (lambda () (%easc-alter-grouped-hitbox-orthogonal this g min-spacing -1.0)))))
      (sweep this (lambda (n) (declare (ignore n)) t))
      (jdo (r schedule) (funcall r))
      (jl-clear schedule))))

(defun %easc-alter-grouped-hitbox-orthogonal (this g spacing fac)
  (let ((master (.master g))
        (small-epsilon edge-aware-scanline-constraint-calculation.small-epsilon))
    (unless master
      (setf master (first (to-cl-list (.c-nodes g)))))
    ;; modify the master
    (%easc-alter-hitbox this master spacing fac)
    (when (= (size (.c-nodes g)) 1)
      (return-from %easc-alter-grouped-hitbox-orthogonal nil))
    ;; adjust all vertical segments
    (let ((delta (* spacing fac)))
      (jdo (n (.c-nodes g))
        (unless (eq n master)
          ;; n represents a vertical segment (by construction of the groups)
          (let ((vs (horizontal-graph-compactor.get-vertical-segment-or-null n)))
            (cond ((.up (.ignore-spacing vs))
                   ;; it's a southward segment
                   (incf (.y (.hitbox n)) (+ delta small-epsilon))
                   (decf (.height (.hitbox n)) (+ delta small-epsilon)))
                  ((.down (.ignore-spacing vs))
                   ;; northward
                   (decf (.height (.hitbox n)) (+ delta small-epsilon))))))))))

(defun %easc-alter-hitbox (this n spacing fac)
  (declare (ignore this))
  (let ((delta (* spacing fac))
        (small-epsilon edge-aware-scanline-constraint-calculation.small-epsilon))
    (cond ((typep (.origin n) 'vertical-segment)
           (let ((vs (horizontal-graph-compactor.get-vertical-segment-or-null n)))
             (cond ((not (.up (.ignore-spacing vs)))
                    (decf (.y (.hitbox n)) (+ delta small-epsilon))
                    (incf (.height (.hitbox n)) (+ delta small-epsilon)))
                   ((not (.down (.ignore-spacing vs)))
                    (incf (.height (.hitbox n)) (+ delta small-epsilon))))))
          ((typep (.origin n) 'l-node)
           (decf (.y (.hitbox n)) delta)
           (incf (.height (.hitbox n)) (* 2 delta))))))
