;;;; misc.lisp -- InLayerConstraintProcessor, HypernodesProcessor, HyperedgeDummyMerger.

(in-package #:elk)

;; Private helpers of this file are called before their defjmethod is compiled.
(define-jcalls :instance
  process
  move-hypernode
  check-merge-allowed
  merge-nodes
  identify-hyperedges
  dfs)

;;; ------------------------------------------------------------ InLayerConstraintProcessor

(defjmethod process ((this in-layer-constraint-processor) layered-graph monitor)
  (begin monitor "Layer constraint edge reversal" 1)
  ;; Iterate through each layer
  (jdo (layer (get-layers layered-graph))
    ;; We'll go through the layer's nodes, remembering two things:
    ;;  1. Once we reach the first non-top-constrained node, we remember its
    ;;     index.  Top-constrained nodes encountered afterwards must be inserted
    ;;     at that point.
    ;;  2. A list of bottom-constrained nodes we have encountered.  They will
    ;;     afterwards be moved to the end of the list, keeping the order in
    ;;     which we've encountered them.
    (let ((top-insertion-index -1)
          (bottom-constrained-nodes (new-jlist)))
      ;; Iterate through an array of its nodes
      (let ((nodes (l-graph-util.to-node-array (get-nodes layer))))
        (loop for i from 0 below (length nodes)
              for node = (aref nodes i)
              do (let ((constraint (get-property node layered-internal-properties.in-layer-constraint)))
                   (if (= top-insertion-index -1)
                       ;; See if this node is the first non-top-constrained node
                       (when (not (eq constraint in-layer-constraint.top))
                         (setf top-insertion-index i))
                       ;; We have already encountered non-top-constrained nodes before
                       (when (eq constraint in-layer-constraint.top)
                         ;; Move the node to the top insertion point
                         (set-layer node nil)
                         (set-layer node (prog1 top-insertion-index (incf top-insertion-index))
                                    layer)))
                   ;; Put BOTTOM-constrained nodes into the corresponding list
                   (when (eq constraint in-layer-constraint.bottom)
                     (jl-add bottom-constrained-nodes node)))))
      ;; Append the bottom-constrained nodes
      (jdo (node bottom-constrained-nodes)
        (set-layer node nil)
        (set-layer node layer))))
  (done monitor))

;;; ------------------------------------------------------------ HypernodesProcessor

(defjmethod process ((this hypernodes-processor) layered-graph monitor)
  (begin monitor "Hypernodes processing" 1)
  (jdo (layer (get-layers layered-graph))
    (jdo (node layer)
      (when (and (get-property node layered-options.hypernode)
                 (<= (size (get-ports node)) 2))
        (let ((top-edges 0) (right-edges 0) (bottom-edges 0) (left-edges 0))
          (jdo (port (get-ports node))
            (enum-case (get-side port)
              (port-side.north (incf top-edges))
              (port-side.east (incf right-edges))
              (port-side.south (incf bottom-edges))
              (port-side.west (incf left-edges))))
          ;; don't move the node if there are any edges to the top or bottom
          (when (and (= top-edges 0) (= bottom-edges 0))
            (move-hypernode this layered-graph node (<= left-edges right-edges)))))))
  (done monitor))

(defjmethod move-hypernode ((this hypernodes-processor) layered-graph hypernode right)
  "Move the given hypernode either towards the previous layer or towards the next layer.
RIGHT true moves the node right (to the next layer), else it is moved left (to the previous layer)."
  ;; find edges that constitute the first join point of the hyperedge
  (let ((bend-edges (new-jlist))
        (bendx (float integer.max-value 1.0d0))
        (diffx (float integer.max-value 1.0d0))
        (diffy (float integer.max-value 1.0d0)))
    (if right
        (progn
          (setf bendx (.x (get-size layered-graph)))
          (jdo (port (get-ports hypernode))
            (jdo (edge (get-outgoing-edges port))
              (unless (is-empty (get-bend-points edge))
                (let ((first-point (get-first (get-bend-points edge))))
                  (when (< (.x first-point) bendx)
                    (setf diffx (- bendx (.x first-point)))
                    (setf diffy (float integer.max-value 1.0d0))
                    (jl-clear bend-edges)
                    (setf bendx (.x first-point)))
                  (when (<= (.x first-point) bendx)
                    (jl-add bend-edges edge)
                    (when (> (size (get-bend-points edge)) 1)
                      (setf diffy (math.min diffy
                                            (abs (- (.y (j-get (get-bend-points edge) 1))
                                                    (.y first-point))))))))))))
        (jdo (port (get-ports hypernode))
          (jdo (edge (get-incoming-edges port))
            (unless (is-empty (get-bend-points edge))
              (let ((last-point (get-last (get-bend-points edge))))
                (when (> (.x last-point) bendx)
                  (setf diffx (- (.x last-point) bendx))
                  (setf diffy (float integer.max-value 1.0d0))
                  (jl-clear bend-edges)
                  (setf bendx (.x last-point)))
                (when (>= (.x last-point) bendx)
                  (jl-add bend-edges edge)
                  (when (> (size (get-bend-points edge)) 1)
                    (setf diffy (math.min diffy
                                          (abs (- (.y (j-get (get-bend-points edge)
                                                             (- (size (get-bend-points edge)) 2)))
                                                  (.y last-point))))))))))))
    (when (and (not (jl-is-empty bend-edges))
               (> diffx (/ (.x (get-size hypernode)) 2.0d0))
               (> diffy (/ (.y (get-size hypernode)) 2.0d0)))
      ;; create new ports for the edges
      (let ((north-port (new-l-port))
            (south-port (new-l-port)))
        (set-node north-port hypernode)
        (set-side north-port port-side.north)
        (setf (.x (get-position north-port)) (/ (.x (get-size hypernode)) 2.0d0))
        (set-node south-port hypernode)
        (set-side south-port port-side.south)
        (setf (.x (get-position south-port)) (/ (.x (get-size hypernode)) 2.0d0))
        (setf (.y (get-position south-port)) (.y (get-size hypernode)))
        ;; replace the first bend point by the new ports
        (jdo (edge bend-edges)
          (let (bp-first bp-second)
            (if right
                (progn
                  (setf bp-first (remove-first (get-bend-points edge)))
                  (setf bp-second (if (is-empty (get-bend-points edge))
                                   (get-absolute-anchor (get-target edge))
                                   (get-first (get-bend-points edge))))
                  (if (>= (.y bp-second) (.y bp-first))
                      (set-source edge south-port)
                      (set-source edge north-port)))
                (progn
                  (setf bp-first (remove-last (get-bend-points edge)))
                  (setf bp-second (if (is-empty (get-bend-points edge))
                                   (get-absolute-anchor (get-source edge))
                                   (get-last (get-bend-points edge))))
                  (if (>= (.y bp-second) (.y bp-first))
                      (set-target edge south-port)
                      (set-target edge north-port))))
            ;; remove junction points that collide with the eliminated bend point
            (let ((junction-points (get-property edge layered-options.junction-points)))
              (when junction-points
                (j-remove junction-points bp-first)))))
        ;; move the node to new position
        (setf (.x (get-position hypernode))
              (- bendx (/ (.x (get-size hypernode)) 2.0d0)))))))

;;; ------------------------------------------------------------ HyperedgeDummyMerger

(defjctor merge-state (allow-merge same-source same-target)
  (setf (.allow-merge this) allow-merge
        (.same-source this) same-source
        (.same-target this) same-target))

(defjmethod process ((this hyperedge-dummy-merger) layered-graph monitor)
  (begin monitor "Hyperedge merging" 1)
  (identify-hyperedges this layered-graph)
  ;; Iterate through the layers
  (jdo (layer (get-layers layered-graph))
    (let ((nodes (get-nodes layer)))
      ;; If there are no nodes anyway, just move on to the next layer
      (unless (is-empty nodes)
        (let ((curr-node nil)
              (curr-node-type nil)
              (last-node nil)
              (last-node-type nil)
              (node-index 0))
          ;; Iterate through the remaining nodes
          (loop while (< node-index (size nodes))
                do (progn
                     ;; Get the next node
                     (setf curr-node (jl-get nodes node-index))
                     (setf curr-node-type (get-type curr-node))
                     ;; We're only interested if the current and last nodes are long edge dummies
                     (when (and (eq curr-node-type node-type.long-edge)
                                (eq last-node-type node-type.long-edge))
                       ;; If the source or the target are identical and we are allowed to merge,
                       ;; merge the current node into the last
                       (let ((state (check-merge-allowed this curr-node last-node)))
                         (when (.allow-merge state)
                           (merge-nodes this curr-node last-node
                                        (.same-source state) (.same-target state))
                           ;; Remove the current node and make the last node the current node
                           (jl-remove-at nodes node-index)
                           (decf node-index)
                           (setf curr-node last-node)
                           (setf curr-node-type last-node-type))))
                     ;; Remember this node for the next iteration
                     (setf last-node curr-node)
                     (setf last-node-type curr-node-type)
                     (incf node-index)))))))
  (done monitor))

(defjmethod check-merge-allowed ((this hyperedge-dummy-merger) curr-node last-node)
  "Returns whether merging the two long edge dummies is eligible as specified in the class's javadoc."
  (let* ((curr-has-label-dummies (get-property curr-node layered-internal-properties.long-edge-has-label-dummies))
         (last-has-label-dummies (get-property last-node layered-internal-properties.long-edge-has-label-dummies))
         ;; Get long edge source and target ports
         (curr-node-source (get-property curr-node layered-internal-properties.long-edge-source))
         (last-node-source (get-property last-node layered-internal-properties.long-edge-source))
         (curr-node-target (get-property curr-node layered-internal-properties.long-edge-target))
         (last-node-target (get-property last-node layered-internal-properties.long-edge-target))
         ;; Find out whether the two long edges come from the same source or target (non-null!)
         (same-source (and curr-node-source (eq curr-node-source last-node-source)))
         (same-target (and curr-node-target (eq curr-node-target last-node-target))))
    (if (and (not curr-has-label-dummies) (not last-has-label-dummies))
        ;; assumption: long edge dummies always have two ports, both have the same id
        (new-merge-state/3 (= (.id (jl-first (get-ports curr-node)))
                              (.id (jl-first (get-ports last-node))))
                           same-source same-target)
        (let* (;; If we can merge on grounds of the two long edges having the same source, we need
               ;; to be in front of the label dummy of each of the two edges (if any)
               (eligible-for-source-merging
                 (and (or (not (get-property curr-node layered-internal-properties.long-edge-has-label-dummies))
                          (get-property curr-node layered-internal-properties.long-edge-before-label-dummy))
                      (or (not (get-property last-node layered-internal-properties.long-edge-has-label-dummies))
                          (get-property last-node layered-internal-properties.long-edge-before-label-dummy))))
               ;; If we can merge on grounds of the two long edges having the same target, we need to
               ;; be behind the label dummy of each of the two edges (if any)
               (eligible-for-target-merging
                 (and (or (not (get-property curr-node layered-internal-properties.long-edge-has-label-dummies))
                          (not (get-property curr-node layered-internal-properties.long-edge-before-label-dummy)))
                      (or (not (get-property last-node layered-internal-properties.long-edge-has-label-dummies))
                          (not (get-property last-node layered-internal-properties.long-edge-before-label-dummy))))))
          (new-merge-state/3 (or (and same-source eligible-for-source-merging)
                                 (and same-target eligible-for-target-merging))
                             same-source same-target)))))

(defjmethod merge-nodes ((this hyperedge-dummy-merger) merge-source merge-target keep-source-port keep-target-port)
  "Merges the merge source node into the merge target node.  All edges that were previously
connected to the merge source's ports are rerouted to the merge target.  The merge target's long
edge source and target ports can be set to NIL."
  ;; We assume that the input port is west, and the output port east
  (let ((merge-target-input-port (jl-first (get-ports merge-target port-side.west)))
        (merge-target-output-port (jl-first (get-ports merge-target port-side.east))))
    (jdo (port (get-ports merge-source))
      (loop while (not (is-empty (get-incoming-edges port)))
            do (set-target (jl-get (get-incoming-edges port) 0) merge-target-input-port))
      (loop while (not (is-empty (get-outgoing-edges port)))
            do (set-source (jl-get (get-outgoing-edges port) 0) merge-target-output-port))))
  ;; Possibly reset source and target ports
  (unless keep-source-port
    (set-property merge-target layered-internal-properties.long-edge-source nil))
  (unless keep-target-port
    (set-property merge-target layered-internal-properties.long-edge-target nil)))

(defjmethod identify-hyperedges ((this hyperedge-dummy-merger) l-graph)
  (let ((ports (new-jlist)))
    (jdo (layer (get-layers l-graph))
      (jdo (node (get-nodes layer))
        (jdo (port (get-ports node))
          (jl-add ports port))))
    (jdo (p ports)
      (setf (.id p) -1))
    (let ((index 0))
      (jdo (p ports)
        (when (= (.id p) -1)
          (dfs this p index)
          (incf index))))))

(defjmethod dfs ((this hyperedge-dummy-merger) p index)
  (setf (.id p) index)
  ;; follow edges connected to the same port
  (jdo (p2 (get-connected-ports p))
    (when (= (.id p2) -1)
      (dfs this p2 index)))
  ;; follow edges connected to the same long edge dummy
  (when (eq (get-type (get-node p)) node-type.long-edge)
    (jdo (p2 (get-ports (get-node p)))
      (when (and (not (eq p2 p)) (= (.id p2) -1))
        (dfs this p2 index)))))
