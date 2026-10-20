;;;; long-edges.lisp -- LongEdgeJoiner, LongEdgeSplitter.

(in-package #:elk)

;;; ------------------------------------------------ LongEdgeJoiner

(defjmethod process ((this long-edge-joiner) layered-graph monitor)
  (begin monitor "Edge joining" 1)
  (let ((add-unnecessary-bendpoints (get-property layered-graph layered-options.unnecessary-bendpoints)))
    (jdo (layer (get-layers layered-graph))
      (let ((nodes (get-nodes layer)) (i 0))
        (loop while (< i (length nodes))
              do (let ((node (aref nodes i)))
                   (if (eq (get-type node) node-type.long-edge)
                       (progn
                         (long-edge-joiner.join-at node add-unnecessary-bendpoints)
                         (jl-remove-at nodes i))
                       (incf i)))))))
  (done monitor))

(defjstatic long-edge-joiner.join-at (long-edge-dummy add-unnecessary-bendpoints)
  ;; the incoming edges are retained, and the outgoing edges are discarded
  (let* ((input-port-edges (get-incoming-edges (elt (to-jvector (get-ports long-edge-dummy port-side.west)) 0)))
         (output-port-edges (get-outgoing-edges (elt (to-jvector (get-ports long-edge-dummy port-side.east)) 0)))
         (edge-count (length input-port-edges))
         (unnecessary-bendpoint (get-absolute-anchor (jl-get (get-ports long-edge-dummy) 0))))
    ;; Edges with the same indices in the two lists originate from the same long edge
    (loop while (> edge-count 0)
          do (decf edge-count)
             (let* ((surviving-edge (jl-get input-port-edges 0))
                    (dropped-edge (jl-get output-port-edges 0))
                    ;; keep the surviving edge at the dropped edge's index (KIPRA-1670)
                    (target-incoming-edges (get-incoming-edges (get-target dropped-edge)))
                    (dropped-edge-list-index (jl-index-of target-incoming-edges dropped-edge)))
               (set-target-and-insert-at-index surviving-edge (get-target dropped-edge) dropped-edge-list-index)
               ;; Remove the dropped edge from the graph
               (set-source dropped-edge nil)
               (set-target dropped-edge nil)
               ;; Join their bend points and add possibly an unnecessary one
               (let ((surviving-bend-points (get-bend-points surviving-edge)))
                 (when add-unnecessary-bendpoints
                   (add surviving-bend-points (new-k-vector unnecessary-bendpoint)))
                 (jdo (bend-point (get-bend-points dropped-edge))
                   (add surviving-bend-points (new-k-vector bend-point))))
               ;; Join their labels
               (let ((surviving-labels (get-labels surviving-edge)))
                 (jdo (label (get-labels dropped-edge))
                   (jl-add surviving-labels label)))
               ;; Join their junction points
               (let ((surviving-junction-points (get-property surviving-edge layered-options.junction-points))
                     (dropped-junction-points (get-property dropped-edge layered-options.junction-points)))
                 (when dropped-junction-points
                   (when (null surviving-junction-points)
                     (setf surviving-junction-points (new-k-vector-chain))
                     (set-property surviving-edge layered-options.junction-points surviving-junction-points))
                   (jdo (jp dropped-junction-points)
                     (add surviving-junction-points (new-k-vector jp)))))))))

;;; ------------------------------------------------ LongEdgeSplitter

(defjmethod process ((this long-edge-splitter) layered-graph monitor)
  (begin monitor "Edge splitting" 1)
  (let ((layers (get-layers layered-graph)))
    (when (<= (length layers) 2)
      (done monitor)
      (return-from process/2 nil))
    (loop for li from 0 below (1- (length layers))
          do (let ((layer (aref layers li))
                   (next-layer (aref layers (1+ li))))
               (jdo (node (get-nodes layer))
                 (jdo (port (get-ports node))
                   (jdo (edge (get-outgoing-edges port))
                     (let ((target-layer (get-layer (get-node (get-target edge)))))
                       ;; If the edge doesn't go to the current or next layer, split it
                       (when (and (not (eq target-layer layer)) (not (eq target-layer next-layer)))
                         (long-edge-splitter.split-edge
                          edge (create-dummy-node this layered-graph next-layer edge))))))))))
  (done monitor))

(defjmethod create-dummy-node ((this long-edge-splitter) layered-graph target-layer edge-to-split)
  (let ((dummy-node (new-l-node layered-graph)))
    (set-type dummy-node node-type.long-edge)
    (set-property dummy-node layered-internal-properties.origin edge-to-split)
    (set-property dummy-node layered-options.port-constraints port-constraints.fixed-pos)
    (set-layer dummy-node target-layer)
    dummy-node))

(defjstatic long-edge-splitter.split-edge (edge dummy-node)
  (let ((old-edge-target (get-target edge))
        (thickness (get-property edge layered-options.edge-thickness)))
    ;; Set thickness of the edge
    (when (< thickness 0)
      (setf thickness 0.0)
      (set-property edge layered-options.edge-thickness thickness))
    (setf (.y (get-size dummy-node)) thickness)
    (let ((port-pos (math.floor (/ thickness 2)))
          (dummy-input (new-l-port))
          (dummy-output (new-l-port)))
      (set-side dummy-input port-side.west)
      (set-node dummy-input dummy-node)
      (setf (.y (get-position dummy-input)) port-pos)
      (set-side dummy-output port-side.east)
      (set-node dummy-output dummy-node)
      (setf (.y (get-position dummy-output)) port-pos)
      (set-target edge dummy-input)
      ;; Create a dummy edge
      (let ((dummy-edge (new-l-edge)))
        (copy-properties dummy-edge edge)
        (set-property dummy-edge layered-options.junction-points nil)
        (set-source dummy-edge dummy-output)
        (set-target dummy-edge old-edge-target)
        (long-edge-splitter.set-dummy-node-properties dummy-node edge dummy-edge)
        (long-edge-splitter.move-head-labels edge dummy-edge)
        dummy-edge))))

(defjstatic long-edge-splitter.set-dummy-node-properties (dummy-node in-edge out-edge)
  (let ((in-edge-source-node (get-node (get-source in-edge)))
        (out-edge-target-node (get-node (get-target out-edge))))
    (cond
      ((eq (get-type in-edge-source-node) node-type.long-edge)
       (set-property dummy-node layered-internal-properties.long-edge-source
                     (get-property in-edge-source-node layered-internal-properties.long-edge-source))
       (set-property dummy-node layered-internal-properties.long-edge-target
                     (get-property in-edge-source-node layered-internal-properties.long-edge-target))
       (set-property dummy-node layered-internal-properties.long-edge-has-label-dummies
                     (get-property in-edge-source-node layered-internal-properties.long-edge-has-label-dummies)))
      ((eq (get-type in-edge-source-node) node-type.label)
       (set-property dummy-node layered-internal-properties.long-edge-source
                     (get-property in-edge-source-node layered-internal-properties.long-edge-source))
       (set-property dummy-node layered-internal-properties.long-edge-target
                     (get-property in-edge-source-node layered-internal-properties.long-edge-target))
       (set-property dummy-node layered-internal-properties.long-edge-has-label-dummies t))
      ((eq (get-type out-edge-target-node) node-type.label)
       (set-property dummy-node layered-internal-properties.long-edge-source
                     (get-property out-edge-target-node layered-internal-properties.long-edge-source))
       (set-property dummy-node layered-internal-properties.long-edge-target
                     (get-property out-edge-target-node layered-internal-properties.long-edge-target))
       (set-property dummy-node layered-internal-properties.long-edge-has-label-dummies t))
      (t
       (set-property dummy-node layered-internal-properties.long-edge-source (get-source in-edge))
       (set-property dummy-node layered-internal-properties.long-edge-target (get-target out-edge))))))

(defjstatic long-edge-splitter.move-head-labels (old-edge new-edge)
  (let ((labels (get-labels old-edge)) (i 0))
    (loop while (< i (length labels))
          do (let ((label (aref labels i)))
               (if (eq (get-property label layered-options.edge-labels-placement) edge-label-placement.head)
                   (progn
                     (jl-remove-at labels i)
                     (jl-add (get-labels new-edge) label)
                     (unless (has-property label layered-internal-properties.end-label-edge)
                       (set-property label layered-internal-properties.end-label-edge old-edge)))
                   (incf i))))))
