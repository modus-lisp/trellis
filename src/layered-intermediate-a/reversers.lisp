;;;; reversers.lisp -- DummySelfLoopProcessor, EdgeAndLayerConstraintEdgeReverser.

(in-package #:elk)

;;; ------------------------------------------------------------ DummySelfLoopProcessor

(defjmethod process ((this dummy-self-loop-processor) layered-graph monitor)
  (begin monitor "Self-loop processing" 1)
  ;; Iterate through all nodes
  (let ((created-dummies (new-jlist)))
    (jdo (layer (get-layers layered-graph))
      (jl-clear created-dummies)
      (jdo (node (get-nodes layer))
        (jdo (port (get-ports node))
          ;; Go through the port's outgoing edges
          (jdo (edge (l-graph-util.to-edge-array (get-outgoing-edges port)))
            ;; We're only interested in edges whose source and target node are identical
            (when (eq (get-node (get-source edge)) (get-node (get-target edge)))
              (let* ((source-port (get-source edge))
                     (target-port (get-target edge))
                     (source-port-side (get-side source-port))
                     (target-port-side (get-side target-port)))
                ;; First, let's deal with the cases where edges have to be reversed
                (cond ((and (or (eq source-port-side port-side.north)
                                (eq source-port-side port-side.south))
                            (eq target-port-side port-side.west))
                       (j-reverse edge layered-graph nil))
                      ((and (eq source-port-side port-side.south)
                            (eq target-port-side port-side.north))
                       (j-reverse edge layered-graph nil))
                      ((and (eq source-port-side port-side.east)
                            (not (eq target-port-side port-side.east)))
                       (j-reverse edge layered-graph nil)))
                ;; Now, let's see if a dummy has to be inserted
                (cond ((and (eq source-port-side port-side.east)
                            (eq target-port-side port-side.west))
                       ;; Note that the edge was reversed, so source and target port have switched
                       (jl-add created-dummies
                               (create-dummy this layered-graph edge target-port source-port)))
                      ((and (eq source-port-side port-side.west)
                            (eq target-port-side port-side.east))
                       (jl-add created-dummies
                               (create-dummy this layered-graph edge source-port target-port)))))))))
      ;; Add the dummies, if any
      (jdo (dummy created-dummies)
        (set-layer dummy layer))))
  (done monitor))

(defjmethod create-dummy ((this dummy-self-loop-processor) layered-graph edge source-port target-port)
  ;; Create a dummy node with an input port and an output port
  (let ((dummy-node (new-l-node layered-graph)))
    (set-type dummy-node node-type.long-edge)
    (set-property dummy-node layered-internal-properties.origin edge)
    (set-property dummy-node layered-options.port-constraints port-constraints.fixed-pos)
    (set-property dummy-node layered-internal-properties.long-edge-source source-port)
    (set-property dummy-node layered-internal-properties.long-edge-target target-port)
    (let ((dummy-input (new-l-port))
          (dummy-output (new-l-port)))
      (set-side dummy-input port-side.west)
      (set-node dummy-input dummy-node)
      (set-side dummy-output port-side.east)
      (set-node dummy-output dummy-node)
      (set-target edge dummy-input)
      ;; Create a dummy edge
      (let ((dummy-edge (new-l-edge)))
        (copy-properties dummy-edge edge)
        (set-property dummy-edge layered-options.junction-points nil)
        (set-source dummy-edge dummy-output)
        (set-target dummy-edge target-port)))
    dummy-node))

;;; ------------------------------------------------------------ EdgeAndLayerConstraintEdgeReverser

(defjmethod process ((this edge-and-layer-constraint-edge-reverser) layered-graph monitor)
  (begin monitor "Edge and layer constraint edge reversal" 1)
  ;; Apply edge reversal to all FIRST and FIRST_SEPARATE as well as LAST and LAST_SEPARATE nodes
  (let ((remaining-nodes (handle-outer-nodes this layered-graph)))
    ;; Apply edge reversal to remaining nodes
    (handle-inner-nodes this layered-graph remaining-nodes))
  (done monitor))

(defun %elcer-edge-constraint (layer-constraint)
  (enum-case layer-constraint
    ((layer-constraint.first layer-constraint.first-separate) edge-constraint.outgoing-only)
    ((layer-constraint.last layer-constraint.last-separate) edge-constraint.incoming-only)
    (t nil)))

(defjmethod handle-outer-nodes ((this edge-and-layer-constraint-edge-reverser) layered-graph)
  (let ((remaining-nodes (new-jlist (length (get-layerless-nodes layered-graph)))))
    (jdo (node (get-layerless-nodes layered-graph))
      ;; Check if there is a layer constraint
      (let* ((layer-constraint (get-property node layered-options.layering-layer-constraint))
             (edge-constraint (%elcer-edge-constraint layer-constraint)))
        (if edge-constraint
            (progn
              ;; Set the edge constraint on the node
              (set-property node layered-internal-properties.edge-constraint
                            edge-constraint.outgoing-only)
              (cond ((eq edge-constraint edge-constraint.incoming-only)
                     (reverse-edges this layered-graph node layer-constraint port-type.input))
                    ((eq edge-constraint edge-constraint.outgoing-only)
                     (reverse-edges this layered-graph node layer-constraint port-type.output))))
            ;; Remember this node for the next loop
            (jl-add remaining-nodes node))))
    remaining-nodes))

(defjmethod handle-inner-nodes ((this edge-and-layer-constraint-edge-reverser) layered-graph remaining-nodes)
  ;; Iterate over the remaining nodes
  (jdo (node remaining-nodes)
    ;; Check if there is a layer constraint
    (let* ((layer-constraint (get-property node layered-options.layering-layer-constraint))
           (edge-constraint (%elcer-edge-constraint layer-constraint)))
      (if edge-constraint
          (progn
            ;; Set the edge constraint on the node
            (set-property node layered-internal-properties.edge-constraint
                          edge-constraint.outgoing-only)
            (cond ((eq edge-constraint edge-constraint.incoming-only)
                   (reverse-edges this layered-graph node layer-constraint port-type.input))
                  ((eq edge-constraint edge-constraint.outgoing-only)
                   (reverse-edges this layered-graph node layer-constraint port-type.output))))
          ;; If the port sides are fixed, but all ports are reversed, that probably means that we
          ;; have a feedback node. Normally, the connected edges would be routed around the node,
          ;; but that hides the feedback node character. We thus simply reverse all connected
          ;; edges and thus make ELK Layered think we have a regular node.
          ;; Note that this behavior is only desired if none of the connected nodes have
          ;; layer constraints set. Otherwise this processing causes issues with an external
          ;; port dummy with FIRST_SEPARATE and an inverted ports on the target node's EAST side.
          (when (and (is-side-fixed (get-property node layered-options.port-constraints))
                     (not (jl-is-empty (get-ports node))))
            (let ((all-ports-reversed t))
              (jdo (port (get-ports node))
                ;; A non-reversed eastern port has more outgoing than incoming edges (net flow <= 0).
                ;; A non-reversed western port has more incoming than outgoing edges (net flow >= 0).
                ;; If this port doesn't violate one of these conditions, not all of the node's ports
                ;; are reversed.
                (unless (or (and (eq (get-side port) port-side.east) (> (get-net-flow port) 0))
                            (and (eq (get-side port) port-side.west) (< (get-net-flow port) 0)))
                  (setf all-ports-reversed nil)
                  (return))
                ;; no LAST or LAST_SEPARATE allowed for the target of outgoing edges
                (jdo (e (get-outgoing-edges port))
                  (let ((lc (get-property (get-node (get-target e))
                                          layered-options.layering-layer-constraint)))
                    (when (or (eq lc layer-constraint.last) (eq lc layer-constraint.last-separate))
                      (setf all-ports-reversed nil)
                      (return))))
                ;; no FIRST or FIRST_SEPARATE allowed for the source of incoming edges
                (jdo (e (get-incoming-edges port))
                  (let ((lc (get-property (get-node (get-source e))
                                          layered-options.layering-layer-constraint)))
                    (when (or (eq lc layer-constraint.first) (eq lc layer-constraint.first-separate))
                      (setf all-ports-reversed nil)
                      (return)))))
              (when all-ports-reversed
                ;; Reverse all kinds of edges
                (reverse-edges this layered-graph node layer-constraint port-type.undefined))))))))

(defjmethod reverse-edges ((this edge-and-layer-constraint-edge-reverser) layered-graph node
                           node-layer-constraint target-port-type)
  ;; Iterate through the node's edges and reverse them, if necessary
  (jdo (port (l-graph-util.to-port-array (get-ports node)))
    ;; Only incoming edges
    (when (or (eq target-port-type port-type.input) (eq target-port-type port-type.undefined))
      (jdo (edge (l-graph-util.to-edge-array (get-outgoing-edges port)))
        ;; Reverse the edge if we're allowed to do so
        (when (can-reverse-outgoing-edge this node-layer-constraint edge)
          (j-reverse edge layered-graph t))))
    ;; Only outgoing edges
    (when (or (eq target-port-type port-type.output) (eq target-port-type port-type.undefined))
      (jdo (edge (l-graph-util.to-edge-array (get-incoming-edges port)))
        ;; Reverse the edge if we're allowed to do so
        (when (can-reverse-incoming-edge this node-layer-constraint edge)
          (j-reverse edge layered-graph t))))))

(defjmethod can-reverse-outgoing-edge ((this edge-and-layer-constraint-edge-reverser)
                                       source-node-layer-constraint edge)
  (block nil
    ;; If the edge is already reversed, we don't want to reverse it again
    (when (get-property edge layered-internal-properties.reversed)
      (return nil))
    (let ((target-node (get-node (get-target edge))))
      ;; If the node is supposed to be in the LAST layer and is connected to a label dummy, we
      ;; won't reverse it since that dummy has to be placed between the LAST and the
      ;; LAST_SEPARATE layer
      (when (and (eq source-node-layer-constraint layer-constraint.last)
                 (eq (get-type target-node) node-type.label))
        (return nil))
      ;; If the target is a node in the LAST_SEPARATE layer, we won't reverse it
      (when (eq (get-property target-node layered-options.layering-layer-constraint)
                layer-constraint.last-separate)
        (return nil))
      t)))

(defjmethod can-reverse-incoming-edge ((this edge-and-layer-constraint-edge-reverser)
                                       target-node-layer-constraint edge)
  (block nil
    ;; If the edge is already reversed, we don't want to reverse it again
    (when (get-property edge layered-internal-properties.reversed)
      (return nil))
    (let ((source-node (get-node (get-source edge))))
      ;; If the node is supposed to be in the FIRST layer and is connected to a label dummy, we
      ;; won't reverse it since that dummy has to be placed between the FIRST_SEPARATE and the
      ;; FIRST layer
      (when (and (eq target-node-layer-constraint layer-constraint.first)
                 (eq (get-type source-node) node-type.label))
        (return nil))
      ;; If the source is a node in the FIRST_SEPARATE layer, we won't reverse it
      (when (eq (get-property source-node layered-options.layering-layer-constraint)
                layer-constraint.first-separate)
        (return nil))
      t)))
