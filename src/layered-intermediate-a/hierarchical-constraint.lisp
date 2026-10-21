;;;; hierarchical-constraint.lisp -- HierarchicalPortConstraintProcessor.

(in-package #:elk)

;; Private helpers of this file are called before their defjmethod is compiled.
(define-jcalls :instance
  process
  process-eastern-and-western-port-dummies
  process-eastern-and-western-layer
  process-northern-and-southern-port-dummies
  is-northern-or-southern-dummy
  create-dummy)

;;; NodeComparator: nodes compared by their position values in ascending order.  Nodes that are
;;; not hierarchical port dummies are sorted to the bottom of a list.

(defun %hpc-node-compare (node1 node2)
  (let ((node-type1 (get-type node1))
        (node-pos1 (get-property node1 layered-internal-properties.port-ratio-or-position))
        (node-type2 (get-type node2))
        (node-pos2 (get-property node2 layered-internal-properties.port-ratio-or-position)))
    (cond ((not (eq node-type2 node-type.external-port)) -1)
          ((not (eq node-type1 node-type.external-port)) 1)
          ((= node-pos1 node-pos2) 0)
          ((< node-pos1 node-pos2) -1)
          (t 1))))

(defjconst hierarchical-port-constraint-processor.dummy-input-port 0)
(defjconst hierarchical-port-constraint-processor.dummy-output-port 1)

(defjmethod process ((this hierarchical-port-constraint-processor) layered-graph monitor)
  (begin monitor "Hierarchical port constraint processing" 1)
  (process-eastern-and-western-port-dummies this layered-graph)
  (process-northern-and-southern-port-dummies this layered-graph)
  (done monitor))

;;; ------------------------------------------------------------ East / West Hierarchical Port Dummies

(defjmethod process-eastern-and-western-port-dummies ((this hierarchical-port-constraint-processor)
                                                      layered-graph)
  "Process eastern and western hierarchical port dummies."
  ;; If the port constraints are not at least FIXED_ORDER, there's nothing to be done here
  (when (is-order-fixed (get-property layered-graph layered-options.port-constraints))
    (let ((layers (get-layers layered-graph)))
      ;; This affects the first and last layer
      (process-eastern-and-western-layer this (jl-get layers 0))
      (process-eastern-and-western-layer this (jl-get layers (1- (size layers)))))))

(defjmethod process-eastern-and-western-layer ((this hierarchical-port-constraint-processor) layer)
  "Process the eastern and western hierarchical port dummies present in the given layer."
  ;; Put the nodes into an array
  (let ((nodes (l-graph-util.to-node-array (get-nodes layer))))
    ;; Sort the array; hierarchical port dummies are at the top, sorted by
    ;; position or ratio in ascending order
    (jl-sort nodes #'%hpc-node-compare)
    ;; Insert in-layer successor constraints where appropriate
    (let ((last-hierarchical-dummy nil))
      (loop for node across nodes
            do (progn
                 (when (not (eq (get-type node) node-type.external-port))
                   ;; No hierarchical port dummy nodes any more
                   (return))
                 ;; Only process dummies created for eastern or western external ports
                 (let ((external-port-side (get-property node layered-internal-properties.ext-port-side)))
                   (when (or (eq external-port-side port-side.west)
                             (eq external-port-side port-side.east))
                     (when last-hierarchical-dummy
                       (add (get-property last-hierarchical-dummy
                                          layered-internal-properties.in-layer-successor-constraints)
                            node))
                     (setf last-hierarchical-dummy node))))))))

;;; ------------------------------------------------------------ North / South Hierarchical Port Dummies

(defjmethod process-northern-and-southern-port-dummies ((this hierarchical-port-constraint-processor)
                                                        layered-graph)
  "Process northern and southern hierarchical port dummies."
  (block north-south
  ;; If the port constraints are not at least FIXED_SIDE, there's nothing to do here
  (let ((port-constraints (get-property layered-graph layered-options.port-constraints)))
    (unless (is-side-fixed port-constraints)
      (return-from north-south nil)))
  (let* ((layers (get-layers layered-graph))
         (layer-count (size layers))
         ;; For each layer, we keep a map of dummy nodes created for a given original external port
         ;; dummy.  This lets us remember which new dummy node was created for which original
         ;; external port dummy in which layer.  We also keep a list of dummy nodes to be added to
         ;; each layer since iterating over the new dummy nodes in the map may mess up their order.
         ;; We keep enough space to hold layerCount + 2 instances of each data structure because we
         ;; might have to add a new first and last layer, even though that shouldn't normally happen
         (ext-port-to-dummy-node-map (new-jlist (+ layer-count 2)))
         (new-dummy-nodes (new-jlist (+ layer-count 2))))
    ;; Add maps and lists for a new first layer that might have to be created as well as for the
    ;; current first layer.  A map for the next layer is added on each iteration of the for loop
    (jl-add ext-port-to-dummy-node-map (new-jmap))
    (jl-add ext-port-to-dummy-node-map (new-jmap))
    (jl-add new-dummy-nodes (new-jlist))
    (jl-add new-dummy-nodes (new-jlist))
    ;; We remember each original external port dummy we encounter (they must be removed from the
    ;; layers later)
    (let ((original-external-port-dummies (new-jlist)))
      ;; Iterate through each layer
      (dotimes (curr-layer-idx layer-count)
        (let* ((current-layer (jl-get layers curr-layer-idx))
               ;; Dummy node maps and lists for the next and previous layer
               (prev-ext-port-to-dummy-nodes-map (jl-get ext-port-to-dummy-node-map curr-layer-idx))
               (next-ext-port-to-dummy-nodes-map (new-jmap))
               (prev-new-dummy-nodes (jl-get new-dummy-nodes curr-layer-idx))
               (next-new-dummy-nodes (new-jlist)))
          (jl-add ext-port-to-dummy-node-map next-ext-port-to-dummy-nodes-map)
          (jl-add new-dummy-nodes next-new-dummy-nodes)
          ;; Iterate through the layer's nodes, looking for normal nodes connected to
          ;; northern / southern hierarchical port dummies
          (jdo (current-node current-layer)
            (if (is-northern-or-southern-dummy this current-node)
                ;; It's a northern or southern external port dummy.  Schedule for removal and
                ;; move on to next node
                (jl-add original-external-port-dummies current-node)
                (progn
                  ;; Iterate over the node's incoming edges
                  (jdo (edge (get-incoming-edges current-node))
                    (let ((source-node (get-node (get-source edge))))
                      ;; Check if it's a northern / southern dummy node
                      (when (is-northern-or-southern-dummy this source-node)
                        ;; See if a dummy has already been created for the previous layer
                        (let ((prev-layer-dummy
                                (jm-get prev-ext-port-to-dummy-nodes-map
                                        (get-property source-node layered-internal-properties.origin))))
                          (when (null prev-layer-dummy)
                            ;; No.  Create one.
                            (setf prev-layer-dummy (create-dummy this layered-graph source-node))
                            (jm-put prev-ext-port-to-dummy-nodes-map
                                    (get-property source-node layered-internal-properties.origin)
                                    prev-layer-dummy)
                            (jl-add prev-new-dummy-nodes prev-layer-dummy))
                          ;; Reroute the edge
                          (set-source edge (jl-get (get-ports prev-layer-dummy)
                                                   hierarchical-port-constraint-processor.dummy-output-port))))))
                  ;; Iterate over the node's outgoing edges
                  (jdo (edge (get-outgoing-edges current-node))
                    (let ((target-node (get-node (get-target edge))))
                      ;; Check if it's a northern / southern dummy node
                      (when (is-northern-or-southern-dummy this target-node)
                        ;; See if a dummy has already been created for the next layer
                        (let ((next-layer-dummy
                                (jm-get next-ext-port-to-dummy-nodes-map
                                        (get-property target-node layered-internal-properties.origin))))
                          (when (null next-layer-dummy)
                            ;; No.  Create one.
                            (setf next-layer-dummy (create-dummy this layered-graph target-node))
                            (jm-put next-ext-port-to-dummy-nodes-map
                                    (get-property target-node layered-internal-properties.origin)
                                    next-layer-dummy)
                            (jl-add next-new-dummy-nodes next-layer-dummy))
                          ;; Reroute the edge
                          (set-target edge (jl-get (get-ports next-layer-dummy)
                                                   hierarchical-port-constraint-processor.dummy-input-port)))))))))))
      ;; Add the newly created dummy nodes
      (dotimes (i (size new-dummy-nodes))
        (let ((node-list (jl-get new-dummy-nodes i)))
          (unless (is-empty node-list)
            ;; Find the layer the dummy nodes should be added to
            (let ((layer nil))
              (cond ((= i 0)
                     ;; A new first layer must be created
                     (setf layer (new-layer layered-graph))
                     (jl-add-at layers 0 layer))
                    ((= i (1- (size ext-port-to-dummy-node-map)))
                     ;; A new layer layer must be created
                     (setf layer (new-layer layered-graph))
                     (jl-add layers layer))
                    (t
                     (setf layer (jl-get layers (1- i)))))
              (jdo (dummy node-list)
                (set-layer dummy layer))))))
      ;; Iterate through the hierarchical port dummies and remove them
      (jdo (original-dummy original-external-port-dummies)
        ;; Remove the original dummy; new dummy nodes have already been created for it
        (set-layer original-dummy nil))
      ;; Remember the original external port dummies in the graph
      (set-property layered-graph layered-internal-properties.ext-port-replaced-dummies
                    original-external-port-dummies)))))

(defjmethod is-northern-or-southern-dummy ((this hierarchical-port-constraint-processor) node)
  "Checks if the node represents a northern or southern external port."
  (let ((node-type (get-type node)))
    (if (eq node-type node-type.external-port)
        (let ((port-side (get-property node layered-internal-properties.ext-port-side)))
          (or (eq port-side port-side.north) (eq port-side port-side.south)))
        nil)))

(defjmethod create-dummy ((this hierarchical-port-constraint-processor) layered-graph original-dummy)
  "Creates a dummy for the given original dummy.  The dummy's ORIGIN property will point to the
original dummy.  This way, the original dummy can later be restored, and the newly created dummy
can be told apart from the original."
  (let ((new-dummy (new-l-node layered-graph)))
    (copy-properties new-dummy original-dummy)
    (set-property new-dummy layered-internal-properties.ext-port-replaced-dummy original-dummy)
    (set-property new-dummy layered-options.port-constraints port-constraints.fixed-pos)
    (set-property new-dummy layered-options.alignment core-alignment.center)
    (set-type new-dummy node-type.external-port)
    (let ((input-port (new-l-port))
          (output-port (new-l-port)))
      (set-node input-port new-dummy)
      (set-side input-port port-side.west)
      (set-node output-port new-dummy)
      (set-side output-port port-side.east))
    new-dummy))
