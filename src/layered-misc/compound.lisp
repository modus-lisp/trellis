;;;; compound.lisp -- org.eclipse.elk.alg.layered.compound
;;;;
;;;; Hierarchy handling: CompoundGraphPreprocessor splits every edge that crosses a
;;;; hierarchy boundary into dummy edge segments (recorded in the graph's
;;;; CROSS_HIERARCHY_MAP); CompoundGraphPostprocessor recombines the laid-out segments
;;;; into one bendpoint chain per original edge.

(in-package #:elk)

;;; ------------------------------------------------------------ helpers

;; Multimap<LEdge, CrossHierarchyEdge>: an EQ hash table from the original edge to
;; a jlist of CrossHierarchyEdge.  HashMultimap.keySet() is hash order in Java; here
;; it is the table's insertion order.
(defun %xh-put (map key value)
  (let ((entry (or (gethash key map)
                   (setf (gethash key map) (new-jlist)))))
    (jl-add entry value)
    value))

(defun %xh-get (map key)
  (or (gethash key map) (new-jlist)))

(defun %xh-clear-chain (chain)
  (jl-clear (.items chain)))

;;; ------------------------------------------------------------ CrossHierarchyEdge

(defjctor cross-hierarchy-edge (new-edge graph type)
  (setf (.new-edge this) new-edge
        (.graph this) graph
        (.type this) type))

(defjmethod to-string ((this cross-hierarchy-edge))
  (jstr (to-string (.type this)) ":" (to-string (.new-edge this))))

(defjmethod get-edge ((this cross-hierarchy-edge))
  (.new-edge this))

(defjmethod get-graph ((this cross-hierarchy-edge))
  (.graph this))

(defjmethod get-type ((this cross-hierarchy-edge))
  (.type this))

(defjmethod get-actual-source ((this cross-hierarchy-edge))
  (if (eq (get-type (get-node (get-source (.new-edge this)))) node-type.external-port)
      (get-property (get-node (get-source (.new-edge this))) layered-internal-properties.origin)
      (get-source (.new-edge this))))

(defjmethod get-actual-target ((this cross-hierarchy-edge))
  (if (eq (get-type (get-node (get-target (.new-edge this)))) node-type.external-port)
      (get-property (get-node (get-target (.new-edge this))) layered-internal-properties.origin)
      (get-target (.new-edge this))))

;;; ------------------------------------------------------------ CrossHierarchyEdgeComparator

(defjctor cross-hierarchy-edge-comparator (graph)
  (setf (.graph this) graph))

(defjmethod compare ((this cross-hierarchy-edge-comparator) edge1 edge2)
  (cond
    ((and (eq (get-type edge1) port-type.output)
          (eq (get-type edge2) port-type.input))
     -1)
    ((and (eq (get-type edge1) port-type.input)
          (eq (get-type edge2) port-type.output))
     1)
    (t
     (let ((level1 (cross-hierarchy-level (get-graph edge1) (.graph this)))
           (level2 (cross-hierarchy-level (get-graph edge2) (.graph this))))
       (if (eq (get-type edge1) port-type.output)
           ;; from deeper level to higher level
           (- level2 level1)
           ;; from higher level to deeper level
           (- level1 level2))))))

(defun cross-hierarchy-level (nested-graph top-level-graph)
  (let ((current-graph nested-graph)
        (level 0))
    (loop
      (when (eq current-graph top-level-graph)
        (return level))
      (let ((current-node (get-parent-node current-graph)))
        (when (null current-node)
          ;; the given node is not an ancestor of the graph node
          (error "IllegalArgumentException: not an ancestor"))
        (setf current-graph (get-graph current-node))
        (incf level)))))

;;; ------------------------------------------------------------ CompoundGraphPreprocessor

(defjmethod process ((this compound-graph-preprocessor) graph monitor)
  (begin monitor "Compound graph preprocessor" 1)
  ;; This can be a HashMultimap since the values are sorted prior to working with them
  (setf (.cross-hierarchy-map this) (new-jmap))
  ;; create new dummy edges at hierarchy bounds and move the labels around accordingly
  (transform-hierarchy-edges this graph nil)
  (move-labels-and-remove-original-edges this graph)
  (set-sides-of-ports-to-sides-of-dummy-nodes this)
  ;; Attach cross hierarchy map to the graph and cleanup
  (set-property graph layered-internal-properties.cross-hierarchy-map (.cross-hierarchy-map this))
  (setf (.cross-hierarchy-map this) nil)
  (clrhash (.dummy-node-map this))
  (done monitor))

(defjmethod set-sides-of-ports-to-sides-of-dummy-nodes ((this compound-graph-preprocessor))
  (jdo-map (external-port dummy-node (.dummy-node-map this))
    (set-property dummy-node layered-internal-properties.origin external-port)
    (set-property external-port layered-internal-properties.port-dummy dummy-node)
    (set-property external-port layered-internal-properties.inside-connections t)
    (set-side external-port (get-property dummy-node layered-internal-properties.ext-port-side))
    (set-property (get-node external-port) layered-options.port-constraints port-constraints.fixed-side)
    (add (get-property (get-graph (get-node external-port)) layered-internal-properties.graph-properties)
         layered-graph-properties.non-free-ports)))

(defjmethod transform-hierarchy-edges ((this compound-graph-preprocessor) graph parent-node)
  ;; process all children and recurse down to gather their external ports
  (let ((contained-external-ports (new-jlist)))
    (jdo (node (get-layerless-nodes graph))
      (let ((nested-graph (get-nested-graph node)))
        (when nested-graph
          ;; recursively process the child graph
          (let ((child-ports (transform-hierarchy-edges this nested-graph node)))
            (jdo (p child-ports) (jl-add contained-external-ports p)))
          ;; process inside self loops
          (process-inside-self-loops this nested-graph node)
          ;; make sure that all hierarchical ports have had dummy nodes created for them (some
          ;; will already have been created, but perhaps not all)
          (when (contains (get-property nested-graph layered-internal-properties.graph-properties)
                          layered-graph-properties.external-ports)
            ;; We need the port constraints to position external ports (Issues KIPRA-1528, ELK-48)
            (let ((port-constraints (get-property node layered-options.port-constraints))
                  (inside-port-labels
                    (contains (get-property node layered-options.port-labels-placement)
                              port-label-placement.inside)))
              (jdo (port (get-ports node))
                ;; Make sure that every port has a dummy node created for it
                (let ((dummy-node (gethash port (.dummy-node-map this))))
                  (when (null dummy-node)
                    (setf dummy-node
                          (l-graph-util.create-external-port-dummy
                           port port-constraints (get-side port) (- (get-net-flow port))
                           nil (new-k-vector/0) (get-size port)
                           (get-property nested-graph core-options.direction) nested-graph))
                    (set-property dummy-node layered-internal-properties.origin port)
                    (setf (gethash port (.dummy-node-map this)) dummy-node)
                    (jl-add (get-layerless-nodes nested-graph) dummy-node))
                  ;; We need to reserve space for external port labels by adding those labels to our dummy nodes
                  (let ((dummy-node-port (jl-get (get-ports dummy-node) 0)))
                    (jdo (ext-port-label (get-labels port))
                      (let ((dummy-port-label (new-l-label/0)))
                        (setf (.x (get-size dummy-port-label)) (.x (get-size ext-port-label))
                              (.y (get-size dummy-port-label)) (.y (get-size ext-port-label)))
                        (jl-add (get-labels dummy-node-port) dummy-port-label)
                        ;; If port labels are placed outside, modify the size. At this point, the port's side
                        ;; may not be known yet if port constraints are free. If they are, however, we know that
                        ;; the port will end up on either the east or west side. (see #596).
                        ;; But if the port labels are fixed, we should consider the part that is inside the node.
                        (unless inside-port-labels
                          (let ((side (get-side port))
                                (inside-part 0d0))
                            (when (port-label-placement.is-fixed
                                   (get-property node layered-options.port-labels-placement))
                              ;; We use 0 as port border offset here, as we only want the label part that is
                              ;; inside the node "after" the port.
                              (setf inside-part
                                    (elk-util.compute-inside-part (get-position ext-port-label)
                                                                  (get-size ext-port-label)
                                                                  (get-size port) 0d0 side)))
                            (if (or (eq port-constraints port-constraints.free)
                                    (contains port-side.sides-east-west side))
                                (setf (.x (get-size dummy-port-label)) inside-part)
                                (setf (.y (get-size dummy-port-label)) inside-part))))))))))))))
    ;; this will be the list of external ports we will export
    (let ((exported-external-ports (new-jlist)))
      ;; process the cross-hierarchy edges connected to the inside of the child nodes
      (process-inner-hierarchical-edge-segments this graph parent-node
                                                contained-external-ports exported-external-ports)
      ;; process the cross-hierarchy edges connected to the outside of the parent node
      (when parent-node
        (process-outer-hierarchical-edge-segments this graph parent-node exported-external-ports))
      exported-external-ports)))

(defjmethod move-labels-and-remove-original-edges ((this compound-graph-preprocessor) graph)
  ;; move all labels of the original edges to the appropriate dummy edges and remove the original
  ;; edges from the graph
  (jdo-map (orig-edge cross-edges (.cross-hierarchy-map this))
    ;; if the original edge had any labels, we need to move them to the newly introduced edge
    ;; segments
    (when (> (length (get-labels orig-edge)) 0)
      ;; retrieve and sort the edge segments introduced for the original edge
      (let ((edge-segments (jlist-from cross-edges)))
        (jl-sort edge-segments (new-cross-hierarchy-edge-comparator/1 graph))
        ;; iterate over the labels and move them to the edge segments
        (jdo (curr-label (jlist-from (get-labels orig-edge)))
          ;; find the index of the dummy edge we will move the label to
          (let* ((placement (get-property curr-label layered-options.edge-labels-placement))
                 (target-dummy-edge-index
                   (cond ((eq placement edge-labels-placement.head)
                          (1- (length edge-segments)))
                         ((eq placement edge-labels-placement.center)
                          (get-shallowest-edge-segment this edge-segments))
                         ((eq placement edge-labels-placement.tail)
                          0)
                         ;; we have no idea what to do with the label, so ignore it
                         (t -1))))
            ;; move the label if we were lucky enough to find a new home for it
            (unless (= target-dummy-edge-index -1)
              (let* ((target-segment (jl-get edge-segments target-dummy-edge-index))
                     (target-edge (get-edge target-segment))
                     (target-graph (get-graph (get-node (get-source target-edge))))
                     (graph-properties (get-property target-graph layered-internal-properties.graph-properties)))
                (jl-add (get-labels target-edge) curr-label)
                (add graph-properties layered-graph-properties.end-labels)
                (add graph-properties layered-graph-properties.center-labels)
                (jl-remove (get-labels orig-edge) curr-label)
                (set-property curr-label layered-internal-properties.original-label-edge orig-edge)))))))
    ;; remove original edge
    (set-source orig-edge nil)
    (set-target orig-edge nil)))

(defjmethod get-shallowest-edge-segment ((this compound-graph-preprocessor) edge-segments)
  (let ((result -1)
        (index 0))
    (dolist (cross-edge (coerce edge-segments 'list))
      (cond
        ((eq (get-type cross-edge) port-type.input)
         ;; We have an edge pointing downwards here.
         ;; If this is the first segment we take this element.
         ;; If there was a segement before, we take that one
         (setf result (if (= index 0) 0 (1- index)))
         (return))
        ((= index (1- (length edge-segments)))
         ;; If this is the last segment and we didn't find a descending edge,
         ;; This edge is always ascending. Place the label at the last
         ;; segment here.
         (setf result index)))
      (incf index))
    result))

;;; ------------------------------------------------------------ Inner Hierarchical Edge Segment Processing

(defjmethod process-inner-hierarchical-edge-segments ((this compound-graph-preprocessor) graph parent-node
                                                      contained-external-ports exported-external-ports)
  ;; we remember the ports and the dummy nodes we create to add them to the graph afterwards
  ;; (this is not strictly necessary, but allows us to reuse methods we also use for outer
  ;; hierarchy edge segments)
  (let ((created-external-ports (new-jlist)))
    ;; iterate over the list of contained external ports
    (jdo (external-port contained-external-ports)
      (let ((current-external-port nil))
        ;; we process output ports and input ports a bit differently
        (if (eq (.type external-port) port-type.output)
            ;; iterate over the port's original edges
            (jdo (out-edge (.orig-edges external-port))
              (let ((target-node (get-node (get-target out-edge))))
                (cond
                  ((eq (get-graph target-node) graph)
                   ;; case 1: edge connects to a direct chlid
                   (connect-child this graph external-port out-edge (.dummy-port external-port)
                                  (get-target out-edge)))
                  ((or (null parent-node) (l-graph-util.is-descendant target-node parent-node))
                   ;; case 2: edge connects two direct children
                   (connect-siblings this graph external-port contained-external-ports out-edge))
                  (t
                   ;; case 3: edge connects to parent node or to the outside world
                   (let ((new-external-port
                           (introduce-hierarchical-edge-segment this graph parent-node out-edge
                                                                (.dummy-port external-port)
                                                                port-type.output current-external-port)))
                     (unless (eq new-external-port current-external-port)
                       (jl-add created-external-ports new-external-port))
                     ;; the port is our new current external port if it is exported
                     (when (.exported new-external-port)
                       (setf current-external-port new-external-port)))))))
            ;; iterate over the port's original edges
            (jdo (in-edge (.orig-edges external-port))
              (let ((source-node (get-node (get-source in-edge))))
                (cond
                  ((eq (get-graph source-node) graph)
                   ;; case 1: edge comes from a direct child
                   (connect-child this graph external-port in-edge (get-source in-edge)
                                  (.dummy-port external-port)))
                  ((or (null parent-node) (l-graph-util.is-descendant source-node parent-node))
                   ;; case 2: edge connects two direct children; this case is handled in the code
                   ;;         for output ports above, so there's nothing to do here
                   nil)
                  (t
                   ;; case 3: edge comes from the parent node or from the outside
                   (let ((new-external-port
                           (introduce-hierarchical-edge-segment this graph parent-node in-edge
                                                                (.dummy-port external-port)
                                                                port-type.input current-external-port)))
                     (unless (eq new-external-port current-external-port)
                       (jl-add created-external-ports new-external-port))
                     (when (.exported new-external-port)
                       (setf current-external-port new-external-port))))))))))
    ;; add dummy nodes and exported external ports
    (add-created-external-ports graph created-external-ports exported-external-ports)))

(defun add-created-external-ports (graph created-external-ports exported-external-ports)
  (jdo (external-port created-external-ports)
    (unless (jl-contains (get-layerless-nodes graph) (.dummy-node external-port))
      (jl-add (get-layerless-nodes graph) (.dummy-node external-port)))
    (when (.exported external-port)
      (jl-add exported-external-ports external-port))))

(defjmethod connect-child ((this compound-graph-preprocessor) graph external-port orig-edge
                           source-port target-port)
  ;; add new dummy edge and connect properly
  (let ((dummy-edge (create-dummy-edge this graph orig-edge)))
    (set-source dummy-edge source-port)
    (set-target dummy-edge target-port)
    (%xh-put (.cross-hierarchy-map this) orig-edge
             (new-cross-hierarchy-edge/3 dummy-edge graph (.type external-port)))))

(defjmethod connect-siblings ((this compound-graph-preprocessor) graph external-output-port
                              contained-external-ports orig-edge)
  ;; find the opposite external port
  (let ((target-external-port
          (find-if (lambda (external-port2)
                     (and (not (eq external-port2 external-output-port))
                          (jl-contains (.orig-edges external-port2) orig-edge)))
                   contained-external-ports)))
    ;; add new dummy edge and connect properly
    (let ((dummy-edge (create-dummy-edge this graph orig-edge)))
      (set-source dummy-edge (.dummy-port external-output-port))
      (set-target dummy-edge (.dummy-port target-external-port))
      (%xh-put (.cross-hierarchy-map this) orig-edge
               (new-cross-hierarchy-edge/3 dummy-edge graph (.type external-output-port))))))

;;; ------------------------------------------------------------ Outer Hierarchical Edge Segment Processing

(defjmethod process-outer-hierarchical-edge-segments ((this compound-graph-preprocessor) graph parent-node
                                                      exported-external-ports)
  ;; we need to remember the ports and the dummy nodes we create to add them to the graph
  ;; afterwards (to avoid concurrent modification exceptions)
  (let ((created-external-ports (new-jlist)))
    ;; iterate over all ports of the graph's child nodes
    (jdo (child-node (get-layerless-nodes graph))
      (jdo (child-port (get-ports child-node))
        ;; we treat outgoing and incoming edges separately
        (let ((current-external-output-port nil))
          (jdo (out-edge (l-graph-util.to-edge-array (get-outgoing-edges child-port)))
            (unless (l-graph-util.is-descendant (get-node (get-target out-edge)) parent-node)
              ;; the edge goes to the outside or to the parent node itself, so create an
              ;; external port if necessary and introduce a new dummy edge
              (let ((new-external-port
                      (introduce-hierarchical-edge-segment this graph parent-node out-edge
                                                           (get-source out-edge) port-type.output
                                                           current-external-output-port)))
                (unless (eq new-external-port current-external-output-port)
                  (jl-add created-external-ports new-external-port))
                ;; the port is our new current external port if it is exported
                (when (.exported new-external-port)
                  (setf current-external-output-port new-external-port)))))
          (let ((current-external-input-port nil))
            (jdo (in-edge (l-graph-util.to-edge-array (get-incoming-edges child-port)))
              (unless (l-graph-util.is-descendant (get-node (get-source in-edge)) parent-node)
                ;; the edge comes from the outside or from the parent node itself, so create an
                ;; external port if necessary and introduce a new dummy edge
                (let ((new-external-port
                        (introduce-hierarchical-edge-segment this graph parent-node in-edge
                                                             (get-target in-edge) port-type.input
                                                             current-external-input-port)))
                  (unless (eq new-external-port current-external-input-port)
                    (jl-add created-external-ports new-external-port))
                  (when (.exported new-external-port)
                    (setf current-external-input-port new-external-port)))))))))
    ;; add dummy nodes and exported external ports
    (add-created-external-ports graph created-external-ports exported-external-ports)))

;;; ------------------------------------------------------------ Inside Self Loop Processing

(defjmethod process-inside-self-loops ((this compound-graph-preprocessor) nested-graph node)
  ;; Check if inside self loops are enabled for the node
  (when (get-property node layered-options.inside-self-loops-activate)
    ;; Iterate over the edges and look for an inside self loop
    (jdo (lport (get-ports node))
      ;; Avoid ConcurrentModificationExceptions
      (jdo (out-edge (copy-seq (l-graph-util.to-edge-array (get-outgoing-edges lport))))
        (let* ((is-self-loop (eq (get-node (get-target out-edge)) node))
               (is-inside-self-loop (and is-self-loop
                                         (get-property out-edge layered-options.inside-self-loops-yo))))
          (when is-inside-self-loop
            ;; Check if the ports have already been transformed into external port dummies
            (let* ((source-port (get-source out-edge))
                   (source-ext-port-dummy (gethash source-port (.dummy-node-map this))))
              (when (null source-ext-port-dummy)
                (setf source-ext-port-dummy
                      (l-graph-util.create-external-port-dummy
                       source-port port-constraints.free (get-side source-port) -1 nil nil
                       (get-size source-port)
                       (get-property nested-graph core-options.direction) nested-graph))
                (set-property source-ext-port-dummy layered-internal-properties.origin source-port)
                (setf (gethash source-port (.dummy-node-map this)) source-ext-port-dummy)
                (jl-add (get-layerless-nodes nested-graph) source-ext-port-dummy))
              (let* ((target-port (get-target out-edge))
                     (target-ext-port-dummy (gethash target-port (.dummy-node-map this))))
                (when (null target-ext-port-dummy)
                  (setf target-ext-port-dummy
                        (l-graph-util.create-external-port-dummy
                         target-port port-constraints.free (get-side target-port) 1 nil nil
                         (get-size target-port)
                         (get-property nested-graph core-options.direction) nested-graph))
                  (set-property target-ext-port-dummy layered-internal-properties.origin target-port)
                  (setf (gethash target-port (.dummy-node-map this)) target-ext-port-dummy)
                  (jl-add (get-layerless-nodes nested-graph) target-ext-port-dummy))
                ;; Create a new dummy edge
                (let ((dummy-edge (create-dummy-edge this nested-graph out-edge)))
                  (set-source dummy-edge (jl-get (get-ports source-ext-port-dummy) 0))
                  (set-target dummy-edge (jl-get (get-ports target-ext-port-dummy) 0))
                  ;; Remember the new edge
                  (%xh-put (.cross-hierarchy-map this) out-edge
                           (new-cross-hierarchy-edge/3 dummy-edge nested-graph port-type.output))
                  (add (get-property nested-graph layered-internal-properties.graph-properties)
                       layered-graph-properties.external-ports))))))))))

;;; ------------------------------------------------------------ General Hierarchical Edge Segment Processing

(defjmethod introduce-hierarchical-edge-segment ((this compound-graph-preprocessor) graph parent-node
                                                 orig-edge opposite-port port-type default-external-port)
  ;; check if external ports are to be merged
  (let ((merge-external-ports (get-property graph layered-options.merge-hierarchy-edges))
        ;; check if the edge connects to the parent node instead of to the outside world; if so, the
        ;; parent-end-port will be non-null
        (parent-end-port
          (cond ((and (eq port-type port-type.input)
                      (eq (get-node (get-source orig-edge)) parent-node))
                 (get-source orig-edge))
                ((and (eq port-type port-type.output)
                      (eq (get-node (get-target orig-edge)) parent-node))
                 (get-target orig-edge))
                (t nil)))
        (external-port default-external-port))
    ;; only create a new external port if the current one is null or if ports are not to be merged
    ;; or if the connection actually ends at the parent node
    (if (or (null external-port) (not merge-external-ports) parent-end-port)
        (let ((external-port-side port-side.undefined))
          (if parent-end-port
              (setf external-port-side (get-side parent-end-port))
              ;; We try to infer the port side from the port type if its node has its port constraints
              ;; set to at least FIXED_SIDE; this may produce strange effects, so the safest thing is
              ;; for people to set compound node port constraints to FREE
              (when (is-side-fixed (get-property parent-node layered-options.port-constraints))
                (setf external-port-side (if (eq port-type port-type.input)
                                             port-side.west
                                             port-side.east))))
          ;; create a dummy node that will represent the external port
          (let* ((dummy-node (create-external-port-dummy this graph parent-node port-type
                                                         external-port-side orig-edge))
                 ;; create a dummy edge to be connected to the port
                 (dummy-edge (create-dummy-edge this (get-graph parent-node) orig-edge)))
            (if (eq port-type port-type.input)
                (progn
                  ;; if the external port is an input port, the source of the edge must be connected to
                  ;; the new dummy node
                  (set-source dummy-edge (jl-get (get-ports dummy-node) 0))
                  (set-target dummy-edge opposite-port))
                (progn
                  ;; if the external port is an output port, the target of the edge must be connected to
                  ;; the new dummy node
                  (set-source dummy-edge opposite-port)
                  (set-target dummy-edge (jl-get (get-ports dummy-node) 0))))
            ;; create the external port (the port is to be exported if the connection is not just to the
            ;; parent node)
            (setf external-port
                  (new-external-port orig-edge dummy-edge dummy-node
                                     (get-property dummy-node layered-internal-properties.origin)
                                     port-type (null parent-end-port)))))
        (progn
          ;; we use an existing external port, so simply add the original edge to its list of
          ;; original edges
          (jl-add (.orig-edges external-port) orig-edge)
          ;; merge the properties of the original edges
          (let ((thickness (math.max (get-property (.new-edge external-port) layered-options.edge-thickness)
                                     (get-property orig-edge layered-options.edge-thickness))))
            (set-property (.new-edge external-port) layered-options.edge-thickness thickness))))
    (%xh-put (.cross-hierarchy-map this) orig-edge
             (new-cross-hierarchy-edge/3 (.new-edge external-port) graph port-type))
    external-port))

(defjmethod create-dummy-edge ((this compound-graph-preprocessor) graph orig-edge)
  (let ((dummy-edge (new-l-edge/0)))
    (copy-properties dummy-edge orig-edge)
    (set-property dummy-edge layered-options.junction-points nil)
    dummy-edge))

(defjmethod calculate-net-flow ((this compound-graph-preprocessor) port)
  (let* ((node (get-node port))
         (inside-self-loops-enabled (get-property node layered-options.inside-self-loops-activate))
         (output-port-vote 0)
         (input-port-vote 0))
    ;; Iterate over outgoing edges
    (jdo (outgoing-edge (get-outgoing-edges port))
      (let* ((is-self-loop (is-self-loop outgoing-edge))
             (is-inside-self-loop (and is-self-loop inside-self-loops-enabled
                                       (get-property outgoing-edge layered-options.inside-self-loops-yo)))
             (target-node (get-node (get-target outgoing-edge))))
        (cond ((and is-self-loop is-inside-self-loop) (incf input-port-vote))
              ((and is-self-loop (not is-inside-self-loop)) (incf output-port-vote))
              ((eq (get-parent-node (get-graph target-node)) node) (incf input-port-vote))
              (t (incf output-port-vote)))))
    ;; Iterate over incoming edges
    (jdo (incoming-edge (get-incoming-edges port))
      (let* ((is-self-loop (is-self-loop incoming-edge))
             (is-inside-self-loop (and is-self-loop inside-self-loops-enabled
                                       (get-property incoming-edge layered-options.inside-self-loops-yo)))
             (source-node (get-node (get-source incoming-edge))))
        (cond ((and is-self-loop is-inside-self-loop) (incf output-port-vote))
              ((and is-self-loop (not is-inside-self-loop)) (incf input-port-vote))
              ((eq (get-parent-node (get-graph source-node)) node) (incf output-port-vote))
              (t (incf input-port-vote)))))
    (- output-port-vote input-port-vote)))

(defjmethod create-external-port-dummy ((this compound-graph-preprocessor) graph parent-node
                                        port-type port-side edge)
  (let* ((dummy-node nil)
         ;; find the port on the outside of its parent node that the edge connects to
         (outside-port (if (eq port-type port-type.input) (get-source edge) (get-target edge)))
         (layout-direction (l-graph-util.get-direction graph)))
    ;; check if the edge connects to the parent node or to something way outside...
    (if (eq (get-node outside-port) parent-node)
        (progn
          ;; we need to check if a dummy node has already been created for the port
          (setf dummy-node (gethash outside-port (.dummy-node-map this)))
          (when (null dummy-node)
            ;; Ticket #160 explains why we need to pass on a position here. While that works fine to determine port
            ;; orders, the exact port positions might yet need to be adjusted for some inside paddings
            (setf dummy-node
                  (l-graph-util.create-external-port-dummy
                   outside-port
                   (get-property parent-node layered-options.port-constraints)
                   port-side
                   (calculate-net-flow this outside-port)
                   nil
                   (get-position outside-port)
                   (get-size outside-port)
                   layout-direction
                   graph))
            (set-property dummy-node layered-internal-properties.origin outside-port)
            (setf (gethash outside-port (.dummy-node-map this)) dummy-node)))
        (progn
          ;; We create a new dummy node in any case, and since there is no port yet we have to create one as well.
          ;; We do need to pass a position vector to keep an NPE from being thrown (#160), but it is questionable
          ;; whether this part of the code should actually be used with anything other than fixed port constraints.
          (setf dummy-node
                (l-graph-util.create-external-port-dummy
                 (compound-graph-preprocessor.create-external-port-properties graph)
                 (get-property parent-node layered-options.port-constraints)
                 port-side
                 (if (eq port-type port-type.input) -1 1)
                 nil
                 (new-k-vector/0)
                 ;; A dummy port is not considered to have a size (#766)
                 (new-k-vector/2 0d0 0d0)
                 layout-direction
                 graph))
          (let ((dummy-port (create-port-for-dummy this dummy-node parent-node port-type)))
            (set-property dummy-node layered-internal-properties.origin dummy-port)
            (setf (gethash dummy-port (.dummy-node-map this)) dummy-node))))
    ;; set a few graph properties
    (add (get-property graph layered-internal-properties.graph-properties)
         layered-graph-properties.external-ports)
    (if (is-side-fixed (get-property graph layered-options.port-constraints))
        (set-property graph layered-options.port-constraints port-constraints.fixed-side)
        (set-property graph layered-options.port-constraints port-constraints.free))
    dummy-node))

(defjstatic compound-graph-preprocessor.create-external-port-properties (graph)
  (let ((property-holder (new-map-property-holder/0)))
    ;; FIXME No idea why ...
    (let ((offset (/ (get-property graph layered-options.spacing-edge-edge) 2d0)))
      (set-property property-holder layered-options.port-border-offset offset))
    property-holder))

(defjmethod create-port-for-dummy ((this compound-graph-preprocessor) dummy-node parent-node port-type)
  (let* ((graph (get-graph parent-node))
         (layout-direction (l-graph-util.get-direction graph))
         (port (new-l-port/0)))
    (set-node port parent-node)
    (cond
      ((eq port-type port-type.input)
       (set-side port (opposed (port-side.from-direction layout-direction))))
      ((eq port-type port-type.output)
       (set-side port (port-side.from-direction layout-direction))))
    (set-property port layered-options.port-border-offset
                  (get-property dummy-node layered-options.port-border-offset))
    port))

;;; ------------------------------------------------------------ ExternalPort

(defjctor external-port (orig-edge new-edge dummy-node dummy-port port-type exported)
  (jl-add (.orig-edges this) orig-edge)
  (setf (.new-edge this) new-edge
        (.dummy-node this) dummy-node
        (.dummy-port this) dummy-port
        (.type this) port-type
        (.exported this) exported))

;;; ------------------------------------------------------------ CompoundGraphPostprocessor

(defjmethod process ((this compound-graph-postprocessor) graph monitor)
  (begin monitor "Compound graph postprocessor" 1)
  ;; whether bend points should be added whenever crossing a hierarchy boundary
  (let ((add-unnecessary-bendpoints (get-property graph layered-options.unnecessary-bendpoints))
        ;; restore the cross-hierarchy map that was built by the preprocessor
        (cross-hierarchy-map (get-property graph layered-internal-properties.cross-hierarchy-map))
        ;; remember all dummy edges we encounter; these need to be removed at the end
        (dummy-edges (new-jset)))
    ;; iterate over all original edges
    (jdo-map (orig-edge cross-edges cross-hierarchy-map)
      ;; find all cross-hierarchy edges the original edge was split into, and sort them from source to target
      (let ((cross-hierarchy-edges (jlist-from cross-edges)))
        (jl-sort cross-hierarchy-edges (new-cross-hierarchy-edge-comparator/1 graph))
        ;; find the original source and target ports for the original edge
        (let* ((source-port (get-actual-source (jl-get cross-hierarchy-edges 0)))
               (target-port (get-actual-target (jl-get cross-hierarchy-edges
                                                       (1- (length cross-hierarchy-edges)))))
               ;; determine the reference graph for all bend points
               (reference-node (get-node source-port))
               (reference-graph
                 (if (l-graph-util.is-descendant (get-node target-port) reference-node)
                     (get-nested-graph reference-node)
                     (get-graph reference-node)))
               ;; check whether there are any junction points
               (junction-points (compound-graph-postprocessor.clear-junction-points orig-edge cross-hierarchy-edges))
               (last-point nil))
          ;; reset bend points (we have computed new ones anyway)
          (%xh-clear-chain (get-bend-points orig-edge))
          ;; apply the computed layouts to the cross-hierarchy edge
          (jdo (ch-edge cross-hierarchy-edges)
            ;; transform all coordinates from the graph of the dummy edge to the reference graph
            (let* ((offset (new-k-vector/0))
                   (ledge (get-edge ch-edge)))
              (l-graph-util.change-coord-system offset (get-graph ch-edge) reference-graph)
              (let ((bend-points (new-k-vector-chain/0)))
                (add-all-as-copies bend-points 0 (get-bend-points ledge))
                (offset bend-points offset)
                ;; Note: if an NPE occurs here, that means ELK Layered has replaced the original edge
                (let ((source-point (new-k-vector/1 (get-absolute-anchor (get-source ledge))))
                      (target-point (new-k-vector/1 (get-absolute-anchor (get-target ledge)))))
                  (add source-point offset)
                  (add target-point offset)
                  (when last-point
                    (let ((next-point (if (is-empty bend-points)
                                          target-point
                                          (get-first bend-points))))
                      ;; we add the source point as a bend point to properly connect the hierarchy levels
                      ;; either if the last point of the previous hierarchy edge segment is a certain
                      ;; level of tolerance away or if we are required to add unnecessary bend points
                      (let ((x-diff-enough (> (abs (- (.x last-point) (.x next-point)))
                                              orthogonal-routing-generator.tolerance))
                            (y-diff-enough (> (abs (- (.y last-point) (.y next-point)))
                                              orthogonal-routing-generator.tolerance)))
                        (when (or (and (not add-unnecessary-bendpoints) x-diff-enough y-diff-enough)
                                  (and add-unnecessary-bendpoints (or x-diff-enough y-diff-enough)))
                          (add (get-bend-points orig-edge) source-point)))))
                  (add-all (get-bend-points orig-edge) bend-points)
                  (setf last-point (if (is-empty bend-points)
                                       source-point
                                       (get-last bend-points)))
                  ;; copy junction points
                  (compound-graph-postprocessor.copy-junction-points ledge junction-points offset)
                  ;; add offset to target port with a special property
                  (when (eq (get-actual-target ch-edge) target-port)
                    (when (not (eq (get-graph (get-node target-port)) (get-graph ch-edge)))
                      ;; the target port is in a different coordinate system -- recompute the offset
                      (setf offset (new-k-vector/0))
                      (l-graph-util.change-coord-system offset (get-graph (get-node target-port))
                                                        reference-graph))
                    (set-property orig-edge layered-internal-properties.target-offset offset))
                  ;; copy labels back to the original edge
                  (compound-graph-postprocessor.copy-labels-back ledge orig-edge reference-graph)
                  ;; remember the dummy edge for later removal (dummy edges may be in use by several
                  ;; different original edges, which is why we cannot just go and remove it now)
                  (js-add dummy-edges ledge)))))
          ;; restore the original source port and target port
          (set-source orig-edge source-port)
          (set-target orig-edge target-port))))
    ;; remove the dummy edges from the graph (dummy ports and dummy nodes are retained)
    (jdo (dummy-edge dummy-edges)
      (set-source dummy-edge nil)
      (set-target dummy-edge nil))
    (done monitor)))

(defjstatic compound-graph-postprocessor.clear-junction-points (orig-edge cross-hierarchy-edges)
  (let ((junction-points (get-property orig-edge layered-options.junction-points)))
    (if (some (lambda (ch-edge)
                (let ((jps (get-property (get-edge ch-edge) layered-options.junction-points)))
                  (and jps (not (is-empty jps)))))
              cross-hierarchy-edges)
        ;; if so, make sure the original edge has an empty non-null junction point list
        (if (null junction-points)
            (progn
              (setf junction-points (new-k-vector-chain/0))
              (set-property orig-edge layered-options.junction-points junction-points))
            (%xh-clear-chain junction-points))
        (when junction-points
          (set-property orig-edge layered-options.junction-points nil)))
    junction-points))

(defjstatic compound-graph-postprocessor.copy-junction-points (source target offset)
  (let ((ledge-jps (get-property source layered-options.junction-points)))
    (when ledge-jps
      (let ((jp-copies (new-k-vector-chain/0)))
        (add-all-as-copies jp-copies 0 ledge-jps)
        (offset jp-copies offset)
        (add-all target jp-copies)))))

(defjstatic compound-graph-postprocessor.copy-labels-back (hierarchy-segment orig-edge reference-graph)
  (jdo (curr-label (jlist-from (get-labels hierarchy-segment)))
    (when (eq (get-property curr-label layered-internal-properties.original-label-edge) orig-edge)
      (l-graph-util.change-coord-system
       (get-position curr-label)
       (get-graph (get-node (get-source hierarchy-segment)))
       reference-graph)
      (jl-remove (get-labels hierarchy-segment) curr-label)
      (jl-add (get-labels orig-edge) curr-label))))
