;;;; hierarchical-router.lisp -- HierarchicalPortOrthogonalEdgeRouter.

(in-package #:elk)

;; Private helpers of this file are called before their defjmethod is compiled.
(define-jcalls :instance
  process
  restore-north-south-dummies
  restore-dummy
  connect-node-to-dummy
  set-north-south-dummy-coordinates
  calculate-north-south-dummy-positions
  apply-north-south-dummy-ratio
  apply-north-south-dummy-position
  ensure-unique-positions
  restore-proper-order
  assign-ascending-coordinates
  route-edges
  remove-temporary-north-south-dummies
  fix-coordinates
  correct-slanted-edge-segments)

(defjmethod process ((this hierarchical-port-orthogonal-edge-router) layered-graph monitor)
  (begin monitor "Orthogonally routing hierarchical port edges" 1)
  (setf (.northern-ext-port-edge-routing-height this) 0.0d0)

  ;; Step 1
  ;; Restore any north / south port dummies removed by the HierarchicalPortConstraintProcessor
  ;; and connect them to the dummies created in their stead.
  (let ((north-south-dummies (restore-north-south-dummies this layered-graph)))
    ;; Step 2
    ;; Calculate coordinates for the north / south port dummies.  Coordinates for the
    ;; east / west port dummies have already been calculated prior to this processor's
    ;; execution.  The coordinates are relative to the node's content area, just like
    ;; normal node coordinates.  (the content area is the node size minus padding minus offset)
    (set-north-south-dummy-coordinates this layered-graph north-south-dummies)

    ;; Step 3
    ;; Orthogonal edge routing.
    (route-edges this monitor layered-graph north-south-dummies)

    ;; Step 4
    ;; Removal of the temporarily created north / south port dummies.
    (remove-temporary-north-south-dummies this layered-graph)

    ;; Step 5
    ;; Finally, the coordinates of east / west hierarchical port dummies have to be corrected
    ;; and set.  The x coordinate must be set, and if north / south port routing resulted
    ;; in a change of offset or graph size, the y coordinates have to be adjusted if port
    ;; constraints are at FIXED_RATIO or FIXED_POS.  The graph's width may also have to be
    ;; adjusted.
    (fix-coordinates this layered-graph)

    ;; Step 6
    ;; Fixing the dummy coordinates can easily lead to slanted edge segments, which needs
    ;; to be corrected.
    (correct-slanted-edge-segments this layered-graph))
  (done monitor))

;;; ------------------------------------------------------------ STEP 1: RESTORE NORTH / SOUTH DUMMIES

(defjmethod restore-north-south-dummies ((this hierarchical-port-orthogonal-edge-router) layered-graph)
  "Iterates through all layers, restoring hierarchical port dummy nodes along the way.  The
restored nodes are connected to the temporary dummy nodes created for them (or, as Carsten calls
them, the \"dummy dummies\").  The restored nodes are added to the last layer.  (which layer they
are added to doesn't make any difference)  Returns the list of restored external port dummies."
  (let ((restored-dummies (new-jlist)))
    (when (has-property layered-graph layered-internal-properties.ext-port-replaced-dummies)
    ;; Restore the original external port dummies
    (jdo (dummy (get-property layered-graph layered-internal-properties.ext-port-replaced-dummies))
      (restore-dummy this dummy layered-graph)
      (jl-add restored-dummies dummy))
    ;; Looking for hierarchical port dummies that replaced the restored hierarchical port dummy
    (jdo (layer (get-layers layered-graph))
      (jdo (node layer)
        (when (eq (get-type node) node-type.external-port)
          (let ((replaced-dummy (get-property node layered-internal-properties.ext-port-replaced-dummy)))
            (when replaced-dummy
              ;; Restore the origin and connect the node to it
              (connect-node-to-dummy this layered-graph node replaced-dummy))))))
    ;; Assign the restored dummies to the graph's last layer (this has nothing to do anymore
    ;; with where they'll be placed; we just need some layer to assign them to, and we simply
    ;; choose the last one)
    (jdo (dummy restored-dummies)
      (set-layer dummy (jl-last (get-layers layered-graph)))))
    restored-dummies))

(defjmethod restore-dummy ((this hierarchical-port-orthogonal-edge-router) dummy graph)
  "Restores the given dummy."
  ;; Depending on the hierarchical port's side, we set the port side of the dummy's ports
  ;; to be able to route properly (northern dummies must have a southern port)
  (let* ((port-side (get-property dummy layered-internal-properties.ext-port-side))
         (dummy-port (jl-get (get-ports dummy) 0)))
    (cond ((eq port-side port-side.north) (set-side dummy-port port-side.south))
          ((eq port-side port-side.south) (set-side dummy-port port-side.north))))
  ;; Since the dummy node was hidden from the algorithm, its port labels are not placed properly
  ;; and its margins are not set acoordingly.  That needs to be fixed (if port labels are to be
  ;; taken into consideration)
  (when (contains (get-property graph layered-options.node-size-constraints) size-constraint.port-labels)
    (let* ((dummy-port (jl-get (get-ports dummy) 0))
           ;; The ElkGraphImporter has set the relevant spacings on the dummy node
           (port-label-spacing-horizontal
             (get-property dummy layered-options.spacing-label-port-horizontal))
           (port-label-spacing-vertical
             (get-property dummy layered-options.spacing-label-port-vertical))
           (label-label-spacing (get-property dummy layered-options.spacing-label-label))
           (port-label-placement (get-property graph layered-options.port-labels-placement)))
      (cond ((contains port-label-placement port-label-placement.inside)
             (let ((current-y port-label-spacing-vertical)
                   (x-center-relative-to-port (- (/ (.x (get-size dummy)) 2.0d0)
                                                 (.x (get-position dummy-port)))))
               (jdo (label (get-labels dummy-port))
                 (setf (.y (get-position label)) current-y)
                 (setf (.x (get-position label))
                       (- x-center-relative-to-port (/ (.x (get-size label)) 2.0d0)))
                 (incf current-y (+ (.y (get-size label)) label-label-spacing)))))
            ((contains port-label-placement port-label-placement.outside)
             ;; Port labels have a vertical size of 0, but we need to set their x coordinate
             (jdo (label (get-labels dummy-port))
               (setf (.x (get-position label))
                     (- (+ port-label-spacing-horizontal (.x (get-size dummy)))
                        (.x (get-position dummy-port)))))))
      ;; Calculate margins
      (process-node (node-dimension-calculation.get-node-margin-calculator
                     (l-graph-adapters.adapt graph nil))
                    (l-graph-adapters.adapt dummy nil)))))

(defjmethod connect-node-to-dummy ((this hierarchical-port-orthogonal-edge-router) layered-graph node dummy)
  "Adds a port to the given node and connects that to the given dummy node."
  ;; First, add a port to the node.  The port side depends on the node's hierarchical port side
  (let ((out-port (new-l-port))
        (ext-port-side (get-property node layered-internal-properties.ext-port-side)))
    (set-node out-port node)
    (set-side out-port ext-port-side)
    ;; Find the dummy node's port
    (let ((in-port (jl-get (get-ports dummy) 0)))
      ;; Connect the two nodes
      (let ((edge (new-l-edge)))
        (set-source edge out-port)
        (set-target edge in-port)))))

;;; ------------------------------------------------------------ STEP 2: SET NORTH / SOUTH DUMMY COORDINATES

(defjmethod set-north-south-dummy-coordinates ((this hierarchical-port-orthogonal-edge-router)
                                               layered-graph north-south-dummies)
  "Set coordinates for northern and southern external port dummy nodes."
  (let* ((constraints (get-property layered-graph layered-options.port-constraints))
         (graph-size (get-size layered-graph))
         (graph-padding (get-padding layered-graph))
         (graph-width (+ (.x graph-size) (.left graph-padding) (.right graph-padding)))
         (north-y (- 0.0d0 (.top graph-padding) (.y (get-offset layered-graph))))
         (south-y (- (+ (.y graph-size) (.top graph-padding) (.bottom graph-padding))
                     (.y (get-offset layered-graph))))
         ;; Lists of northern and southern external port dummies
         (northern-dummies (new-jlist))
         (southern-dummies (new-jlist)))
    (jdo (dummy north-south-dummies)
      ;; Set x coordinate
      (enum-case constraints
        ((port-constraints.free port-constraints.fixed-side port-constraints.fixed-order)
         (calculate-north-south-dummy-positions this dummy))
        (port-constraints.fixed-ratio
         (apply-north-south-dummy-ratio this dummy graph-width)
         (border-to-content-area-coordinates dummy t nil))
        (port-constraints.fixed-pos
         (apply-north-south-dummy-position this dummy)
         (border-to-content-area-coordinates dummy t nil)
         ;; Ensure that the graph is wide enough to hold the port
         (setf (.x graph-size)
               (math.max (.x graph-size)
                         (+ (.x (get-position dummy)) (/ (.x (get-size dummy)) 2.0d0))))))
      ;; Set y coordinates and add the dummy to its respective list
      (enum-case (get-property dummy layered-internal-properties.ext-port-side)
        (port-side.north
         (setf (.y (get-position dummy)) north-y)
         (jl-add northern-dummies dummy))
        (port-side.south
         (setf (.y (get-position dummy)) south-y)
         (jl-add southern-dummies dummy))))
    ;; Check for correct ordering of nodes in the FIXED_ORDER case and for dummy nodes that
    ;; have been put on top of one another
    (enum-case constraints
      ((port-constraints.free port-constraints.fixed-side)
       (ensure-unique-positions this northern-dummies layered-graph)
       (ensure-unique-positions this southern-dummies layered-graph))
      (port-constraints.fixed-order
       (restore-proper-order this northern-dummies layered-graph)
       (restore-proper-order this southern-dummies layered-graph)))))

(defjmethod calculate-north-south-dummy-positions ((this hierarchical-port-orthogonal-edge-router) dummy)
  "Calculates the positions of northern and southern dummy nodes.  The position is based on the
nodes the dummy nodes are connected to, which is taken as the position of the port's center."
  ;; We use a simple algorithm that simply adds the horizontal positions of all
  ;; connected ports and divides the sum by the number of connected ports
  ;; First, get the dummy's port (it has only one)
  (let ((dummy-in-port (jl-get (get-ports dummy) 0)))
    ;; It might be that the node is not actually connected to anything.  In that case, we assign
    ;; a default position
    (if (= (get-degree dummy-in-port) 0)
        (setf (.x (get-position dummy)) 0.0d0)
        ;; Now, iterate over all connected ports, adding their horizontal position
        (let ((pos-sum 0.0d0))
          (jdo (connected-port (get-connected-ports dummy-in-port))
            (incf pos-sum (+ (.x (get-position (get-node connected-port)))
                             (.x (get-position connected-port))
                             (.x (get-anchor connected-port)))))
          ;; Assign the dummy's x coordinate
          (let* ((anchor (get-property dummy layered-options.port-anchor))
                 (offset (if anchor (.x anchor) 0.0d0)))
            (setf (.x (get-position dummy))
                  (- (/ pos-sum (get-degree dummy-in-port)) offset)))))))

(defjmethod apply-north-south-dummy-ratio ((this hierarchical-port-orthogonal-edge-router) dummy width)
  "Sets the dummy's x coordinate to respect the ratio defined for its original port."
  (let* ((anchor (get-property dummy layered-options.port-anchor))
         (offset (if anchor (.x anchor) 0.0d0)))
    (setf (.x (get-position dummy))
          (- (* width (get-property dummy layered-internal-properties.port-ratio-or-position))
             offset))))

(defjmethod apply-north-south-dummy-position ((this hierarchical-port-orthogonal-edge-router) dummy)
  "Sets the dummy's x coordinate to its original port's x coordinate."
  (let* ((anchor (get-property dummy layered-options.port-anchor))
         (offset (if anchor (.x anchor) 0.0d0)))
    (setf (.x (get-position dummy))
          (- (get-property dummy layered-internal-properties.port-ratio-or-position) offset))))

(defjmethod ensure-unique-positions ((this hierarchical-port-orthogonal-edge-router) dummies graph)
  "Ensures that no two dummy nodes in the given list are assigned the same x coordinate.  This
method must only be called if port constraints are set to FREE or FIXED_SIDE as it may not
preserve the original order of dummy nodes."
  (unless (is-empty dummies)
    ;; Turn the list into an array of dummy nodes and sort that by their x coordinate
    (let ((dummy-array (l-graph-util.to-node-array dummies)))
      (jl-sort dummy-array (lambda (a b) (double.compare (.x (get-position a)) (.x (get-position b)))))
      (assign-ascending-coordinates this dummy-array graph))))

(defjmethod restore-proper-order ((this hierarchical-port-orthogonal-edge-router) dummies graph)
  "Checks if the automatically calculated node coordinates violate their fixed order and fixes the
coordinates.  Calling this method only makes sense if port constraints are set to FIXED_ORDER."
  (unless (is-empty dummies)
    ;; Turn the list into an array of dummy nodes and sort that by their original x coordinate
    (let ((dummy-array (l-graph-util.to-node-array dummies)))
      (jl-sort dummy-array
               (lambda (a b)
                 (double.compare (get-property a layered-internal-properties.port-ratio-or-position)
                                 (get-property b layered-internal-properties.port-ratio-or-position))))
      (assign-ascending-coordinates this dummy-array graph))))

(defjmethod assign-ascending-coordinates ((this hierarchical-port-orthogonal-edge-router) dummies graph)
  "Iterates over the given array of dummy nodes, making sure that their x coordinates are strictly
ascending.  Dummy nodes whose coordinates are in violation of this rule are moved to the right.
Once this method is finished, the coordinates of the dummy nodes reflect their order in the array."
  ;; Find the port-port spacing
  (let* ((spacing (get-property graph layered-options.spacing-port-port))
         ;; Now, iterate over the array, remembering the last assigned position.  If we find a
         ;; position that is less than or equal to the last position, assign a new position of
         ;; "lastPosition + edgeSpacing"
         (next-valid-coordinate (+ (.x (get-position (aref dummies 0)))
                                   (.x (get-size (aref dummies 0)))
                                   (.right (get-margin (aref dummies 0)))
                                   spacing)))
    (loop for index from 1 below (length dummies)
          do (let* ((current-position (get-position (aref dummies index)))
                    (current-size (get-size (aref dummies index)))
                    (current-margin (get-margin (aref dummies index)))
                    ;; Ensure spacings are adhered to
                    (delta (- (.x current-position) (.left current-margin) next-valid-coordinate)))
               (when (< delta 0)
                 (decf (.x current-position) delta))
               ;; Ensure the graph is large enough for this node
               (let ((graph-size (get-size graph)))
                 (setf (.x graph-size) (math.max (.x graph-size)
                                                 (+ (.x current-position) (.x current-size)))))
               ;; Compute next valid coordinate
               (setf next-valid-coordinate (+ (.x current-position) (.x current-size)
                                              (.right current-margin) spacing))))))

;;; ------------------------------------------------------------ STEP 3: EDGE ROUTING

(defjmethod route-edges ((this hierarchical-port-orthogonal-edge-router) monitor layered-graph north-south-dummies)
  "Routes northern and southern hierarchical port edges and adjusts the graph's height and offsets
accordingly."
  ;; Prepare south and target layers for northern and southern routing
  (let ((northern-source-layer (new-jset))
        (northern-target-layer (new-jset))
        (southern-source-layer (new-jset))
        (southern-target-layer (new-jset))
        ;; Find some routing parameters
        (node-spacing (get-property layered-graph layered-options.spacing-node-node))
        (edge-spacing (get-property layered-graph layered-options.spacing-edge-edge)))
    ;; Assemble the northern and southern hierarchical port dummies and the nodes they are
    ;; connected to
    (jdo (hierarchical-port-dummy north-south-dummies)
      (let ((port-side (get-property hierarchical-port-dummy layered-internal-properties.ext-port-side)))
        (cond ((eq port-side port-side.north)
               (js-add northern-target-layer hierarchical-port-dummy)
               (jdo (edge (get-incoming-edges hierarchical-port-dummy))
                 (js-add northern-source-layer (get-node (get-source edge)))))
              ((eq port-side port-side.south)
               (js-add southern-target-layer hierarchical-port-dummy)
               (jdo (edge (get-incoming-edges hierarchical-port-dummy))
                 (js-add southern-source-layer (get-node (get-source edge))))))))
    ;; Northern routing
    (unless (is-empty northern-source-layer)
      ;; Route the edges using a south-to-north orthogonal edge router
      (let* ((routing-generator (new-orthogonal-routing-generator routing-direction.south-to-north
                                                                  edge-spacing "extnorth"))
             (slots (route-edges routing-generator
                                 monitor
                                 layered-graph
                                 northern-source-layer
                                 0
                                 northern-target-layer
                                 (- (- node-spacing) (.y (get-offset layered-graph))))))
        ;; If anything was routed, adjust the graph's offset and height
        (when (> slots 0)
          (setf (.northern-ext-port-edge-routing-height this)
                (+ node-spacing (* (1- slots) edge-spacing)))
          (incf (.y (get-offset layered-graph)) (.northern-ext-port-edge-routing-height this))
          (incf (.y (get-size layered-graph)) (.northern-ext-port-edge-routing-height this)))))
    ;; Southern routing
    (unless (is-empty southern-source-layer)
      ;; Route the edges using a north-to-south orthogonal edge router
      (let* ((routing-generator (new-orthogonal-routing-generator routing-direction.north-to-south
                                                                  edge-spacing "extsouth"))
             (slots (route-edges routing-generator
                                 monitor
                                 layered-graph
                                 southern-source-layer
                                 0
                                 southern-target-layer
                                 (- (+ (.y (get-size layered-graph)) node-spacing)
                                    (.y (get-offset layered-graph))))))
        ;; Adjust graph height.
        (when (> slots 0)
          (incf (.y (get-size layered-graph))
                (+ node-spacing (* (1- slots) edge-spacing))))))))

;;; ------------------------------------------------------------ STEP 4: REMOVE TEMPORARY DUMMIES

(defjmethod remove-temporary-north-south-dummies ((this hierarchical-port-orthogonal-edge-router) layered-graph)
  "Removes the temporary hierarchical port dummies, reconnecting their incoming and outgoing edges
to the original dummies and setting the appropriate bend points."
  (let ((nodes-to-remove (new-jlist)))
    ;; Iterate through all layers
    (jdo (layer (get-layers layered-graph))
      (jdo (node layer)
        (when (and (eq (get-type node) node-type.external-port)
                   ;; We're only looking for temporary north / south dummies
                   (has-property node layered-internal-properties.ext-port-replaced-dummy))
          ;; There must be a port where all edges come in, another port where edges go out, and
          ;; a port with an edge connecting node and origin (that one was added previously by
          ;; this processor)
          (let ((node-in-port nil)
                (node-out-port nil)
                (node-origin-port nil))
            (jdo (port (get-ports node))
              (enum-case (get-side port)
                (port-side.west (setf node-in-port port))
                (port-side.east (setf node-out-port port))
                (t (setf node-origin-port port))))
            ;; Find the edge connecting this dummy to the original external port dummy that we
            ;; restored just a while ago
            (let* ((node-to-origin-edge (jl-get (get-outgoing-edges node-origin-port) 0))
                   ;; Compute bend points for incoming edges
                   (incoming-edge-bend-points (new-k-vector-chain (get-bend-points node-to-origin-edge)))
                   (first-bend-point (clone (get-position node-origin-port))))
              (add first-bend-point (get-position node))
              (add incoming-edge-bend-points 0 first-bend-point)
              ;; Compute bend points for outgoing edges
              (let* ((outgoing-edge-bend-points (k-vector-chain.reverse (get-bend-points node-to-origin-edge)))
                     (last-bend-point (clone (get-position node-origin-port))))
                (add last-bend-point (get-position node))
                (add outgoing-edge-bend-points last-bend-point)
                ;; Retrieve the original hierarchical port dummy
                (let* ((replaced-dummy (get-property node layered-internal-properties.ext-port-replaced-dummy))
                       (replaced-dummy-port (jl-get (get-ports replaced-dummy) 0)))
                  ;; Reroute all the input port's edges
                  (let ((edges (jl-copy (get-incoming-edges node-in-port))))
                    (jdo (edge edges)
                      (set-target edge replaced-dummy-port)
                      (add-all-as-copies (get-bend-points edge) (size (get-bend-points edge))
                                         incoming-edge-bend-points)))
                  ;; Reroute all the output port's edges
                  (let ((edges (l-graph-util.to-edge-array (get-outgoing-edges node-out-port))))
                    (jdo (edge edges)
                      (set-source edge replaced-dummy-port)
                      (add-all-as-copies (get-bend-points edge) 0 outgoing-edge-bend-points)))))
              ;; Remove connection between node and original hierarchical port dummy
              (set-source node-to-origin-edge nil)
              (set-target node-to-origin-edge nil))
            ;; Remember the temporary node for removal
            (jl-add nodes-to-remove node)))))
    ;; Remove nodes
    (jdo (node nodes-to-remove)
      (set-layer node nil))))

;;; ------------------------------------------------------------ STEP 5: FIX DUMMY COORDINATES

(defjmethod fix-coordinates ((this hierarchical-port-orthogonal-edge-router) layered-graph)
  "Fixes all hierarchical port dummy coordinates.  For east / west external port dummies, this
means setting the x coordinate appropriately, and, in case of FIXED_RATIO, checking that the ratio
is respected.  For north / south hierarchical port dummies, this means setting the y coordinate
appropriately."
  (let* ((constraints (get-property layered-graph layered-options.port-constraints))
         ;; East port dummies are in the first layer; all other dummies are in the last layer
         (layers (get-layers layered-graph)))
    (fix-coordinates this (jl-get layers 0) constraints layered-graph)
    (fix-coordinates this (jl-get layers (1- (size layers))) constraints layered-graph)))

(defjmethod fix-coordinates ((this hierarchical-port-orthogonal-edge-router) layer constraints graph)
  "Fixes the coordinates of the nodes in the given layer."
  ;; Get some geometric values from the graph
  (let* ((padding (get-padding graph))
         (offset (get-offset graph))
         (graph-actual-size (get-actual-size graph))
         (new-actual-graph-height (.y graph-actual-size)))
    ;; During the first iteration, EAST and WEST dummy nodes are fixed.  This may change the height
    ;; of the graph, so we're setting y coordinates of NORTH and SOUTH dummies in a second iteration
    (jdo (node layer)
      (when (eq (get-type node) node-type.external-port)
        (let* ((ext-port-side (get-property node layered-internal-properties.ext-port-side))
               (ext-port-size (get-property node layered-internal-properties.ext-port-size))
               (node-position (get-position node))
               (required-actual-graph-height 0.0d0))
          ;; Set x coordinate
          (enum-case ext-port-side
            (port-side.east (setf (.x node-position) (- (+ (.x (get-size graph)) (.right padding)) (.x offset))))
            (port-side.west (setf (.x node-position) (- (- (.x offset)) (.left padding)))))
          ;; Set y coordinate
          (when (or (eq ext-port-side port-side.east) (eq ext-port-side port-side.west))
            (cond ((eq constraints port-constraints.fixed-ratio)
                   (let ((ratio (get-property node layered-internal-properties.port-ratio-or-position)))
                     (setf (.y node-position)
                           (- (* (.y graph-actual-size) ratio)
                              (.y (get-property node layered-options.port-anchor))))
                     (setf required-actual-graph-height (+ (.y node-position) (.y ext-port-size)))
                     (border-to-content-area-coordinates node nil t)))
                  ((eq constraints port-constraints.fixed-pos)
                   (setf (.y node-position)
                         (- (get-property node layered-internal-properties.port-ratio-or-position)
                            (.y (get-property node layered-options.port-anchor))))
                   (setf required-actual-graph-height (+ (.y node-position) (.y ext-port-size)))
                   (border-to-content-area-coordinates node nil t))))
          (setf new-actual-graph-height (math.max new-actual-graph-height required-actual-graph-height)))))
    ;; Make the graph larger, if necessary
    (incf (.y (get-size graph)) (- new-actual-graph-height (.y graph-actual-size)))
    ;; Iterate over NORTH and SOUTH dummies now that the graph's height is fixed
    (jdo (node layer)
      (when (eq (get-type node) node-type.external-port)
        (let ((ext-port-side (get-property node layered-internal-properties.ext-port-side))
              (node-position (get-position node)))
          (enum-case ext-port-side
            (port-side.north (setf (.y node-position) (- (- (.y offset)) (.top padding))))
            (port-side.south (setf (.y node-position)
                                   (- (+ (.y (get-size graph)) (.bottom padding)) (.y offset))))))))))

;;; ------------------------------------------------------------ STEP 6: SLANTED EDGE SEGMENT CORRECTION

(defjmethod correct-slanted-edge-segments ((this hierarchical-port-orthogonal-edge-router) (layered-graph l-graph))
  "Goes over the eastern and western hierarchical dummy nodes and checks whether their incident
edges have slanted segments."
  ;; East port dummies are in the first layer; all other dummies are in the last layer
  (let ((layers (get-layers layered-graph)))
    (correct-slanted-edge-segments this (jl-get layers 0))
    (correct-slanted-edge-segments this (jl-get layers (1- (size layers))))))

(defjmethod correct-slanted-edge-segments ((this hierarchical-port-orthogonal-edge-router) (layer layer))
  "Goes over the eastern and western hierarchical dummy nodes in the given layer and checks whether
their incident edges have slanted segments.  Note that only the first and last segment needs to be
checked."
  (jdo (node layer)
    (when (eq (get-type node) node-type.external-port)
      (let ((ext-port-side (get-property node layered-internal-properties.ext-port-side)))
        (when (or (eq ext-port-side port-side.east) (eq ext-port-side port-side.west))
          (jdo (edge (get-connected-edges node))
            (let ((bend-points (get-bend-points edge)))
              (unless (is-empty bend-points)
                ;; Correct a slanted segment connected to the source port if it belongs to our node
                (let ((source-port (get-source edge)))
                  (when (eq (get-node source-port) node)
                    (let ((first-bend-point (get-first bend-points)))
                      (setf (.y first-bend-point) (.y (get-absolute-anchor source-port))))))
                ;; Correct a slanted segment connected to the target port if it belongs to our node
                (let ((target-port (get-target edge)))
                  (when (eq (get-node target-port) node)
                    (let ((last-bend-point (get-last bend-points)))
                      (setf (.y last-bend-point) (.y (get-absolute-anchor target-port))))))))))))))
