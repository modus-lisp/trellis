;;;; hierarchical.lisp -- HierarchicalPortDummySizeProcessor, HierarchicalPortPositionProcessor,
;;;; HierarchicalNodeResizingProcessor (the hierarchical-port constraint and orthogonal router
;;;; are further down this file).

(in-package #:elk)

;; Private helpers of this file are called before their defjmethod is compiled.
(define-jcalls :instance
  process
  set-widths
  fix-coordinates
  graph-layout-to-node
  is-nested
  resize-graph
  resize-graph-no-really-i-mean-it)

;;; ------------------------------------------------------------ HierarchicalPortDummySizeProcessor

(defjmethod process ((this hierarchical-port-dummy-size-processor) layered-graph monitor)
  (begin monitor "Hierarchical port dummy size processing" 1)
  (let ((northern-dummies (new-jlist))
        (southern-dummies (new-jlist)))
    ;; Calculate the width difference (this assumes CENTER node alignment)
    ;;  The idea behind this is that ports are stacked on top of each other at the center of the
    ;;  node.  By iteratively increasing their size by twice the edgeEdge spacing between layers,
    ;;  vertical edge segments are spaced by that amount since the edge's anchor will either
    ;;  be the very first or very last point of the port.
    (let* ((edge-spacing (get-property layered-graph layered-options.spacing-edge-edge-between-layers))
           (delta (* edge-spacing 2)))
      ;; Iterate through the layers
      (jdo (layer (get-layers layered-graph))
        (jl-clear northern-dummies)
        (jl-clear southern-dummies)
        ;; Collect northern and southern hierarchical port dummies
        (jdo (node layer)
          (when (eq (get-type node) node-type.external-port)
            (let ((side (get-property node layered-internal-properties.ext-port-side)))
              (cond ((eq side port-side.north) (jl-add northern-dummies node))
                    ((eq side port-side.south) (jl-add southern-dummies node))))))
        ;; Set widths
        (set-widths this northern-dummies t delta)
        (set-widths this southern-dummies nil delta))))
  (done monitor))

(defjmethod set-widths ((this hierarchical-port-dummy-size-processor) nodes top-down delta)
  (let ((current-width 0.0d0)
        (step delta))
    (unless top-down
      ;; Start with the widest node, decreasing node size
      (setf current-width (* delta (1- (size nodes))))
      (setf step (* step -1.0d0)))
    (jdo (node nodes)
      (set-property node layered-options.alignment core-alignment.center)
      (setf (.x (get-size node)) current-width)
      ;; Move eastern ports to the node's right border
      (jdo (port (get-ports node port-side.east))
        (setf (.x (get-position port)) current-width))
      (incf current-width step))))

;;; ------------------------------------------------------------ HierarchicalPortPositionProcessor

(defjmethod process ((this hierarchical-port-position-processor) layered-graph monitor)
  (begin monitor "Hierarchical port position processing" 1)
  (let ((layers (get-layers layered-graph)))
    ;; We're interested in EAST and WEST external port dummies only; since they can only be in
    ;; the first or last layer, only fix coordinates of nodes in those two layers
    (when (> (size layers) 0)
      (fix-coordinates this (jl-get layers 0) layered-graph))
    (when (> (size layers) 1)
      (fix-coordinates this (jl-get layers (1- (size layers))) layered-graph)))
  (done monitor))

(defjmethod fix-coordinates ((this hierarchical-port-position-processor) layer layered-graph)
  "Fixes the y coordinates of external port dummies in the given layer."
  (let ((port-constraints (get-property layered-graph layered-options.port-constraints)))
    ;; If coordinates are free to be set, we're done
    (when (or (eq port-constraints port-constraints.fixed-ratio)
              (eq port-constraints port-constraints.fixed-pos))
      (let ((graph-height (.y (get-actual-size layered-graph))))
        ;; Iterate over the layer's nodes
        (jdo (node layer)
          ;; We only care about external port dummies...
          (when (eq (get-type node) node-type.external-port)
            ;; ...representing eastern or western ports.
            (let ((ext-port-side (get-property node layered-internal-properties.ext-port-side)))
              (when (or (eq ext-port-side port-side.east) (eq ext-port-side port-side.west))
                (let ((final-y-coordinate
                        (get-property node layered-internal-properties.port-ratio-or-position)))
                  (when (eq port-constraints port-constraints.fixed-ratio)
                    ;; finalYCoordinate is a ratio that must be multiplied with the graph's height
                    (setf final-y-coordinate (* final-y-coordinate graph-height)))
                  ;; Apply the node's new Y coordinate
                  (setf (.y (get-position node))
                        (- final-y-coordinate
                           (.y (get-property node layered-options.port-anchor))))
                  (border-to-content-area-coordinates node nil t))))))))))

;;; ------------------------------------------------------------ HierarchicalNodeResizingProcessor

(defjmethod process ((this hierarchical-node-resizing-processor) graph progress-monitor)
  (begin progress-monitor "Resize child graph to fit parent." 1)
  (jdo (layer (get-layers graph))
    (jl-add-all (get-layerless-nodes graph) (get-nodes layer))
    (jl-clear (get-nodes layer)))
  (jdo (node (get-layerless-nodes graph))
    (set-layer node nil))
  (jl-clear (get-layers graph))
  (resize-graph this graph)
  (when (is-nested this graph)
    (graph-layout-to-node this (get-parent-node graph) graph))
  (done progress-monitor))

(defjmethod graph-layout-to-node ((this hierarchical-node-resizing-processor) node lgraph)
  "Transfer the layout of the given graph to the given associated node."
  ;; Process external ports
  (jdo (child-node (get-layerless-nodes lgraph))
    (let ((origin (get-property child-node layered-internal-properties.origin)))
      (when (typep origin 'l-port)
        (let ((port-position (l-graph-util.get-external-port-position
                              lgraph child-node (.x (get-size origin)) (.y (get-size origin)))))
          (setf (.x (get-position origin)) (.x port-position))
          (setf (.y (get-position origin)) (.y port-position))
          (set-side origin (get-property child-node layered-internal-properties.ext-port-side))))))
  ;; Setup the parent node
  (let ((actual-graph-size (get-actual-size lgraph)))
    (if (contains (get-property lgraph layered-internal-properties.graph-properties)
                  layered-graph-properties.external-ports)
        (progn
          ;; Ports have positions assigned
          (set-property node layered-options.port-constraints port-constraints.fixed-pos)
          (add (get-property (get-graph node) layered-internal-properties.graph-properties)
               layered-graph-properties.non-free-ports)
          (l-graph-util.resize-node node actual-graph-size nil t))
        ;; Ports have not been positioned yet - leave this for next layouter
        (l-graph-util.resize-node node actual-graph-size t t))))

(defjmethod is-nested ((this hierarchical-node-resizing-processor) graph)
  (not (null (get-parent-node graph))))

;;; Graph Postprocessing (Size and External Ports)

(defjmethod resize-graph ((this hierarchical-node-resizing-processor) lgraph)
  "Sets the size of the given graph such that size constraints are adhered to.  Furthermore, the
padding is added to the graph size and the graph offset.  Afterwards, the border spacing property
is reset to 0."
  (let ((size-constraint (get-property lgraph layered-options.node-size-constraints))
        (size-options (get-property lgraph layered-options.node-size-options)))
    ;; getActualSize() used to take the border spacing (what is now included in the padding)
    ;; into account, which is why by this point it had to be cleared since it had already
    ;; been applied to the offset and the graph size.  It currently does not take the padding
    ;; into account anymore, but if it does, it needs to be cleared again
    (let* ((calculated-size (get-actual-size lgraph))
           (adjusted-size (clone calculated-size)))
      ;; calculate the new size
      (when (contains size-constraint size-constraint.minimum-size)
        (let ((min-size (get-property lgraph layered-options.node-size-minimum)))
          ;; if minimum width or height are not set, maybe default to default values
          (when (contains size-options size-options.default-minimum-size)
            (when (<= (.x min-size) 0)
              (setf (.x min-size) elk-util.default-min-width))
            (when (<= (.y min-size) 0)
              (setf (.y min-size) elk-util.default-min-height)))
          ;; apply new size including border spacing
          (setf (.x adjusted-size) (math.max (.x calculated-size) (.x min-size)))
          (setf (.y adjusted-size) (math.max (.y calculated-size) (.y min-size)))))
      (resize-graph-no-really-i-mean-it this lgraph calculated-size adjusted-size))))

(defjmethod resize-graph-no-really-i-mean-it ((this hierarchical-node-resizing-processor)
                                              lgraph old-size new-size)
  "Applies a new effective size to a graph that previously had an old size calculated by the
layout algorithm.  This method takes care of adjusting content alignments as well as external
ports that would be misplaced if the new size is larger than the old one."
  ;; obey to specified alignment constraints
  (let ((content-alignment (get-property lgraph layered-options.content-alignment)))
    ;; horizontal alignment
    (when (> (.x new-size) (.x old-size))
      (cond ((contains content-alignment content-alignment.h-center)
             (incf (.x (get-offset lgraph)) (/ (- (.x new-size) (.x old-size)) 2.0d0)))
            ((contains content-alignment content-alignment.h-right)
             (incf (.x (get-offset lgraph)) (- (.x new-size) (.x old-size))))))
    ;; vertical alignment
    (when (> (.y new-size) (.y old-size))
      (cond ((contains content-alignment content-alignment.v-center)
             (incf (.y (get-offset lgraph)) (/ (- (.y new-size) (.y old-size)) 2.0d0)))
            ((contains content-alignment content-alignment.v-bottom)
             (incf (.y (get-offset lgraph)) (- (.y new-size) (.y old-size))))))
    ;; correct the position of eastern and southern hierarchical ports, if necessary
    (when (and (contains (get-property lgraph layered-internal-properties.graph-properties)
                         layered-graph-properties.external-ports)
               (or (> (.x new-size) (.x old-size)) (> (.y new-size) (.y old-size))))
      ;; iterate over the graph's nodes, looking for eastern / southern external ports
      ;; (at this point, the graph's nodes are not divided into layers anymore)
      (jdo (node (get-layerless-nodes lgraph))
        ;; we're only looking for external port dummies
        (when (eq (get-type node) node-type.external-port)
          ;; check which side the external port is on
          (let ((ext-port-side (get-property node layered-internal-properties.ext-port-side)))
            (cond ((eq ext-port-side port-side.east)
                   (incf (.x (get-position node)) (- (.x new-size) (.x old-size))))
                  ((eq ext-port-side port-side.south)
                   (incf (.y (get-position node)) (- (.y new-size) (.y old-size)))))))))
    ;; Actually apply the new size
    (let ((padding (get-padding lgraph)))
      (setf (.x (get-size lgraph)) (- (.x new-size) (.left padding) (.right padding)))
      (setf (.y (get-size lgraph)) (- (.y new-size) (.top padding) (.bottom padding))))))
