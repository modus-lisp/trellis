;;;; graph-transformer.lisp -- GraphTransformer (transforms the graph to and from the internal
;;;; left-to-right drawing direction).

(in-package #:elk)

;; Private helpers of this file are called before their defjmethod is compiled.
(define-jcalls :instance
  process
  rotate-90-clockwise
  rotate-90-counter-clockwise
  mirror-all-x
  mirror-all-y
  transpose-all
  mirror-x-nodes
  mirror-x-vector
  mirror-x-spacing
  mirror-node-label-placement-x
  mirror-port-side-x
  mirror-external-port-side-x
  get-mirrored-port-side-x
  mirror-layer-constraint-x
  mirror-y-nodes
  mirror-y-vector
  mirror-y-spacing
  mirror-node-label-placement-y
  mirror-port-side-y
  mirror-external-port-side-y
  get-mirrored-port-side-y
  mirror-in-layer-constraint-y
  transpose-nodes
  transpose-vector
  transpose-spacing
  transpose-node-label-placement
  transpose-port-side
  get-transposed-port-side
  transpose-edge-label-placement
  transpose-external-port-side
  transpose-layer-constraint
  transpose-properties
  reverse-index)

;;; The Java overloads mirrorX(List, LGraph), mirrorX(KVector, double) and mirrorX(Spacing) are
;;; separate methods here (and likewise for mirrorY, transpose, transposePortSide), since they are
;;; private helpers that differ only by argument type.

(defjctor graph-transformer (themode)
  (setf (.mode this) themode))

(defjmethod process ((this graph-transformer) layered-graph monitor)
  (begin monitor (jstr "Graph transformation (" (name (.mode this)) ")") 1)
  (let ((nodes (jl-copy (get-layerless-nodes layered-graph)))
        (mode (.mode this)))
    (jdo (layer (get-layers layered-graph))
      (jl-add-all nodes (get-nodes layer)))
    (let ((congruency (get-property layered-graph layered-options.direction-congruency)))
      (if (eq congruency direction-congruency.reading-direction)
          (enum-case (get-property layered-graph layered-options.direction)
            (core-direction.left
             (mirror-all-x this layered-graph nodes))
            (core-direction.down
             (transpose-all this layered-graph nodes))
            (core-direction.up
             (if (eq mode layered-intermediate-mode.to-internal-ltr)
                 (progn
                   (transpose-all this layered-graph nodes)
                   (mirror-all-y this layered-graph nodes))
                 (progn
                   (mirror-all-y this layered-graph nodes)
                   (transpose-all this layered-graph nodes)))))
          (if (eq mode layered-intermediate-mode.to-internal-ltr)
              (enum-case (get-property layered-graph layered-options.direction)
                (core-direction.left
                 (mirror-all-x this layered-graph nodes)
                 (mirror-all-y this layered-graph nodes))
                (core-direction.down
                 (rotate-90-clockwise this layered-graph nodes))
                (core-direction.up
                 (rotate-90-counter-clockwise this layered-graph nodes)))
              (enum-case (get-property layered-graph layered-options.direction)
                (core-direction.left
                 (mirror-all-x this layered-graph nodes)
                 (mirror-all-y this layered-graph nodes))
                (core-direction.down
                 (rotate-90-counter-clockwise this layered-graph nodes))
                (core-direction.up
                 (rotate-90-clockwise this layered-graph nodes)))))))
  (done monitor))

(defjmethod rotate-90-clockwise ((this graph-transformer) layered-graph nodes)
  (transpose-all this layered-graph nodes)
  (mirror-all-x this layered-graph nodes))

(defjmethod rotate-90-counter-clockwise ((this graph-transformer) layered-graph nodes)
  (mirror-all-x this layered-graph nodes)
  (transpose-all this layered-graph nodes))

(defjmethod mirror-all-x ((this graph-transformer) layered-graph nodes)
  (mirror-x-nodes this nodes layered-graph)
  (mirror-x-spacing this (get-padding layered-graph))
  (mirror-x-spacing this (get-property layered-graph layered-options.node-labels-padding)))

(defjmethod mirror-all-y ((this graph-transformer) layered-graph nodes)
  (mirror-y-nodes this nodes layered-graph)
  (mirror-y-spacing this (get-padding layered-graph))
  (mirror-y-spacing this (get-property layered-graph layered-options.node-labels-padding)))

(defjmethod transpose-all ((this graph-transformer) layered-graph nodes)
  (transpose-nodes this nodes)
  (transpose-edge-label-placement this layered-graph)
  (transpose-vector this (get-offset layered-graph))
  (transpose-vector this (get-size layered-graph))
  (transpose-spacing this (get-padding layered-graph))
  (transpose-spacing this (get-property layered-graph layered-options.node-labels-padding)))

;;; ------------------------------------------------------------ mirror X

(defjmethod mirror-x-nodes ((this graph-transformer) nodes graph)
  "Mirror the x coordinates of the given graph.  Assuming that no nodes extend into negative x
coordinates, mirroring a node means that the space left to its left border equals the space right to
its right border when mirrored.  We use the offset variable to store graphWidth, since that's the
constant offset against which we calculate the new node positions.  Once nodes are allowed to extend
into negative coordinates, the amount of space the graph extends into negative coordinates (the
graph's offset) is subtracted from the graph width."
  (let ((offset 0.0d0))
    (if (= (.x (get-size graph)) 0)
        (jdo (node nodes)
          (setf offset (math.max offset
                                 (+ (.x (get-position node)) (.x (get-size node))
                                    (.right (get-margin node))))))
        (setf offset (- (.x (get-size graph)) (.x (get-offset graph)))))
    (decf offset (.x (get-offset graph)))
    (jdo (node nodes)
      (mirror-x-vector this (get-position node) (- offset (.x (get-size node))))
      (mirror-x-spacing this (get-padding node))
      (mirror-node-label-placement-x this node)
      (when (jm-contains-key (get-all-properties node) layered-options.position)
        (mirror-x-vector this (get-property node layered-options.position)
                         (- offset (.x (get-size node)))))
      (enum-case (get-property node layered-options.alignment)
        (core-alignment.left (set-property node layered-options.alignment core-alignment.right))
        (core-alignment.right (set-property node layered-options.alignment core-alignment.left)))
      (let ((node-size (get-size node)))
        (jdo (port (get-ports node))
          (mirror-x-vector this (get-position port) (- (.x node-size) (.x (get-size port))))
          (mirror-x-vector this (get-anchor port) (.x (get-size port)))
          (mirror-port-side-x this port)
          (reverse-index this port)
          (jdo (edge (get-outgoing-edges port))
            (jdo (bend-point (get-bend-points edge))
              (mirror-x-vector this bend-point offset))
            (let ((junction-points (get-property edge layered-options.junction-points)))
              (when junction-points
                (jdo (jp junction-points)
                  (mirror-x-vector this jp offset))))
            (jdo (label (get-labels edge))
              (mirror-x-vector this (get-position label) (- offset (.x (get-size label))))))
          (jdo (label (get-labels port))
            (mirror-x-vector this (get-position label)
                             (- (.x (get-size port)) (.x (get-size label))))))
        (when (eq (get-type node) node-type.external-port)
          (mirror-external-port-side-x this node)
          (mirror-layer-constraint-x this node))
        (jdo (label (get-labels node))
          (mirror-node-label-placement-x this label)
          (mirror-x-vector this (get-position label) (- (.x node-size) (.x (get-size label)))))))))

(defjmethod mirror-x-vector ((this graph-transformer) v offset)
  "Mirror the x coordinate of the given vector and add an offset."
  (setf (.x v) (- offset (.x v))))

(defjmethod mirror-x-spacing ((this graph-transformer) spacing)
  "Mirrors the given spacing in X direction."
  (let ((old-left (.left spacing))
        (old-right (.right spacing)))
    (setf (.left spacing) old-right)
    (setf (.right spacing) old-left)))

(defjmethod mirror-node-label-placement-x ((this graph-transformer) shape)
  "Horizontally mirrors the node label placement options, if any are set."
  (when (has-property shape layered-options.node-labels-placement)
    (let ((old-placement (get-property shape layered-options.node-labels-placement)))
      (cond ((contains old-placement node-label-placement.h-left)
             (j-remove old-placement node-label-placement.h-left)
             (add old-placement node-label-placement.h-right))
            ((contains old-placement node-label-placement.h-right)
             (j-remove old-placement node-label-placement.h-right)
             (add old-placement node-label-placement.h-left))))))

(defjmethod mirror-port-side-x ((this graph-transformer) port)
  "Mirror the side of the given port.  Undefined port sides are left untouched."
  (set-side port (get-mirrored-port-side-x this (get-side port))))

(defjmethod mirror-external-port-side-x ((this graph-transformer) node)
  "Mirror the side of the external port represented by the given external port dummy."
  (set-property node layered-internal-properties.ext-port-side
                (get-mirrored-port-side-x this
                                          (get-property node layered-internal-properties.ext-port-side))))

(defjmethod get-mirrored-port-side-x ((this graph-transformer) side)
  "Returns the port side that is horizontally mirrored from the given side."
  (enum-case side
    (port-side.east port-side.west)
    (port-side.west port-side.east)
    (t side)))

(defjmethod mirror-layer-constraint-x ((this graph-transformer) node)
  "Horizontally mirrors the layer constraint set on a node.  This is only meant for handling external
port dummy nodes."
  (enum-case (get-property node layered-options.layering-layer-constraint)
    (layer-constraint.first
     (set-property node layered-options.layering-layer-constraint layer-constraint.last))
    (layer-constraint.first-separate
     (set-property node layered-options.layering-layer-constraint layer-constraint.last-separate))
    (layer-constraint.last
     (set-property node layered-options.layering-layer-constraint layer-constraint.first))
    (layer-constraint.last-separate
     (set-property node layered-options.layering-layer-constraint layer-constraint.first-separate))))

;;; ------------------------------------------------------------ mirror Y

(defjmethod mirror-y-nodes ((this graph-transformer) nodes graph)
  "Mirror the y coordinates of the given graph."
  (let ((offset 0.0d0))
    (if (= (.y (get-size graph)) 0)
        (jdo (node nodes)
          (setf offset (math.max offset
                                 (+ (.y (get-position node)) (.y (get-size node))
                                    (.bottom (get-margin node))))))
        (setf offset (- (.y (get-size graph)) (.y (get-offset graph)))))
    (decf offset (.y (get-offset graph)))
    (jdo (node nodes)
      (mirror-y-vector this (get-position node) (- offset (.y (get-size node))))
      (mirror-y-spacing this (get-padding node))
      (mirror-node-label-placement-y this node)
      (when (jm-contains-key (get-all-properties node) layered-options.position)
        (mirror-y-vector this (get-property node layered-options.position)
                         (- offset (.y (get-size node)))))
      (enum-case (get-property node layered-options.alignment)
        (core-alignment.top (set-property node layered-options.alignment core-alignment.bottom))
        (core-alignment.bottom (set-property node layered-options.alignment core-alignment.top)))
      (let ((node-size (get-size node)))
        (jdo (port (get-ports node))
          (mirror-y-vector this (get-position port) (- (.y node-size) (.y (get-size port))))
          (mirror-y-vector this (get-anchor port) (.y (get-size port)))
          (mirror-port-side-y this port)
          (reverse-index this port)
          (jdo (edge (get-outgoing-edges port))
            (jdo (bend-point (get-bend-points edge))
              (mirror-y-vector this bend-point offset))
            (let ((junction-points (get-property edge layered-options.junction-points)))
              (when junction-points
                (jdo (jp junction-points)
                  (mirror-y-vector this jp offset))))
            (jdo (label (get-labels edge))
              (mirror-y-vector this (get-position label) (- offset (.y (get-size label))))))
          (jdo (label (get-labels port))
            (mirror-y-vector this (get-position label)
                             (- (.y (get-size port)) (.y (get-size label))))))
        (when (eq (get-type node) node-type.external-port)
          (mirror-external-port-side-y this node)
          (mirror-in-layer-constraint-y this node))
        (jdo (label (get-labels node))
          (mirror-node-label-placement-y this label)
          (mirror-y-vector this (get-position label) (- (.y node-size) (.y (get-size label)))))))))

(defjmethod mirror-y-vector ((this graph-transformer) v offset)
  "Mirror the y coordinate of the given vector and add an offset."
  (setf (.y v) (- offset (.y v))))

(defjmethod mirror-y-spacing ((this graph-transformer) spacing)
  "Mirrors the given spacing in Y direction."
  (let ((old-top (.top spacing))
        (old-bottom (.bottom spacing)))
    (setf (.top spacing) old-bottom)
    (setf (.bottom spacing) old-top)))

(defjmethod mirror-node-label-placement-y ((this graph-transformer) shape)
  "Vertically mirrors the node label placement options, if any are set."
  (when (has-property shape layered-options.node-labels-placement)
    (let ((old-placement (get-property shape layered-options.node-labels-placement)))
      (cond ((contains old-placement node-label-placement.v-top)
             (j-remove old-placement node-label-placement.v-top)
             (add old-placement node-label-placement.v-bottom))
            ((contains old-placement node-label-placement.v-bottom)
             (j-remove old-placement node-label-placement.v-bottom)
             (add old-placement node-label-placement.v-top))))))

(defjmethod mirror-port-side-y ((this graph-transformer) port)
  "Mirror the side of the given port.  Undefined port sides are left untouched."
  (set-side port (get-mirrored-port-side-y this (get-side port))))

(defjmethod mirror-external-port-side-y ((this graph-transformer) node)
  "Mirror the side of the external port represented by the given external port dummy."
  (set-property node layered-internal-properties.ext-port-side
                (get-mirrored-port-side-y this
                                          (get-property node layered-internal-properties.ext-port-side))))

(defjmethod get-mirrored-port-side-y ((this graph-transformer) side)
  "Returns the port side that is vertically mirrored from the given side."
  (enum-case side
    (port-side.north port-side.south)
    (port-side.south port-side.north)
    (t side)))

(defjmethod mirror-in-layer-constraint-y ((this graph-transformer) node)
  "Vertically mirrors the in-layer constraint set on a node.  This is only meant for handling external
port dummy nodes."
  (enum-case (get-property node layered-internal-properties.in-layer-constraint)
    (in-layer-constraint.top
     (set-property node layered-internal-properties.in-layer-constraint in-layer-constraint.bottom))
    (in-layer-constraint.bottom
     (set-property node layered-internal-properties.in-layer-constraint in-layer-constraint.top))))

;;; ------------------------------------------------------------ transpose

(defjmethod transpose-nodes ((this graph-transformer) nodes)
  "Transpose the x and y coordinates of the given nodes."
  (jdo (node nodes)
    (transpose-vector this (get-position node))
    (transpose-vector this (get-size node))
    (transpose-spacing this (get-padding node))
    (transpose-node-label-placement this node)
    (transpose-properties this node)
    (jdo (port (get-ports node))
      (transpose-vector this (get-position port))
      (transpose-vector this (get-anchor port))
      (transpose-vector this (get-size port))
      (transpose-port-side this port)
      (reverse-index this port)
      (jdo (edge (get-outgoing-edges port))
        (jdo (bend-point (get-bend-points edge))
          (transpose-vector this bend-point))
        (let ((junction-points (get-property edge layered-options.junction-points)))
          (when junction-points
            (jdo (jp junction-points)
              (transpose-vector this jp))))
        (jdo (label (get-labels edge))
          (transpose-vector this (get-position label))
          (transpose-vector this (get-size label))))
      (jdo (label (get-labels port))
        (transpose-vector this (get-position label))
        (transpose-vector this (get-size label))))
    (when (eq (get-type node) node-type.external-port)
      (transpose-external-port-side this node)
      (transpose-layer-constraint this node))
    (jdo (label (get-labels node))
      (transpose-node-label-placement this label)
      (transpose-vector this (get-size label))
      (transpose-vector this (get-position label)))))

(defjmethod transpose-vector ((this graph-transformer) v)
  "Transpose the x and y coordinate of the given vector."
  (let ((temp (.x v)))
    (setf (.x v) (.y v))
    (setf (.y v) temp)))

(defjmethod transpose-spacing ((this graph-transformer) spacing)
  "Transposes the given spacing."
  (let ((old-top (.top spacing))
        (old-bottom (.bottom spacing))
        (old-left (.left spacing))
        (old-right (.right spacing)))
    (setf (.top spacing) old-left)
    (setf (.bottom spacing) old-right)
    (setf (.left spacing) old-top)
    (setf (.right spacing) old-bottom)))

(defjmethod transpose-node-label-placement ((this graph-transformer) shape)
  "Transposes the node label placement options, if any are set."
  (when (has-property shape layered-options.node-labels-placement)
    (let ((old-placement (get-property shape layered-options.node-labels-placement)))
      (unless (is-empty old-placement)
        (let ((new-placement (enum-set.none-of 'node-label-placement)))
          (if (contains old-placement node-label-placement.inside)
              (add new-placement node-label-placement.inside)
              (add new-placement node-label-placement.outside))
          (unless (contains old-placement node-label-placement.h-priority)
            (add new-placement node-label-placement.h-priority))
          (cond ((contains old-placement node-label-placement.h-left)
                 (add new-placement node-label-placement.v-top))
                ((contains old-placement node-label-placement.h-center)
                 (add new-placement node-label-placement.v-center))
                ((contains old-placement node-label-placement.h-right)
                 (add new-placement node-label-placement.v-bottom)))
          (cond ((contains old-placement node-label-placement.v-top)
                 (add new-placement node-label-placement.h-left))
                ((contains old-placement node-label-placement.v-center)
                 (add new-placement node-label-placement.h-center))
                ((contains old-placement node-label-placement.v-bottom)
                 (add new-placement node-label-placement.h-right)))
          (set-property shape layered-options.node-labels-placement new-placement))))))

(defjmethod transpose-port-side ((this graph-transformer) p)
  "Transpose the side of the given port.  Undefined port sides are left untouched."
  (set-side p (get-transposed-port-side this (get-side p))))

(defjmethod get-transposed-port-side ((this graph-transformer) side)
  "Returns the transposed side of the given port side."
  (enum-case side
    (port-side.north port-side.west)
    (port-side.west port-side.north)
    (port-side.south port-side.east)
    (port-side.east port-side.south)
    (t port-side.undefined)))

(defjmethod transpose-edge-label-placement ((this graph-transformer) graph)
  "Transpose the placement of edge labels in the graph."
  (let ((old-side (get-property graph layered-options.edge-labels-side-selection)))
    (when old-side
      (set-property graph layered-options.edge-labels-side-selection (transpose old-side)))))

(defjmethod transpose-external-port-side ((this graph-transformer) node)
  "Transpose the side of the external port represented by the given external port dummy."
  (set-property node layered-internal-properties.ext-port-side
                (get-transposed-port-side this
                                          (get-property node layered-internal-properties.ext-port-side))))

(defjmethod transpose-layer-constraint ((this graph-transformer) node)
  "The layer constraint and in-layer constraint set on a node.  A node with layer constraint
FIRST_SEPARATE will end up with an in-layer constraint TOP.  This is only meant for external port
dummy nodes and only supports the cases that can occur with them."
  (let ((layer-constraint (get-property node layered-options.layering-layer-constraint))
        (in-layer-constraint (get-property node layered-internal-properties.in-layer-constraint)))
    (cond ((eq layer-constraint layer-constraint.first-separate)
           (set-property node layered-options.layering-layer-constraint layer-constraint.none)
           (set-property node layered-internal-properties.in-layer-constraint in-layer-constraint.top))
          ((eq layer-constraint layer-constraint.last-separate)
           (set-property node layered-options.layering-layer-constraint layer-constraint.none)
           (set-property node layered-internal-properties.in-layer-constraint in-layer-constraint.bottom))
          ((eq in-layer-constraint in-layer-constraint.top)
           (set-property node layered-options.layering-layer-constraint layer-constraint.first-separate)
           (set-property node layered-internal-properties.in-layer-constraint in-layer-constraint.none))
          ((eq in-layer-constraint in-layer-constraint.bottom)
           (set-property node layered-options.layering-layer-constraint layer-constraint.last-separate)
           (set-property node layered-internal-properties.in-layer-constraint in-layer-constraint.none)))))

(defjmethod transpose-properties ((this graph-transformer) node)
  "Checks a node's properties for ones that need to be transposed.  Currently, the minimum size,
the alignment and the position are transposed."
  (let ((min-size (get-property node layered-options.node-size-minimum)))
    (set-property node layered-options.node-size-minimum (new-k-vector/2 (.y min-size) (.x min-size))))
  (enum-case (get-property node layered-options.alignment)
    (core-alignment.left (set-property node layered-options.alignment core-alignment.top))
    (core-alignment.right (set-property node layered-options.alignment core-alignment.bottom))
    (core-alignment.top (set-property node layered-options.alignment core-alignment.left))
    (core-alignment.bottom (set-property node layered-options.alignment core-alignment.right)))
  (when (jm-contains-key (get-all-properties node) layered-options.position)
    (let* ((pos (get-property node layered-options.position))
           (tmp (.x pos)))
      (setf (.x pos) (.y pos))
      (setf (.y pos) tmp))))

(defjmethod reverse-index ((this graph-transformer) port)
  "Reverse the port index."
  (let ((index (get-property port layered-options.port-index)))
    (when index
      (set-property port layered-options.port-index (- index)))))
