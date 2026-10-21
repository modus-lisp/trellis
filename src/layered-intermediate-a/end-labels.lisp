;;;; end-labels.lisp -- EndLabelPostprocessor, EndLabelPreprocessor, EndLabelSorter.

(in-package #:elk)

;;; ------------------------------------------------------------ EndLabelPostprocessor

(defjmethod process ((this end-label-postprocessor) layered-graph monitor)
  (begin monitor "End label post-processing" 1)
  ;; We iterate over each node's label cells and offset and place them
  (jdo (layer (get-layers layered-graph))
    (jdo (node (get-nodes layer))
      (when (and (or (eq (get-type node) node-type.normal)
                     (eq (get-type node) node-type.external-port))
                 (has-property node layered-internal-properties.end-labels))
        (process-node this node))))
  (done monitor))

(defjmethod process-node ((this end-label-postprocessor) node)
  ;; The node should have a non-empty list of label cells, or something went TERRIBLY WRONG!!!
  (let ((end-label-cells (get-property node layered-internal-properties.end-labels))
        (node-pos (get-position node)))
    (jdo (label-cell (jm-values end-label-cells))
      (let ((label-cell-rect (get-cell-rectangle label-cell)))
        (move label-cell-rect node-pos)
        (apply-label-layout label-cell)))
    ;; Remove label cells
    (set-property node layered-internal-properties.end-labels nil)))

;;; ------------------------------------------------------------ EndLabelPreprocessor

(defjconst end-label-preprocessor.no-incident-edge-thickness -1.0)

(defjmethod process ((this end-label-preprocessor) layered-graph monitor)
  (begin monitor "End label pre-processing" 1)
  (let ((edge-label-spacing (get-property layered-graph layered-options.spacing-edge-label))
        (label-label-spacing (get-property layered-graph layered-options.spacing-label-label))
        (vertical-layout (is-vertical (get-property layered-graph layered-options.direction))))
    ;; We iterate over each node and place the end labels of its incident edges
    (jdo (layer (get-layers layered-graph))
      (jdo (node (get-nodes layer))
        (process-node this node edge-label-spacing label-label-spacing vertical-layout))))
  (done monitor))

(defjmethod process-node ((this end-label-preprocessor) node edge-label-spacing label-label-spacing
                          vertical-layout)
  ;; Iterate over all ports and collect their labels in label cells
  (let* ((port-count (length (get-ports node)))
         (port-label-cells (make-array port-count :initial-element nil)))
    (dotimes (port-index port-count)
      (let ((port (jl-get (get-ports node) port-index)))
        (setf (.id port) port-index)
        (setf (aref port-label-cells port-index)
              (create-configured-label-cell this (end-label-preprocessor.gather-labels port)
                                            label-label-spacing vertical-layout))))
    ;; Actually go off and place them labels!
    (place-labels this node port-label-cells label-label-spacing edge-label-spacing vertical-layout)
    ;; Turn the array into a map and save that in the node
    (let ((port-to-label-cell-map (new-jmap)))
      (dotimes (index (length port-label-cells))
        (when (aref port-label-cells index)
          (jm-put port-to-label-cell-map (jl-get (get-ports node) index)
                  (aref port-label-cells index))))
      (when (plusp (hash-table-count port-to-label-cell-map))
        (set-property node layered-internal-properties.end-labels port-to-label-cell-map)
        ;; Update the node's margins
        (update-node-margins this node port-label-cells)))))

(defjmethod create-configured-label-cell ((this end-label-preprocessor) labels label-label-spacing
                                          vertical-layout)
  (when (or (null labels) (zerop (size labels)))
    (return-from create-configured-label-cell/3 nil))
  ;; Create the new label cell and setup its alignments depending on the port's side
  (let ((label-cell (new-label-cell label-label-spacing (not vertical-layout))))
    (jdo (label labels)
      (add-label label-cell (l-graph-adapters.adapt label)))
    ;; Setup the label cell's size
    (let ((label-cell-rect (get-cell-rectangle label-cell)))
      (setf (.height label-cell-rect) (get-minimum-height label-cell))
      (setf (.width label-cell-rect) (get-minimum-width label-cell)))
    label-cell))

;;; Label Gathering

(defjstatic end-label-preprocessor.gather-labels (port)
  (let* ((labels (new-jlist))
         ;; Gather labels of the port itself
         (max-edge-thickness (end-label-preprocessor.gather-labels port labels))
         ;; If it has a dummy associated with it, we need to go through the dummy's ports and
         ;; process those that were created for the current port (see the
         ;; NorthSouthPortPreprocessor)
         (dummy-node (get-property port layered-internal-properties.port-dummy)))
    (when dummy-node
      (jdo (dummy-port (get-ports dummy-node))
        (when (eq (get-property dummy-port layered-internal-properties.origin) port)
          (setf max-edge-thickness
                (math.max max-edge-thickness
                          (end-label-preprocessor.gather-labels dummy-port labels))))))
    ;; Only save the maximum edge thickness if we'll be interested in it later
    (unless (jl-is-empty labels)
      (set-property port layered-internal-properties.max-edge-thickness max-edge-thickness))
    (if (/= max-edge-thickness end-label-preprocessor.no-incident-edge-thickness)
        labels
        nil)))

(defjstatic end-label-preprocessor.gather-labels (port target-list)
  (let ((max-edge-thickness -1.0)
        (labels (new-jlist)))
    (jdo (incident-edge (get-connected-edges port))
      (setf max-edge-thickness
            (math.max max-edge-thickness
                      (get-property incident-edge layered-options.edge-thickness)))
      (if (eq (get-source incident-edge) port)
          ;; It's an outgoing edge; all tail labels belong to this port
          (jdo (label (get-labels incident-edge))
            (when (eq (get-property label layered-options.edge-labels-placement)
                      edge-label-placement.tail)
              (jl-add labels label)))
          ;; It's an incoming edge; all head labels belong to this port
          (jdo (label (get-labels incident-edge))
            (when (eq (get-property label layered-options.edge-labels-placement)
                      edge-label-placement.head)
              (jl-add labels label))))
      ;; Remember the edge each label came from
      (jdo (label labels)
        (unless (has-property label layered-internal-properties.end-label-edge)
          (set-property label layered-internal-properties.end-label-edge incident-edge)))
      (jl-add-all target-list labels)
      (jl-clear labels))
    max-edge-thickness))

;;; Label Placement

(defjmethod place-labels ((this end-label-preprocessor) node port-label-cells label-label-spacing
                          edge-label-spacing vertical-layout)
  ;; First, place them as we usually would. This step can result in overlaps for northern /
  ;; southern labels (for horizontal layout directions) or for eastern / western labels (for
  ;; vertical layout directions).
  (let ((port-sides-with-labels (enum-set.none-of 'port-side)))
    (jdo (port (get-ports node))
      (when (aref port-label-cells (.id port))
        (place-labels this port (aref port-label-cells (.id port)) edge-label-spacing)
        (add port-sides-with-labels (get-side port)))))
  ;; If there are ports on the problematic sides, go ahead and remove overlaps between them
  (if vertical-layout
      (progn
        (remove-label-overlaps this node port-label-cells port-side.east (* 2 label-label-spacing)
                               edge-label-spacing)
        (remove-label-overlaps this node port-label-cells port-side.west (* 2 label-label-spacing)
                               edge-label-spacing))
      (progn
        (remove-label-overlaps this node port-label-cells port-side.north (* 2 label-label-spacing)
                               edge-label-spacing)
        (remove-label-overlaps this node port-label-cells port-side.south (* 2 label-label-spacing)
                               edge-label-spacing))))

(defjmethod place-labels ((this end-label-preprocessor) port label-cell edge-label-spacing)
  ;; Some necessary position information
  (let* ((label-cell-rect (get-cell-rectangle label-cell))
         (node-size (get-size (get-node port)))
         (node-margin (get-margin (get-node port)))
         (port-pos (get-position port))
         (port-anchor (k-vector.sum port-pos (get-anchor port))))
    ;; Calculate cell position depending on port side
    (enum-case (get-side port)
      (port-side.north
       (set-vertical-alignment label-cell vertical-label-alignment.bottom)
       (setf (.y label-cell-rect) (- (- (.top node-margin)) edge-label-spacing (.height label-cell-rect)))
       (if (eq (get-label-side this label-cell) label-side.above)
           (progn
             (set-horizontal-alignment label-cell horizontal-label-alignment.right)
             (setf (.x label-cell-rect) (- (.x port-anchor) (max-edge-thickness this port)
                                           edge-label-spacing (.width label-cell-rect))))
           (progn
             (set-horizontal-alignment label-cell horizontal-label-alignment.left)
             (setf (.x label-cell-rect) (+ (.x port-anchor) (max-edge-thickness this port)
                                           edge-label-spacing)))))
      (port-side.east
       (set-horizontal-alignment label-cell horizontal-label-alignment.left)
       (setf (.x label-cell-rect) (+ (.x node-size) (.right node-margin) edge-label-spacing))
       (if (eq (get-label-side this label-cell) label-side.above)
           (progn
             (set-vertical-alignment label-cell vertical-label-alignment.bottom)
             (setf (.y label-cell-rect) (- (.y port-anchor) (max-edge-thickness this port)
                                           edge-label-spacing (.height label-cell-rect))))
           (progn
             (set-vertical-alignment label-cell vertical-label-alignment.top)
             (setf (.y label-cell-rect) (+ (.y port-anchor) (max-edge-thickness this port)
                                           edge-label-spacing)))))
      (port-side.south
       (set-vertical-alignment label-cell vertical-label-alignment.top)
       (setf (.y label-cell-rect) (+ (.y node-size) (.bottom node-margin) edge-label-spacing))
       (if (eq (get-label-side this label-cell) label-side.above)
           (progn
             (set-horizontal-alignment label-cell horizontal-label-alignment.right)
             (setf (.x label-cell-rect) (- (.x port-anchor) (max-edge-thickness this port)
                                           edge-label-spacing (.width label-cell-rect))))
           (progn
             (set-horizontal-alignment label-cell horizontal-label-alignment.left)
             (setf (.x label-cell-rect) (+ (.x port-anchor) (max-edge-thickness this port)
                                           edge-label-spacing)))))
      (port-side.west
       (set-horizontal-alignment label-cell horizontal-label-alignment.right)
       (setf (.x label-cell-rect) (- (- (.left node-margin)) edge-label-spacing (.width label-cell-rect)))
       (if (eq (get-label-side this label-cell) label-side.above)
           (progn
             (set-vertical-alignment label-cell vertical-label-alignment.bottom)
             (setf (.y label-cell-rect) (- (.y port-anchor) (max-edge-thickness this port)
                                           edge-label-spacing (.height label-cell-rect))))
           (progn
             (set-vertical-alignment label-cell vertical-label-alignment.top)
             (setf (.y label-cell-rect) (+ (.y port-anchor) (max-edge-thickness this port)
                                           edge-label-spacing))))))))

(defjmethod remove-label-overlaps ((this end-label-preprocessor) node port-label-cells port-side
                                   label-label-spacing edge-label-spacing)
  (let ((overlap-remover
          (with-start-coordinate
              (with-gap (rectangle-strip-overlap-remover.create-for-direction
                         (port-side-to-overlap-removal-direction this port-side))
                label-label-spacing label-label-spacing)
            (calculate-overlap-start-coordinate this node port-side edge-label-spacing))))
    ;; Gather the rectangles
    (jdo (port (get-ports node port-side))
      (when (aref port-label-cells (.id port))
        (add-rectangle overlap-remover (get-cell-rectangle (aref port-label-cells (.id port))))))
    ;; Remove overlaps. Since we have stuffed the original label cell rectangles into this thing,
    ;; we won't ever have to apply or post-process anything.
    (remove-overlaps overlap-remover)))

(defjmethod calculate-overlap-start-coordinate ((this end-label-preprocessor) node port-side
                                                edge-label-spacing)
  (let ((node-size (get-size node))
        (node-margin (get-margin node)))
    (enum-case port-side
      (port-side.north (- (- (.top node-margin)) edge-label-spacing))
      (port-side.south (+ (.y node-size) (.bottom node-margin) edge-label-spacing))
      (port-side.east (+ (.x node-size) (.right node-margin) edge-label-spacing))
      (port-side.west (- (- (.left node-margin)) edge-label-spacing))
      ;; Should never happen
      (t 0))))

;;; Node Margins

(defjmethod update-node-margins ((this end-label-preprocessor) node label-cells)
  (let* ((node-margin (get-margin node))
         (node-size (get-size node))
         ;; Calculate the rectangle that describes the node's current margin
         (node-margin-rectangle
           (new-elk-rectangle (- (.left node-margin))
                              (- (.top node-margin))
                              (+ (.left node-margin) (.x node-size) (.right node-margin))
                              (+ (.top node-margin) (.y node-size) (.bottom node-margin)))))
    ;; Union the rectangle with each rectangle that describes a label cell
    (jdo (label-cell label-cells)
      (when label-cell
        (j-union node-margin-rectangle (get-cell-rectangle label-cell))))
    ;; Reapply the new rectangle to the margin
    (setf (.left node-margin) (- (.x node-margin-rectangle)))
    (setf (.top node-margin) (- (.y node-margin-rectangle)))
    (setf (.right node-margin) (- (.width node-margin-rectangle) (.left node-margin) (.x node-size)))
    (setf (.bottom node-margin) (- (.height node-margin-rectangle) (.top node-margin) (.y node-size)))))

;;; Utility Methods

(defjmethod get-label-side ((this end-label-preprocessor) label-cell)
  (get-property (j-get (get-labels label-cell) 0) layered-internal-properties.label-side))

(defjmethod max-edge-thickness ((this end-label-preprocessor) port)
  (get-property port layered-internal-properties.max-edge-thickness))

(defjmethod port-side-to-overlap-removal-direction ((this end-label-preprocessor) port-side)
  (enum-case port-side
    (port-side.north overlap-removal-direction.up)
    (port-side.south overlap-removal-direction.down)
    (port-side.east overlap-removal-direction.right)
    (port-side.west overlap-removal-direction.left)
    ;; Shouldn't happen
    (t nil)))

;;; ------------------------------------------------------------ EndLabelSorter

(defjconst end-label-sorter.label-group-comparator
  (lambda (group1 group2)
    ;; If they are not connected to the same source port, use the difference as a basis for the
    ;; comparison
    (let ((source-port-diff (integer.compare (.id (get-source (.edge group1)))
                                             (.id (get-source (.edge group2))))))
      (if (/= source-port-diff 0)
          source-port-diff
          ;; They are connected to the same source port. Sort by target node, if that makes a
          ;; difference.
          (let ((target-node-diff (integer.compare (.id (get-node (get-target (.edge group1))))
                                                   (.id (get-node (get-target (.edge group2)))))))
            (if (/= target-node-diff 0)
                target-node-diff
                ;; Same source port and same target node. Compare target ports, but backwards.
                ;; Since western ports are ordered from bottom to top, not sorting backwards
                ;; would yield the opposite of our desires.
                (integer.compare (.id (get-target (.edge group2)))
                                 (.id (get-target (.edge group1))))))))))

(defjmethod process ((this end-label-sorter) layered-graph monitor)
  (begin monitor "Sort end labels" 1)
  (jdo (layer (get-layers layered-graph))
    (jdo (node (get-nodes layer))
      (when (eq (get-type node) node-type.normal)
        (process-node this node))))
  (done monitor))

;;; Node Processing and Initialization

(defjmethod process-node ((this end-label-sorter) node)
  (let ((initialize-method-called nil))
    (when (has-property node layered-internal-properties.end-labels)
      (let ((label-cell-map (get-property node layered-internal-properties.end-labels)))
        ;; Iterate over all ports and check for each port if it requires its labels to be sorted
        (jdo (port (get-ports node))
          (when (needs-sorting this port)
            ;; Check if we need to initialize
            (unless initialize-method-called
              (initialize this (get-graph node))
              (setf initialize-method-called t))
            (j-sort this port (jm-get label-cell-map port))))))))

(defjmethod needs-sorting ((this end-label-sorter) port)
  (let ((edges-with-end-labels 0))
    (jdo (in-edge (get-incoming-edges port))
      (when (some (lambda (label)
                    (eq (get-property label layered-options.edge-labels-placement)
                        edge-label-placement.head))
                  (to-jvector (get-labels in-edge)))
        (incf edges-with-end-labels)))
    (jdo (out-edge (get-outgoing-edges port))
      (when (some (lambda (label)
                    (eq (get-property label layered-options.edge-labels-placement)
                        edge-label-placement.tail))
                  (to-jvector (get-labels out-edge)))
        (incf edges-with-end-labels)))
    (>= edges-with-end-labels 2)))

(defjmethod initialize ((this end-label-sorter) l-graph)
  ;; Give nodes and ports ascending IDs
  (let ((next-element-id 0))
    (jdo (layer (get-layers l-graph))
      (jdo (node (get-nodes layer))
        (setf (.id node) next-element-id)
        (incf next-element-id)
        (jdo (port (get-ports node))
          (setf (.id port) next-element-id)
          (incf next-element-id))))))

;;; Sorting

(defjmethod j-sort ((this end-label-sorter) port port-label-cell)
  (let ((label-groups (create-label-groups this port-label-cell)))
    (jl-sort label-groups end-label-sorter.label-group-comparator)
    ;; Re-add the label cell's labels in the proper order. We directly access the cell's label
    ;; list here, which won't cause the cell to recompute its size
    (let ((port-label-cell-labels (get-labels port-label-cell)))
      (clear port-label-cell-labels)
      (jdo (group label-groups)
        (add-all port-label-cell-labels (.labels group))))))

(defjmethod create-label-groups ((this end-label-sorter) port-label-cell)
  ;; HashMap<LEdge, LabelGroup>: the iteration order of the Java HashMap depends on identity hash
  ;; codes; we keep insertion order (see NOTES.md).
  (let ((edge-to-group-map (new-jmap)))
    ;; Make sure every label is contained in a label group
    (jdo (label (get-labels port-label-cell))
      (let ((edge (get-property label layered-internal-properties.end-label-edge)))
        ;; Create a new label group for the edge if we see it for the first time. Note that this
        ;; works even if the label didn't have an END_LABEL_EDGE property set
        (unless (jm-contains-key edge-to-group-map edge)
          (jm-put edge-to-group-map edge (new-label-group edge)))
        (jl-add (.labels (jm-get edge-to-group-map edge)) label)))
    (jlist-from (jm-values edge-to-group-map))))

(defjctor label-group (edge)
  (jl-add-all (.labels this) (.labels this))
  (setf (.edge this) edge))


