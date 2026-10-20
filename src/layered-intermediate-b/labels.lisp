;;;; labels.lisp -- LabelAndNodeSizeProcessor, LabelDummyInserter, LabelDummyRemover.

(in-package #:elk)

;;; ------------------------------------------------ LabelAndNodeSizeProcessor

(defjmethod process ((this label-and-node-size-processor) layered-graph monitor)
  (begin monitor "Node and Port Label Placement and Node Sizing" 1)
  (node-dimension-calculation.calculate-label-and-node-sizes
   (l-graph-adapters.adapt layered-graph t t (lambda (node) (eq (get-type node) node-type.normal))))
  ;; If the graph has external ports, labels of external port dummies are treated differently
  (when (contains (get-property layered-graph layered-internal-properties.graph-properties)
                  graph-properties.external-ports)
    (let* ((port-label-placement (get-property layered-graph layered-options.port-labels-placement))
           (place-next-to-port (contains port-label-placement port-label-placement.next-to-port-if-possible))
           (treat-as-group (get-property layered-graph layered-options.port-labels-treat-as-group)))
      (jdo (layer (get-layers layered-graph))
        (jdo (node (jlist-from (get-nodes layer)))
          (when (eq (get-type node) node-type.external-port)
            (place-external-port-dummy-labels this node port-label-placement place-next-to-port treat-as-group))))))
  (done monitor))

(defjmethod place-external-port-dummy-labels ((this label-and-node-size-processor) dummy graph-port-label-placement
                                              place-next-to-port-if-possible treat-as-group)
  (let* ((label-port-spacing-horizontal (get-property dummy layered-options.spacing-label-port-horizontal))
         (label-port-spacing-vertical (get-property dummy layered-options.spacing-label-port-vertical))
         (label-label-spacing (get-property dummy layered-options.spacing-label-label))
         (dummy-size (get-size dummy))
         ;; External port dummies have exactly one port
         (dummy-port (jl-get (get-ports dummy) 0))
         (dummy-port-pos (get-position dummy-port))
         (box (compute-port-label-box this dummy-port label-label-spacing)))
    (when (null box)
      (return-from place-external-port-dummy-labels/4 nil))
    (flet ((label-height ()
             (if treat-as-group
                 (.height box)
                 (.y (get-size (jl-get (get-labels dummy-port) 0))))))
      (cond
        ((contains graph-port-label-placement port-label-placement.inside)
         (enum-case (get-property dummy layered-internal-properties.ext-port-side)
           (port-side.north
            (setf (.x box) (- (/ (- (.x dummy-size) (.width box)) 2) (.x dummy-port-pos))
                  (.y box) label-port-spacing-vertical))
           (port-side.south
            (setf (.x box) (- (/ (- (.x dummy-size) (.width box)) 2) (.x dummy-port-pos))
                  (.y box) (- (- label-port-spacing-vertical) (.height box))))
           (port-side.east
            (if (label-next-to-port this dummy-port t place-next-to-port-if-possible)
                (setf (.y box) (- (/ (- (.y dummy-size) (label-height)) 2) (.y dummy-port-pos)))
                (setf (.y box) (- (+ (.y dummy-size) label-port-spacing-vertical) (.y dummy-port-pos))))
            (setf (.x box) (- (- label-port-spacing-horizontal) (.width box))))
           (port-side.west
            (if (label-next-to-port this dummy-port t place-next-to-port-if-possible)
                (setf (.y box) (- (/ (- (.y dummy-size) (label-height)) 2) (.y dummy-port-pos)))
                (setf (.y box) (- (+ (.y dummy-size) label-port-spacing-vertical) (.y dummy-port-pos))))
            (setf (.x box) label-port-spacing-horizontal))))
        ((contains graph-port-label-placement port-label-placement.outside)
         (enum-case (get-property dummy layered-internal-properties.ext-port-side)
           ((port-side.north port-side.south)
            (setf (.x box) (+ (.x dummy-port-pos) label-port-spacing-horizontal)))
           ((port-side.east port-side.west)
            (if (label-next-to-port this dummy-port nil place-next-to-port-if-possible)
                (setf (.y box) (- (/ (- (.y dummy-size) (label-height)) 2) (.y dummy-port-pos)))
                (setf (.y box) (+ (.y dummy-port-pos) label-port-spacing-vertical))))))))
    ;; Place the labels
    (let ((current-y (.y box)))
      (jdo (label (get-labels dummy-port))
        (let ((label-pos (get-position label)))
          (setf (.x label-pos) (.x box)
                (.y label-pos) current-y)
          (incf current-y (+ (.y (get-size label)) label-label-spacing)))))))

(defjmethod compute-port-label-box ((this label-and-node-size-processor) dummy-port label-label-spacing)
  (if (jl-is-empty (get-labels dummy-port))
      nil
      (let ((result (new-elk-rectangle)))
        (jdo (label (get-labels dummy-port))
          (let ((label-size (get-size label)))
            (setf (.width result) (math.max (.width result) (.x label-size)))
            (incf (.height result) (.y label-size))))
        (incf (.height result) (* (1- (length (get-labels dummy-port))) label-label-spacing))
        result)))

(defjmethod label-next-to-port ((this label-and-node-size-processor) dummy-port inside-labels place-next-to-port-if-possible)
  (cond ((not place-next-to-port-if-possible) nil)
        (inside-labels (and (jl-is-empty (get-incoming-edges dummy-port))
                            (jl-is-empty (get-outgoing-edges dummy-port))))
        (t (not (is-connected-to-external-nodes dummy-port)))))

;;; ------------------------------------------------ LabelDummyInserter

(defparameter label-dummy-inserter.center-label
  (lambda (label) (eq (get-property label layered-options.edge-labels-placement) edge-label-placement.center))
  "Predicate<LLabel>: is the label a center label?")

(defjmethod process ((this label-dummy-inserter) layered-graph monitor)
  (begin monitor "Label dummy insertions" 1)
  (let ((new-dummy-nodes (new-jlist))
        (edge-label-spacing (get-property layered-graph layered-options.spacing-edge-label))
        (label-label-spacing (get-property layered-graph layered-options.spacing-label-label))
        (layout-direction (get-property layered-graph layered-options.direction)))
    (jdo (node (get-layerless-nodes layered-graph))
      (jdo (edge (jlist-from (get-outgoing-edges node)))
        (when (edge-needs-to-be-processed this edge)
          (let* ((thickness (retrieve-thickness this edge))
                 (represented-labels (new-jlist (max 1 (length (get-labels edge)))))
                 (dummy-node (create-label-dummy this layered-graph edge thickness represented-labels))
                 (dummy-size (get-size dummy-node)))
            (jl-add new-dummy-nodes dummy-node)
            (let ((labels (get-labels edge)) (i 0))
              (loop while (< i (length labels))
                    do (let ((label (aref labels i)))
                         (if (eq (get-property label layered-options.edge-labels-placement) edge-label-placement.center)
                             (progn
                               (if (is-vertical layout-direction)
                                   (setf (.x dummy-size) (+ (.x dummy-size) (.x (get-size label)) label-label-spacing)
                                         (.y dummy-size) (math.max (.y dummy-size) (.y (get-size label))))
                                   (setf (.x dummy-size) (math.max (.x dummy-size) (.x (get-size label)))
                                         (.y dummy-size) (+ (.y dummy-size) (.y (get-size label)) label-label-spacing)))
                               (jl-add represented-labels label)
                               (jl-remove-at labels i))
                             (incf i)))))
            ;; The dummy now contains a superfluous label-label spacing and lacks the edge-label spacing
            (if (is-vertical layout-direction)
                (progn (decf (.x dummy-size) label-label-spacing)
                       (incf (.y dummy-size) (+ edge-label-spacing thickness)))
                (incf (.y dummy-size) (+ (- edge-label-spacing label-label-spacing) thickness)))))))
    ;; Add created dummies to graph
    (jl-add-all (get-layerless-nodes layered-graph) new-dummy-nodes))
  (done monitor))

(defjmethod edge-needs-to-be-processed ((this label-dummy-inserter) edge)
  (and (not (eq (get-node (get-source edge)) (get-node (get-target edge))))
       (some label-dummy-inserter.center-label (get-labels edge))))

(defjmethod retrieve-thickness ((this label-dummy-inserter) edge)
  (let ((thickness (get-property edge layered-options.edge-thickness)))
    (when (< thickness 0)
      (setf thickness 0.0)
      (set-property edge layered-options.edge-thickness thickness))
    thickness))

(defjmethod create-label-dummy ((this label-dummy-inserter) layered-graph edge thickness represented-labels)
  (let ((dummy-node (new-l-node layered-graph)))
    (set-type dummy-node node-type.label)
    (set-property dummy-node layered-internal-properties.origin edge)
    (set-property dummy-node layered-internal-properties.represented-labels represented-labels)
    (set-property dummy-node layered-options.port-constraints port-constraints.fixed-pos)
    (set-property dummy-node layered-internal-properties.long-edge-source (get-source edge))
    (set-property dummy-node layered-internal-properties.long-edge-target (get-target edge))
    ;; Actually split the edge
    (long-edge-splitter.split-edge edge dummy-node)
    ;; Place ports at the edge's center
    (let ((port-pos (math.floor (/ thickness 2))))
      (jdo (dummy-port (get-ports dummy-node))
        (setf (.y (get-position dummy-port)) port-pos)))
    dummy-node))

;;; ------------------------------------------------ LabelDummyRemover

(defjmethod process ((this label-dummy-remover) layered-graph monitor)
  (begin monitor "Label dummy removal" 1)
  (let ((edge-label-spacing (get-property layered-graph layered-options.spacing-edge-label))
        (label-label-spacing (get-property layered-graph layered-options.spacing-label-label))
        (layout-direction (get-property layered-graph layered-options.direction)))
    (jdo (layer (get-layers layered-graph))
      (let ((nodes (get-nodes layer)) (i 0))
        (loop while (< i (length nodes))
              do (let ((node (aref nodes i)))
                   (if (eq (get-type node) node-type.label)
                       (let* ((origin-edge (get-property node layered-internal-properties.origin))
                              (thickness (jd (get-property origin-edge layered-options.edge-thickness)))
                              (labels-below-edge (eq (get-property node layered-internal-properties.label-side)
                                                     label-side.below))
                              (curr-label-pos (new-k-vector (get-position node))))
                         (when labels-below-edge
                           (incf (.y curr-label-pos) (+ thickness edge-label-spacing)))
                         (let ((label-space (new-k-vector
                                             (.x (get-size node))
                                             (+ (.y (get-size node))
                                                (if (is-inline-edge-label node) 0 (- (- thickness) edge-label-spacing)))))
                               (represented-labels (get-property node layered-internal-properties.represented-labels)))
                           (if (is-vertical layout-direction)
                               (place-labels-for-vertical-layout this represented-labels curr-label-pos label-label-spacing
                                                                 label-space labels-below-edge layout-direction)
                               (place-labels-for-horizontal-layout this represented-labels curr-label-pos
                                                                   label-label-spacing label-space))
                           ;; Add represented labels back to the original edge
                           (jl-add-all (get-labels origin-edge) represented-labels)
                           (long-edge-joiner.join-at node (eq (get-property layered-graph layered-options.edge-routing)
                                                              edge-routing.polyline))
                           ;; Remove the node
                           (jl-remove-at nodes i)))
                       (incf i)))))))
  (done monitor))

(defjmethod place-labels-for-horizontal-layout ((this label-dummy-remover) labels label-pos label-spacing label-space)
  (jdo (label labels)
    (setf (.x (get-position label)) (+ (.x label-pos) (/ (- (.x label-space) (.x (get-size label))) 2.0))
          (.y (get-position label)) (.y label-pos))
    (incf (.y label-pos) (+ (.y (get-size label)) label-spacing))))

(defjmethod place-labels-for-vertical-layout ((this label-dummy-remover) labels label-pos label-spacing label-space
                                              left-aligned layout-direction)
  (let ((inline (every (lambda (label) (get-property label layered-options.edge-labels-inline)) labels))
        (effective-labels labels))
    (when (eq layout-direction direction.up)
      (setf effective-labels (reverse effective-labels)))
    (jdo (label effective-labels)
      (setf (.x (get-position label)) (.x label-pos))
      (setf (.y (get-position label))
            (cond (inline (+ (.y label-pos) (/ (- (.y label-space) (.y (get-size label))) 2)))
                  (left-aligned (.y label-pos))
                  (t (- (+ (.y label-pos) (.y label-space)) (.y (get-size label))))))
      (incf (.x label-pos) (+ (.x (get-size label)) label-spacing)))))
