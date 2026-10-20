;;;; label-management.lisp -- LabelManagementProcessor, LabelSideSelector.

(in-package #:elk)

;;; ------------------------------------------------ LabelManagementProcessor

(defjconst label-management-processor.min-width-port-labels 20.0)
(defjconst label-management-processor.min-width-node-labels 40.0)
(defjconst label-management-processor.min-width-edge-labels 60.0)

(defjctor label-management-processor (center-labels)
  (setf (.center-labels this) center-labels))

(defjmethod process ((this label-management-processor) layered-graph monitor)
  (begin monitor "Label management" 1)
  (let ((label-manager (get-property layered-graph label-management-options.label-manager)))
    (when label-manager
      (let ((edge-label-spacing (jd (get-property layered-graph layered-options.spacing-edge-label)))
            (label-label-spacing (jd (get-property layered-graph layered-options.spacing-label-label))))
        (if (.center-labels this)
            (manage-center-labels this layered-graph label-manager edge-label-spacing label-label-spacing)
            (manage-non-center-labels this layered-graph label-manager label-label-spacing)))))
  (done monitor))

;;; Everything Except Center Edge Labels

(defjmethod manage-non-center-labels ((this label-management-processor) l-graph label-manager label-label-spacing)
  (let ((vertical-layout (is-vertical (get-property l-graph layered-options.direction))))
    (jdo (layer (get-layers l-graph))
      (jdo (node (get-nodes layer))
        (when (eq (get-type node) node-type.normal)
          ;; node labels
          (label-management-processor.do-manage-labels
           label-manager (get-labels node) label-management-processor.min-width-node-labels
           label-label-spacing vertical-layout)
          ;; ports
          (jdo (port (get-ports node))
            (label-management-processor.do-manage-labels
             label-manager (get-labels port) label-management-processor.min-width-port-labels
             label-label-spacing vertical-layout))
          ;; attached comments
          (when (has-property node layered-internal-properties.top-comments)
            (do-manage-attached-comment-labels this label-manager
              (get-property node layered-internal-properties.top-comments)
              label-management-processor.min-width-node-labels vertical-layout))
          (when (has-property node layered-internal-properties.bottom-comments)
            (do-manage-attached-comment-labels this label-manager
              (get-property node layered-internal-properties.bottom-comments)
              label-management-processor.min-width-node-labels vertical-layout)))
        ;; Edges can have head and tail labels that need to be managed as well
        (jdo (edge (get-outgoing-edges node))
          (label-management-processor.do-manage-labels
           label-manager (get-labels edge) label-management-processor.min-width-edge-labels 0.0 vertical-layout))))))

(defjmethod do-manage-attached-comment-labels ((this label-management-processor) label-manager comment-nodes
                                               min-width-node-labels vertical-layout)
  (jdo (comment-node comment-nodes)
    (unless (jl-is-empty (get-labels comment-node))
      (label-management-processor.do-manage-labels
       label-manager (get-labels comment-node) min-width-node-labels 0.0 vertical-layout))))

;;; Center Edge Labels

(defjmethod manage-center-labels ((this label-management-processor) l-graph label-manager
                                  edge-label-spacing label-label-spacing)
  (let ((vertical-layout (is-vertical (get-property l-graph layered-options.direction))))
    (jdo (layer (get-layers l-graph))
      ;; The maximum width is used as the target width for center edge labels
      (let ((max-width (math.max label-management-processor.min-width-edge-labels
                                 (l-graph-util.find-max-non-dummy-node-width layer nil))))
        (jdo (node (get-nodes layer))
          (when (eq (get-type node) node-type.label)
            (let* ((edge (%lib-first (get-connected-edges node)))
                   (edge-thickness (jd (get-property edge layered-options.edge-thickness)))
                   (labels (get-property node layered-internal-properties.represented-labels))
                   (space (label-management-processor.do-manage-labels
                           label-manager labels max-width label-label-spacing vertical-layout)))
              (setf (.x (get-size node)) (.x space)
                    (.y (get-size node)) (+ (.y space) edge-thickness edge-label-spacing)))))))))

;;; Actual Label Management

(defjstatic label-management-processor.do-manage-labels (label-manager labels target-width label-label-spacing vertical-layout)
  (let ((required (new-k-vector))
        (items (to-jvector labels)))
    (when (plusp (length items))
      (jdo (label items)
        (let ((label-size (get-size label))
              (origin (get-property label layered-internal-properties.origin)))
          (when origin
            (let ((new-size (manage-label-size label-manager origin target-width)))
              (when new-size
                (if vertical-layout
                    ;; Our labels are turned 90 degrees
                    (setf (.x label-size) (.y new-size)
                          (.y label-size) (.x new-size))
                    (setf (.x label-size) (.x new-size)
                          (.y label-size) (.y new-size))))))
          (if vertical-layout
              (setf (.x required) (+ (.x required) label-label-spacing (.x (get-size label)))
                    (.y required) (math.max (.y required) (.y label-size)))
              (setf (.x required) (math.max (.x required) (.x label-size))
                    (.y required) (+ (.y required) label-label-spacing (.y (get-size label)))))))
      ;; There is one label-label spacing too much
      (if vertical-layout
          (decf (.x required) label-label-spacing)
          (decf (.y required) label-label-spacing)))
    required))

;;; ------------------------------------------------ LabelSideSelector

(defjmethod process ((this label-side-selector) layered-graph monitor)
  (let ((mode (get-property layered-graph layered-options.edge-labels-side-selection)))
    (begin monitor (jstr "Label side selection (" (name mode) ")") 1)
    (enum-case mode
      (edge-label-side-selection.always-up (same-side this layered-graph label-side.above))
      (edge-label-side-selection.always-down (same-side this layered-graph label-side.below))
      (edge-label-side-selection.direction-up (based-on-direction this layered-graph label-side.above))
      (edge-label-side-selection.direction-down (based-on-direction this layered-graph label-side.below))
      (edge-label-side-selection.smart-up (smart this layered-graph label-side.above))
      (edge-label-side-selection.smart-down (smart this layered-graph label-side.below))))
  (done monitor))

;;; Simple Placement Strategies

(defjmethod same-side ((this label-side-selector) graph label-side)
  (jdo (layer (get-layers graph))
    (jdo (node (get-nodes layer))
      (when (eq (get-type node) node-type.label)
        (apply-label-side this node label-side))
      (jdo (edge (get-outgoing-edges node))
        (apply-label-side this edge label-side)))))

(defjmethod based-on-direction ((this label-side-selector) graph side-for-rightward-edges)
  (jdo (layer (get-layers graph))
    (jdo (node (get-nodes layer))
      (when (eq (get-type node) node-type.label)
        (apply-label-side this node (if (does-edge-point-right this node)
                                        side-for-rightward-edges
                                        (opposite side-for-rightward-edges))))
      (jdo (edge (get-outgoing-edges node))
        (apply-label-side this edge (if (does-edge-point-right this edge)
                                        side-for-rightward-edges
                                        (opposite side-for-rightward-edges)))))))

;;; Smart Placement Strategy

(defjmethod smart ((this label-side-selector) graph default-side)
  (let ((dummy-node-queue (new-jlist)))
    (jdo (layer (get-layers graph))
      (let ((top-group t)
            (label-dummies-in-queue 0))
        (jdo (node (get-nodes layer))
          (let ((type (get-type node)))
            (cond
              ((eq type node-type.label)
               (incf label-dummies-in-queue)
               (jl-add dummy-node-queue node))
              ((eq type node-type.long-edge)
               (jl-add dummy-node-queue node))
              (t
               (when (eq type node-type.normal)
                 (smart-for-regular-node this node default-side))
               (unless (jl-is-empty dummy-node-queue)
                 (smart-for-consecutive-dummy-node-run this dummy-node-queue label-dummies-in-queue
                                                       top-group nil default-side))
               (setf top-group nil
                     label-dummies-in-queue 0)))))
        (unless (jl-is-empty dummy-node-queue)
          (smart-for-consecutive-dummy-node-run this dummy-node-queue label-dummies-in-queue
                                                top-group t default-side))))))

(defjmethod smart-for-consecutive-dummy-node-run ((this label-side-selector) dummy-nodes label-dummy-count
                                                  top-group bottom-group default-side)
  (cond
    ((and top-group
          (or (not bottom-group) (> (length dummy-nodes) 1))
          (= label-dummy-count 1)
          (eq (get-type (jl-peek-first dummy-nodes)) node-type.label))
     (apply-label-side this (jl-peek-first dummy-nodes) label-side.above))
    ((and bottom-group
          (or (not top-group) (> (length dummy-nodes) 1))
          (= label-dummy-count 1)
          (eq (get-type (jl-peek-last dummy-nodes)) node-type.label))
     (apply-label-side this (jl-peek-last dummy-nodes) label-side.below))
    ((= (length dummy-nodes) 2)
     (apply-label-side this (jl-poll-first dummy-nodes) label-side.above)
     (apply-label-side this (jl-poll-first dummy-nodes) label-side.below))
    (t
     (apply-for-dummy-node-run-with-simple-loops this dummy-nodes label-dummy-count default-side)))
  ;; Ensure the list is cleared
  (jl-clear dummy-nodes))

(defjmethod apply-for-dummy-node-run-with-simple-loops ((this label-side-selector) dummy-nodes label-dummy-count default-side)
  (declare (ignore label-dummy-count))
  (let ((label-dummy-run (new-jlist))
        (prev-long-edge-source nil)
        (prev-long-edge-target nil))
    (jdo (current-dummy dummy-nodes)
      (let ((curr-source (get-long-edge-end-node this current-dummy t))
            (curr-target (get-long-edge-end-node this current-dummy nil)))
        (when (or (not (eq prev-long-edge-source curr-source))
                  (not (eq prev-long-edge-target curr-target)))
          ;; We're starting a new run
          (apply-label-sides-to-label-dummy-run this label-dummy-run default-side)
          (setf prev-long-edge-source curr-source
                prev-long-edge-target curr-target))
        (jl-add label-dummy-run current-dummy)))
    (apply-label-sides-to-label-dummy-run this label-dummy-run default-side)))

(defjmethod get-long-edge-end-node ((this label-side-selector) label-dummy source)
  (let ((end-port (get-property label-dummy (if source
                                                layered-internal-properties.long-edge-source
                                                layered-internal-properties.long-edge-target))))
    (if end-port (get-node end-port) nil)))

(defjmethod apply-label-sides-to-label-dummy-run ((this label-side-selector) label-dummy-run default-side)
  (unless (jl-is-empty label-dummy-run)
    (if (= (length label-dummy-run) 2)
        (progn
          (apply-label-side this (jl-get label-dummy-run 0) label-side.above)
          (apply-label-side this (jl-get label-dummy-run 1) label-side.below))
        (jdo (dummy-node label-dummy-run)
          (apply-label-side this dummy-node default-side)))
    (jl-clear label-dummy-run)))

(defjmethod smart-for-regular-node ((this label-side-selector) node default-side)
  (let ((end-label-queue (new-jlist))
        (current-port-side nil))
    ;; This is where we assume that the list of ports is properly sorted
    (jdo (port (get-ports node))
      (unless (eq (get-side port) current-port-side)
        (unless (jl-is-empty end-label-queue)
          (smart-for-regular-node-port-end-labels this end-label-queue current-port-side default-side))
        (jl-clear end-label-queue)
        (setf current-port-side (get-side port)))
      (let ((port-end-labels (end-label-preprocessor.gather-labels port)))
        (when port-end-labels
          (jl-add end-label-queue port-end-labels))))
    (unless (jl-is-empty end-label-queue)
      (smart-for-regular-node-port-end-labels this end-label-queue current-port-side default-side))))

(defjmethod smart-for-regular-node-port-end-labels ((this label-side-selector) end-label-queue port-side default-side)
  (if (= (length end-label-queue) 2)
      (if (or (eq port-side port-side.north) (eq port-side port-side.east))
          (progn
            (apply-label-side this (jl-poll-first end-label-queue) label-side.above)
            (apply-label-side this (jl-poll-first end-label-queue) label-side.below))
          (progn
            (apply-label-side this (jl-poll-first end-label-queue) label-side.below)
            (apply-label-side this (jl-poll-first end-label-queue) label-side.above)))
      (jdo (label-list end-label-queue)
        (apply-label-side this label-list default-side))))

;;; Helper Methods

(defjmethod apply-label-side ((this label-side-selector) target side)
  "applyLabelSide(LNode | LEdge | List<LLabel>, LabelSide)."
  (cond
    ((typep target 'l-node)
     (let ((label-dummy target))
       (when (eq (get-type label-dummy) node-type.label)
         (let ((effective-side (if (is-inline-edge-label label-dummy) label-side.inline side)))
           (set-property label-dummy layered-internal-properties.label-side effective-side)
           ;; If the label is not below the edge, the ports need to be moved
           (unless (eq effective-side label-side.below)
             (let* ((origin-edge (get-property label-dummy layered-internal-properties.origin))
                    (thickness (get-property origin-edge layered-options.edge-thickness))
                    (port-pos 0.0))
               (cond
                 ((eq effective-side label-side.above)
                  (setf port-pos (- (.y (get-size label-dummy)) (math.ceil (/ thickness 2)))))
                 ((eq effective-side label-side.inline)
                  (let ((edge-label-spacing (get-property (get-graph label-dummy) layered-options.spacing-edge-label)))
                    (setf port-pos (/ (math.ceil (- (.y (get-size label-dummy)) edge-label-spacing thickness)) 2.0))
                    (decf (.y (get-size label-dummy)) edge-label-spacing)
                    (decf (.y (get-size label-dummy)) thickness))))
               (jdo (port (get-ports label-dummy))
                 (setf (.y (get-position port)) port-pos))))))))
    ((typep target 'l-edge)
     (jdo (label (get-labels target))
       (set-property label layered-internal-properties.label-side side)))
    (t
     (jdo (label target)
       (set-property label layered-internal-properties.label-side side)))))

(defjmethod does-edge-point-right ((this label-side-selector) x)
  "doesEdgePointRight(LEdge | LNode)."
  (if (typep x 'l-edge)
      (not (get-property x layered-internal-properties.reversed))
      (let ((incoming (%lib-first (get-incoming-edges x)))
            (outgoing (%lib-first (get-outgoing-edges x))))
        (or (does-edge-point-right this incoming) (does-edge-point-right this outgoing)))))
