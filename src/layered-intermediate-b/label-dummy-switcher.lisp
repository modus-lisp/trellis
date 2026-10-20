;;;; label-dummy-switcher.lisp -- LabelDummySwitcher and its LabelDummyInfo.

(in-package #:elk)

(defproperty label-dummy-switcher.include-label "edgelabelcenterednessanalysis.includelabel" nil)

(defun %lib-first (coll)
  "coll.iterator().next()"
  (let ((v (to-jvector coll)))
    (if (zerop (length v)) (jthrow 'no-such-element-exception) (elt v 0))))

(defjmethod process ((this label-dummy-switcher) layered-graph monitor)
  (begin monitor "Label dummy switching" 1)
  (let* ((default-placement-strategy
           (get-property layered-graph layered-options.edge-labels-center-label-placement-strategy))
         (label-dummy-infos (progn (assign-ids-to-layers this layered-graph)
                                   (gather-label-dummy-infos this layered-graph default-placement-strategy))))
    ;; If at least one width-based strategy is active, calculate layer widths
    (setf (.layer-widths this) (make-array (length (get-layers layered-graph)) :initial-element 0.0))
    (jdo (strategy (center-edge-label-placement-strategy.values))
      (when (and (uses-label-size-information strategy)
                 (not (jl-is-empty (gethash strategy label-dummy-infos))))
        (calculate-layer-widths this layered-graph)
        (return)))
    ;; Non-width-based strategies first
    (jdo (strategy (center-edge-label-placement-strategy.values))
      (unless (uses-label-size-information strategy)
        (process-strategy this (gethash strategy label-dummy-infos))))
    ;; Now size-based strategies
    (jdo (strategy (center-edge-label-placement-strategy.values))
      (when (uses-label-size-information strategy)
        (process-strategy this (gethash strategy label-dummy-infos)))))
  (setf (.layer-widths this) nil)
  (done monitor))

(defjmethod assign-ids-to-layers ((this label-dummy-switcher) layered-graph)
  (let ((layer-index 0))
    (jdo (layer (get-layers layered-graph))
      (setf (.id layer) layer-index)
      (incf layer-index))))

(defjmethod gather-label-dummy-infos ((this label-dummy-switcher) layered-graph default-placement-strategy)
  (let ((infos (new-jmap)))
    (jdo (strategy (center-edge-label-placement-strategy.values))
      (setf (gethash strategy infos) (new-jlist)))
    (jdo (layer (get-layers layered-graph))
      (jdo (node (get-nodes layer))
        (when (eq (get-type node) node-type.label)
          (let ((dummy-info (new-label-dummy-info node default-placement-strategy)))
            (jl-add (gethash (.placement-strategy dummy-info) infos) dummy-info)))))
    infos))

(defjmethod calculate-layer-widths ((this label-dummy-switcher) layered-graph)
  (jdo (layer (get-layers layered-graph))
    (setf (aref (.layer-widths this) (.id layer))
          (l-graph-util.find-max-non-dummy-node-width layer nil))))

(defjmethod process-strategy ((this label-dummy-switcher) label-dummy-infos)
  (when (jl-is-empty label-dummy-infos)
    (return-from process-strategy/1 nil))
  (if (eq (.placement-strategy (jl-get label-dummy-infos 0))
          center-edge-label-placement-strategy.space-efficient-layer)
      (compute-space-efficient-assignment this label-dummy-infos)
      (jdo (info label-dummy-infos)
        (enum-case (.placement-strategy info)
          (center-edge-label-placement-strategy.center-layer
           (assign-layer this info (find-center-layer-target-id this info)))
          (center-edge-label-placement-strategy.median-layer
           (assign-layer this info (find-median-layer-target-id this info)))
          (center-edge-label-placement-strategy.widest-layer
           (assign-layer this info (find-widest-layer-target-id this info)))
          (center-edge-label-placement-strategy.head-layer
           (set-end-layer-node-alignment this info)
           (assign-layer this info (find-end-layer-target-id this info t)))
          (center-edge-label-placement-strategy.tail-layer
           (set-end-layer-node-alignment this info)
           (assign-layer this info (find-end-layer-target-id this info nil))))
        (update-long-edge-source-label-dummy-info this info))))

;;; Widest Layer

(defjmethod find-widest-layer-target-id ((this label-dummy-switcher) info)
  (let ((widths (.layer-widths this))
        (widest (.leftmost-layer-id info)))
    (loop for index from (1+ widest) to (.rightmost-layer-id info)
          do (when (> (aref widths index) (aref widths widest))
               (setf widest index)))
    widest))

;;; Center Layer

(defjmethod find-center-layer-target-id ((this label-dummy-switcher) info)
  (let* ((sums (compute-layer-width-sums this info))
         (threshold (/ (aref sums (1- (length sums))) 2)))
    (dotimes (i (length sums))
      (when (>= (aref sums i) threshold)
        (return-from find-center-layer-target-id/1 (+ (.leftmost-layer-id info) i))))
    (+ (.leftmost-layer-id info) (length (.left-long-edge-dummies info)))))

(defjmethod compute-layer-width-sums ((this label-dummy-switcher) info)
  (let* ((lgraph (get-graph (.label-dummy info)))
         (edge-node-spacing (* (get-property lgraph layered-options.spacing-edge-node-between-layers) 2))
         (node-node-spacing (get-property lgraph layered-options.spacing-node-node-between-layers))
         (min-space (math.max edge-node-spacing node-node-spacing))
         (widths (.layer-widths this))
         (sums (make-array (total-dummy-count info) :initial-element 0.0))
         (current (- min-space))
         (index 0))
    (flet ((push-dummy (dummy)
             (incf current (+ (aref widths (.id (get-layer dummy))) min-space))
             (setf (aref sums index) current)
             (incf index)))
      (jdo (left (.left-long-edge-dummies info)) (push-dummy left))
      (push-dummy (.label-dummy info))
      (jdo (right (.right-long-edge-dummies info)) (push-dummy right)))
    sums))

;;; Median Layer

(defjmethod find-median-layer-target-id ((this label-dummy-switcher) info)
  (let* ((layers (total-dummy-count info))
         (lower-median (idiv (1- layers) 2)))
    (+ (.leftmost-layer-id info) lower-median)))

;;; End Layer

(defjmethod find-end-layer-target-id ((this label-dummy-switcher) info head-layer)
  (let ((reversed (is-part-of-reversed-edge this info)))
    (if (or (and head-layer (not reversed)) (and (not head-layer) reversed))
        (.rightmost-layer-id info)
        (.leftmost-layer-id info))))

(defjmethod set-end-layer-node-alignment ((this label-dummy-switcher) info)
  (let ((is-head-label (eq (.placement-strategy info) center-edge-label-placement-strategy.head-layer))
        (reversed (is-part-of-reversed-edge this info)))
    (if (or (and is-head-label (not reversed)) (and (not is-head-label) reversed))
        (set-property (.label-dummy info) layered-options.alignment alignment.right)
        (set-property (.label-dummy info) layered-options.alignment alignment.left))))

(defjmethod is-part-of-reversed-edge ((this label-dummy-switcher) info)
  (let ((incoming (%lib-first (get-incoming-edges (.label-dummy info))))
        (outgoing (%lib-first (get-outgoing-edges (.label-dummy info)))))
    (or (get-property incoming layered-internal-properties.reversed)
        (get-property outgoing layered-internal-properties.reversed))))

;;; Space Efficient

(defjmethod compute-space-efficient-assignment ((this label-dummy-switcher) label-dummy-infos)
  (let ((non-trivial (perform-trivial-assignments this label-dummy-infos)))
    (when (jl-is-empty non-trivial)
      (return-from compute-space-efficient-assignment/1 nil))
    ;; Sort descendingly by size
    (jl-sort non-trivial (lambda (i1 i2) (double.compare (.x (get-size (.label-dummy i2)))
                                                         (.x (get-size (.label-dummy i1))))))
    (dotimes (label-index (length non-trivial))
      (assign-layer this (jl-get non-trivial label-index)
                    (find-potentially-widest-layer this non-trivial label-index)))))

(defjmethod perform-trivial-assignments ((this label-dummy-switcher) label-dummy-infos)
  (let ((remaining (new-jlist)))
    (jdo (info label-dummy-infos)
      (cond ((= (.leftmost-layer-id info) (.rightmost-layer-id info))
             (assign-layer this info (.leftmost-layer-id info)))
            ((not (assign-to-wider-layer this info))
             (jl-add remaining info))))
    remaining))

(defjmethod assign-to-wider-layer ((this label-dummy-switcher) info)
  (let* ((dummy-width (.x (get-size (.label-dummy info))))
         (valid-layers (jl-sub-list (get-layers (get-graph (.label-dummy info)))
                                    (.leftmost-layer-id info) (1+ (.rightmost-layer-id info)))))
    (jdo (layer valid-layers)
      (when (>= (.x (get-size layer)) dummy-width)
        (assign-layer this info (.id layer))
        (return-from assign-to-wider-layer/1 t)))
    nil))

(defjmethod find-potentially-widest-layer ((this label-dummy-switcher) label-dummy-infos label-index)
  (let* ((widths (.layer-widths this))
         (label-count (length label-dummy-infos))
         (info (jl-get label-dummy-infos label-index))
         (label-dummy-width (.x (get-size (.label-dummy info))))
         (widest-layer-index (.leftmost-layer-id info))
         (widest-layer-width 0.0))
    (loop for layer from (.leftmost-layer-id info) to (.rightmost-layer-id info)
          do (when (<= label-dummy-width (aref widths layer))
               (return-from find-potentially-widest-layer/2 layer))
             (let ((potential-width (aref widths layer))
                   (largest-unassigned nil))
               (loop for label from (1+ label-index) below label-count
                     do (let ((curr (jl-get label-dummy-infos label)))
                          (when (and (<= (.leftmost-layer-id curr) layer)
                                     (>= (.rightmost-layer-id curr) layer))
                            (setf largest-unassigned curr))))
               (when largest-unassigned
                 (setf potential-width (math.max potential-width
                                                 (.x (get-size (.label-dummy largest-unassigned))))))
               (when (> potential-width widest-layer-width)
                 (setf widest-layer-index layer
                       widest-layer-width potential-width))))
    widest-layer-index))

;;; Swapping Utilities

(defjmethod assign-layer ((this label-dummy-switcher) info target-layer-index)
  (unless (= target-layer-index (+ (.leftmost-layer-id info) (length (.left-long-edge-dummies info))))
    (swap-nodes this (.label-dummy info)
                (ith-dummy-node info (- target-layer-index (.leftmost-layer-id info)))))
  (let ((new-layer-id (.id (get-layer (.label-dummy info))))
        (widths (.layer-widths this)))
    (setf (aref widths new-layer-id)
          (math.max (aref widths new-layer-id) (.x (get-size (.label-dummy info))))))
  (jdo (label (get-property (.label-dummy info) layered-internal-properties.represented-labels))
    (set-property label label-dummy-switcher.include-label t)))

(defjmethod swap-nodes ((this label-dummy-switcher) label-dummy long-edge-dummy)
  (let* ((layer1 (get-layer label-dummy))
         (layer2 (get-layer long-edge-dummy))
         (dummy1-layer-position (jl-index-of (get-nodes layer1) label-dummy))
         (dummy2-layer-position (jl-index-of (get-nodes layer2) long-edge-dummy))
         (input-port1 (%lib-first (get-ports label-dummy port-type.input)))
         (output-port1 (%lib-first (get-ports label-dummy port-type.output)))
         (input-port2 (%lib-first (get-ports long-edge-dummy port-type.input)))
         (output-port2 (%lib-first (get-ports long-edge-dummy port-type.output)))
         (incoming-edges1 (l-graph-util.to-edge-array (get-incoming-edges input-port1)))
         (outgoing-edges1 (l-graph-util.to-edge-array (get-outgoing-edges output-port1)))
         (incoming-edges2 (l-graph-util.to-edge-array (get-incoming-edges input-port2)))
         (outgoing-edges2 (l-graph-util.to-edge-array (get-outgoing-edges output-port2))))
    ;; Put first dummy into second dummy's layer and reroute second dummy's edges to first dummy
    (set-layer label-dummy dummy2-layer-position layer2)
    (jdo (edge incoming-edges2) (set-target edge input-port1))
    (jdo (edge outgoing-edges2) (set-source edge output-port1))
    ;; Put second dummy into first dummy's layer and reroute first dummy's edges to second dummy
    (set-layer long-edge-dummy dummy1-layer-position layer1)
    (jdo (edge incoming-edges1) (set-target edge input-port2))
    (jdo (edge outgoing-edges1) (set-source edge output-port2))))

(defjmethod update-long-edge-source-label-dummy-info ((this label-dummy-switcher) info)
  ;; Predecessors
  (do-update-long-edge-label-dummy-info
      this (.label-dummy info)
    (lambda (node) (get-node (get-source (%lib-first (get-incoming-edges node)))))
    t))

(defjmethod do-update-long-edge-label-dummy-info ((this label-dummy-switcher) label-dummy next-element value)
  (let ((long-edge-dummy (funcall next-element label-dummy)))
    (loop while (eq (get-type long-edge-dummy) node-type.long-edge)
          do (set-property long-edge-dummy layered-internal-properties.long-edge-before-label-dummy value)
             (setf long-edge-dummy (funcall next-element long-edge-dummy)))))

;;; Label Dummy Info

(defjctor label-dummy-info (label-dummy default-placement-strategy)
  (setf (.label-dummy this) label-dummy
        (.placement-strategy this) default-placement-strategy)
  (gather-left-long-edge-dummies this)
  (gather-right-long-edge-dummies this)
  (let ((left (.left-long-edge-dummies this))
        (right (.right-long-edge-dummies this)))
    (setf (.leftmost-layer-id this)
          (if (jl-is-empty left) (.id (get-layer label-dummy)) (.id (get-layer (jl-get left 0)))))
    (setf (.rightmost-layer-id this)
          (if (jl-is-empty right) (.id (get-layer label-dummy)) (.id (get-layer (jl-last right))))))
  ;; Check if the label wants to deviate from the default placement strategy
  (jdo (label (get-property label-dummy layered-internal-properties.represented-labels))
    (when (has-property label layered-options.edge-labels-center-label-placement-strategy)
      (setf (.placement-strategy this)
            (get-property label layered-options.edge-labels-center-label-placement-strategy))
      (return))))

(defjmethod gather-left-long-edge-dummies ((this label-dummy-info))
  (let ((source (.label-dummy this)))
    (loop
      (setf source (get-node (get-source (%lib-first (get-incoming-edges source)))))
      (when (eq (get-type source) node-type.long-edge)
        (jl-add (.left-long-edge-dummies this) source))
      (unless (eq (get-type source) node-type.long-edge) (return)))
    ;; produce a reversed version
    (setf (.left-long-edge-dummies this) (jl-reverse (jl-copy (.left-long-edge-dummies this))))))

(defjmethod gather-right-long-edge-dummies ((this label-dummy-info))
  (let ((target (.label-dummy this)))
    (loop
      (setf target (get-node (get-target (%lib-first (get-outgoing-edges target)))))
      (when (eq (get-type target) node-type.long-edge)
        (jl-add (.right-long-edge-dummies this) target))
      (unless (eq (get-type target) node-type.long-edge) (return)))))

(defjmethod total-dummy-count ((this label-dummy-info))
  (1+ (- (.rightmost-layer-id this) (.leftmost-layer-id this))))

(defjmethod ith-dummy-node ((this label-dummy-info) i)
  (let ((left (.left-long-edge-dummies this)))
    (cond ((< i (length left)) (jl-get left i))
          ((= i (length left)) (.label-dummy this))
          (t (jl-get (.right-long-edge-dummies this) (- i (length left) 1))))))
