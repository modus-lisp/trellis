;;;; greedyswitch.lisp -- org.eclipse.elk.alg.layered.intermediate.greedyswitch
;;;; SwitchDecider and GreedySwitchHeuristic.

(in-package #:elk)

;;; ------------------------------------------------------------ SwitchDecider

(defjctor switch-decider (free-layer-index graph crossing-matrix-filler port-positions graph-data one-sided)
  (setf (.crossing-matrix-filler this) crossing-matrix-filler
        (.graph-data this) graph-data)
  (when (>= free-layer-index (length graph))
    (jthrow 'index-out-of-bounds-exception "Greedy SwitchDecider: Free layer not in graph."))
  (setf (.free-layer this) (aref graph free-layer-index))
  (setf (.left-in-layer-counter this) (new-crossings-counter port-positions))
  (init-port-positions-for-in-layer-crossings (.left-in-layer-counter this) (.free-layer this) port-side.west)
  (setf (.right-in-layer-counter this) (new-crossings-counter port-positions))
  (init-port-positions-for-in-layer-crossings (.right-in-layer-counter this) (.free-layer this) port-side.east)
  (setf (.north-south-counter this)
        (new-north-south-edge-neighbouring-node-crossings-counter (.free-layer this)))
  (setf (.count-crossings-caused-by-port-switch this)
        (and (not one-sided)
             (has-parent graph-data)
             (not (dont-sweep-into graph-data))
             (eq (get-type (aref (.free-layer this) 0)) node-type.external-port)))
  (when (.count-crossings-caused-by-port-switch this)
    (%sd-init-parent-crossings-counters this free-layer-index (length graph))))

(defun %sd-init-parent-crossings-counters (this free-layer-index length)
  (let* ((graph-data (.graph-data this))
         (parent-graph-data (parent-graph-data graph-data))
         (parent-node-order (current-node-order parent-graph-data))
         (port-pos (port-positions parent-graph-data)))
    (setf (.parent-cross-counter this) (new-crossings-counter port-pos))
    (let* ((parent-node-layer-pos (.id (get-layer (parent graph-data))))
           (left-layer (if (> parent-node-layer-pos 0)
                           (aref parent-node-order (1- parent-node-layer-pos))
                           (vector)))
           (middle-layer (aref parent-node-order parent-node-layer-pos))
           (right-layer (if (< parent-node-layer-pos (1- (length parent-node-order)))
                            (aref parent-node-order (1+ parent-node-layer-pos))
                            (vector)))
           (right-most-layer (= free-layer-index (1- length))))
      (if right-most-layer
          (init-for-counting-between (.parent-cross-counter this) middle-layer right-layer)
          (init-for-counting-between (.parent-cross-counter this) left-layer middle-layer)))))

(defjmethod notify-of-switch ((this switch-decider) upper-node lower-node)
  (switch-nodes (.left-in-layer-counter this) upper-node lower-node port-side.west)
  (switch-nodes (.right-in-layer-counter this) upper-node lower-node port-side.east)
  (when (.count-crossings-caused-by-port-switch this)
    (let ((upper-port (get-property upper-node layered-internal-properties.origin))
          (lower-port (get-property lower-node layered-internal-properties.origin)))
      (switch-ports (.parent-cross-counter this) upper-port lower-port))))

(defjmethod does-switch-reduce-crossings ((this switch-decider) upper-node-index lower-node-index)
  (when (%sd-constraints-prevent-switch this upper-node-index lower-node-index)
    (return-from does-switch-reduce-crossings/2 nil))
  (let* ((free-layer (.free-layer this))
         (upper-node (aref free-layer upper-node-index))
         (lower-node (aref free-layer lower-node-index))
         (left-inlayer (count-in-layer-crossings-between-nodes-in-both-orders
                        (.left-in-layer-counter this) upper-node lower-node port-side.west))
         (right-inlayer (count-in-layer-crossings-between-nodes-in-both-orders
                         (.right-in-layer-counter this) upper-node lower-node port-side.east))
         (ns-counter (.north-south-counter this)))
    (count-crossings ns-counter upper-node lower-node)
    (let ((upper-lower (+ (get-crossing-matrix-entry (.crossing-matrix-filler this) upper-node lower-node)
                          (get-first left-inlayer) (get-first right-inlayer)
                          (get-upper-lower-crossings ns-counter)))
          (lower-upper (+ (get-crossing-matrix-entry (.crossing-matrix-filler this) lower-node upper-node)
                          (get-second left-inlayer) (get-second right-inlayer)
                          (get-lower-upper-crossings ns-counter))))
      (when (.count-crossings-caused-by-port-switch this)
        (let* ((upper-port (get-property upper-node layered-internal-properties.origin))
               (lower-port (get-property lower-node layered-internal-properties.origin))
               (crossing-numbers (count-crossings-between-ports-in-both-orders
                                  (.parent-cross-counter this) upper-port lower-port)))
          (incf upper-lower (get-first crossing-numbers))
          (incf lower-upper (get-second crossing-numbers))))
      (> upper-lower lower-upper))))

(defun %sd-constraints-prevent-switch (this node-index lower-node-index)
  (let* ((free-layer (.free-layer this))
         (upper-node (aref free-layer node-index))
         (lower-node (aref free-layer lower-node-index)))
    (or (%sd-have-successor-constraints upper-node lower-node)
        (%sd-have-layout-unit-constraints upper-node lower-node)
        (%sd-are-normal-and-north-south-port-dummy upper-node lower-node))))

(defun %sd-have-successor-constraints (upper-node lower-node)
  (let ((constraints (get-property upper-node layered-internal-properties.in-layer-successor-constraints)))
    (and constraints
         (/= (length constraints) 0)
         (contains constraints lower-node))))

(defun %sd-have-layout-unit-constraints (upper-node lower-node)
  (let* ((neither-node-is-long-edge-dummy
           (and (not (eq (get-type upper-node) node-type.long-edge))
                (not (eq (get-type lower-node) node-type.long-edge))))
         (upper-layout-unit (get-property upper-node layered-internal-properties.in-layer-layout-unit))
         (lower-layout-unit (get-property lower-node layered-internal-properties.in-layer-layout-unit))
         (are-in-different-layout-units (not (eq upper-layout-unit lower-layout-unit)))
         (nodes-have-layout-units
           (or (%sd-part-of-multi-node-layout-unit upper-node upper-layout-unit)
               (%sd-part-of-multi-node-layout-unit lower-node lower-layout-unit)))
         (upper-node-has-northern-edges (%sd-has-edges-on-side upper-node port-side.north))
         (lower-node-has-southern-edges (%sd-has-edges-on-side lower-node port-side.south)))
    ;; hotfix for #162, if north or south edges are present, there must be a layout unit
    (setf nodes-have-layout-units
          (or nodes-have-layout-units
              (%sd-has-edges-on-side upper-node port-side.south)
              (%sd-has-edges-on-side lower-node port-side.north)))
    (let ((has-layout-unit-constraint
            (or (and nodes-have-layout-units are-in-different-layout-units)
                (or upper-node-has-northern-edges lower-node-has-southern-edges))))
      (and neither-node-is-long-edge-dummy has-layout-unit-constraint))))

(defun %sd-has-edges-on-side (node side)
  (jdo (port (get-port-side-view node side))
    (when (or (not (null (get-property port layered-internal-properties.port-dummy)))
              (not (is-empty (get-connected-edges port))))
      (return-from %sd-has-edges-on-side t)))
  nil)

(defun %sd-part-of-multi-node-layout-unit (node layout-unit)
  (and layout-unit (not (eq layout-unit node))))

(defun %sd-are-normal-and-north-south-port-dummy (upper-node lower-node)
  (or (and (%sd-is-north-south-port-node upper-node) (%sd-is-normal-node lower-node))
      (and (%sd-is-north-south-port-node lower-node) (%sd-is-normal-node upper-node))))

(defun %sd-is-normal-node (node)
  (eq (get-type node) node-type.normal))

(defun %sd-is-north-south-port-node (node)
  (eq (get-type node) node-type.north-south-port))

;;; ------------------------------------------------------------ GreedySwitchHeuristic

(defjctor greedy-switch-heuristic (greedy-type graph-data)
  (setf (.graph-data this) graph-data
        (.greedy-switch-type this) greedy-type))

(defjmethod minimize-crossings ((this greedy-switch-heuristic) order free-layer-index forward-sweep is-first-sweep)
  (declare (ignore is-first-sweep))
  (%gsh-set-up this order free-layer-index forward-sweep)
  (%gsh-continue-switching-until-no-improvement-in-layer this free-layer-index))

(defjmethod set-first-layer-order ((this greedy-switch-heuristic) current-order is-forward-sweep)
  (let ((start-index (%gsh-start-index is-forward-sweep (length current-order))))
    (%gsh-set-up this current-order start-index is-forward-sweep)
    (%gsh-sweep-downward-in-layer this start-index)))

(defun %gsh-set-up (this order free-layer-index forward-sweep)
  (setf (.current-node-order this) order)
  (let ((side (if forward-sweep crossing-count-side.west crossing-count-side.east)))
    (setf (.switch-decider this) (%gsh-get-new-switch-decider this free-layer-index side))))

(defun %gsh-get-new-switch-decider (this free-layer-index side)
  (let ((crossing-matrix-filler (new-crossing-matrix-filler (.greedy-switch-type this)
                                                            (.current-node-order this)
                                                            free-layer-index side)))
    (new-switch-decider free-layer-index (.current-node-order this) crossing-matrix-filler
                        (.port-positions this) (.graph-data this)
                        (eq (.greedy-switch-type this) cross-min-type.one-sided-greedy-switch))))

(defun %gsh-continue-switching-until-no-improvement-in-layer (this free-layer-index)
  (let ((improved nil)
        (continue-switching nil))
    (loop
      (setf continue-switching (%gsh-sweep-downward-in-layer this free-layer-index))
      (setf improved (or improved continue-switching))
      (unless continue-switching (return)))
    improved))

(defun %gsh-sweep-downward-in-layer (this layer-index)
  (let ((continue-switching nil)
        (length-of-free-layer (length (aref (.current-node-order this) layer-index))))
    (loop for upper-node-index from 0 below (1- length-of-free-layer)
          do (let ((lower-node-index (1+ upper-node-index)))
               (setf continue-switching
                     (or continue-switching
                         (%gsh-switch-if-improves this layer-index upper-node-index lower-node-index)))))
    continue-switching))

(defun %gsh-switch-if-improves (this layer-index upper-node-index lower-node-index)
  (if (does-switch-reduce-crossings (.switch-decider this) upper-node-index lower-node-index)
      (progn
        (%gsh-exchange-nodes this upper-node-index lower-node-index layer-index)
        t)
      nil))

(defun %gsh-exchange-nodes (this index-one index-two layer-index)
  (let ((layer (aref (.current-node-order this) layer-index)))
    (notify-of-switch (.switch-decider this) (aref layer index-one) (aref layer index-two))
    (rotatef (aref layer index-two) (aref layer index-one))))

(defun %gsh-start-index (is-forward-sweep length)
  (if is-forward-sweep 0 (1- length)))

(defjmethod always-improves ((this greedy-switch-heuristic))
  (not (eq (.greedy-switch-type this) cross-min-type.one-sided-greedy-switch)))

(defjmethod is-deterministic ((this greedy-switch-heuristic))
  t)

(defjmethod init-at-port-level ((this greedy-switch-heuristic) l n p node-order)
  (declare (ignore l n p node-order))
  (incf (.n-ports this)))

(defjmethod init-at-layer-level ((this greedy-switch-heuristic) l node-order)
  (setf (.id (get-layer (aref (aref node-order l) 0))) l))

(defjmethod init-after-traversal ((this greedy-switch-heuristic))
  (setf (.port-positions this) (make-array (.n-ports this) :initial-element 0)))
