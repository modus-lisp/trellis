;;;; simple.lisp -- small processors of org.eclipse.elk.alg.layered.intermediate:
;;;; InnermostNodeMarginCalculator, ReversedEdgeRestorer, NodePlacementRepeater,
;;;; PartitionPostprocessor, SelfLoopPostProcessor, SelfLoopPortRestorer,
;;;; SelfLoopRouter, InteractiveExternalPortPositioner, InvertedPortProcessor.

(in-package #:elk)

;;; ------------------------------------------------ InnermostNodeMarginCalculator

(defjmethod process ((this innermost-node-margin-calculator) layered-graph monitor)
  (begin monitor "Node margin calculation" 1)
  ;; Calculate the margins using ELK's utility methods. What is not included in the
  ;; margins yet is space required for self loops and comment boxes.
  (process (exclude-edge-head-tail-labels
            (node-dimension-calculation.get-node-margin-calculator
             (l-graph-adapters.adapt layered-graph nil))))
  (done monitor))

;;; ------------------------------------------------ ReversedEdgeRestorer

(defjmethod process ((this reversed-edge-restorer) layered-graph monitor)
  (begin monitor "Restoring reversed edges" 1)
  (jdo (layer (get-layers layered-graph))
    (jdo (node (get-nodes layer))
      (jdo (port (get-ports node))
        ;; Iterate over a copy of the edges
        (jdo (edge (l-graph-util.to-edge-array (get-outgoing-edges port)))
          (when (get-property edge layered-internal-properties.reversed)
            (j-reverse edge layered-graph nil))))))
  (done monitor))

;;; ------------------------------------------------ NodePlacementRepeater

(defjmethod process ((this node-placement-repeater) graph progress-monitor)
  (begin progress-monitor "Node placement repeater" 1)
  (let ((node-placer (create (get-property graph layered-options.node-placement-network-simplex-node-flexibility-recompute-node-placement))))
    ;; disable node flexibility for second node placement run
    (set-property graph layered-options.node-placement-network-simplex-node-flexibility-default nil)
    (jdo (layer (get-layers graph))
      (jdo (node (get-nodes layer))
        (set-property node layered-options.node-placement-network-simplex-node-flexibility nil)
        (set-property node layered-options.node-size-constraints nil)
        (setf (.y (get-position node)) 0.0)))
    (let ((adapter-graph (l-graph-adapters.adapt graph t t
                                                 (lambda (node) (eq (get-type node) node-type.normal)))))
      ;; recompute port and label positions based on now fixed node sizes
      (node-label-and-size-calculator.process adapter-graph)
      ;; recalculate node margins
      (node-dimension-calculation.calculate-node-margins adapter-graph)
      ;; second node placement run
      (process node-placer graph progress-monitor)))
  (done progress-monitor))

;;; ------------------------------------------------ PartitionPostprocessor

(defjmethod process ((this partition-postprocessor) l-graph monitor)
  (begin monitor "Partition postprocessing" 1)
  ;; Remove all added ports and edges.
  (jdo (layer (get-layers l-graph))
    (let ((nodes (get-nodes layer)) (i 0))
      (loop while (< i (length nodes))
            do (let ((node (aref nodes i)))
                 (if (get-property node layered-internal-properties.partition-dummy)
                     (jl-remove-at nodes i)
                     (incf i))
                 (let ((ports (get-ports node)) (j 0))
                   (loop while (< j (length ports))
                         do (if (get-property (aref ports j) layered-internal-properties.partition-dummy)
                                (jl-remove-at ports j)
                                (incf j))))))))
  (done monitor))

;;; ------------------------------------------------ self loop processors

(defun %lib-self-loop-holders (graph)
  "The self loop holders of all NORMAL nodes that have one, in layer order."
  (let ((result (new-jlist)))
    (jdo (layer (get-layers graph))
      (jdo (l-node (get-nodes layer))
        (when (and (eq (get-type l-node) node-type.normal)
                   (has-property l-node layered-internal-properties.self-loop-holder))
          (jl-add result l-node))))
    result))

(defjmethod process ((this self-loop-post-processor) graph progress-monitor)
  (begin progress-monitor "Self-Loop post-processing" 1)
  (jdo (l-node (%lib-self-loop-holders graph))
    (process-node this l-node))
  (done progress-monitor))

(defjmethod process-node ((this self-loop-post-processor) l-node)
  (let ((sl-holder (get-property l-node layered-internal-properties.self-loop-holder)))
    (jdo (sl-loop (get-sl-hyper-loops sl-holder))
      (jdo (sl-edge (get-sl-edges sl-loop))
        (restore-edge this l-node sl-edge)))
    (jdo (sl-loop (get-sl-hyper-loops sl-holder))
      (when (get-sl-labels sl-loop)
        (apply-placement (get-sl-labels sl-loop) (get-position l-node))))))

(defjmethod restore-edge ((this self-loop-post-processor) l-node sl-edge)
  (let ((l-edge (get-l-edge sl-edge)))
    (set-source l-edge (get-l-port (get-sl-source sl-edge)))
    (set-target l-edge (get-l-port (get-sl-target sl-edge)))
    (offset (get-bend-points l-edge) (get-position l-node))))

(defjmethod process ((this self-loop-port-restorer) graph progress-monitor)
  (begin progress-monitor "Self-Loop ordering" 1)
  (jdo (l-node (%lib-self-loop-holders graph))
    (process-node this (get-property l-node layered-internal-properties.self-loop-holder) progress-monitor))
  (done progress-monitor))

(defjmethod process-node ((this self-loop-port-restorer) sl-holder monitor)
  ;; Restore and order pure self loop ports if they were previously hidden
  (if (are-ports-hidden sl-holder)
      (let ((pc (get-property (get-l-node sl-holder) layered-internal-properties.original-port-constraints)))
        (when (or (eq pc port-constraints.undefined) (eq pc port-constraints.free))
          ;; assign port sides first and then fall through to restore ports
          (assign-port-sides (.port-side-assigner this) sl-holder))
        (when (or (eq pc port-constraints.undefined) (eq pc port-constraints.free)
                  (eq pc port-constraints.fixed-side))
          (compute-self-loop-types this sl-holder)
          (restore-ports (.port-restorer this) sl-holder monitor)))
      ;; Ensure that self loops types are computed
      (compute-self-loop-types this sl-holder)))

(defjmethod compute-self-loop-types ((this self-loop-port-restorer) sl-holder)
  (jdo (sl-loop (get-sl-hyper-loops sl-holder))
    (compute-ports-per-side sl-loop)))

(defjmethod process ((this self-loop-router) graph progress-monitor)
  (begin progress-monitor "Self-Loop routing" 1)
  (let ((router (router-for-graph this graph))
        (label-manager (get-property graph label-management-options.label-manager)))
    (jdo (l-node (%lib-self-loop-holders graph))
      (process-node this (get-property l-node layered-internal-properties.self-loop-holder)
                    label-manager router progress-monitor)))
  (done progress-monitor))

(defjmethod router-for-graph ((this self-loop-router) graph)
  (enum-case (get-property graph layered-options.edge-routing)
    (edge-routing.polyline (new-polyline-self-loop-router))
    (edge-routing.splines (new-spline-self-loop-router))
    (t (new-orthogonal-self-loop-router))))

(defjmethod process-node ((this self-loop-router) sl-holder label-manager sl-router monitor)
  (determine-loop-routes (.routing-director this) sl-holder)
  (place-labels (.label-placer this) sl-holder label-manager monitor)
  (assign-routing-slots (.routing-slot-assigner this) sl-holder)
  (route-self-loops sl-router sl-holder))

;;; ------------------------------------------------ InteractiveExternalPortPositioner

(defjconst interactive-external-port-positioner.arbitrary-spacing 10)

(defjmethod process ((this interactive-external-port-positioner) layered-graph progress-monitor)
  ;; if the graph does not contain any external ports, nothing we can do about it
  (unless (contains (get-property layered-graph layered-internal-properties.graph-properties)
                    layered-graph-properties.external-ports)
    (return-from process/2 nil))
  (let ((spacing interactive-external-port-positioner.arbitrary-spacing))
    ;; find the minimum and maximum x coordinates of the graph
    (jdo (node (get-layerless-nodes layered-graph))
      (when (eq (get-type node) node-type.normal)
        (let ((margins (get-property node layered-options.margins))
              (pos (get-position node)) (size (get-size node)))
          (setf (.min-x this) (math.min (.min-x this) (- (.x pos) (.left margins)))
                (.max-x this) (math.max (.max-x this) (+ (.x pos) (.x size) (.right margins)))
                (.min-y this) (math.min (.min-y this) (- (.y pos) (.top margins)))
                (.max-y this) (math.max (.max-y this) (+ (.y pos) (.y size) (.bottom margins)))))))
    ;; assign reasonable coordinates to external port dummies
    (jdo (node (get-layerless-nodes layered-graph))
      (unless (eq (get-type node) node-type.normal)
        (enum-case (get-type node)
          (node-type.external-port
           (block case
             (let ((lc (get-property node layered-options.layering-layer-constraint))
                   (pos (get-position node)))
               (when (eq lc layer-constraint.first-separate)
                 ;; it's a WEST port
                 (setf (.x pos) (- (.min-x this) spacing))
                 (let ((d (find-y-coordinate this node (lambda (e) (get-node (get-target e))))))
                   (when d (setf (.y pos) d)))
                 (return-from case))
               (when (eq lc layer-constraint.last-separate)
                 ;; it's an EAST port
                 (setf (.x pos) (+ (.max-x this) spacing))
                 (let ((d (find-y-coordinate this node (lambda (e) (get-node (get-source e))))))
                   (when d (setf (.y pos) d)))
                 (return-from case))
               (let ((ilc (get-property node layered-internal-properties.in-layer-constraint)))
                 (when (eq ilc in-layer-constraint.top)
                   (let ((x (find-north-south-port-x-coordinate this node)))
                     (when x (setf (.x pos) (+ x spacing))))
                   (setf (.y pos) (- (.min-y this) spacing))
                   (return-from case))
                 (when (eq ilc in-layer-constraint.bottom)
                   (let ((x (find-north-south-port-x-coordinate this node)))
                     (when x (setf (.x pos) (+ x spacing))))
                   (setf (.y pos) (+ (.max-y this) spacing))
                   (return-from case))))))
          (t (jthrow 'illegal-argument-exception
                     "The node type ~A is not supported by the ~A" (get-type node) (class-name (class-of this)))))))))

(defjmethod find-y-coordinate ((this interactive-external-port-positioner) dummy fun-get-other-node)
  "Optional<Double>: NIL when absent."
  (jdo (e (get-connected-edges dummy))
    (let ((other (funcall fun-get-other-node e)))
      (return-from find-y-coordinate/2
        (+ (.y (get-position other)) (/ (.y (get-size other)) 2)))))
  nil)

(defjmethod find-north-south-port-x-coordinate ((this interactive-external-port-positioner) dummy)
  (let ((port (jl-get (get-ports dummy) 0)))
    (when (and (not (jl-is-empty (get-outgoing-edges port)))
               (not (jl-is-empty (get-incoming-edges port))))
      (jthrow 'illegal-state-exception
              "Interactive layout does not support NORTH/SOUTH ports with incoming _and_ outgoing edges."))
    (unless (jl-is-empty (get-outgoing-edges port))
      (let ((min double.positive-infinity))
        (jdo (e (get-outgoing-edges port))
          (let* ((n (get-node (get-target e)))
                 (margins (get-property n layered-options.margins)))
            (setf min (math.min min (- (.x (get-position n)) (.left margins))))))
        (return-from find-north-south-port-x-coordinate/1 min)))
    (unless (jl-is-empty (get-incoming-edges port))
      (let ((max double.negative-infinity))
        (jdo (e (get-incoming-edges port))
          (let* ((n (get-node (get-source e)))
                 (margins (get-property n layered-options.margins)))
            (setf max (math.max max (+ (.x (get-position n)) (.x (get-size n)) (.right margins))))))
        (return-from find-north-south-port-x-coordinate/1 max)))
    nil))

;;; ------------------------------------------------ InvertedPortProcessor

(defjmethod process ((this inverted-port-processor) layered-graph monitor)
  (begin monitor "Inverted port preprocessing" 1)
  (let ((layers (get-layers layered-graph))
        (current-layer nil)
        (unassigned-nodes (new-jlist)))
    (jdo (layer layers)
      (let ((previous-layer current-layer))
        (setf current-layer layer)
        ;; If the previous layer had unassigned nodes, assign them now and clear the list
        (jdo (node unassigned-nodes) (set-layer node previous-layer))
        (jl-clear unassigned-nodes)
        (jdo (node (get-nodes current-layer))
          (when (and (eq (get-type node) node-type.normal)
                     (is-side-fixed (get-property node layered-options.port-constraints)))
            ;; input ports on the right side
            (jdo (port (jlist-from (get-ports node port-type.input port-side.east)))
              (jdo (edge (l-graph-util.to-edge-array (get-incoming-edges port)))
                (create-east-port-side-dummies this layered-graph port edge unassigned-nodes)))
            ;; output ports on the left side
            (jdo (port (jlist-from (get-ports node port-type.output port-side.west)))
              (jdo (edge (l-graph-util.to-edge-array (get-outgoing-edges port)))
                (create-west-port-side-dummies this layered-graph port edge unassigned-nodes)))))))
    ;; There may be unassigned nodes left
    (jdo (node unassigned-nodes) (set-layer node current-layer)))
  (done monitor))

(defun %lib-make-inverted-dummy (layered-graph edge layer-node-list)
  "Common part of create{East,West}PortSideDummies: returns (values dummy in out)."
  (let ((dummy (new-l-node layered-graph)))
    (set-type dummy node-type.long-edge)
    (set-property dummy layered-internal-properties.origin edge)
    (set-property dummy layered-options.port-constraints port-constraints.fixed-pos)
    (jl-add layer-node-list dummy)
    (let ((dummy-input (new-l-port))
          (dummy-output (new-l-port)))
      (set-node dummy-input dummy)
      (set-side dummy-input port-side.west)
      (set-node dummy-output dummy)
      (set-side dummy-output port-side.east)
      (values dummy dummy-input dummy-output))))

(defjmethod create-east-port-side-dummies ((this inverted-port-processor) layered-graph eastward-port edge layer-node-list)
  ;; Ignore self loops
  (when (eq (get-node (get-source edge)) (get-node eastward-port))
    (return-from create-east-port-side-dummies/4 nil))
  (multiple-value-bind (dummy dummy-input dummy-output)
      (%lib-make-inverted-dummy layered-graph edge layer-node-list)
    ;; Reroute the original edge
    (set-target edge dummy-input)
    ;; Connect the dummy with the original port
    (let ((dummy-edge (new-l-edge)))
      (copy-properties dummy-edge edge)
      (set-property dummy-edge layered-options.junction-points nil)
      (set-source dummy-edge dummy-output)
      (set-target dummy-edge eastward-port)
      (set-long-edge-source-and-target this dummy dummy-input dummy-output eastward-port)
      ;; Move head labels from the old edge over to the new one
      (let ((labels (get-labels edge)) (i 0))
        (loop while (< i (length labels))
              do (let ((label (aref labels i)))
                   (if (eq (get-property label layered-options.edge-labels-placement) edge-label-placement.head)
                       (progn
                         (unless (has-property label layered-internal-properties.end-label-edge)
                           (set-property label layered-internal-properties.end-label-edge edge))
                         (jl-remove-at labels i)
                         (jl-add (get-labels dummy-edge) label))
                       (incf i))))))))

(defjmethod create-west-port-side-dummies ((this inverted-port-processor) layered-graph westward-port edge layer-node-list)
  ;; Ignore self loops
  (when (eq (get-node (get-target edge)) (get-node westward-port))
    (return-from create-west-port-side-dummies/4 nil))
  (multiple-value-bind (dummy dummy-input dummy-output)
      (%lib-make-inverted-dummy layered-graph edge layer-node-list)
    (let ((original-target (get-target edge)))
      (set-target edge dummy-input)
      (let ((dummy-edge (new-l-edge)))
        (copy-properties dummy-edge edge)
        (set-property dummy-edge layered-options.junction-points nil)
        (set-source dummy-edge dummy-output)
        (set-target dummy-edge original-target)
        ;; Move any head labels over to the new dummy edge
        (let ((labels (get-labels edge)) (i 0))
          (loop while (< i (length labels))
                do (let ((label (aref labels i)))
                     (if (eq (get-property label layered-options.edge-labels-placement) edge-label-placement.head)
                         (progn
                           (set-property label layered-internal-properties.end-label-edge edge)
                           (jl-remove-at labels i)
                           (jl-add (get-labels dummy-edge) label))
                         (incf i)))))
        (set-long-edge-source-and-target this dummy dummy-input dummy-output westward-port)))))

(defjmethod set-long-edge-source-and-target ((this inverted-port-processor) long-edge-dummy dummy-input-port dummy-output-port odd-port)
  (let* ((source-port (get-source (jl-get (get-incoming-edges dummy-input-port) 0)))
         (source-node (get-node source-port))
         (target-port (get-target (jl-get (get-outgoing-edges dummy-output-port) 0)))
         (target-node (get-node target-port)))
    (if (eq (get-type source-node) node-type.long-edge)
        (set-property long-edge-dummy layered-internal-properties.long-edge-source
                      (get-property source-node layered-internal-properties.long-edge-source))
        (set-property long-edge-dummy layered-internal-properties.long-edge-source source-port))
    (if (eq (get-type target-node) node-type.long-edge)
        (set-property long-edge-dummy layered-internal-properties.long-edge-target
                      (get-property target-node layered-internal-properties.long-edge-target))
        (set-property long-edge-dummy layered-internal-properties.long-edge-target target-port))))
