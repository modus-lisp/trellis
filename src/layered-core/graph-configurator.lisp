;;;; graph-configurator.lisp -- org.eclipse.elk.alg.layered.GraphConfigurator
;;;;
;;;; Configures an LGraph's properties and assembles the list of layout
;;;; processors (phases + intermediate processors) from the layout options.

(in-package #:elk)

;;; ------------------------------------------------------------ constants

(defjconst graph-configurator.baseline-processing-configuration
  (let ((c (layout-processor-configuration.create)))
    (add-before c layered-phases.p4-node-placement
                layered-intermediate-processor-strategy.innermost-node-margin-calculator)
    (add-before c layered-phases.p4-node-placement
                layered-intermediate-processor-strategy.label-and-node-size-processor)
    (add-before c layered-phases.p5-edge-routing
                layered-intermediate-processor-strategy.layer-size-and-graph-height-calculator)
    (add-after c layered-phases.p5-edge-routing
               layered-intermediate-processor-strategy.end-label-sorter)
    c))

(defjconst graph-configurator.label-management-additions
  (let ((c (layout-processor-configuration.create)))
    (add-before c layered-phases.p4-node-placement
                layered-intermediate-processor-strategy.center-label-management-processor)
    (add-before c layered-phases.p4-node-placement
                layered-intermediate-processor-strategy.end-node-port-label-management-processor)
    c))

(defjconst graph-configurator.hierarchical-additions
  (let ((c (layout-processor-configuration.create)))
    (add-after c layered-phases.p5-edge-routing
               layered-intermediate-processor-strategy.hierarchical-node-resizer)
    c))

(defjconst graph-configurator.min-edge-spacing 2.0)

;;; ------------------------------------------------------------ graph preprocessing

(defun %graph-configurator-configure-graph-properties (this lgraph)
  (declare (ignore this))
  (let ((edge-spacing (get-property lgraph layered-options.spacing-edge-edge)))
    (when (< edge-spacing graph-configurator.min-edge-spacing)
      ;; at least 2 in order to avoid overlapping edges
      (set-property lgraph layered-options.spacing-edge-edge graph-configurator.min-edge-spacing)))
  (when (eq (get-property lgraph layered-options.direction) core-direction.undefined)
    (set-property lgraph layered-options.direction (l-graph-util.get-direction lgraph)))
  ;; the random number generator based on the random seed option
  (let ((random-seed (get-property lgraph layered-options.random-seed)))
    (if (eql random-seed 0)
        (set-property lgraph layered-internal-properties.random (new-random))
        (set-property lgraph layered-internal-properties.random (new-random random-seed))))
  ;; NOTE: Java tests the Boolean for null; a stored FALSE is indistinguishable
  ;; from "unset" in this port's property holders (NIL removes the entry).
  (when (null (get-property lgraph layered-options.node-placement-favor-straight-edges))
    (set-property lgraph layered-options.node-placement-favor-straight-edges
                  (eq (get-property lgraph layered-options.edge-routing) edge-routing.orthogonal)))
  ;; keep a list of the original port constraints
  (%graph-configurator-copy-port-contraints lgraph)
  ;; pre-calculate spacing information
  (set-property lgraph layered-internal-properties.spacings (new-spacings lgraph)))

(defun %graph-configurator-copy-port-contraints (lgraph)
  (jdo (lnode (get-layerless-nodes lgraph))
    (%graph-configurator-copy-port-constraints lnode))
  (jdo (layer (get-layers lgraph))
    (jdo (lnode (get-nodes layer))
      (%graph-configurator-copy-port-constraints lnode))))

(defun %graph-configurator-copy-port-constraints (node)
  (set-property node layered-internal-properties.original-port-constraints
                (get-property node layered-options.port-constraints))
  (let ((nested-graph (get-nested-graph node)))
    (when nested-graph
      (%graph-configurator-copy-port-contraints nested-graph))))

;;; ------------------------------------------------------------ updating the configuration

(defjmethod prepare-graph-for-layout ((this graph-configurator) lgraph)
  (%graph-configurator-configure-graph-properties this lgraph)
  (let ((aa (.algorithm-assembler this)))
    (reset aa)
    (set-phase aa layered-phases.p1-cycle-breaking
               (get-property lgraph layered-options.cycle-breaking-strategy))
    (set-phase aa layered-phases.p2-layering
               (get-property lgraph layered-options.layering-strategy))
    (set-phase aa layered-phases.p3-node-ordering
               (get-property lgraph layered-options.crossing-minimization-strategy))
    (set-phase aa layered-phases.p4-node-placement
               (get-property lgraph layered-options.node-placement-strategy))
    (set-phase aa layered-phases.p5-edge-routing
               (edge-router-factory.factory-for (get-property lgraph layered-options.edge-routing)))
    (add-processor-configuration
     aa (%graph-configurator-phase-independent-configuration this lgraph))
    (set-property lgraph layered-internal-properties.processors (build aa lgraph))))

(defun %graph-configurator-phase-independent-configuration (this lgraph)
  (declare (ignore this))
  (let ((graph-properties (get-property lgraph layered-internal-properties.graph-properties))
        (configuration (layout-processor-configuration.create-from
                        graph-configurator.baseline-processing-configuration)))
    ;; Hierarchical layout (INCLUDE_CHILDREN has been propagated to all parents with INHERIT).
    (when (eq (get-property lgraph layered-options.hierarchy-handling)
              hierarchy-handling.include-children)
      (add-all configuration graph-configurator.hierarchical-additions))
    ;; Port side processor, put to first slot only if requested
    (if (get-property lgraph layered-options.feedback-edges)
        (add-before configuration layered-phases.p1-cycle-breaking
                    layered-intermediate-processor-strategy.port-side-processor)
        (add-before configuration layered-phases.p3-node-ordering
                    layered-intermediate-processor-strategy.port-side-processor))
    ;; label management additions
    (when (get-property lgraph label-management-options.label-manager)
      (add-all configuration graph-configurator.label-management-additions))
    ;; interactive layout / position and layer ids
    (when (or (get-property lgraph layered-options.interactive-layout)
              (get-property lgraph layered-options.generate-position-and-layer-ids))
      (add-after configuration layered-phases.p5-edge-routing
                 layered-intermediate-processor-strategy.constraints-postprocessor))
    ;; graph transformations for unusual layout directions
    (enum-case (get-property lgraph layered-options.direction)
      ((core-direction.left core-direction.down core-direction.up)
       (add-before configuration layered-phases.p1-cycle-breaking
                   layered-intermediate-processor-strategy.direction-preprocessor)
       (add-after configuration layered-phases.p5-edge-routing
                  layered-intermediate-processor-strategy.direction-postprocessor))
      (t nil))
    ;; Additional dependencies
    (when (contains graph-properties layered-graph-properties.comments)
      (add-before configuration layered-phases.p1-cycle-breaking
                  layered-intermediate-processor-strategy.comment-preprocessor)
      (add-before configuration layered-phases.p4-node-placement
                  layered-intermediate-processor-strategy.comment-node-margin-calculator)
      (add-after configuration layered-phases.p5-edge-routing
                 layered-intermediate-processor-strategy.comment-postprocessor))
    ;; Node promotion
    (unless (eq (get-property lgraph layered-options.layering-node-promotion-strategy)
                node-promotion-strategy.none)
      (add-before configuration layered-phases.p3-node-ordering
                  layered-intermediate-processor-strategy.node-promotion))
    ;; Partitions
    (when (contains graph-properties layered-graph-properties.partitions)
      (add-before configuration layered-phases.p1-cycle-breaking
                  layered-intermediate-processor-strategy.partition-preprocessor)
      (add-before configuration layered-phases.p2-layering
                  layered-intermediate-processor-strategy.partition-midprocessor)
      (add-before configuration layered-phases.p3-node-ordering
                  layered-intermediate-processor-strategy.partition-postprocessor))
    ;; horizontal compaction depends on orthogonal edge routing
    (when (and (not (eq (get-property lgraph layered-options.compaction-post-compaction-strategy)
                        graph-compaction-strategy.none))
               (not (eq (get-property lgraph layered-options.edge-routing) edge-routing.polyline)))
      (add-after configuration layered-phases.p5-edge-routing
                 layered-intermediate-processor-strategy.horizontal-compactor))
    ;; high degree nodes
    (when (get-property lgraph layered-options.high-degree-nodes-treatment)
      (add-before configuration layered-phases.p3-node-ordering
                  layered-intermediate-processor-strategy.high-degree-node-layer-processor))
    ;; semi-interactive crossing minimization
    (when (get-property lgraph layered-options.crossing-minimization-semi-interactive)
      (add-before configuration layered-phases.p3-node-ordering
                  layered-intermediate-processor-strategy.semi-interactive-crossmin-processor))
    ;; greedy switch
    (when (graph-configurator.activate-greedy-switch-for lgraph)
      (let* ((greedy-switch-type
               (if (%graph-configurator-hierarchical-layout-p lgraph)
                   (get-property lgraph layered-options.crossing-minimization-greedy-switch-hierarchical-type)
                   (get-property lgraph layered-options.crossing-minimization-greedy-switch-type)))
             (internal-greedy-type
               (if (eq greedy-switch-type greedy-switch-type.one-sided)
                   layered-intermediate-processor-strategy.one-sided-greedy-switch
                   layered-intermediate-processor-strategy.two-sided-greedy-switch)))
        (add-before configuration layered-phases.p4-node-placement internal-greedy-type)))
    (enum-case (get-property lgraph layered-options.layer-unzipping-strategy)
      (layer-unzipping-strategy.alternating
       (add-before configuration layered-phases.p4-node-placement
                   layered-intermediate-processor-strategy.alternating-layer-unzipper))
      (t nil))
    ;; wrapping of graphs
    (enum-case (get-property lgraph layered-options.wrapping-strategy)
      (wrapping-strategy.single-edge
       (add-before configuration layered-phases.p4-node-placement
                   layered-intermediate-processor-strategy.single-edge-graph-wrapper))
      (wrapping-strategy.multi-edge
       (add-before configuration layered-phases.p3-node-ordering
                   layered-intermediate-processor-strategy.breaking-point-inserter)
       (add-before configuration layered-phases.p4-node-placement
                   layered-intermediate-processor-strategy.breaking-point-processor)
       (add-after configuration layered-phases.p5-edge-routing
                  layered-intermediate-processor-strategy.breaking-point-remover))
      (t nil))
    (unless (eq (get-property lgraph layered-options.consider-model-order-strategy)
                ordering-strategy.none)
      (add-before configuration layered-phases.p3-node-ordering
                  layered-intermediate-processor-strategy.sort-by-input-order-of-model))
    (when (and (find-if (lambda (n) (node-flexibility.get-node-flexibility n))
                        (to-jvector (get-layerless-nodes lgraph)))
               (has-property lgraph
                             layered-options.node-placement-network-simplex-node-flexibility-recompute-node-placement))
      (add-after configuration layered-phases.p4-node-placement
                 layered-intermediate-processor-strategy.node-placement-repeater))
    configuration))

(defjstatic graph-configurator.activate-greedy-switch-for (lgraph)
  (if (%graph-configurator-hierarchical-layout-p lgraph)
      ;; only activated for the root; ElkLayered#reviewAndCorrectHierarchicalProcessors
      ;; sets it for the whole hierarchy
      (and (null (get-parent-node lgraph))
           (not (eq (get-property lgraph layered-options.crossing-minimization-greedy-switch-hierarchical-type)
                    greedy-switch-type.off)))
      (let* ((greedy-switch-type (get-property lgraph layered-options.crossing-minimization-greedy-switch-type))
             (interactive-cross-min
               (or (get-property lgraph layered-options.crossing-minimization-semi-interactive)
                   (eq (get-property lgraph layered-options.crossing-minimization-strategy)
                       crossing-minimization-strategy.interactive)))
             (activation-threshold
               (get-property lgraph layered-options.crossing-minimization-greedy-switch-activation-threshold))
             (graph-size (jsize (get-layerless-nodes lgraph))))
        (and (not interactive-cross-min)
             (not (eq greedy-switch-type greedy-switch-type.off))
             (or (eql activation-threshold 0) (> activation-threshold graph-size))))))

(defun %graph-configurator-hierarchical-layout-p (lgraph)
  (eq (get-property lgraph layered-options.hierarchy-handling) hierarchy-handling.include-children))
