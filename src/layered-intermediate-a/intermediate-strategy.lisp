;;;; intermediate-strategy.lisp -- IntermediateProcessorStrategy: the intermediate processors listed
;;;; by the earliest slot in which they can sensibly be used, and their factory.

(in-package #:elk)

;; Private helpers of this file are called before their defjmethod is compiled.
(define-jcalls :instance
  create)

(defjmethod create ((this layered-intermediate-processor-strategy))
  "Creates an instance of the layout processor described by this instance."
  (enum-case this
    (layered-intermediate-processor-strategy.breaking-point-inserter (new-breaking-point-inserter))
    (layered-intermediate-processor-strategy.breaking-point-processor (new-breaking-point-processor))
    (layered-intermediate-processor-strategy.breaking-point-remover (new-breaking-point-remover))
    (layered-intermediate-processor-strategy.center-label-management-processor
     (new-label-management-processor t))
    (layered-intermediate-processor-strategy.comment-node-margin-calculator
     (new-comment-node-margin-calculator))
    (layered-intermediate-processor-strategy.comment-postprocessor (new-comment-postprocessor))
    (layered-intermediate-processor-strategy.comment-preprocessor (new-comment-preprocessor))
    (layered-intermediate-processor-strategy.constraints-postprocessor (new-constraints-postprocessor))
    (layered-intermediate-processor-strategy.direction-postprocessor
     (new-graph-transformer layered-intermediate-mode.to-internal-ltr))
    (layered-intermediate-processor-strategy.direction-preprocessor
     (new-graph-transformer layered-intermediate-mode.to-input-direction))
    (layered-intermediate-processor-strategy.edge-and-layer-constraint-edge-reverser
     (new-edge-and-layer-constraint-edge-reverser))
    (layered-intermediate-processor-strategy.end-label-postprocessor (new-end-label-postprocessor))
    (layered-intermediate-processor-strategy.end-label-preprocessor (new-end-label-preprocessor))
    (layered-intermediate-processor-strategy.end-node-port-label-management-processor
     (new-label-management-processor nil))
    (layered-intermediate-processor-strategy.final-spline-bendpoints-calculator
     (new-final-spline-bendpoints-calculator))
    (layered-intermediate-processor-strategy.hierarchical-node-resizer
     (new-hierarchical-node-resizing-processor))
    (layered-intermediate-processor-strategy.hierarchical-port-constraint-processor
     (new-hierarchical-port-constraint-processor))
    (layered-intermediate-processor-strategy.hierarchical-port-dummy-size-processor
     (new-hierarchical-port-dummy-size-processor))
    (layered-intermediate-processor-strategy.hierarchical-port-orthogonal-edge-router
     (new-hierarchical-port-orthogonal-edge-router))
    (layered-intermediate-processor-strategy.hierarchical-port-position-processor
     (new-hierarchical-port-position-processor))
    (layered-intermediate-processor-strategy.high-degree-node-layer-processor
     (new-high-degree-node-layering-processor))
    (layered-intermediate-processor-strategy.horizontal-compactor
     (new-horizontal-graph-compactor))
    (layered-intermediate-processor-strategy.hyperedge-dummy-merger (new-hyperedge-dummy-merger))
    (layered-intermediate-processor-strategy.hypernode-processor (new-hypernodes-processor))
    (layered-intermediate-processor-strategy.in-layer-constraint-processor
     (new-in-layer-constraint-processor))
    (layered-intermediate-processor-strategy.innermost-node-margin-calculator
     (new-innermost-node-margin-calculator))
    (layered-intermediate-processor-strategy.interactive-external-port-positioner
     (new-interactive-external-port-positioner))
    (layered-intermediate-processor-strategy.inverted-port-processor (new-inverted-port-processor))
    (layered-intermediate-processor-strategy.label-and-node-size-processor
     (new-label-and-node-size-processor))
    (layered-intermediate-processor-strategy.label-dummy-inserter (new-label-dummy-inserter))
    (layered-intermediate-processor-strategy.label-dummy-remover (new-label-dummy-remover))
    (layered-intermediate-processor-strategy.label-dummy-switcher (new-label-dummy-switcher))
    (layered-intermediate-processor-strategy.label-side-selector (new-label-side-selector))
    (layered-intermediate-processor-strategy.end-label-sorter (new-end-label-sorter))
    (layered-intermediate-processor-strategy.layer-constraint-postprocessor
     (new-layer-constraint-postprocessor))
    (layered-intermediate-processor-strategy.layer-constraint-preprocessor
     (new-layer-constraint-preprocessor))
    (layered-intermediate-processor-strategy.layer-size-and-graph-height-calculator
     (new-layer-size-and-graph-height-calculator))
    (layered-intermediate-processor-strategy.long-edge-joiner (new-long-edge-joiner))
    (layered-intermediate-processor-strategy.long-edge-splitter (new-long-edge-splitter))
    (layered-intermediate-processor-strategy.node-placement-repeater (new-node-placement-repeater))
    (layered-intermediate-processor-strategy.node-promotion (new-node-promotion))
    (layered-intermediate-processor-strategy.north-south-port-postprocessor
     (new-north-south-port-postprocessor))
    (layered-intermediate-processor-strategy.north-south-port-preprocessor
     (new-north-south-port-preprocessor))
    (layered-intermediate-processor-strategy.one-sided-greedy-switch
     (new-layer-sweep-crossing-minimizer cross-min-type.one-sided-greedy-switch))
    (layered-intermediate-processor-strategy.partition-midprocessor (new-partition-midprocessor))
    (layered-intermediate-processor-strategy.partition-postprocessor (new-partition-postprocessor))
    (layered-intermediate-processor-strategy.partition-preprocessor (new-partition-preprocessor))
    (layered-intermediate-processor-strategy.port-list-sorter (new-port-list-sorter))
    (layered-intermediate-processor-strategy.port-side-processor (new-port-side-processor))
    (layered-intermediate-processor-strategy.reversed-edge-restorer (new-reversed-edge-restorer))
    (layered-intermediate-processor-strategy.self-loop-preprocessor (new-self-loop-pre-processor))
    (layered-intermediate-processor-strategy.self-loop-port-restorer (new-self-loop-port-restorer))
    (layered-intermediate-processor-strategy.alternating-layer-unzipper
     (new-alternating-layer-unzipper))
    (layered-intermediate-processor-strategy.self-loop-postprocessor (new-self-loop-post-processor))
    (layered-intermediate-processor-strategy.self-loop-router (new-self-loop-router))
    (layered-intermediate-processor-strategy.semi-interactive-crossmin-processor
     (new-semi-interactive-cross-min-processor))
    (layered-intermediate-processor-strategy.single-edge-graph-wrapper
     (new-single-edge-graph-wrapper))
    (layered-intermediate-processor-strategy.sort-by-input-order-of-model
     (new-sort-by-input-model-processor))
    (layered-intermediate-processor-strategy.two-sided-greedy-switch
     (new-layer-sweep-crossing-minimizer cross-min-type.two-sided-greedy-switch))
    (t (jthrow 'illegal-argument-exception
               (jstr "No implementation is available for the layout processor " (name this))))))
