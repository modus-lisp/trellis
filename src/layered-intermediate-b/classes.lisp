;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered.intermediate
;;;; (the processors of this module and the private helper classes they own).

(in-package #:elk)

;;; Lisp helper: coll.iterator().next() (used by LabelDummySwitcher and LabelManagementProcessor).
(defun %lib-first (coll)
  "coll.iterator().next()"
  (let ((v (to-jvector coll)))
    (if (zerop (length v)) (jthrow 'no-such-element-exception) (elt v 0))))


;;; InnermostNodeMarginCalculator
(defjclass innermost-node-margin-calculator (i-layout-processor) ())

;;; InteractiveExternalPortPositioner
(defjclass interactive-external-port-positioner (i-layout-processor)
  ((min-x double.positive-infinity)
   (max-x double.negative-infinity)
   (min-y double.positive-infinity)
   (max-y double.negative-infinity)))

;;; InvertedPortProcessor
(defjclass inverted-port-processor (i-layout-processor) ())

;;; LabelAndNodeSizeProcessor
(defjclass label-and-node-size-processor (i-layout-processor) ())

;;; LabelDummyInserter
(defjclass label-dummy-inserter (i-layout-processor) ())

;;; LabelDummyRemover
(defjclass label-dummy-remover (i-layout-processor) ())

;;; LabelDummySwitcher
(defjclass label-dummy-switcher (i-layout-processor)
  ((layer-widths nil)))

;;; LabelDummySwitcher.LabelDummyInfo (private static final class)
(defjclass label-dummy-info ()
  ((label-dummy nil)
   (placement-strategy nil)
   (left-long-edge-dummies (new-jlist))
   (right-long-edge-dummies (new-jlist))
   (leftmost-layer-id 0)
   (rightmost-layer-id 0)))

;;; LabelManagementProcessor
(defjclass label-management-processor (i-layout-processor)
  ((center-labels nil)))

;;; LabelSideSelector
(defjclass label-side-selector (i-layout-processor) ())

;;; LayerConstraintPostprocessor
(defjclass layer-constraint-postprocessor (i-layout-processor) ())

;;; LayerConstraintPreprocessor
(defjclass layer-constraint-preprocessor (i-layout-processor) ())

;;; LayerSizeAndGraphHeightCalculator
(defjclass layer-size-and-graph-height-calculator (i-layout-processor) ())

;;; LongEdgeJoiner
(defjclass long-edge-joiner (i-layout-processor) ())

;;; LongEdgeSplitter
(defjclass long-edge-splitter (i-layout-processor) ())

;;; NodePlacementRepeater
(defjclass node-placement-repeater (i-layout-processor) ())

;;; NodePromotion
(defjclass node-promotion (i-layout-processor)
  ((master-graph nil)
   (nodes-with-incoming-edges nil)
   (nodes nil)
   (current-width nil)
   (current-width-pixel nil)
   (layers nil)
   (degree-diff nil)
   (max-width 0)
   (max-width-pixel 0.0)
   (dummy-node-count 0)
   (max-height 0)
   (node-size-affix 0.0)
   (dummy-size 0.0)
   (promotion-strategy nil)
   (bi-layer-map (new-bi-linked-hash-multi-map))))

;;; NorthSouthPortPostprocessor
(defjclass north-south-port-postprocessor (i-layout-processor) ())

;;; NorthSouthPortPreprocessor
(defjclass north-south-port-preprocessor (i-layout-processor) ())

;;; PartitionMidprocessor
(defjclass partition-midprocessor (i-layout-processor) ())

;;; PartitionPostprocessor
(defjclass partition-postprocessor (i-layout-processor) ())

;;; PartitionPreprocessor
(defjclass partition-preprocessor (i-layout-processor) ())

;;; PortListSorter
(defjclass port-list-sorter (i-layout-processor) ())

;;; PortSideProcessor
(defjclass port-side-processor (i-layout-processor) ())

;;; ReversedEdgeRestorer
(defjclass reversed-edge-restorer (i-layout-processor) ())

;;; SelfLoopPortRestorer
(defjclass self-loop-port-restorer (i-layout-processor)
  ((port-side-assigner (new-port-side-assigner))
   (port-restorer (new-port-restorer))))

;;; SelfLoopPostProcessor
(defjclass self-loop-post-processor (i-layout-processor) ())

;;; SelfLoopPreProcessor
(defjclass self-loop-pre-processor (i-layout-processor) ())

;;; SelfLoopRouter
(defjclass self-loop-router (i-layout-processor)
  ((routing-director (new-routing-director))
   (label-placer (new-layered-label-placer))
   (routing-slot-assigner (new-routing-slot-assigner))))

;;; SemiInteractiveCrossMinProcessor
(defjclass semi-interactive-cross-min-processor (i-layout-processor) ())

;;; SortByInputModelProcessor
(defjclass sort-by-input-model-processor (i-layout-processor) ())
