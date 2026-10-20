;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered.intermediate
;;;; (first part: BiLinkedHashMultiMap .. InLayerConstraintProcessor).

(in-package #:elk)

;;; BiLinkedHashMultiMap<K extends Comparable<K>, V>
(defjclass bi-linked-hash-multi-map ()
  ((multi-map-key-to-values (new-jmap))
   (hash-map-values-to-key (new-jmap))))

(defjclass comment-node-margin-calculator (i-layout-processor) ())
(defjclass comment-postprocessor (i-layout-processor) ())
(defjclass comment-preprocessor (i-layout-processor) ())
(defjclass constraints-postprocessor (i-layout-processor) ())
(defjclass dummy-self-loop-processor (i-layout-processor) ())
(defjclass edge-and-layer-constraint-edge-reverser (i-layout-processor) ())
(defjclass end-label-postprocessor (i-layout-processor) ())
(defjclass end-label-preprocessor (i-layout-processor) ())
(defjclass end-label-sorter (i-layout-processor) ())

;;; EndLabelSorter.LabelGroup
(defjclass label-group ()
  ((labels (new-jlist))
   (edge nil)))

(defjclass final-spline-bendpoints-calculator (i-layout-processor)
  ((edge-edge-spacing 0.0)
   (edge-node-spacing 0.0)
   (spline-routing-mode nil)
   (compaction-strategy nil)))

(defjclass graph-transformer (i-layout-processor)
  ((mode nil)))

(defjclass hierarchical-node-resizing-processor (i-layout-processor) ())
(defjclass hierarchical-port-constraint-processor (i-layout-processor) ())
(defjclass hierarchical-port-dummy-size-processor (i-layout-processor) ())

(defjclass hierarchical-port-orthogonal-edge-router (i-layout-processor)
  ((northern-ext-port-edge-routing-height 0.0)))

(defjclass hierarchical-port-position-processor (i-layout-processor) ())

(defjclass high-degree-node-layering-processor (i-layout-processor)
  ((layered-graph nil)
   (degree-threshold 0)
   (tree-height-threshold 0)))

;;; HighDegreeNodeLayeringProcessor.HighDegreeNodeInformation
(defjclass high-degree-node-information ()
  ((inc-trees-max-height -1)
   (inc-tree-roots nil)
   (out-trees-max-height -1)
   (out-tree-roots nil)))

(defjclass hyperedge-dummy-merger (i-layout-processor) ())

;;; HyperedgeDummyMerger.MergeState
(defjclass merge-state ()
  ((allow-merge nil)
   (same-source nil)
   (same-target nil)))

(defjclass hypernodes-processor (i-layout-processor) ())
(defjclass in-layer-constraint-processor (i-layout-processor) ())
