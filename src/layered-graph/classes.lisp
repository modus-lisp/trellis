;;;; classes.lisp -- class definitions of org.eclipse.elk.alg.layered.graph
;;;; and graph.transform (LGraph and friends, adapters, Tarjan, transformers).

(in-package #:elk)

;;; ------------------------------------------------------------ LGraph core

;; LGraphElement extends MapPropertyHolder; the public field `id`.
(defjclass l-graph-element (map-property-holder)
  ((id 0)))

;; LShape: position and size, both live KVectors.
(defjclass l-shape (l-graph-element)
  ((pos (new-k-vector))
   (size (new-k-vector))))

;; LMargin extends ElkMargin, LPadding extends ElkPadding.
(defjclass l-margin (elk-margin) ())
(defjclass l-padding (elk-padding) ())

(defjclass l-label (l-shape)
  ((text "")))

;; Layer implements Iterable<LNode>.
(defjclass layer (l-graph-element)
  ((owner nil)
   (size (new-k-vector))
   (nodes (new-jlist))))

;; LGraph implements Iterable<Layer>.
(defjclass l-graph (l-graph-element)
  ((size (new-k-vector))
   (padding (new-l-padding))
   (offset (new-k-vector))
   (layerless-nodes (new-jlist))
   (layers (new-jlist))
   (parent-node nil)))

(defjclass l-node (l-shape)
  ((graph nil)
   (layer nil)
   (type node-type.normal)
   (ports (new-jlist 6))
   (labels (new-jlist 2))
   (nested-graph nil)
   (margin (new-l-margin))
   (padding (new-l-padding))
   ;; EnumMap<PortSide, Pair<Integer,Integer>> -> eq hash table side -> (first . second)
   (port-side-indices nil)
   (port-sides-cached nil)))

(defjclass l-port (l-shape)
  ((owner nil)
   (side port-side.undefined)
   (anchor (new-k-vector))
   (explicitly-supplied-port-anchor nil)
   (margin (new-l-margin))
   (labels (new-jlist 2))
   (incoming-edges (new-jlist 4))
   (outgoing-edges (new-jlist 4))
   (connected-to-external-nodes t)))

(defjclass l-edge (l-graph-element)
  ((bend-points (new-k-vector-chain))
   (source nil)
   (target nil)
   (labels (new-jlist 3))))

;;; ------------------------------------------------------------ Tarjan

(defjclass tarjan ()
  ((edges-to-be-reversed nil)
   (index 0)
   (strongly-connected-components nil)
   (stack (new-jlist))
   (node-to-sccid (new-jmap))))

;;; ------------------------------------------------------------ LGraphAdapters

(defjclass abstract-l-shape-adapter (graph-element-adapter)
  ((element nil)))

(defjclass l-graph-adapter (graph-adapter)
  ((element nil)
   (node-adapters nil)
   (transparent-north-south-edges nil)
   (transparent-comment-nodes nil)
   (node-filter nil)))

(defjclass l-node-adapter (abstract-l-shape-adapter node-adapter)
  ((parent-graph-adapter nil)
   (label-adapters nil)
   (port-adapters nil)
   (transparent-north-south-edges nil)))

(defjclass l-port-adapter (abstract-l-shape-adapter port-adapter)
  ((label-adapters nil)
   (incoming-edge-adapters nil)
   (outgoing-edge-adapters nil)
   (transparent-north-south-edges nil)))

(defjclass l-label-adapter (abstract-l-shape-adapter label-adapter) ())

(defjclass l-edge-adapter (edge-adapter)
  ((element nil)
   (label-adapters nil)))

;; LGraphAdapters.PortComparator implements Comparator<LPort>.
(defjclass layered-port-comparator () ())

;;; ------------------------------------------------------------ transform

(defjinterface layered-i-graph-transformer)
(defjclass layered-elk-graph-transformer (layered-i-graph-transformer) ())
(defjclass layered-elk-graph-importer ()
  ((node-and-port-map (new-jmap 'eq))))
(defjclass elk-graph-layout-transferrer () ())
