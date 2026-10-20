;;;; networksimplex-classes.lisp -- org.eclipse.elk.alg.common.networksimplex classes

(in-package #:elk)

(in-package #:elk)

;;; ------------------------------------------------------------ classes

(defjclass n-graph ()
  ((nodes (new-jlist))))

(defjclass n-node ()
  ((id 0)
   (internal-id 0)
   (origin nil)
   (type "")
   (layer 0)
   ;; ChangeAwareArrayList in Java; the mod-count only served to cache
   ;; getConnectedEdges(), which is rebuilt fresh here (same contents/order).
   (outgoing-edges (new-jlist))
   (incoming-edges (new-jlist))
   (tree-node nil)
   (unknown-cutvalues (new-jlist))))

(defjclass n-node-builder ()
  ((node (make-instance 'n-node))))

(defjclass n-edge ()
  ((id 0)
   (internal-id 0)
   (origin nil)
   (source nil)
   (target nil)
   (weight 0d0)
   (delta 1)
   (tree-edge nil)))

(defjclass n-edge-builder ()
  ((edge (make-instance 'n-edge))))

(defjclass network-simplex ()
  ((previous-layering-node-counts nil)
   (balance nil)
   (iteration-limit integer.max-value)
   (graph nil)
   (edges nil)
   (tree-edges nil)                     ; LinkedHashSet -> insertion-ordered jlist
   (sources nil)
   (edge-visited nil)
   (post-order 0)
   (po-id nil)
   (lowest-po-id nil)
   (cutvalue nil)
   (subtree-nodes-stack nil)))

(defjconst network-simplex.remove-subtrees-thresh 40)
(defjconst network-simplex.fuzzy-st-zero -1d-10)
