;;;; classes.lisp -- class definitions of org.eclipse.elk.alg.layered.p4nodes(.bk)

(in-package #:elk)

;;; ------------------------------------------------------------ p4nodes.bk

(defjclass bk-aligned-layout ()
  ((root nil)
   (block-size nil)
   (align nil)
   (inner-shift nil)
   (sink nil)
   (shift nil)
   (y nil)
   (vdir nil)
   (hdir nil)
   (su nil)
   (od nil)
   (layered-graph nil)
   (spacings nil)))

(defjinterface layered-i-compactor)

(defjclass neighborhood-information ()
  ((node-count 0)
   (layer-index nil)
   (node-index nil)
   (left-neighbors nil)
   (right-neighbors nil)
   (neighbor-comparator nil)))

(defjclass bk-aligner ()
  ((layered-graph nil)
   (ni nil)))

(defjclass class-node ()
  ((class-shift nil)
   (node nil)
   (outgoing (new-jlist))
   (indegree 0)))

(defjclass class-edge ()
  ((separation 0d0)
   (target nil)))

(defjclass bk-compactor (layered-i-compactor)
  ((layered-graph nil)
   (thresh-strategy nil)
   (ni nil)
   (spacings nil)
   (sink-nodes (new-jmap))))

(defjclass threshold-strategy ()
  ((bal nil)
   (ni nil)
   (block-finished (new-jset))
   (post-processables-queue (new-jlist))
   (post-processables-stack (new-jlist))))

(defjclass null-threshold-strategy (threshold-strategy) ())
(defjclass simple-threshold-strategy (threshold-strategy) ())

(defjclass postprocessable ()
  ((free nil)
   (is-root nil)
   (has-edges nil)
   (edge nil)))

(defjclass bk-node-placer (i-layout-phase)
  ((l-graph nil)
   (marked-edges (new-jset))
   (ni nil)
   (produce-balanced-layout nil)))

;;; ------------------------------------------------------------ p4nodes

(defjclass simple-node-placer (i-layout-phase) ())

(defjclass interactive-node-placer (i-layout-phase)
  ((spacings nil)))

(defjclass linear-segment ()
  ((nodes (new-jlist))
   (id 0)
   (index-in-last-layer -1)
   (last-layer -1)
   (deflection 0d0)
   (weight 0)
   (ref-segment nil)
   (node-type nil)))

(defjclass linear-segments-node-placer (i-layout-phase)
  ((linear-segments nil)
   (spacings nil)))

(defjclass node-rep ()
  ((origin nil)
   (head nil)
   (tail nil)
   (is-flexible nil)))

(defjclass edge-rep ()
  ((origin nil)
   (left nil)
   (right nil)))

;; Path extends ArrayList<LEdge>: the elements live in EDGES.
(defjclass path ()
  ((edges (new-jlist))))

(defjclass network-simplex-placer (i-layout-phase)
  ((l-graph nil)
   (spacings nil)
   (n-graph nil)
   (node-reps nil)
   (edge-reps nil)
   (port-map (new-jmap))
   (node-count 0)
   (edge-count 0)
   (node-state nil)
   (two-paths nil)
   (crossing nil)
   (flexible-where-space-permits-edges (new-jset))))
