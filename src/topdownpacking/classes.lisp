;;;; topdownpacking/classes.lisp -- org.eclipse.elk.alg.topdownpacking classes.

(in-package #:elk)

(defjinterface grid)
(defjinterface i-node-arranger)

;; GridElkNode implements ElkNode by delegation to a wrapped node.
(defjclass grid-elk-node (grid)
  ((grid nil)
   (rows 0)
   (cols 0)
   (node nil)))

(defjclass left-right-top-down-node-placer (i-layout-phase i-node-arranger) ())
(defjclass bottom-row-equal-whitespace-eliminator (i-layout-phase) ())

(defjclass topdownpacking-layout-provider (abstract-layout-provider i-topdown-layout-provider)
  ((algorithm-assembler (algorithm-assembler.create 'topdown-packing-phases))))
