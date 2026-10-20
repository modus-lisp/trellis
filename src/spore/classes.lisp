;;;; spore/classes.lisp -- org.eclipse.elk.alg.spore classes.

(in-package #:elk)

(defjinterface spore-i-graph-importer)

;; graph.Graph
(defjclass spore-graph (map-property-holder)
  ((vertices (new-jlist))
   (t-edges nil)
   (tree-construction-strategy nil)
   (tree nil)
   (compaction-strategy nil)
   (cost-function nil)          ; ICostFunction = closure (TEdge) -> double
   (preferred-root nil)
   (orthogonal-compaction nil)))

(defjclass spore-elk-graph-importer (spore-i-graph-importer)
  ((graph nil)
   (node-map nil)               ; vertex key -> (Node . ElkNode), see %spore-vkey
   (elk-graph nil)
   (spacing-node-node 0.0)))

(defjclass shrink-tree ()
  ((algorithm-assembler (algorithm-assembler.create 'sp-or-e-phases))
   (algorithm nil)))

(defjclass shrink-tree-layout-provider (abstract-layout-provider)
  ((shrinktree (new-shrink-tree))))

(defjclass overlap-removal-layout-provider (abstract-layout-provider)
  ((algorithm-assembler (algorithm-assembler.create 'sp-or-e-phases))
   (algorithm nil)))

(defjclass delaunay-triangulation-phase (i-layout-phase) ())
(defjclass min-st-phase (i-layout-phase)
  ((node-map (new-jmap 'equal))))
(defjclass max-st-phase (min-st-phase) ())
(defjclass shrink-tree-compaction-phase (i-layout-phase) ())
(defjclass grow-tree-phase (i-layout-phase)
  ((svg nil)
   (root nil)
   (overlaps-existed nil)))
