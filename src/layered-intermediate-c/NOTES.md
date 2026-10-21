# layered-intermediate-c

Port of `org.eclipse.elk.alg.layered.intermediate` subpackages `loops` (+ `ordering`,
`routing`), `greedyswitch`, `preserveorder`, `unzipping`.

Files (load order, see files.txt): classes.lisp, loops.lisp, loops-ordering.lisp,
loops-routing.lisp, greedyswitch-counting.lisp, greedyswitch.lisp, preserveorder.lisp,
unzipping.lisp.

## Deviations and non-obvious points

- **Comparators are funcallable instances.** `ModelOrderNodeComparator` and
  `ModelOrderPortComparator` are `sb-mop` funcallable standard objects; the constructor
  installs `(lambda (a b) (compare this a b))`, so `(funcall cmp a b)` and `(compare cmp a b)`
  are the same.  `PortSideComparator` is a plain class with `compare`.
- **Overloaded constructors with the same arity.** Both comparators have a public constructor
  taking a `Layer` and one taking `LNode[]`.  One constructor dispatches on `previousLayer`:
  a `layer` is copied to a simple-vector of its nodes, anything else is kept as given.
  The Java private 5-argument constructor is not needed and is not ported.
- **Inner classes.** `BetweenLayerEdgeTwoNodeCrossingsCounter.AdjacencyList` takes its
  enclosing counter as an explicit `outer` argument (it reads the outer `portPositions`).
  `Adjacency` is a plain class.  `AdjacencyList` methods `size`, `isEmpty`, `first`
  (`j-first`), `reset`, `removeFirst` are defined on `adjacency-list`.
- **Java `%` is `rem`.** In `AlternatingLayerUnzipper` the layer index is `(rem target-layer n)`,
  so a `-1` target stays negative as in Java.
- **Java `continue`** in `AlternatingLayerUnzipper.process` is a `(block layer-body ...)`
  with `return-from`.
- **Java `HashSet` iteration order** is replaced by insertion order (`jset`).  The sets are
  only used for membership and for adding into other sets, so the result is the same.
- **Inlined helpers.** `BetweenLayerEdgeTwoNodeCrossingsCounter.freeLayerIsNotFirstLayer` /
  `freeLayerIsNotLastLayer` are inlined in `%blc-set-port-positions-for-neighbouring-layers`.
  `RoutingDirector.determineOneSideLoopRoutes`, `determineTwoSideCornerLoopRoutes`,
  `sortHyperLoopPortLists` and `assignPortIds` are inlined in `determine-loop-routes`
  (loops-routing.lisp), with the same effect.  `LabelPlacer` and `PortRestorer` helpers
  use `%lic-` names.
- The `get-first-incoming-*` / `get-first-outgoing-*` helpers of `ModelOrderNodeComparator`
  are `%mo-first-*` functions.
- The ported comparator `clearTransitiveOrdering` methods are `clear-transitive-ordering`
  on both comparator classes.

## Open / not verified

- The module compiles with `tools/check-module.lisp layered-intermediate-c`.  No test in
  `org.eclipse.elk.alg.layered.test` was run: it needs the full layout pipeline.
- Undefined names referenced from this module that are defined elsewhere, at the time of
  the check: `long-edge-splitter.split-edge` (layered-intermediate-b; its files.txt was empty
  then), `label-management-processor.do-manage-labels` (intermediate-b), `left/0`, `right/0`,
  `is-vertical/is-horizontal` (PortSide/Direction, core: core.lisp had a read error then),
  `splines-math.port-side-to-direction` (core-math/common), `new-nub-spline/3`,
  `get-bezier-cp/0` (p5 NubSpline).  Those in loops-routing.lisp / loops.lisp predate this
  session's additions.
