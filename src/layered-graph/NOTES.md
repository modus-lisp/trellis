# layered-graph

Port of `org.eclipse.elk.alg.layered.graph` (LGraphElement, LShape, LNode, LPort,
LEdge, LLabel, Layer, LGraph, LMargin, LPadding, LGraphUtil, LGraphAdapters,
Tarjan) and `graph.transform` (ElkGraphImporter, ElkGraphLayoutTransferrer,
IGraphTransformer, ElkGraphTransformer).

## Files

- `classes.lisp`, `graph.lisp`, `graph-util.lisp`, `adapters.lisp`, `tarjan.lisp`:
  written in an earlier session (covered every non-transform Java method name;
  a name-diff against the Java sources is clean apart from Java `Iterator`
  implementations, which are `jiterable-items` / `jdo` here).
- `transform-interface.lisp`: IGraphTransformer as `defjgeneric import-graph 1` / `apply-layout 1`.
- `transform-util.lisp`: no Java counterpart. `%lg-any-p` (stream `anyMatch`),
  `%lg-ends-with` (`String.endsWith`), `%lg-root-container` (EcoreUtil.getRootContainer).
- `elk-graph-importer.lisp`: ElkGraphImporter, all methods (instance, `defjmethod`).
- `elk-graph-layout-transferrer.lisp`: ElkGraphLayoutTransferrer.
- `elk-graph-transformer.lisp`: ElkGraphTransformer.

## Naming decisions

- Java private *static* methods are qualified statics:
  `elk-graph-layout-transferrer.apply-node-layout/3`, `.apply-edge-layout/5`,
  `.apply-parent-node-layout/2`, `.represents-node/1`, `.collect-nodes/1`,
  `.calculate-hierarchical-offset-non-mutating/1`, and
  `layered-elk-graph-transformer.get-identifier/1` / `.get-origin-identifier/1`.
  Their instance-private namesakes (`apply-node-layout/2`, `apply-edge-layout/4`,
  `apply-parent-node-layout/1`, `calculate-hierarchical-offset/1`) are plain
  `defjmethod`s. The call macro takes its kind from the first registration of a
  name, so mixing instance and static arities under one name would break it.
- `ElkGraphLayoutTransferrer.ZERO_OFFSET` -> `elk-graph-layout-transferrer.zero-offset`.
- `LayeredOptions/InternalProperties` GraphProperties enum is `layered-graph-properties`
  (classnames.tsv), so constants are `layered-graph-properties.external-ports` etc.
- `LayeredSpacings.withBaseValue(v).apply(g)` -> `(j-apply (layered-spacings.with-base-value v) g)` (CL symbol clash).
- `Set<GraphProperties>` locals are EnumSets; `EnumSet.noneOf` -> `(enum-set.none-of 'layered-graph-properties)`.

## Deviations / Java quirks kept

- `ElkGraphLayoutTransferrer.applyEdgeLayout` (static, non-mutating) calls
  `KVector.diff(sourcePoint, offset);` and discards the result, so the subtraction
  has no effect. Ported as-is (`k-vector.diff` result unused). Java bug, faithful copy.
- The non-mutating junction-point path does `clone` on `getProperty(JUNCTION_POINTS)`
  without a null check, like the Java `.clone()` (NPE in Java, a Lisp error here).
- `ElkGraphImporter.importHierarchicalGraph` tests `needsModelOrderBasedOnParent(elkgraph)`
  with the top-level graph inside the edge loop, as Java does.
- Java `assert` statements are dropped (assertions are off in ELK builds).
- `DebugUtil` / logging: none in this package.

## Cross-module dependencies (called by mechanical name, not yet defined anywhere)

- `ecore-util.get-root-container`: not used; `%lg-root-container` walks `e-container` locally.
- `elk-graph-adapters.adapt/1`, `elk-graph-adapters.adapt-single-node/1`: referenced by
  calculate-minimum-graph-size / create-l-graph; no definition in the port yet
  (the graph-adapters module is not present in the tree).
- `offset-non-mutating` on KVectorChain: Java `KVectorChain.offsetNonMutating` (KVectorChain.java:275)
  is MISSING from `src/core-math/vectors.lisp`; the transferrer calls it.
- `direction.*`, `alignment.*` enum constants: come from `src/core/core.lisp`, which
  currently has an unmatched close paren at line 277, so these are unbound in a check run.
- `label-dummy-switcher.include-label` (layered-intermediate-b), `get-sl-port-map` and
  related SelfLoop accessors, `label-side.label-side` (core options), `l-graph-adapters`
  default sorter used before definition: forward references, resolved at load.

## Checking

`tools/check-module.lisp` as shipped cannot run here: `src/core/files.txt` lists
`classes.lisp` with the extension (the script appends `.lisp`), and SBCL's default
heap is too small (`--dynamic-space-size 6144` is needed). Verified with a patched
copy of the script (kept outside the repo). Result for layered-graph: all files
compile with no warnings of its own; remaining undefined names are the cross-module
ones listed above.
