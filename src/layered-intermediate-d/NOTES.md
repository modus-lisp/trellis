# layered-intermediate-d

Port of `org.eclipse.elk.alg.layered.intermediate.wrapping` (9 files) and
`org.eclipse.elk.alg.layered.intermediate.compaction` (5 files).

Files (load order, see files.txt): `classes.lisp`, `wrapping.lisp`, `compaction.lisp`.

Java classes ported: 13 classes + 1 interface (`ICutIndexCalculator`, as the
`i-cut-index-calculator` generic functions). Wrapping: BreakingPointInserter,
BreakingPointProcessor, BreakingPointRemover, SingleEdgeGraphWrapper, CuttingUtils,
GraphStats, ARDCutIndexHeuristic, MSDCutIndexHeuristic, ICutIndexCalculator
(the inner `BPInfo` and `Cut` classes are ported too; `manual-cut-index-calculator`
is defined in classes.lisp but has no Java file in this package). Compaction:
HorizontalGraphCompactor (with its anonymous
ISpacingsHandler as `%hgc-special-spacings-handler`), LGraphToCGraphTransformer,
NetworkSimplexCompaction, EdgeAwareScanlineConstraintCalculation, VerticalSegment.

Every Java method in the 14 files has a Lisp definition under its mechanical name
(checked by name-level audit; the two apparent misses were kebab-case spellings
`get-l-node-or-null` and `%lgct-vertical-segment-to-c-node`).

## Deviations and non-obvious points

- **Direction enum.** The compaction Java imports `org.eclipse.elk.core.options.Direction`
  (`UNDEFINED, RIGHT, LEFT, DOWN, UP`), whose generated name is `core-direction`
  (tools/classnames.tsv). Constants are spelled `core-direction.right` etc. Not
  `direction.*`, which nothing defines. (Fixed in this pass; 13 uses.)
- **Duplicate `new-horizontal-graph-compactor/0` warning.** `defjclass
  horizontal-graph-compactor` generates a zero-argument maker, and the
  `defjctor horizontal-graph-compactor ()` with a body redefines it. Intended:
  the body sets the anonymous spacings handler, which the generated maker does not.
- `horizontal-graph-compactor.network-simplex-compaction` is created lazily
  (`lazy-network-simplex-compaction`) because its superclass is in the common module.
- Inner class `%hgc-special-spacings-handler` holds an `outer` reference because Java's
  anonymous class reads `HorizontalGraphCompactor.this.lGraph` at call time.

## Verification status

`tools/check-module.lisp layered-intermediate-d` does NOT complete in the current tree.
A scratch copy (/tmp/check-lid.lisp, same logic plus a fixed module-file path and
`--dynamic-space-size 8192`) was used instead. Results for this module:

- classes.lisp, wrapping.lisp: compile with no warnings.
- compaction.lisp: no compile errors. One benign warning (duplicate constructor, above).
- No undefined names that are defined in this module's own files.

Blockers outside this module (not fixed here):

1. `tools/check-module.lisp` default SBCL heap (1 GB) is too small. It needs
   `--dynamic-space-size 8192`. Its `module-files` also turns `classes.lisp` into
   `classes.lisp.lisp` for `src/core/files.txt`, which entries carry the extension.
2. Earlier modules have read/compile errors: `runtime/metadata.lisp` (unmatched `)`),
   `core-util/layout-providers.lisp` line 73, `core/core.lisp` line 277,
   `common/root.lisp` line 409. Loading `layered-p4/p4nodes.lisp` and compiling
   `layered-intermediate-a` both exhaust an 8 GB heap, so the full earlier-module
   chain cannot be loaded.

## Cross-module assumptions (undefined in the tree at check time)

Owned by common, not defined under mechanical names:
`change-direction/1` (only `%ocomp-change-direction` exists), `set-lock-function/1`,
`set-spacings-handler/1`, `set-constraint-algorithm/1`, `set-compaction-algorithm/1`,
`to-string-delegate/1`, `nodes/1`, `hitbox/1`, `master/1`, `finish/0`, `compact/0`,
`is-horizontal/0`, `new-common-c-graph/1`, `new-common-one-dimensional-compactor/1`,
`common-c-node.of/0`, `common-c-group.of/0`, `common-one-dimensional-compactor.quadratic-constraints`,
`add-c-node/1`, `apply-or/1` and `.up`/`.down` on the common Quadruplet, and
`port-side.sides-north-south`.

Owned by other layered modules (skipped in the scratch load):
`.is-straight`, `.initial-segment`, `.last-segment` (SplineSegment, p5),
`.target-node` / `.source-node` (edges), `.inverse-order` setter, `.bounding-box`,
`long-edge-joiner.join-at/2` (intermediate-b).
