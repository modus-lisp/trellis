# common-nodespacing: port notes

Port of `org.eclipse.elk.alg.common.nodespacing` (26 Java files: top level,
`cellsystem/`, `internal/`, `internal/algorithm/`).

## Layout

| file | Java source |
|---|---|
| classes.lisp | class skeletons for cells, NodeContext, PortContext, NodeMarginCalculator |
| cellsystem.lisp | cellsystem/* (Cell, AtomicCell, ContainerCell, Strip/GridContainerCell, LabelCell) |
| internal.lisp | internal/NodeContext, PortContext, NodeLabelLocation |
| port-placement.lisp | Horizontal/VerticalPortPlacementSizeCalculator, PortPlacementCalculator |
| port-label-placement.lisp | PortLabelPlacementCalculator, LabelPlacer |
| algorithm.lisp | CellSystemConfigurator, InsidePortLabelCellCreator, NodeLabelCellCreator, NodeSizeCalculator, PortContextCreator, NodeLabelAndSizeUtilities |
| nodespacing.lisp | NodeDimensionCalculation, NodeLabelAndSizeCalculator, NodeMarginCalculator |

## Conventions

- Private Java helpers are named `%ns-...` (e.g. `%ns-h-free`, `%ns-v-fixed-pos`)
  rather than mechanically. Public API uses the mechanical names.
- `sumWithGaps` (GridContainerCell, StripContainerCell) is `%ns-sum-active`.
  Semantics match: sum of positive values plus gap between each pair of active values.
- Java `setupTopAndBottomPadding` is folded into `%ns-setup-east-or-west-port-label-cell`
  (same condition and assignments).

## Status

- `tools/check-module.lisp common-nodespacing` compiles all 7 files with no
  warnings of its own. The remaining undefined names belong to other modules
  (ElkUtil, RectangleStripOverlapRemover, ElkShape/ElkNode accessors via
  mechanical names such as GET-SIZE/0).
- Coverage was checked by name only (every non-trivial Java method name has a
  matching Lisp definition, either mechanical or `%ns-` helper). Geometry has not
  been checked against the ELK JUnit tests in
  `test/org.eclipse.elk.alg.common.test/.../nodespacing/`, because those need the
  full layout pipeline. Those tests are the next step.

## Open questions

- Cross-module names not yet confirmed: `rectangle-strip-overlap-remover.create-for-direction`,
  `elk-util.compute-inside-part`, `with-start-coordinate`/`with-gap` (RectangleStripOverlapRemover),
  `remove-overlaps`.
- `new-ns-port-contexts` is referenced in classes.lisp before its definition in internal.lisp.
  It is only called at construction time, so the forward reference is harmless.
