# rectpacking port notes

Files (load order in files.txt): classes, options, util-blocks, util-drawing,
intermediate, p1-width-approximation, p2-compaction, p2-packing,
p3-whitespace-elimination, layout-provider.

Coverage: every Java class of org.eclipse.elk.alg.rectpacking is ported.
Enums with generated constants (RectPackingLayoutPhases, OptimizationGoal,
WidthApproximationStrategy, PackingStrategy, WhiteSpaceEliminationStrategy,
DrawingDataDescriptor) keep their `create` methods here; options are generated.

Deviations and notes:
- Java `Pair<Boolean,Boolean>` from Compaction.compact is `new-pair/2`.
- Compactor's private helpers (`clone`, `copyPosition`, `copyRowWidthChangeValues`,
  `configureSecondIteration`) are instance methods on COMPACTOR, as in Java.
- `clone` uses ElkGraphFactory via `(create-elk-node elk-graph-factory.e-instance)`.
- compact()'s `continue`/`break` are named RETURN-FROM blocks (COMPACT-CONTINUE,
  COMPACT-LOOP); `blockId--` before `continue` is kept, the loop increments after.
- `||`/`|=` semantics preserved: `somethingWasChanged |= absorbBlocks(...)` always
  calls absorbBlocks.
- Progress logging (logGraph / isLoggingEnabled) is dropped.
- Java's `System.err.println` in Compaction.compact is dropped.
- `equals` on RectRow/Block is identity (eq), as Java does not override it.
- The module's pre-existing `intermediate.lisp` used `get-property/2`,
  `set-property/3`, `has-property/2` (wrong arities); corrected to the mechanical
  names `get-property` / `set-property` / `has-property`.
- `InteractiveRectPackingGraphVisitor.setInteractiveOptions` keeps Java's reversed
  `ALGORITHM_ID.endsWith(algorithm)` test.

Verification status:
- Compile check (tools/check-module.lisp loads ALL earlier modules and crashes
  before reaching rectpacking at HEAD: core.lisp and layered-graph/elk-graph-importer.lisp
  have unmatched parens, and the full load exhausts the heap). A private copy of
  the checker that loads only runtime..common (10 modules) compiles the module with
  0 warnings. Undefined names left: ELK-GRAPH-ADAPTERS.ADAPT/1,
  ELK-GRAPH-ADAPTERS.ADAPT-SINGLE-NODE/1 (layered-graph, not loaded) and
  INITIAL-PLACEMENT.PLACE-RECT-IN-BLOCK/5 (defined in p2-packing, called from
  p2-compaction which loads first; resolved at load time).
- Tests (test/org.eclipse.elk.alg.rectpacking.test: CompactionTest, PaddingTest,
  Issue583Test) were NOT run: the whole system cannot be loaded yet.
