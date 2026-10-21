# layered-misc: notes

Packages ported: `org.eclipse.elk.alg.layered.{compound,components,compaction}`
(35 Java files; the Lisp side is split across `classes.lisp`, `compaction.lisp`,
`compaction-components.lisp`).

Deviations / non-obvious points:
- `defjctor layered-c-group ()` etc. redefine the default `NEW-X/0` that
  `defjclass` already defines. Intentional: a Java no-arg constructor that
  differs from the field initforms is written as an explicit `defjctor`. The
  compile-time "redefining NEW-...-/0" warnings are expected for these three
  (LAYERED-C-GROUP, LAYERED-QUADRUPLET, LAYERED-SCANLINE-CONSTRAINT-CALCULATOR).
- CNode's `cNodes` LinkedHashSet is a jlist (insertion order matters; remove and
  re-add moves the element to the end).
- CSG: `Quadruplet`/`CGroup` varargs constructors take one list argument.

Verification status:
- `tools/check-module.lisp layered-misc` cannot reach this module yet: loading
  earlier modules blows the heap (see report). A reduced check (runtime,
  gen-enums, gen-options, layered-options, graph, core-math, core-util, core
  loaded first) compiles all three files with no compile failures; only the
  constructor redefinition warnings above.
- No runtime tests were run (the test suite needs the full pipeline).

## components and compound (added)

Files: `components.lisp` (AbstractGraphPlacer and subclasses, ComponentGroup,
ModelOrderComponentGroup, ComponentsProcessor, ComponentsCompactor and its inner
classes) and `compound.lisp` (CompoundGraphPreprocessor/Postprocessor,
CrossHierarchyEdge, CrossHierarchyEdgeComparator).

Deviations:
- `Set<PortSide>`-keyed multimaps (ComponentGroup.components, CONSTRAINTS,
  MODEL_ORDER_CONSTRAINTS) are EQUAL hash tables keyed by sorted ordinals, value
  `(original-set . jlist)`; key/value order is insertion order (Java's is hash order).
- CrossHierarchyMap is an EQ hash table of jlists; its key order is insertion order.
- `Hullpoints.add` overloads are one `add/2` method dispatching on argument type.
- `ModelOrderComponentGroup(component)` calls `add` and then appends to
  componentOrder again, as Java does, so the first component appears twice in
  `component-order`. Kept faithfully.
