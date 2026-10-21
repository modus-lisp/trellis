# test-layered-c: blockers

Batch: the layered test classes listed in /tmp/L3 (27 Java files).  Ported files
are in this directory, load order in `files.txt`.  Results are from
`/tmp/layered-c-driver/run.lisp` (not in the repo).

## 1. Not ported: base class not in the tree

Most unit-level tests in the batch extend (or depend on)
`org.eclipse.elk.alg.layered.intermediate.greedyswitch.TestGraphCreator`,
`InLayerEdgeTestGraphCreator` or `NorthSouthEdgeTestGraphCreator`.  None of
these exist in `src/` (no `layered-greedyswitch-test-graph-creator`).  Their
Java sources are in `test/.../intermediate/greedyswitch/`, outside this batch.
These files are NOT ported (118 tests in total):

- p3order/LayerSweepCrossingMinimizerTest.java (37 tests, 1693 lines)
- p3order/LayerSweepHierarchicalTwoSidedGreedySwitchTest.java (6)
- p3order/LayerSweepTypeDeciderTest.java (3)
- p3order/AbstractBarycenterPortDistributorTest.java (6)
- p3order/BarycenterHeuristicTest.java (7)
- p3order/GreedyPortDistributorTest.java (11)
- p3order/counting/AllCrossingsCounterTest.java (14)
- p3order/counting/CrossingsCounterTest.java (14)
- p3order/counting/NorthSouthEdgeAllCrossingsCounterTest.java (20)

Fix: port intermediate/greedyswitch/TestGraphCreator (and its two subclasses)
first, then these.

Note: Issue726Test, Issue734Test, BasicCycleBreakerTest, BasicEdgeRouterTest,
BasicNodePlacementTest and BasicCrossingMinimizationTest also import the
greedyswitch TestGraphCreator, but none of them call its members, so they are
ported without it.  `test-graph-creator.lisp` is the port of the layered
`org.eclipse.elk.alg.layered.TestGraphCreator` (in this batch); nothing in
this batch uses it yet.

## 2. Engine blockers (ported; test fails or errors)

a. **Label sizes are not measured.**  `*label-size-function*` (test-framework
   algorithm-config.lisp) is NIL, so zero-size labels get no size (NOTES.md says
   the same).  Causes:
   - issue701-test: all six tests fail (label overlap checks, expected node
     widths 126 vs 100 etc.).
   - issue871-test test-no-feedback-edges-still-working: n2 x 50, expected 70.
   - model-order-layer-assignment-test test-backward-promotion-no-end-node2:
     n4 x 80, expected 100 (probable, same cause; not confirmed).

b. **ElkReflect has no entry for some property types** (src/runtime/metadata.lisp:209):
   - 'org.eclipse.elk.padding': issue700-test test-edges-routed on
     examples/hierarchy/hierarchicalLayoutMixing.elkt.
   - 'org.eclipse.elk.nodeSize.minimum': issue905-test (both models).
   - basic-crossing-minimization-test: 'org.eclipse.elk.nodeSize.minimum' on
     some models, plus "The value ... is not of type (OR FUNCTION SYMBOL)" on others
     (partial run: 76 errors so far).
   - 'org.eclipse.elk.position': forster-constraint-resolver-test (all four
     experiments error at the first processor run).

c. **CrossHierarchyEdgeComparator passed where a function is needed**
   (src/layered-misc/compound.lisp:194, `(jl-sort edge-segments (new-cross-hierarchy-edge-comparator/1 graph))`):
   "The value #<CROSS-HIERARCHY-EDGE-COMPARATOR> is not of type (OR FUNCTION SYMBOL)".
   Affects issue700-test test-edges-routed on examples/hierarchy/hierarchicalEdges.elkt
   and issue734-test test-orthogonal-edges on 734_nodeEdgeSpacingHierarchical.elkt.

## 3. Framework gaps (test-framework is not mine to edit)

- `assertEquals(double, double, delta)` and `assertThat(x, closeTo(..))` are not in
  test-framework/assert.lisp.  Ported as `tlc-assert-equals-delta` in helpers.lisp.
- Plain JUnit classes (no LayoutTestRunner, no graph resource) have no runner.
  Ported as `defplaintest` / `run-plain-junit` (helpers.lisp).  Used by
  overall-layout-test, issue871-test, binary-indexed-tree-test,
  model-order-layer-assignment-test, layered-spacing-test.
- `graph-test-utils.get-child` and `graph-test-utils.get-port` (varargs as a list)
  are defined locally in helpers.lisp.  If test-common later ports GraphTestUtils,
  delete these two.
- Layout is run inside `with-java-floats` in the plain tests; the framework's
  own layout call is not wrapped (the deflayouttest runs did not show FP traps).

## 4. Runs

- Complete (Summary printed): overall-layout-test, issue871-test,
  binary-indexed-tree-test, model-order-layer-assignment-test, layered-spacing-test,
  issue680-test, issue682-test, issue700-test, issue734-test, issue726-test,
  issue870-test, issue905-test, issue701-test, forster-constraint-resolver-test.
- INCOMPLETE: basic-cycle-breaker-test, basic-layer-assignment-test,
  basic-model-order-test, model-order-barycenter-heuristic-test,
  basic-edge-router-test, basic-crossing-minimization-test,
  basic-node-placement-test.  Each runs every experiment over the 550 ptolemy
  models (hours).  Partial output is in /tmp/layered-c-driver/par-<class>.txt
  while the background processes run.

## 5. Other

- layered-spacing-test: 15 pass, 1 fail.  test-spacing-node-node-between-layers
  gets a gap of 0.0 where Java expects 66; not investigated.
- issue680-test and issue682-test are LayoutTestRunner-style here: the graph
  provider builds the graph and the framework lays it out (the Java tests call
  RecursiveGraphLayoutEngine directly).  issue682 has one experiment per
  direction in place of JUnit's Parameterized runner.
- model-order-layer-assignment-test is data-driven: each Java test is one call
  to %MOLAT-CHECK with its graph and expected coordinates.
- MockRandom (classes.lisp) is a subclass of java-random.  Its ctor seeds 0.
