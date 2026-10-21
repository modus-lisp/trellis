# test-layered-a blockers

## Batch-wide (engine / framework)

- `initialize-plain-java-layout` is undefined in src/ (jcalls maps only
  `plain-java-initialization.initialize-plain-java-layout`, which has no defjstatic).
  Used by helpers.lisp (`%la-setup`) and by the plain-JUnit ports (@Before/@BeforeClass).
  Driver runs stubbed it; re-run once the engine symbol exists.
- `assertEquals(msg, double, double, tolerance)` has no framework function. Worked around
  locally with `%la-assert-equals-delta` in helpers.lisp (not a framework edit).
- Plain JUnit classes (no LayoutTestRunner) have no runner in the framework; they are
  ordinary defuns with a `<class>.run-all` and are not registered with run-elk-tests:
  rectlinear-convex-hull-test, component-group-test, allow-non-flow-ports-to-switch-sides-test
  (Parameterized), alternating-layer-unzipper-test, self-loop-holder-test,
  polyline-self-loop-router-test, routing-director-test, switch-decider-test and
  greedy-switch-processor-test (Parameterized), concrete-sort-by-input-model-test,
  layered-compaction-test.
- Direction is `core-direction.*`; `direction.right` is unbound in this tree.
- Layout calls must run inside `with-java-floats`, or they signal FLOATING-POINT-INVALID-OPERATION.
- `new-basic-layer-assignment-test` and its test-no-* methods (used by node-promotion-test)
  are not defined in this tree's ported test modules.
- Duplicate call-macro warnings for `new-...-test/0` (north-south-edge-test-graph-creator,
  between-layer-edge..., in-layer-edge..., north-south-edge-neighbouring...): a defun with
  the test class's own constructor name is defined in two places within the greedyswitch files.

## Duplicate with the pilot
- long-edge-splitter-test.lisp defines the same deflayouttest name as
  src/test-layered-pilot/long-edge-splitter-test.lisp. The later load wins; both are the same test.

## Not run
- Classes not yet run are listed in the coordinator report.
