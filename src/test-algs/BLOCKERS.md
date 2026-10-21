# test-algs blockers

Batch: /tmp/algs-tests.txt (alg, topdown, rectpacking, spore, radial, mrtree, force, disco).
Driver: /tmp/algs-driver/run.lisp (`ALGS_CLASS=<class> sbcl --script`).

## Engine / other-module causes

1. **Boolean `false` is stored as unset (NIL)** — `src/core` property holder / `set-property`.
   `(set-property g force-options.separate-connected-components nil)` then
   `(get-property g ...)` returns the default `T`; `has-property` is NIL.
   Java's `setProperty(p, false)` stores `Boolean.FALSE`, which is not null.
   Measured with a probe (/tmp/algs-driver/run-probe.lisp).  Likely affects every
   Boolean option whose default is true.
   - Caused: `force-import-test` `test-do-not-separate-connected-components` FAIL
     (expected 1 graph, got 2; `force-components-processor.split` at
     src/force/force.lisp:127 reads the property).

2. **`src/radial/files.txt` is empty** although src/radial/{classes,compaction,
   optimization,placement,sorting,util}.lisp exist, so `radial-layout-provider` is
   never defined in a run.  Also `new-radial-layout-provider/0` is named in
   jcalls.lisp but has no defjctor.
   - Caused: `center-on-root-test` ERROR 2/2 (`There is no class named
     ELK::RADIAL-LAYOUT-PROVIDER`).  The test is ported (src/test-algs/center-on-root-test.lisp)
     and uses `(make-instance 'radial-layout-provider)` pending a constructor.

3. **`src/mrtree/` has no sources** (files.txt empty, no .lisp).
   - Not ported: `MrTreeGraphSizeTest` (3 parameterised cases x 2 tests, 6 checks total).

4. **`ecore-util.copy` (EMF `Copier`) missing** (also listed in test-framework NOTES).
   - Not ported: `ScanlineOverlapRemovalTest.scanlineTest` (spore).  It also needs
     `SporeMetaDataProvider` registration through `layout-meta-data-service`.

## Test-framework / shared-helper gaps

5. **`PlainJavaInitialization.initializePlainJavaLayout` is not in the port.**
   The generated call `plain-java-initialization.initialize-plain-java-layout` is in
   jcalls.lisp but has no definition in src/.  test-algs/support.lisp defines
   `algs-init` as a no-op.  Tests pass because algorithm metadata is already
   registered at load time, which is a weaker guarantee than Java's.  Owner of
   test-framework should define the real one and `algs-init` should be replaced.

6. **No plain JUnit runner.**  No `deftest` exists in the tree.  test-algs defines
   `defalgtest` (src/test-algs/support.lisp) and runs through /tmp/algs-driver.
   If the coordinator wants a shared runner, this should move to test-framework.

7. **check-module warnings:** `tools/check-module.lisp test-algs` reports
   "GRAPH defined but never used" (3 warnings) and an undefined `ELK::GRAPH`
   from the deflayouttest clauses in alg-framework-tests.lisp, which have
   unused `graph` parameters.  The runtime results are unaffected; the source of the
   undefined reference was not isolated.

## Results (runs on this tree)

| class | file | pass | fail | error | notes |
|---|---|---|---|---|---|
| two-bit-grid-test | disco | 3 | 0 | 0 | |
| force-import-test | force | 2 | 1 | 0 | blocker 1 |
| center-on-root-test | radial | 0 | 0 | 2 | blocker 2 |
| utils-test | spore | 3 | 0 | 0 | |
| compaction-test | rectpacking | 10 | 0 | 0 | |
| padding-test | rectpacking | 4 | 0 | 0 | |
| issue583-test | rectpacking | 1 | 0 | 0 | |
| topdown-layout-test | topdown | 5 | 0 | 0 | |
| topdown-packing-test | topdown | 4 | 0 | 0 | |
| port-placement-calculation-test | topdown | 2 | 0 | 0 | |
| configurator-provider-test | alg.test | 1 | 0 | 0 | |
| configurator-test | alg.test | 1 | 0 | 0 | |
| ignored-test | alg.test | 2 | 0 | 0 | 2 ignored |
| plain-java-initialization-test | alg.test | 12 | 0 | 0 | |
| white-box-test | alg.test | 2 | 0 | 0 | |
| mrtree-graph-size-test | mrtree | — | — | — | not ported (blocker 3) |
| scanline-overlap-removal-test | spore | — | — | — | not ported (blocker 4) |

Framework files in the batch (TestAlgorithm, annotations, config classes,
TestGraph, LayoutTestRunner, TestUtil, GraphTestUtils) are the test-framework
module's job and were not ported here.
