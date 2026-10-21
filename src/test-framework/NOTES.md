# test-framework: notes

Port of org.eclipse.elk.alg.test.framework (LayoutTestRunner, ExperimentRunner,
ExperimentStatement, ExperimentalObject, WhiteboxRecorder, TestAlgorithm,
TestConfiguration + subclasses, TestGraph + sources, io/* resource paths and
filters, TestUtil) and the Assert methods tests use.  JUnit itself is replaced
by `run-elk-tests`.

## Usage
    (deflayouttest my-test
      (:algorithm "org.eclipse.elk.layered")
      (:default-configuration :nodes t)
      (:graph-resources (list (model-resource-path "tests/**")))
      (:test check-something () (root) (assert-true (plusp (length (get-children root)))))
      (:after-processor check-layering (:processors (network-simplex-layerer)
                                        :fail-if-not-executed t) (graph) ...))
    (run-elk-tests 'my-test)       ; or (run-elk-tests :all)

Clauses and option keywords are documented in registry.lisp.

## Calling convention (important)
JUnit's static Assert methods are registered in jcalls.lisp as INSTANCE calls,
so `(assert-true c)` expands to ASSERT-TRUE/0 (suffix = args minus one).
assert.lisp defines the functions under those suffixes.  Same for `fail`.

## Deviations from Java
- Java `Class<?>` processor annotations are class-name symbols; whitebox
  matching is `(eq (type-of processor) sym)` (exact getClass() equality).
- @Test/@OnlyOnRootNode: Java's ensureNotAnnotatedWith compares
  `annotation.getClass()` and so never fires; the port reports the intended
  error.
- Labels of zero size are NOT measured: ComputeLabelSize needs AWT font metrics.
  `*label-size-function*` is a seam (default NIL).  Test results that depend on
  label sizes will differ from Java until a measurer is supplied.
- FileNameFilter takes a predicate on the file name (no regex engine).
- Directory listings are sorted by name (Java's listFiles order is undefined).
- ModelResourcePath reads $MODELS_REPO, defaulting to /tmp/elk-src/models.
- Whitebox results: one result per whitebox test per experiment; failures from
  several processor invocations are joined into one message (JUnit reports each).
- Test "instances" do not exist: test bodies and providers are closures; state
  is captured lexically.  Java's createTestClassInstance is a no-op here.

## Cross-module dependencies (not yet in the port, or not loaded when checked)
- read-graph-file (graph-io): .elkt/.elkg -> ElkNode.
- read-rand-graph-file, new-random-graph-generator, random-graph-generator
  generate: the .elkr reader and RandomGraphGenerator (random-graph module).
  Needed only by @RandomGeneratorFile / @RandomGeneratorOptions.
- ecore-util.copy: EcoreUtil.copy for random graphs.
- core: new-test-controller, add/remove-layout-execution-listener,
  layout/3 on recursive-graph-layout-engine, new-basic-progress-monitor,
  layout-meta-data-service.get-instance, get-algorithm-data (service/0 and /1),
  get-instance-pool, configure on layout-configurator.
- core.lisp currently has a READ error in this tree, so the real layout path
  was not exercised.  tools/check-module.lisp test-framework compiles cleanly.

## Verification
- `sbcl --script tools/check-module.lisp test-framework`: no compile failures or
  warnings in these files.
- Smoke run (/tmp/tfsmoke/smoke.lisp, outside the repo) loads the earlier modules,
  stubs the layout engine to fire processor events through the listener, and
  checks: blackbox pass/fail/error/ignored and custom messages; whitebox
  before/after dispatch with @OnlyOnRootNode and @FailIfNotExecuted; init
  errors (no graphs; whitebox without explicit algorithm).
