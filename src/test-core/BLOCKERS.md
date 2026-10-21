# test-core blockers

Known gaps found while porting the core/shared/graph JUnit batch.  Results are
per class in the coordinator's report; this file lists causes.

## Not ported
- `GraphValidatorTest.testCustomValidation`: needs `LayoutAlgorithmData.Builder`,
  `AbstractLayoutProvider` subclass and the registry lambda form of
  `registerLayoutMetaDataProviders`.
- `ElkReflectTest.testSimpleProperty`: walks `Class.getDeclaredFields()` of
  Java classes; no reflection over Java classes in the port.
- `ElkReflectTest.testLayoutOptionData`: ported without the reflective
  `getClass().getConstructor().newInstance()` step (only the default path).

## Missing engine / shared-test symbols
- `graph-test-utils.create-simple-graph` and `graph-test-utils.create-hierarchical-graph`
  (shared.test `GraphTestUtils`) are not in the port.  Used by `DirectLayoutTest`
  and `PlainLayoutTest`; they error until added.

## Framework / port conventions used here
- `assertEquals(double, double, delta)` collides with the message form
  `assert-equals/2` in jcalls, so the port uses `assert-near` (test-core/junit-plain).
- `assertSame`/`assertNotSame` are written as `(assert-true (eq ...))`.
- Plain (non-layout) JUnit classes have no runner in test-framework; they run
  through `defcore-class`/`run-core-tests` in test-core/junit-plain.lisp.
- Java varargs are one list parameter: `ElkMath.maxi(1,7,5,6)` is `(elk-math.maxi (list ...))`.
- `@Before registerLayoutMetaDataProviders(new CoreOptions())` is a no-op: the
  port defines CoreOptions/LayeredOptions metadata at load time.
