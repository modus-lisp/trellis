# test-common blockers

Ported: org.eclipse.elk.alg.common.test (8 classes).  Results from
/tmp/common-driver (run-common.lisp, run-plain.lisp, run-diag.lisp).

## Results (faithful run)

| class | pass | fail | error | cause of errors |
|---|---|---|---|---|
| Issue475Test | 2 | 0 | 0 | |
| InsidePortLabelTest | 1 | 0 | 0 | |
| NextToPortLabelPlacementTest | 2 | 0 | 0 | |
| NodeMicroLayoutTest | 0 | 0 | 24 | B2 (ElkReflect) |
| PortLabelPlacementVariantsTest | 24 | 0 | 0 | |
| NetworkSimplexTest | 1 | 0 | 0 | needs --control-stack-size 1000MB (see B6) |
| RectilinearConvexHullTest | 2 | 0 | 0 | |
| OneDimensionalCompactorTest | 0 | 0 | 17 | B1 (EnumSet) |

## B1: src/common/compaction-oned.lisp, js-contains-or-member

`(defun js-contains-or-member (coll x) (if (jset-p coll) ... (position x coll)))`
does not accept an ENUM-SET.  `new CGraph(EnumSet.allOf(Direction.class))`
therefore fails at the first `supports` call in every test with
"The value #S(ENUM-SET ...) is not of type SEQUENCE".  All 17 tests error.

Diagnostic (driver-only, in /tmp/common-driver/run-diag.lisp, not in the repo):
redefining js-contains-or-member to handle `enum-set-p` with `contains` makes
all 17 tests PASS.  Fix belongs in src/common: add an enum-set arm.

## B2: NodeMicroLayoutTest, KVector property type not registered with ElkReflect

`ILLEGAL-STATE-EXCEPTION: Couldn't create new instance of property
'org.eclipse.elk.nodeSize.minimum'` (runtime/metadata.lisp:209).  The default
of core-options.node-size-minimum is a KVector, and `elk-reflect.new-instance`
has no registration for k-vector (only graph/properties.lisp defines the
registry; nothing registers KVector).  Fails all 24 experiments.  Owner: core-util
/ graph (registration), not test-common.

## B3: no plain-test form in the framework

test-framework has only DEFLAYOUTTEST.  Three of the eight Java classes are
plain JUnit (no LayoutTestRunner).  I added test-common/plain-tests.lisp
(DEFTEST, DEFINE-PLAIN-TEST-CLASS, RUN-PLAIN-TESTS, same Summary line as
RUN-ELK-TESTS).  The coordinator may want to move it into test-framework.

## B4: assertEquals with a delta collides with the message form (jcalls arity)

JUnit `assertEquals(expected, actual, delta)` has 3 arguments, so the
call macro sends it to ASSERT-EQUALS/2, the (message expected actual) form in
assert.lisp.  It would silently compare the delta as the actual value.  Test
code uses `%assert-equals-delta` and `%assert-equals-delta-m` (helpers.lisp).
Framework fix: give the delta form its own name in assert.lisp.

## B5: jequals on sequences is identity; EMF e-lists are not sequences

- `assertEquals(list, list)` (Java List.equals, element-wise) becomes
  `(assert-true "Java List.equals" (%elements-equal-p ...))`.  Plain
  `assert-equals/1` on two jlists compares identity and fails (RectilinearConvexHullTest,
  first run).  Framework-level: assert.lisp's jequals has no sequence arm.
- EMF e-lists (`get-children`, `get-ports`, `get-labels`, ...) are not
  SEQUENCEs: use `jlist-from`, `j-get`, `jdo`; `length`/`jl-get` fail on them.

## B6: interpreted loading needs a big control stack

Under the driver's interpreted load (`sb-ext:*evaluator-mode* :interpret`, as
in run-pilot.lisp), NetworkSimplexTest's 4000-node/8000-edge graphs exhaust the
default control stack ("fatal error: Control stack exhausted" in SB-EVAL).
Running with `sbcl --control-stack-size 1000MB` passes in about 4 minutes.
The pilot driver's default stack would hit the same limit at this size.

## Notes

- `check-module.lisp test-common` compiles; only redefinition warnings for
  the macros (the module was also loaded leniently first) and one duplicate
  NEW-ONE-D-TEST-SPACINGS-HANDLER/0 from defjclass + defjctor.
- Port faithful choices: Issue475 uses the org.eclipse.elk.fixed algorithm id;
  the Java anonymous ISpacingsHandler classes are `one-d-test-spacings-handler`
  with closure slots; `new Point(int, double)` arguments are written as doubles.
