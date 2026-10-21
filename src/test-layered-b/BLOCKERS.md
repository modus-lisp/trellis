# test-layered-b: blockers and port notes

Ported from the Java classes listed in /tmp/L2 (27 files). Results are from
`/tmp/layered-b-driver/run-class.lisp <class>` (outside the repo).

## Not ported as DEFLAYOUTTEST

- `Issue316Test` -> already ported as `src/test-layered-pilot/issue316-test.lisp`
  (same registered name `issue316-test`). Not duplicated here.
- `SelfLoopPreprocessorTest` (plain JUnit) -> `self-loop-preprocessor-test.lisp`,
  `run-self-loop-preprocessor-test`. BLOCKED: `SelfLoopTestGraphCreator`
  (`self-loop-test-graph-creator.basic-graph-without-self-loops`, `.ports`,
  `.edge`) is not defined anywhere in src/ (only named in jcalls.lisp). All 5 tests ERROR.
- `Issue562Test` (plain JUnit) -> `issue562-test.lisp`, `run-issue562-test`.
  BLOCKED: `elk-util.apply-visitors/3` is undefined (src/core-util/elk-util.lisp
  defines it under another arity or not at all).

## Port conventions used

- JUnit `assertEquals/assertNotEquals` with a double delta (and message forms)
  are `%b-assert-equals-delta` / `%b-assert-not-equals-delta` in b-helpers.lisp.
  The arity-keyed `ASSERT-EQUALS/2` is the message variant (framework), so the
  3-arg delta form cannot use it.
- Guava `DoubleMath.fuzzyEquals`, `Rectangle2D.contains`, and E-list counting
  are written out as `%b-fuzzy-equals`, `%b-rect2d-contains-*`, `%b-count-matching`.
- Some assertion failure messages are simplified (element `toString` detail dropped).
- `Issue405Test.getRelevantPortsAndPortLabels` is `issue405-test.get-relevant-ports-and-port-labels`
  (defjstatic), used by Issue405_2Test.
- Issue405_2Test's `reduce(false, a ^ b)` is ported as an odd-count test (XOR fold).
- PartitionTest's `node.isHierarchical()` filter uses the root `node` as in Java (faithful).

## Test results (x64, models at /tmp/elk-src/models)

| class | pass | fail | error | cause |
|---|---|---|---|---|
| north-south-port-postprocessor-test | 7 | 0 | 0 | |
| north-south-port-preprocessor-test | 7 | 0 | 0 | |
| partition-post-processor-test | 9 | 1 | 0 | FAIL partition order (engine output) |
| partition-test | 1 | 4 | 0 | FAIL partition coordinates (engine output) |
| sort-by-input-model-processor-test | 15 | 0 | 0 | |
| issue143-and318-test | 1 | 1 | 0 | FAIL edge proximity (engine output) |
| issue405-test | 2 | 1 | 0 | FAIL port/label positions (engine output) |
| issue405-2-test | 1 | 1 | 0 | FAIL port label position (engine output) |
| issue433-test | 0 | 1 | 0 | FAIL self loop label outside graph |
| issue444/445/463/665 | 1 each | 0 | 0 | empty or trivial bodies |
| issue447-test | 0 | 0 | 1 | ILLEGAL-STATE: ElkReflect cannot create `port.anchor` property |
| issue455-test | 1 | 0 | 1 | cross-hierarchy comparator is not a function (engine) |
| issue476-test | 0 | 0 | 2 | ILLEGAL-STATE: ElkReflect cannot create `nodeSize.minimum` |
| issue502-test | 0 | 1 | 0 | FAIL node too small (engine output) |
| issue515-test | 0 | 3 | 0 | FAIL edge overlaps node (engine output) |
| issue541/548 | 2 / 1 | 0 | 0 | empty bodies |
| issue546-test | 1 | 0 | 0 | |
| issue552-test | 1 | 0 | 0 | |
| issue587-test | 2 | 0 | 1 | no GET-LAYOUT-PROCESSOR-CONFIGURATION/1 for spline-edge-router |
| issue596-test | 1 | 0 | 0 | |
| issue603-test | 1 | 0 | 0 | |
| issue628-test | 0 | 0 | 4 | ILLEGAL-STATE: ElkReflect cannot create `padding` property |
| port-list-sorter-test | see RESULT line in log | | | |
| port-side-processor-test | see RESULT line in log | | | |
| reversed-edge-restorer-test | see RESULT line in log | | | |

## Top blockers (grouped by cause)

1. **ElkReflect registration** (issue447, issue476, issue628): `Couldn't create new
   instance of property 'org.eclipse.elk.port.anchor' / 'nodeSize.minimum' / 'padding'`.
   Property value types are not registered for reflection.
2. **Missing test helper** `SelfLoopTestGraphCreator` (self-loop-preprocessor-test, 5 tests).
3. **Missing `elk-util.apply-visitors/3`** (issue562-test).
4. **Missing `get-layout-processor-configuration/1` for spline-edge-router** (issue587-test).
5. **Cross-hierarchy comparator** is not a function (issue455-test: `CROSS-HIERARCHY-EDGE-COMPARATOR`
   used where `(OR FUNCTION SYMBOL)` is expected).
6. **Engine output differences** (FAIL, not errors): partition order/coordinates,
   edge proximity, port label positions, self-loop label placement, edge overlaps.
   Possibly related to label sizes being unmeasured (NOTES.md: `*label-size-function*` is NIL).
