# layered-p1p2 notes

Ports `org.eclipse.elk.alg.layered.p1cycles` (11 Java files) and
`org.eclipse.elk.alg.layered.p2layers` (9 Java files).

Files: `classes.lisp` (defjclass forms), `p1cycles.lisp`, `p2layers.lisp`
(load order as in `files.txt`).

## Coverage

- All 20 Java classes have a `defjclass` or are covered by a shared helper
  (`GroupModelOrderCalculator` is a class of its own; `LayerSpan` from
  `InteractiveLayerer` is `layer-span`).  Every `process/2` and
  `getLayoutProcessorConfiguration/1` is present.  `LongestPath*`,
  `StretchWidthLayerer`, `MinWidthLayerer`, `CoffmanGrahamLayerer` and the
  BFS/DFS model-order layerers are all ported method-for-method.
- `RANDOM` is read from `layered-internal-properties.random` and used through
  `next-int` (the mechanical `random.nextInt(n)` helper) in
  `GreedyCycleBreaker.chooseNodeWithMaxOutflow`; `GreedyModelOrderCycleBreaker`
  overrides it deterministically.  No other Random use in these packages.

## Compile status

`sbcl --dynamic-space-size 6144 --script /tmp/p1p2chk/check.lisp layered-p1p2`
(a copy of `tools/check-module.lisp` with the file-suffix handling fixed and
`*root*` pinned; see the report) compiles both files with no failures and no
undefined names.  Three derived-type warnings remain, all for the lexical
variable `connected-components` in `NetworkSimplexLayerer.process` (SBCL
derives ENUM-SET/HASH-TABLE/JSET for the `(connected-components this ...)`
result, asserted SEQUENCE by `length`).  The runtime value is always a
`new-jlist` (adjustable vector), so these look like inference noise, not
defects; they were not reproduced at run time.

## Deviations / notes

- `NetworkSimplexLayerer.connectedComponents` keeps Java's reuse of
  `nodeVisited` (only re-filled when not reallocated).
- `connectedComponentsDFS` uses the Java node ids assigned by
  `connectedComponents`; nothing else in the module relies on ids.
- Smoke-tested only by compilation.  The Java tests in
  `org.eclipse.elk.alg.layered.test/.../p1cycles/BasicCycleBreakerTest.java`
  and `p2layers/*` are not run from this module; they belong to the
  test-layered-* modules.
