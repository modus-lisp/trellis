# Porting ELK (Java) to Common Lisp — the conventions

This repository is a **faithful port** of the Eclipse Layout Kernel: same
classes, same algorithms, same option ids, same processor names, so that
ELK's own test suite can be run against it unchanged in substance.  Do not
"improve" algorithms while porting; port them.  Upstream sources live in
`/tmp/elk-src/elk` (code: `plugins/`, tests: `test/`) and the test models in
`/tmp/elk-src/models`.

All code is in package `ELK` (`(in-package #:elk)`), compiled with
`*read-default-float-format*` = `double-float`, so `1.5` is a double.

## Scope (decided 2026-10-21)

The goal is a comprehensive layout library in Common Lisp, using ELK as the
reference design. It is not a line-for-line port of every ELK class or test.

- **In scope:** the layered algorithm and its options, core options and graph
  handling, and the tests that check real layout behaviour.
- **Out of scope:** JVM and EMF machinery (ElkReflect reflection, Ecore copier,
  plain-Java initialization), and tests that only mirror Java internals. Such a
  test is removed with a note saying why, not left as an error.
- **Rendering:** not here. SVG output is handed to `~/stencil` (SVG parse and
  render, via gesso). This repo produces laid-out geometry and SVG markup only.
- **Metrics:** label sizes come from a fixed test measurer, so results are
  repeatable and do not depend on AWT fonts.
- **Other algorithms** (mrtree, force, radial, disco, spore, rectpacking,
  topdown, stress): deferred until layered passes, and kept only if needed.

Iterate on narrowed model sets; run the full ptolemy corpus only for classes
that are close to passing.

## Names are mechanical — so you can call code that nobody has written yet

`java-name->lisp` / `tools/javanames.py` define the rule: camelCase and
PascalCase split into words joined by `-`, lower case; ALL_CAPS uses `_`→`-`.

| Java | Lisp |
|---|---|
| class `LGraph`, `KVectorChain`, `BKNodePlacer` | class `l-graph`, `k-vector-chain`, `bk-node-placer` |
| class whose simple name is not unique | the name in `tools/classnames.tsv` (e.g. layered's `InternalProperties` is `layered-internal-properties`) — **always look it up there** |
| `new LNode(graph)` | `(new-l-node graph)` |
| `x.getPorts()`, `x.setLayer(l)` | `(get-ports x)`, `(set-layer x l)` |
| method name that is a CL symbol (`get`, `set`, `remove`, `first`, `sort`, `reverse`, `apply`, `values`, `count`, `max`, ...) | prefix `j-`: `(j-remove x y)`, `(j-get m k)` |
| field `node.id`, `this.x` | accessor `(.id node)`, `(.x this)`, `(setf (.id node) 3)` |
| static method `LGraphUtil.placeNodesHorizontally(...)` | `(l-graph-util.place-nodes-horizontally ...)` |
| static field / constant `LayeredOptions.SPACING_NODE_NODE` | special variable `layered-options.spacing-node-node` |
| enum constant `PortSide.NORTH` | special variable `port-side.north` (an object; compare with `eq`) |
| `PortSide.values()`, `PortSide.valueOf("NORTH")` | `(port-side.values)`, `(port-side.value-of "NORTH")` |
| `side.ordinal()`, `side.name()` | `(ordinal side)`, `(name side)` |

### Calls dispatch on the number of arguments

Every method/static/constructor name is a **macro** (`src/runtime/jcalls.lisp`,
generated from the Java sources) that rewrites to a function named by its
arity: `(foo x a b)` → `(foo/2 x a b)`; `(new-l-node g)` → `(new-l-node/1 g)`;
`(l-graph-util.foo a b)` → `(l-graph-util.foo/2 a b)`.  So Java overloads by
arity need no thought.  Consequences:

- You cannot `funcall`/`#'` a call macro.  Use `(jfun get-x 0)` → `#'get-x/0`,
  or a lambda.
- Overloads with the **same arity but different parameter types** become one
  method with a `typecase`/CLOS specializer, e.g. `getPorts(PortType)` vs
  `getPorts(PortSide)` → specialize the parameter on the enum class:
  `(defjmethod get-ports ((this l-node) (side port-side)) ...)` and
  `(defjmethod get-ports ((this l-node) (type port-type)) ...)`.
- Varargs (`Foo... xs`) are ONE parameter: pass a list or vector.
- If you need a name that is not in `jcalls.lisp` (Lisp-only helper), use a
  plain `defun` with a `%module-` prefix, or add `(define-jcall name :instance)`
  in your own file.

### Defining things

```lisp
(defjinterface i-layout-processor)               ; interface (an empty class)
(defjclass l-node (l-shape)                      ; class extends LShape
  ((layer nil)                                   ; field + initializer -> (.layer x)
   (ports (new-jlist))))                         ; initforms run per instance

(defjctor l-node (graph)                         ; constructor -> NEW-L-NODE/1
  (setf (.graph this) graph))                    ; implicit super() runs first
(defjctor foo (a b)
  (super-ctor parent this a)                     ; explicit super(a) -- first form
  ...)
(defjctor foo (a) (this-ctor foo this a 0))      ; this(a, 0)

(defjmethod get-layer ((this l-node)) (.layer this))          ; GET-LAYER/0
(defjmethod set-layer ((this l-node) layer) ...)               ; SET-LAYER/1
(defjgeneric process 2)                          ; abstract/interface method PROCESS/2
(defjstatic l-graph-util.offset-graph (graph x y) ...)
(defjconst some-class.some-constant 42)          ; static final
```

- `super.foo(x)` inside an overriding method is `(call-next-method)`.
- Inner (non-static) classes: pass the outer instance explicitly as a field.
- Anonymous classes / lambdas implementing a single-method interface used only
  as a callback (Comparator, Function, Predicate, Runnable, ...) → a Lisp
  closure, called with `funcall`.  An anonymous class implementing an ELK
  interface that is dispatched on (e.g. `ILayoutProcessor`) → a small named
  `defjclass`.
- Interfaces with `default` methods → `defjmethod` on the interface class.
- `instanceof` → `typep`; casts disappear.
- `toString()` → `(defjmethod to-string ...)` if anything depends on it.
- `equals`/`hashCode` overrides: define `(defjmethod equals ...)`; hash tables
  keyed by such objects need care (CL tables are `eq`/`equal` only).

## Values

| Java | Lisp |
|---|---|
| `null` | `nil` |
| `boolean` | `t` / `nil` |
| `int`, `long` | integer — **`a / b` on ints is `(idiv a b)`** (truncates), `%` is `(imod a b)`; a cast `(int) d` is `(to-int d)` |
| `double`, `float` | double-float; literal `1.5`, `0.0`.  Integer + double gives a double in CL too |
| `Double.POSITIVE_INFINITY`, `NaN`, `Integer.MAX_VALUE` | `double.positive-infinity`, `double.nan`, `integer.max-value` |
| `Math.max/min/abs/sqrt/pow/floor/ceil/round/...` | `math.max` etc. (Java semantics incl. NaN; `math.floor` returns a double) |
| `Double.compare`, `Integer.compare` | `double.compare`, `integer.compare` |
| `Double.isNaN(x)` | `(nan-p x)` |
| `String` | CL string; `+` concatenation `(jstr a b c)`; `.equals` → `string=` |
| `Objects.equals(a,b)`, `a.equals(b)` | `(jequals a b)` |

Floating point: layout entry points run inside `(with-java-floats ...)` so
division by zero gives infinity/NaN as in Java.  Do not wrap individual
functions.

## Collections

| Java | Lisp representation | operations |
|---|---|---|
| `ArrayList`, `LinkedList`, `ArrayDeque`, `List`, `Deque`, `Queue` | adjustable vector with fill pointer: `(new-jlist)`, `(jlist a b c)`, `(jlist-from coll)` | `jl-add jl-add-at jl-get jl-set jl-remove-at jl-remove jl-index-of jl-contains jl-clear jl-add-all jl-first jl-last jl-remove-first jl-remove-last jl-poll-first jl-poll-last jl-peek-first jl-peek-last jl-add-first jl-sub-list jl-copy jl-reverse jl-sort jl-remove-if jl-swap jl-shuffle jl-is-empty`, `length`, `aref` |
| `HashMap`, `LinkedHashMap`, `Map` | CL hash table: `(new-jmap)` (eql keys) / `(new-jmap 'equal)` (string keys) | `jm-get jm-put jm-contains-key jm-remove jm-keys jm-values jm-compute-if-absent jm-put-if-absent`, `gethash`, `jdo-map` |
| `HashSet`, `LinkedHashSet`, `Set` | `jset`: `(new-jset)`, `(jset-from coll)` | `js-add js-contains js-remove js-add-all js-clear` |
| `EnumSet<E>` | `enum-set`: `(enum-set.none-of 'port-side)`, `(enum-set.of a b)`, `(enum-set.all-of 'e)`, `(enum-set.copy-of s)` | generic `add contains j-remove size is-empty` |
| `PriorityQueue` | `(new-jpq comparator)` | `add offer poll peek size is-empty contains j-remove` |
| `Iterable` results (Guava `Iterables.concat`, filtered views) | build a fresh jlist or CL list | |
| `Collections.sort(l, cmp)` / `l.sort(cmp)` | `(jl-sort l cmp)` — stable, CMP returns an integer | |
| `Collections.shuffle(l, random)` | `(jl-shuffle l random)` | |
| `for (X x : coll)` | `(jdo (x coll) ...)` — any of the above, CL lists, enum sets, ported collection classes | |
| `for (Map.Entry e : m.entrySet())` | `(jdo-map (k v m) ...)` | |
| `coll.size()`, `isEmpty()`, `add`, `contains`... on an unknown collection type | the generic call macros `(size c)`, `(is-empty c)`, `(add c x)`, `(contains c x)`, `(j-get c i)`, `(j-remove c x)` work on all of the above | |

Prefer the explicit `jl-`/`jm-`/`js-` helpers when the Java static type is
known — they are faster and say what they mean.  `list.remove(int)` is
`jl-remove-at`; `list.remove(Object)` is `jl-remove`.

Iterators with `remove()`/`ListIterator.add()`: rewrite as an index loop.
Concurrent modification behaviour is not emulated — keep Java's iteration
order semantics (iterate a snapshot where Java iterates a copy).

Comparators: functions `(lambda (a b) ...)` → integer.  Helpers:
`comparing comparing-double comparing-int then-comparing reversed-comparator
natural-order reverse-order jcompare`.

A ported class that extends a Java collection (e.g. `KVectorChain extends
LinkedList<KVector>`) keeps its elements in a slot and defines the generic
collection methods it needs (`add/1`, `size/0`, ...) plus
`(defmethod jiterable-items ((c k-vector-chain)) ...)` so `jdo` works.

`java.util.Random` is exact: `(new-random seed)`, `(next-int r)`,
`(next-int r bound)`, `(next-double r)`, `(next-boolean r)`, `(next-float r)`,
`(next-gaussian r)` — same sequences as Java.  Tests depend on that.

## Enums

All enum CONSTANTS are generated into `src/gen/enums.lisp` (`defjenum`).  Do
**not** redefine them.  Port enum METHODS next to the code that owns the enum,
as `defjmethod`s specialized on the enum class:

```lisp
(defjmethod opposed ((this port-side))
  (enum-case this
    (port-side.north port-side.south) ...
    (t port-side.undefined)))
```

`switch (x) { case A: ... }` → `(enum-case x (a-const ...) ((b c) ...) (t ...))`.
Enums with constructor arguments (6 of them: ElementType, GraphFeature,
Severity, LoggedGraph.Type, NodeLabelLocation, PortSideAssigner.Target): add
the field as a `defjmethod` returning the value per constant.

## Properties

`(get-property holder layered-options.direction)`,
`(set-property holder prop value)`, `(has-property holder prop)`,
`(copy-properties h other)`.  `defproperty` for `new Property<>(id, default)`
constants (e.g. `InternalProperties`):

```lisp
(defproperty layered-internal-properties.origin "origin")
(defproperty layered-internal-properties.long-edge-source "longEdgeSource" nil)
```

Layout options and algorithm option holders (`CoreOptions.*`,
`LayeredOptions.*`, `LayeredMetaDataProvider.*`, ...) are GENERATED in
`src/gen/options.lisp` from the `.melk` files — never hand-write them.  A
mutable default (KVector, ElkPadding, EnumSet, lists) is cloned on read, as in
ELK; a cloneable class mixes in `jcloneable` and defines `jclone`.

## Files and loading

- Each module directory has a `files.txt` listing its files in load order;
  the system definition (`trellis.asd`) reads them.  Put class definitions
  (`defjclass`, `defjinterface`) before methods that specialize on them —
  a module's first file is usually `classes.lisp` with all its classes.
- One Java package → one Lisp file (or a few for big packages), named after
  the package (`p4nodes-bk.lisp`).  Keep Java's member order and comments that
  explain *why*; drop Javadoc that only restates the signature.
- Do not edit files outside your module.  If you need something from another
  module that does not exist yet, call it by its mechanical name anyway.
- Logging/debug output (`DebugUtil`, `JsonDebugUtil`, logging in progress
  monitors) is a no-op.

## Checking your work

```
sbcl --script tools/check-module.lisp <module-dir>
```

loads the runtime, the generated metadata and every module before yours, then
compiles your files and prints errors and warnings grouped by kind.  Undefined
function warnings for names in other, unfinished modules are expected; any
other warning is yours to fix.
