# graph module — notes

Port of `org.eclipse.elk.graph` (interfaces + `impl`), `graph.util` and
`graph.properties` (`plugins/org.eclipse.elk.graph/src/org/eclipse/elk/graph`).

## Layout
- `classes.lisp` — one `defjclass` per EMF interface/impl pair (ElkNode, ElkEdge,
  ElkPort, ElkLabel, ElkBendPoint, ElkEdgeSection, ElkPersistentEntry, ...),
  plus `e-object` (EObject container bookkeeping), `e-list`/`e-map` (EMF
  EList / EMap views), `elk-graph-factory`, `graph-identifier-generator`.
- `elist.lisp` — `e-list`: containment/opposite semantics (`%el-basic-add`,
  `%el-inverse-add`, reparenting on add/remove) and the generic collection methods
  (`add`, `add-all`, `j-get`, `j-remove`, `size`, `contains`, `clear`, `sub-list`,
  `to-array`, `for-each`, `jiterable-items`, ...).
- `graph.lisp` — getters/setters, `to-string`, `e-container`, `e-contents`,
  `e-all-contents` (pre-order, root excluded, returned as a snapshot jlist),
  EMapPropertyHolder property map (`j-get-properties`), factory methods, `ElkPersistentEntry`.
- `properties.lisp` — `ElkReflect`, `PropertyHolderComparator`, advanced/experimental
  property-value markers and `IPropertyValueProxy` resolution. Uses the runtime's
  `property` / `map-property-holder` from `src/runtime/properties.lisp`.
- `util.lisp` — `ElkGraphUtil` (all public static methods) and `GraphIdentifierGenerator`.

## Deviations from Java
- EMF boilerplate is not ported as code: `ElkGraphPackage` (2110 lines of EClass /
  EAttribute / EReference metamodel literals), `ElkGraphPackageImpl`,
  `ElkGraphFactoryImpl`'s `create(EClass)`/`createFromString`/`convertToString`,
  `ElkGraphAdapterFactory`, and `ElkGraphSwitch`. `ElkGraphSwitch` is replaced by
  `typecase` in `GraphIdentifierGenerator` (its only use). These are reflection
  machinery with no layout-visible behaviour.
- `ElkGraphFactory.eINSTANCE` is `elk-graph-factory.e-instance` (a `defjconst`
  singleton of the Lisp class `elk-graph-factory`); `ElkGraphFactoryImpl.init()` is
  `elk-graph-factory-impl.init`.
- `impl` classes are merged into their interface's class (one Lisp class per
  EMF interface+impl pair).
- `eAllContents()` returns a jlist snapshot instead of a lazy TreeIterator.
- `Object.hashCode`/`toString` identity hash is the object address in hex.
- `e-list` of a non-containment reference holds plain references; containment lists
  set `e-internal-container` / `e-container-feature-id` on their elements.

## Verification
- `sbcl --script tools/check-module.lisp graph`: compiles cleanly, 0 other warnings.
  The "undefined" list is only forward references between graph's own files
  (e.g. `elist.lisp` calls `get-ports/0` defined in `graph.lisp`); they resolve at load.
- Smoke run (via check-module's loader): create graph/node/port, parent/container
  links, simple edge with source/target incidence, property get/set, `e-all-contents`.

## Open questions
- No upstream ELK test is exercised yet from this module; the ELK graph tests
  (`test/.../graph`) depend on the runtime `e-list` semantics being exact.
