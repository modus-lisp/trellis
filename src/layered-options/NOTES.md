# layered-options: notes

Port of `org.eclipse.elk.alg.layered.options` (35 Java files).

Files (load order, `files.txt`): `classes`, `internal-properties`, `options`.

- `classes.lisp`: `Spacings` (defjclass), `Spacings.UnspecifiedSpacingException`
  (condition over `runtime-exception`), `LayeredSpacings.LayeredSpacingsBuilder`.
- `internal-properties.lisp`: all 66 `InternalProperties` constants as
  `layered-internal-properties.*`. Ids and defaults checked one by one against the
  Java source. Java `Boolean false` defaults are NIL; `null` defaults are omitted or NIL.
- `options.lisp`: method bodies of the layered option enums (`create()` phase
  factories of CrossingMinimizationStrategy, CycleBreakingStrategy, LayeringStrategy,
  NodePlacementStrategy; `usesLabelSizeInformation`; `EdgeLabelSideSelection.transpose`;
  `LongEdgeOrderingStrategy.returnValue`; NodeFlexibility's predicates and
  `getNodeFlexibility`), plus `Spacings`, `LayeredSpacings` and its builder.
  Enum constants come from gen-enums.

Deviations and non-obvious points:

- `Spacings` overloads with the same arity are merged into one function that
  dispatches at runtime on argument types (`get-horizontal-spacing` takes
  LGraphElement / LNode / NodeType pairs; the private `nodeTypeSpacing` overloads
  share `%spacings-node-type-spacing`, which dispatches on whether the second argument
  is a NodeType).
- `NodeFlexibility.isAtLeast`: Java's `NODE_SIZE_WHERE_SPACE_PERMITS` case has no return
  and falls through to `PORT_POSITION`; the port keeps that.
- `NodeFlexibility.getNodeFlexibility` uses `has-property` for Java's
  `getAllProperties().containsKey(...)`, which is equivalent because holders are keyed by id.
- `LayeredSpacingsBuilder`'s static initializer assertions (base spacing default
  non-null and non-zero) are not ported.
- `LayeredSpacings.OPTION_TARGET_FILTER` is a defjconst holding a Lisp closure
  (holder, property) -> boolean. The target test accepts keyword targets
  (`:nodes`, `:parents`, ...) or LayoutOptionData.Target enum objects.
- `NodeType` is not in this package (it is in the graph package), so it is not ported here.
- `GraphProperties`, `PortType` and most other enums in this package have no methods,
  so there is nothing to port beyond the generated constants.

Verification: `sbcl --script tools/check-module.lisp layered-options` compiles with
no warnings in this module. The remaining undefined names belong to other modules
(`new-*` layerers/placers/cycle breakers, `l-graph`/`l-node`/..., `get-type`,
`get-option-data`, `layout-meta-data-service.get-instance`). InternalProperties was
cross-checked by script against the Java source (66/66 ids and defaults match).
