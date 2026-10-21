# core module: status

Files (load order, src/core/files.txt): classes.lisp, alg.lisp, data.lisp,
data-content-assist.lisp, testing.lisp, core.lisp, options.lisp.
Compile check (`sbcl --script tools/check-module.lisp core`): no errors, no
undefined names. Two intentional redefinitions are reported as warnings (below).
Nothing here has been run against ELK's test suite yet.

Ordering note: `core.lisp` must come after `testing.lisp` and `data.lisp`
(it calls `install`, `layout-meta-data-service.get-instance`), and `options.lisp`
comes last because it calls `dispose`/`layout` defined in core.lisp.

## Ported

- Root package: RecursiveGraphLayoutEngine, AbstractLayoutProvider, LayoutConfigurator,
  exceptions, IGraphLayoutEngine / ITopdownLayoutProvider.
- alg/: AlgorithmAssembler, LayoutProcessorConfiguration, EnumBasedFactoryComparator,
  ILayoutProcessor/Phase and factory interfaces.
- data/: LayoutMetaDataService (+ its Registry), LayoutCategoryData and its Builder,
  LayoutOptionData.Builder, LayoutAlgorithmData.Builder, LayoutAlgorithmResolver,
  DeprecatedLayoutOptionReplacer, LayoutDataContentAssist (+ Proposal),
  ILayoutMetaData, ILayoutMetaDataProvider.
- The Java API of LayoutOptionData and LayoutAlgorithmData (getId, getType, getTargets,
  parseValue, canParseValue, getChoices, getDefault, knowsOption, getInstancePool, ...)
  is in src/runtime/metadata.lisp, as instructed. That file also gained a
  `defining-bundle-id` slot on layout-algorithm-data.
- options/: every enum method (Direction, ContentAlignment, EdgeLabelPlacement, LabelSide,
  NodeLabelPlacement, PortConstraints, PortLabelPlacement, PortSide, SizeConstraint,
  SizeOptions), the PortSide.SIDES_* sets, LabelSide.LABEL_SIDE (defproperty),
  ITopdownSizeApproximator/TopdownSizeApproximator (per-constant bodies in one
  GET-SIZE method), TopdownSizeApproximatorUtil.
- testing/: TestController (IWhiteBoxTestable and ILayoutExecutionListener are the
  interfaces in classes.lisp).

## Deviations and things to know

- Enum-set statics (PortSide.SIDES_*, NodeLabelPlacement.*, ...) return mutable enum-sets;
  Java returns unmodifiable sets.
- `node-label-placement.value-of` and `size-options.value-of` are redefined here to add
  the valueOf(int) overload next to the generated valueOf(String). The "redefining"
  warnings from check-module are these two, on purpose. The String behaviour is the
  generated one (exact constant name).
- Registry: the service's option and algorithm tables are the globals of metadata.lisp.
  Option legacy ids share the table with real ids; a real id wins over a legacy one.
  Registering an existing id keeps its list position (LinkedHashMap semantics).
- CoreOptions and the ServiceLoader providers are not "applied" at service creation:
  the generated options register at load time. ElkServicePlugin is not loaded.
- ElkReflect: KVector, KVectorChain, ElkMargin, ElkPadding and IndividualSpacings register
  their constructor and a JCLONE-based clone. No entry is made for ArrayList, LinkedList,
  HashSet, LinkedHashSet, TreeSet (no Lisp class to key on); JCLONE covers them.
- LayoutOptionData type comes from the generated option-class: :boolean :int :double :string,
  (:enum X), (:enum-set X), (:object "Name"). (:list "Integer") maps to OBJECT, which is a
  guess. A builder may set the type explicitly.
- BOOLEAN parseValue returns NIL for both "false" and invalid text (Java: FALSE vs null).
- LayoutOptionData.getTargets and getVisibility return fresh enum objects built from the
  keyword spelling stored by the generator.
- TestController.removeLayoutExecutionListener calls add, not remove, exactly as the Java
  source does. Preserved on purpose; it looks like an upstream bug.
- LayoutAlgorithmData.getDefaultValue: when the stored default is a property, its default
  is returned (the generated registration stores properties, not values).
- LayoutDataContentAssist.getLayoutOptionValueProposal for OBJECT options instantiates the
  class through ElkReflect (nil when unregistered, which gives an empty proposal).

## Fixes made to existing code in this module

- core.lisp: %RGLE-TOPDOWN-LAYOUT had one extra closing paren (read error, so the whole
  file failed to load). Removed.
- core.lisp: LayoutConfigurator's getProperties was defined as GET-PROPERTIES, which is a
  COMMON-LISP symbol (package lock). Renamed to J-GET-PROPERTIES per PORTING.md, call sites
  updated.
- classes.lisp: TestController.listeners is a LinkedHashSet, now (new-jset).

## Open / not verified

- No runtime tests yet (check-module only). Parse, enum-set and suffix-lookup paths
  are unexercised.
- The suffix lookup (getOptionDataBySuffix) caches in the service's option-suffix-map,
  cleared by registerLayoutMetaDataProviders as in Java.
- util/ math/ comments/ validation/ belong to other module directories; not checked here.
