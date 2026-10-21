# layered-core (org.eclipse.elk.alg.layered, root package)

## Files
classes.lisp, graph-configurator.lisp, elk-layered.lisp,
layered-layout-provider.lisp, interactive-layered-graph-visitor.lisp (load order in files.txt).

## Coverage
- ElkLayered, GraphConfigurator, LayeredLayoutProvider, InteractiveLayeredGraphVisitor:
  every Java method has a port counterpart (checked by name against the Java sources).
- LayeredPhases: generated from src/gen-enums/enums.lisp (`layered-phases`), not redefined here.
- IHierarchyAwareLayoutProcessor: marker interface in classes.lisp.
- DebugUtil, DotDebugUtil, JsonDebugUtil: not ported (debug output, no-op per convention).

## Verification status
- Every file reads cleanly (parenthesis/syntax check with the READ function).
- tools/check-module.lisp could NOT complete for layered-core, for reasons outside this module:
  1. Default SBCL heap (1 GB) is exhausted while the earlier modules are eval'd; with
     `--dynamic-space-size 6144` the run still exhausts the heap inside `core`.
  2. src/core/files.txt lists entries WITH the `.lisp` extension, while
     tools/check-module.lisp's module-files appends `.lisp` itself, producing
     `src/core/classes.lisp.lisp` (file-does-not-exist). A patched copy in /tmp
     tolerates both forms but still hits 1.
  3. Earlier modules have compile errors at this commit: src/core-util/layout-providers.lisp
     (unmatched close paren, line 73), src/core/core.lisp (unmatched close paren, line 277;
     `pi` used as a LET variable; `(declare (ignore ...))` in a defjctor body).
- Not yet exercised at runtime.

## Notes
- ElkLayered's Java `Iterator<ILayoutProcessor>` processor loop is ported as a small
  generator-style struct (%elk-layered-gal) rather than an anonymous class.
- Test-controller notifications are kept as in Java (no-op when no controller is set).
