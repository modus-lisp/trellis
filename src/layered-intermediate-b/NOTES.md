# layered-intermediate-b

Port of the top-level files of `org.eclipse.elk.alg.layered.intermediate` (the
processors whose names sort between InnermostNodeMarginCalculator and
SortByInputModelProcessor).  Every processor class is declared in `classes.lisp`
as `defjclass ... (i-layout-processor)`; the process bodies are in the topic files.

Files:
- `classes.lisp` -- one defjclass per Java class; `label-dummy-info` is the private
  nested class LabelDummySwitcher.LabelDummyInfo; `HiddenNodeConnections` (private enum
  of LayerConstraintPreprocessor) is generated in gen-enums and is not redefined.
- `simple.lisp`, `labels.lisp`, `label-management.lisp`, `label-dummy-switcher.lisp`,
  `layer-constraints.lisp`, `long-edges.lisp`, `node-promotion.lisp`, `partitions.lisp`,
  `ports.lisp`, `self-loop-pre.lisp`, `sorting.lisp`, `north-south.lisp`.

Deviations / notes:
- `NorthSouthPortPreprocessor.USE_NEW_APPROACH` is `true` in Java; the dead
  old-approach branches are kept as `(unless north-south-port-preprocessor.use-new-approach ...)`.
- `modelOrderNorthSouthInputReversing` relies on Guava's `Lists.reverse(list).addAll(c)`,
  a write-through reversed view: the result is `reverse(outgoing)` followed by `incoming`
  in its original order.  Ported exactly that way.
- Java `assert` statements are not ported (disabled in ELK by default).
- Helper `%lib-*` defuns are local substitutes for Guava/Java collection idioms.

Verification status (see report):
- Compiles with no compile failures or load errors in this module's files.
  Remaining undefined names are forward references into layered-intermediate-c
  (self-loop holders, loops, model-order comparators, routers), into modules loaded
  earlier whose DEFJSTATIC forms never ran (see below), and this module's own
  LONG-EDGE-SPLITTER.SPLIT-EDGE / LONG-EDGE-JOINER.JOIN-AT (called from labels.lisp,
  defined later in long-edges.lisp).
- BLOCKER OUTSIDE THIS MODULE: `defjstatic` in src/runtime/java.lisp puts
  `(multiple-value-bind (decls rest) (split-leading-declares body) ...)` inside its own
  backquote, so it is emitted as code and fails at load with "The variable BODY is
  unbound" (this also breaks enums.lisp).  No DEFJSTATIC in the tree defines its
  function until that is fixed.  The check was run with a checker-only corrected copy
  of the macro (not committed).  The fix is to move the MULTIPLE-VALUE-BIND outside
  the backquote.
