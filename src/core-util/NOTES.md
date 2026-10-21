# core-util: notes

Ported from `plugins/org.eclipse.elk.core/src/org/eclipse/elk/core/`:
`util/` (incl. `adapters/`, `internal/`, `persistence/`, `selection/`), `labels/`,
`comments/`, `validation/`.  `tools/check-module.lisp core-util` compiles with
no warnings of its own.

## Files (load order, see files.txt)

- `classes.lisp` — every class skeleton (slots and the interface defjinterfaces).
- `util-basic.lisp` — Pair, Triple, Quadruple, Maybe, ExclusiveBounds, InstancePool,
  LoggedGraph and its Type enum (as `core-util-type`), comparators, WrappedException,
  Rectangle helpers, AbstractRandomListAccessor (helpers).
- `elk-util.lisp` — ElkUtil (incl. `new-string-builder`, `%cu-elk-class-name`).
- `spacings.lisp` — ElkSpacings, IndividualSpacings, the spacings builder.
- `util-internal.lisp` — internal LayoutOptionProxy, persistence (XMI helper/resource).
- `layout-providers.lisp` — FixedLayoutProvider, RandomLayoutProvider.
- `box-layout-provider.lisp` — BoxLayoutProvider and its nested Group.
- `comments.lisp` — comments/ (matchers, filters, providers, CommentAttacher) and the
  interface default methods (IBoundsProvider/IDataProvider `cached()` etc.).
- `validation.lisp` — GraphIssue, GraphValidator, LayoutOptionValidator.
- `adapters-elk.lisp` — ElkGraphAdapters.  **Not written by this port**: it appeared
  in the directory during the run from another writer and is listed in files.txt; it
  was not reviewed here.

## Deviations and things to know

- **defjstatic in the runtime (uncommitted change in `src/runtime/java.lisp`).**  The
  working-tree `defjstatic` puts `(multiple-value-bind ... body ...)` inside the
  backquote, so `BODY` is evaluated at load time and every static method fails to
  load ("The variable BODY is unbound").  HEAD's version is correct.  comments.lisp
  therefore spells its statics as `define-jcall` + `defun NAME/N` with `(block NAME/N)`,
  which is equivalent.  The other files in this module (elk-util, spacings, ...) still
  use `defjstatic` and will hit the bug until the runtime is fixed.  Not edited here
  (outside this module).
- **Regex.**  NodeReferenceMatcher builds java.util.regex patterns.  The runtime has
  no regex engine, so `%cu-strict-find-p` and `%cu-fuzzy-find-p` match the exact
  patterns ELK builds (`\b` + literal, or `\b` segments joined by `[\h\v]*`, case-
  insensitive for fuzzy).  Segment splitting follows `fuzzyRegexpFor`.
- **Hash iteration order.**  Java HashMap iteration (AggregatedMatchDecider's entry
  loop, CommentAttacher's candidate maps) is not reproduced; SBCL hash tables iterate in
  insertion order.  Ties in the aggregated maximum therefore can pick a different target
  than Java would.
- **Aggregators.**  `summingDouble` / `averagingDouble` use the JDK's compensated (Kahan)
  summation so results match Java's bits.
- **Lambdas.**  IExplicitAttachmentProvider, IFilter, IDecider and the Function arguments
  accept CL functions as well as objects (`%cu-explicit-attachment` etc.).  Functions
  have no preprocess/cleanup, so those are skipped for them.
- **Booleans in properties.**  Java `false` is non-null; here NIL is also "absent", so a
  property explicitly set to false is invisible to LayoutOptionValidator's null check.
- **Validator classes.**  `layout-algorithm-data`'s validator-class is instantiated as
  a symbol naming a defjclass (NEW-<name>/0), a constructor function, or used as-is.
- **EMF.**  No EMF in the port.  `createXMLSave`/`createXMLHelper` are `create-xmi-save`
  / `create-xmi-helper`; `ElkGraphXMIHelper.createFromString` takes the data type as a
  keyword (`:iproperty`, `:property-value`), and any other type signals.
- **Constructors.**  Several classes had redundant zero-argument `defjctor`s that
  redefined the defjclass default constructor (warnings).  Removed.  Group's constructor
  dispatches on its argument (ElkNode vs. a list) since both are arity 1.
- **Skipped on purpose.**  Debug-folder and log-file methods of BasicProgressMonitor
  (no-op per PORTING.md), `toString` of Group, Pair/Maybe `iterator`/`remove` (Java-only
  iteration, use `jdo`), the `ElkUtil` and `ExclusiveBounds` private constructors.
- **Forward reference.**  `box-layout-provider` and the layout providers inherit from
  `abstract-layout-provider`, which is defined in module `core` (loaded after core-util).
  CLOS finalises this at first instantiation; the module check cannot see it.

## Verification

- `tools/check-module.lisp core-util`: clean, no warnings from this module.
- ElkUtilTest.translateWithContentAlignmentTest: all 10 cases match the expected values
  (run through `elk-util.translate`).
- GraphValidatorTest.testUnconnectedEdge: one issue, "Edge is not connected.", ERROR.
  (Compared by fields; GraphIssue.toString is not ported.)
- Not run: GraphValidatorTest.testCustomValidation (needs the layout-metadata registry
  and AlgorithmFactory) and LayoutOptionValidatorTest.
- BoxLayoutProvider simple mode: a three-box case matches a hand calculation of the Java
  algorithm (row wrapping, spacing, padding, parent size).  Grouped modes run but have
  no reference values here.
- Smoke tests for the comments helpers (rectangles, distance/alignment, fuzzy and strict
  name matching, normalisation, aggregation, prefix filter) pass.
