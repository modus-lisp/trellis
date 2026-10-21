# graph-io

Ports `org.eclipse.elk.graph.json` (JsonImporter, JsonExporter, JsonAdapter,
JsonMetaDataConverter, ElkGraphJson) and the ELK Graph Text reader from
`org.eclipse.elk.graph.text`, plus `read-graph-file`.

Files (load order, see files.txt): `json.lisp`, `json-graph.lisp`, `elkt.lisp`,
`read-graph.lisp`.

## JSON value representation

Gson's JsonElement tree, as plain Lisp data (`json.lisp`):

| JSON | Lisp |
|---|---|
| object | `(:object . alist)`: string keys, insertion order, mutable (`json-put`, `json-remove`) |
| array | adjustable vector with fill pointer (a jlist, `make-json-array`) |
| string | CL string |
| number | integer when the text has no `.`/`e`; double-float otherwise |
| `true` / `false` / `null` | `:true` / `:false` / `:null` |

Missing members are NIL from `json-get`; `json-has` distinguishes absence from
`:null`. `json-parse` has Gson's two modes: strict, and LENIENT (the ElkGraphJson
default: comments, unquoted and single-quoted names/strings, `;` separators,
`=`/`=>` name separators). `json-parse-file` is lenient by default.

## JSON import / export

`json-graph.lisp` is JsonImporter (`transform`, `transfer-layout`) and
JsonExporter (`set-options`, `j-export`), with the ElkGraphJson builders
(`import-builder`, `export-builder`, `elk-graph-json.for-graph`) and
JsonMetaDataConverter (`json-meta-data-converter.to-json`).  The id maps
are EQUAL tables for id keys (ids may be strings or integers) and EQ tables
for element keys.  JsonAdapter's role (typed access to Gson values with
Java's error messages) is folded into `json.lisp` and the `%gio-*` accessors.

## ELKT reader (`elkt.lisp`)

Upstream is an Xtext grammar, so this is a hand-written recursive-descent
parser over a token vector, rule for rule as ElkGraph.xtext.  Entry points:
`parse-elkt-string`, `parse-elkt-file`.  Conditions: `elkt-parse-exception`.

Value conversion follows the Xtext converters:
- Number: `Double` (`NumberValueConverter`), so layout coordinates are doubles.
- Property key: `ID ('.' ID)*`, with `^` stripped per segment.  A known option
  (by suffix) is the key; an unknown key becomes a plain `(new-property raw)`.
- Property value: STRING (quotes stripped, no escapes), number text,
  `true`/`false`, `null` (no value), or a qualified id (`^` stripped per
  segment).  `parse-value` against the option; otherwise a `layout-option-proxy`.
- Label text: the STRING terminal, with backslash escapes unescaped.

Deliberate differences:
- Keywords are recognised by position, not reserved.
- References are linked after the whole file is parsed, so forward references
  work (Xtext's linking phase is also after parsing).
- The Xtext scope is approximated by a path walk.  A reference `a.b.c` is tried
  from the containing node of the edge, then each ancestor in turn, and each
  step matches a child node or port by identifier.  Upstream's qualified-name
  provider gives the same names in the common case; its full local-scope
  behaviour for same-named siblings is not reproduced.
- Edges keep the containing node where they were declared (no
  `updateContainment`), as the Xtext loader does.  JsonImporter does call it.

## read-graph-file (`read-graph.lisp`)

`(read-graph-file path)` returns the root ElkNode: `.elkt` is parsed; `.json`
is read with JsonImporter; `.elkg` (EMF XMI) is read from its sibling `.json`,
which every `.elkg` in the models has.

## Status / checks

- `tools/check-module.lisp` cannot run on graph-io yet: `src/core/files.txt`
  names `classes.lisp` with its extension, and `core.lisp` (line 277) and
  `core-util/layout-providers.lisp` (line 73) have read errors.  The check was
  run with a copy of the tool that accepts names with extensions; graph-io
  compiles with no warnings of its own.
- Model sweep: all 1448 `.elkt`/`.elkg` files under `tests/`, `tickets/`,
  `examples/` and `realworld/ptolemy/` read without error (1412 with the current
  core image; the rest need ElkReflect registration, see below).
- Undefined-function warnings for `NEW-JSON-IMPORTER/0` and similar are
  compile-time only: `defjclass` defines the default constructor inside an
  `unless fboundp`, which `compile-file` does not see.  They resolve at load.

## Open items

- `layout-meta-data-service.get-instance` (src/core/data.lisp) does not run
  `layout-meta-data-service.init-elk-reflect`, which Java's LayoutMetaDataService
  static initializer runs.  Without it, options whose values are structured
  (`org.eclipse.elk.padding`, `position`, `nodeSize.minimum`, `spacing.portsSurrounding`,
  `port.anchor`) fail with ILLEGAL-STATE-EXCEPTION on any JSON or ELKT input
  that sets them (36 of the 1448 models).  Calling the init by hand makes all
  1448 load.  This is core's fix, not graph-io's.
