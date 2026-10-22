# trellis

Automatic graph layout for Common Lisp, after the
[Eclipse Layout Kernel](https://eclipse.dev/elk/) (ELK). It reads and writes
ELK's JSON graph format, so elkjs input works unchanged.

```lisp
(trellis:layout-json
 "{\"id\":\"root\",\"layoutOptions\":{\"elk.direction\":\"DOWN\"},
   \"children\":[{\"id\":\"a\",\"width\":40,\"height\":30},
                 {\"id\":\"b\",\"width\":40,\"height\":30}],
   \"edges\":[{\"id\":\"e\",\"sources\":[\"a\"],\"targets\":[\"b\"]}]}")
;; => the same graph with x/y on every node and "sections" on every edge

(trellis:graph->svg (trellis:layout (trellis:read-graph-file "graph.json")))
;; => SVG markup, a string when no stream is given
```

## API

| function | |
|---|---|
| `layout-json text` | ELK JSON text in, laid-out ELK JSON text out |
| `layout graph` | lays an ELK graph out in place and returns it |
| `graph->svg graph &optional stream` | SVG markup of a laid-out graph |
| `read-graph-file path` | `.json`, `.elkt`, or `.elkg` (read through its sibling `.json`) |
| `read-elkt path` | an ELK Graph Text file |

Each level of a graph is laid out by its `elk.algorithm` option. The
`layered` algorithm is the one ported so far.

## Status

The layered engine is a port of ELK's, with ELK's class structure and option
names. ELK's own layered test suite is ported under `src/test-*`. Most of it
passes; six layout-quality checks still fail (port and label placement, a
self-loop label, a node sized too small for its content, and partition
order). Spline edge routing runs, but its geometry has not been checked
against ELK's reference output.

Not ported: the other ELK algorithms (box, fixed, force, mrtree, radial,
rectpacking, spore, stress, topdown).

## Running the tests

The test models are ELK's and are not in this repository. Point `MODELS_REPO`
at a checkout of them. `src/test-framework` has the runner, `run-elk-tests`.

## Source layout

`src/MODULES` lists the modules in load order, and each module has a
`files.txt`. `PORTING.md` describes the naming conventions.

## Dependencies

[json-simple](https://github.com/modus-lisp) for JSON. SVG rendering is done
by [stencil](https://github.com/modus-lisp), not by this library.

## License

MIT. See `LICENSE`.
