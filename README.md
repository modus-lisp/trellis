# diagram

Automatic graph layout for Common Lisp, after the
[Eclipse Layout Kernel](https://eclipse.dev/elk/) (ELK).  It reads and writes
**ELK's JSON graph format**, so an elkjs input works unchanged, and it has no
dependencies.

```lisp
(diagram:layout-json "{\"id\":\"root\",\"layoutOptions\":{\"elk.direction\":\"DOWN\"},
  \"children\":[{\"id\":\"a\",\"width\":40,\"height\":30},{\"id\":\"b\",\"width\":40,\"height\":30}],
  \"edges\":[{\"id\":\"e\",\"sources\":[\"a\"],\"targets\":[\"b\"]}]}")
;; => the same graph with x/y on every node and "sections" on every edge

(diagram:graph->svg
  (diagram:layout
    (diagram:plist->graph
      '(:id "root" :children ((:id "a" :labels ((:text "a"))) (:id "b" :labels ((:text "b"))))
        :edges ((:id "e" :sources ("a") :targets ("b")))))))
```

## Algorithms

| `elk.algorithm` | |
|---|---|
| `layered` (default) | Sugiyama: DFS cycle breaking, longest-path layering, dummy nodes for long edges, barycenter sweeps + transposition for crossings, node placement by isotonic regression toward neighbours, orthogonal routing with per-channel slot assignment (or polyline) |
| `box` / `rectpacking` | rows of nodes, straight edges |
| `fixed` | keep given positions, straight edges |

Supported options (the `org.eclipse.elk.` / `elk.` / `layered.` prefixes are optional):
`direction` (RIGHT DOWN LEFT UP), `edgeRouting` (ORTHOGONAL POLYLINE; SPLINES
falls back to polyline), `padding`, `spacing.nodeNode`,
`spacing.nodeNodeBetweenLayers`, `spacing.edgeEdge`, `spacing.edgeNode`,
`spacing.componentComponent`, `separateConnectedComponents`.  Options are
inherited from ancestors.

Hierarchy: each compound node is laid out bottom-up with its own options and
sized to fit.  An edge belongs to the deepest node that contains both ends
(reported as `"container"`; its points are relative to that node).

Ports: a port with outgoing edges goes on the downstream side, incoming on
the upstream side, ordered to avoid crossings.

## Not yet (vs. ELK)

- edges that cross a hierarchy boundary attach to the outer compound node,
  not through it to the inner node (ELK's `INCLUDE_CHILDREN`)
- `portConstraints` other than FREE; port labels are not placed
- Brandes–Köpf placement, network-simplex layering, splines, edge-label
  space reservation, the force/stress/mrtree/radial algorithms

## Tests

    sbcl --script test/run.lisp

Property checks rather than golden output, which is ELK's own style: no
sibling overlaps, children inside parents, every orthogonal segment
axis-aligned, every edge anchored on its endpoints' borders, flow order per
direction, JSON round trip keeping unknown fields; plus 40 seeded random
graphs.  `examples/` has rendered SVGs.

License: EPL-2.0, ELK's.
