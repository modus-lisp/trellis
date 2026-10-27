# ELK random layout corpus

1,000 randomly generated graphs, each laid out by the Eclipse Layout Kernel built from source
(eclipse/elk @ `3514a6a`, version 0.13.0-SNAPSHOT). Each case is stored as ELK JSON before and after layout,
and the manifest records what was sampled and what happened.

The sampling aims for a broad spread across the input space, with a mild tilt toward edge cases:

| dimension | what varies |
|---|---|
| algorithm | 14 algorithms, weighted: layered 40%, then mrtree, radial, force, stress, rectpacking, topdownpacking, SPOrE (overlap + compaction), DisCo, VertiFlex, box, random, fixed |
| graph family | gnm, dag, tree, forest, path, cycle, grid, complete, bipartite, star, caterpillar, powerlaw, components, layered-dag. Tree algorithms get tree-shaped families 75% of the time, other families the rest |
| size | log-uniform 2–250 nodes; 2% empty or single node; 2% large (500–1,500) |
| node size | uniform, varied, label-driven, dots, heavy-tailed (lognormal up to 2,000 px) |
| labels | node labels in ~30–40% of cases (5% very long), edge labels in ~20%, port labels on 25% of ports |
| ports | ~35% of layered cases, ~8% otherwise; mostly on the flow-appropriate side, sometimes any side; ports are sometimes reused (fan-in/out) |
| hierarchy | ~18% of non-tree cases nest nodes into up to 8 compound nodes. Layered uses INCLUDE_CHILDREN 60% of the time; 25% give compound nodes a random algorithm of their own |
| layout options | 0–~8 options chosen by reflection from the options the algorithm declares (boolean / int / double / enum / enum-set) on the root, plus node-, edge- and port-level options on random subsets. Numbers are drawn around the default (lognormal) and clamped to declared bounds; 12% get an extreme value (0, −1, negative, ×50/×1000, huge) |
| edge-case injections | 15% of cases get 1–2 of: self-loops, multi-edges, zero-size nodes, isolated nodes, a huge node, thin nodes, negative size, NaN size, a far-away position, very long labels, zero/negative/huge spacing, an overloaded port |

Each layout runs with a 30-second timeout, followed by these checks:
- non-finite coordinates
- overlapping sibling nodes
- edge endpoints that don't touch their source or target (3 px tolerance)
- edges with no route
- child nodes that fall outside their parent

## Files

```
SUMMARY.md          readable overview: outcomes, by algorithm/family, grouped exceptions, timeouts, issues
summary.json        the same numbers, machine-readable
manifest.jsonl      one line per case (fields below)
cases/NNNN.in.json  input graph in ELK JSON, including every layout option that was set
cases/NNNN.out.json laid-out graph in ELK JSON (missing when the layout threw or timed out)
src/                the generator (Corpus.java) and the shared helpers/checks (EdgeCases.java)
summarize_corpus.py builds SUMMARY.md / summary.json from the manifest
```

Manifest fields: `id`, `seed`, `algorithm`, `family`, `sizeModel`, `requestedNodes`, `nodes`, `edges`
(counted over all hierarchy levels), `ports`, `nodeLabels`, `edgeLabels`, `hierarchy`, `includeChildren`,
`mixedAlgorithms`, `presetPositions`, `edgeCases` (injections and extreme option values), `rootOptions`,
`elementOptions` (value and how many elements got it), `status` (`ok` / `warn` / `error` / `timeout`), `millis`,
`width`, `height`, `issues`, `error` (first lines of the stack trace).

The `*.in.json` files load in [ELK Live](https://rtsys.informatik.uni-kiel.de/elklive/json.html) or elkjs, and with
`ElkGraphJson.forGraph(json).toElk()` in Java. That is the quickest way to replay a single case. A few inputs contain
values that JSON can't represent (NaN sizes), so Gson writes them as `NaN`; a strict JSON parser will reject those
files.

## Reproducing

Case *i* uses seed `20261025 + i × 7919`, so any case can be regenerated on its own. With the ELK jars on the
classpath:

```
javac -d classes -cp "$CP" src/EdgeCases.java src/Corpus.java
java -cp "classes:$CP" Corpus out 1000 20261025 0 1      # or shard k of n:  ... 20261025 k n
python3 summarize_corpus.py out
```

Layout times come from 4 shards running in parallel on a 4-core container, so treat them as relative. 52 cases
(ids 807–999 from shards 2 and 3) were re-run by themselves in one JVM after a harness slip lost their manifest
lines. Their inputs and results are identical by construction, but their timings come from that separate run. To
re-run specific cases, pass a comma-separated id list in place of the shard arguments:
`java -cp "classes:$CP" Corpus out 1000 20261025 12,345,678`.
