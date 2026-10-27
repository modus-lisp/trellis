import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

import org.eclipse.elk.core.RecursiveGraphLayoutEngine;
import org.eclipse.elk.core.data.LayoutAlgorithmData;
import org.eclipse.elk.core.data.LayoutMetaDataService;
import org.eclipse.elk.core.data.LayoutOptionData;
import org.eclipse.elk.core.math.ElkPadding;
import org.eclipse.elk.core.options.*;
import org.eclipse.elk.core.util.BasicProgressMonitor;
import org.eclipse.elk.graph.*;
import org.eclipse.elk.graph.json.ElkGraphJson;
import org.eclipse.elk.graph.util.ElkGraphUtil;

/**
 * Generates a large, seeded corpus of random graphs spread across ELK's input space (algorithms, graph families,
 * sizes, node/label/port shapes, hierarchy and layout options), lays each one out, and records the input graph,
 * the laid-out graph and a manifest line with timing and sanity checks. Mildly biased toward edge cases.
 *
 * Usage: Corpus <outDir> <count> [baseSeed] [shard shards | id,id,...]
 */
public class Corpus {

    // Weighted algorithm mix: layered dominates because it has by far the largest option space.
    static final Object[][] ALGORITHMS = {
        {"org.eclipse.elk.layered", 40}, {"org.eclipse.elk.mrtree", 7}, {"org.eclipse.elk.radial", 6},
        {"org.eclipse.elk.force", 7}, {"org.eclipse.elk.stress", 7}, {"org.eclipse.elk.rectpacking", 7},
        {"org.eclipse.elk.topdownpacking", 3}, {"org.eclipse.elk.sporeOverlap", 4}, {"org.eclipse.elk.sporeCompaction", 3},
        {"org.eclipse.elk.disco", 4}, {"org.eclipse.elk.vertiflex", 3}, {"org.eclipse.elk.box", 3},
        {"org.eclipse.elk.random", 3}, {"org.eclipse.elk.fixed", 3},
    };
    static final String[] FAMILIES = {"gnm", "dag", "tree", "forest", "path", "cycle", "grid", "complete", "bipartite",
        "star", "caterpillar", "powerlaw", "components", "layered-dag"};
    static final String[] SIZE_MODELS = {"uniform", "varied", "labels", "dots", "heavy-tail"};
    // Options that would only make the run slower or noisier without exploring layout behaviour.
    static final Set<String> SKIP_OPTIONS = Set.of("org.eclipse.elk.algorithm", "org.eclipse.elk.debugMode",
        "org.eclipse.elk.resolvedAlgorithm", "org.eclipse.elk.disco.debug.discoGraph", "org.eclipse.elk.disco.debug.discoPolys",
        "org.eclipse.elk.layered.wrapping.validify.forbiddenIndices", "org.eclipse.elk.position", "org.eclipse.elk.timeout");

    /** Everything we decided for one case, written to the manifest. */
    static class Spec {
        int index;
        long seed;
        String algorithm, family, sizeModel;
        int requestedNodes;
        boolean ports, nodeLabels, edgeLabels, hierarchy, includeChildren, mixedAlgorithms, presetPositions;
        List<String> edgeCases = new ArrayList<>();
        Map<String, String> rootOptions = new TreeMap<>();
        Map<String, String> elementOptions = new TreeMap<>(); // option id -> "value (on k elements)"
    }

    public static void main(String[] args) throws Exception {
        Path out = Paths.get(args[0]);
        int count = Integer.parseInt(args[1]);
        long baseSeed = args.length > 2 ? Long.parseLong(args[2]) : 20261025L;
        // args[3] is either a shard number (with args[4] = shard count) or a comma-separated list of case ids to rerun.
        List<Integer> only = args.length > 3 && args[3].contains(",") ? Arrays.stream(args[3].split(",")).map(Integer::parseInt).toList() : null;
        int shard = only == null && args.length > 3 ? Integer.parseInt(args[3]) : 0, shards = args.length > 4 ? Integer.parseInt(args[4]) : 1;
        EdgeCases.registerProviders();
        Path cases = out.resolve("cases");
        Files.createDirectories(cases);

        ExecutorService ex = Executors.newCachedThreadPool(r -> { Thread t = new Thread(r); t.setDaemon(true); return t; });
        String manifestName = only == null ? "manifest-" + shard + ".jsonl" : "manifest-rerun.jsonl";
        List<Integer> indices = only != null ? only : java.util.stream.IntStream.iterate(shard, i -> i < count, i -> i + shards).boxed().toList();
        try (PrintWriter manifest = new PrintWriter(Files.newBufferedWriter(out.resolve(manifestName)))) {
            for (int i : indices) {
                manifest.println(runOne(i, baseSeed + i * 7919L, cases, ex));
                manifest.flush();
                EdgeCases.GROUPS.clear();
                EdgeCases.UNDIRECTED.clear();
            }
        }
        System.exit(0); // don't wait for any timed-out layout threads
    }

    static String runOne(int index, long seed, Path cases, ExecutorService ex) {
        Spec s = new Spec();
        s.index = index;
        s.seed = seed;
        String name = String.format("%04d", index);
        EdgeCases.Result res = new EdgeCases.Result();
        String stage = "generate";
        ElkNode g = null;
        try {
            g = generate(s, new Random(seed));
            stage = "export-input";
            Files.writeString(cases.resolve(name + ".in.json"), export(g));
            stage = "layout";
            ElkNode graph = g;
            Future<Long> f = ex.submit(() -> {
                long t0 = System.nanoTime();
                new RecursiveGraphLayoutEngine().layout(graph, new BasicProgressMonitor());
                return (System.nanoTime() - t0) / 1_000_000;
            });
            res.millis = f.get(30, TimeUnit.SECONDS);
            stage = "check";
            EdgeCases.check(g, res);
            outsideParent(g, res);
            stage = "export-output";
            Files.writeString(cases.resolve(name + ".out.json"), export(g));
        } catch (TimeoutException e) {
            res.status = "timeout";
            res.error = "layout did not finish within 30 s";
        } catch (Throwable t) {
            Throwable c = t instanceof ExecutionException ? t.getCause() : t;
            res.status = stage.equals("layout") ? "error" : stage + "-error";
            StringWriter sw = new StringWriter();
            c.printStackTrace(new PrintWriter(sw));
            res.error = sw.toString().lines().limit(6).reduce((a, b) -> a + "\n" + b).orElse(c.toString());
        }
        int nodes = g == null ? 0 : EdgeCases.countNodes(g), edges = g == null ? 0 : EdgeCases.countEdges(g);
        StringBuilder sb = new StringBuilder("{");
        kv(sb, "id", name); kv(sb, "seed", s.seed); kv(sb, "algorithm", s.algorithm); kv(sb, "family", s.family);
        kv(sb, "sizeModel", s.sizeModel); kv(sb, "requestedNodes", s.requestedNodes); kv(sb, "nodes", nodes); kv(sb, "edges", edges);
        kv(sb, "ports", s.ports); kv(sb, "nodeLabels", s.nodeLabels); kv(sb, "edgeLabels", s.edgeLabels);
        kv(sb, "hierarchy", s.hierarchy); kv(sb, "includeChildren", s.includeChildren); kv(sb, "mixedAlgorithms", s.mixedAlgorithms);
        kv(sb, "presetPositions", s.presetPositions);
        sb.append("\"edgeCases\":[").append(String.join(",", s.edgeCases.stream().map(EdgeCases::js).toList())).append("],");
        sb.append("\"rootOptions\":").append(map(s.rootOptions)).append(',');
        sb.append("\"elementOptions\":").append(map(s.elementOptions)).append(',');
        kv(sb, "status", res.status); kv(sb, "millis", res.millis);
        sb.append("\"width\":").append(Double.isFinite(res.width) ? EdgeCases.f("%.1f", res.width) : "null").append(',');
        sb.append("\"height\":").append(Double.isFinite(res.height) ? EdgeCases.f("%.1f", res.height) : "null").append(',');
        sb.append("\"issues\":[").append(String.join(",", res.issues.stream().map(EdgeCases::js).toList())).append("],");
        sb.append("\"error\":").append(EdgeCases.js(res.error)).append('}');
        System.out.printf(Locale.ROOT, "%s %-16s %-12s n=%-5d %-8s %6d ms %s%n", name, shortName(s.algorithm), s.family, nodes,
                res.status, res.millis, res.error != null ? res.error.lines().findFirst().orElse("") : String.join("; ", res.issues));
        return sb.toString();
    }

    static String shortName(String a) { return a == null ? "?" : a.substring(a.lastIndexOf('.') + 1); }

    static void kv(StringBuilder sb, String k, Object v) {
        sb.append('"').append(k).append("\":");
        if (v instanceof String str) sb.append(EdgeCases.js(str)); else sb.append(v == null ? "null" : v.toString());
        sb.append(',');
    }

    static String map(Map<String, String> m) {
        StringBuilder sb = new StringBuilder("{");
        m.forEach((k, v) -> sb.append(EdgeCases.js(k)).append(':').append(EdgeCases.js(v)).append(','));
        if (sb.length() > 1) sb.setLength(sb.length() - 1);
        return sb.append('}').toString();
    }

    static String export(ElkNode g) {
        return ElkGraphJson.forGraph(g).prettyPrint(false).omitZeroPositions(true).omitUnknownLayoutOptions(false).toJson();
    }

    /** Counts children whose box extends beyond their parent's box (only meaningful for compound nodes). */
    static void outsideParent(ElkNode g, EdgeCases.Result res) {
        int outside = 0;
        Deque<ElkNode> st = new ArrayDeque<>(g.getChildren());
        while (!st.isEmpty()) {
            ElkNode p = st.pop();
            for (ElkNode c : p.getChildren()) {
                if (c.getX() < -1 || c.getY() < -1 || c.getX() + c.getWidth() > p.getWidth() + 1 || c.getY() + c.getHeight() > p.getHeight() + 1) outside++;
                st.push(c);
            }
        }
        if (outside > 0) {
            res.issues.add(outside + " child node(s) outside their parent");
            res.status = "warn";
        }
    }

    // =====================================================================================================
    // Sampling
    // =====================================================================================================

    static <T> T pick(Random r, T[] xs) { return xs[r.nextInt(xs.length)]; }

    static String pickAlgorithm(Random r) {
        int total = 0;
        for (Object[] a : ALGORITHMS) total += (Integer) a[1];
        int x = r.nextInt(total);
        for (Object[] a : ALGORITHMS) { x -= (Integer) a[1]; if (x < 0) return (String) a[0]; }
        throw new AssertionError();
    }

    static int sampleSize(Random r, Spec s) {
        double u = r.nextDouble();
        if (u < 0.02) { s.edgeCases.add("empty-or-single"); return r.nextInt(2); }
        if (u < 0.04) { s.edgeCases.add("large"); return 500 + r.nextInt(1000); }
        return (int) Math.round(Math.exp(Math.log(2) + r.nextDouble() * (Math.log(250) - Math.log(2))));
    }

    static ElkNode generate(Spec s, Random r) {
        s.algorithm = pickAlgorithm(r);
        s.family = pick(r, FAMILIES);
        s.sizeModel = pick(r, SIZE_MODELS);
        s.requestedNodes = sampleSize(r, s);
        boolean undirected = s.algorithm.endsWith("force") || s.algorithm.endsWith("stress") || s.algorithm.endsWith("disco");
        boolean tree = s.algorithm.endsWith("mrtree") || s.algorithm.endsWith("radial") || s.algorithm.endsWith("vertiflex");
        // Tree algorithms usually get trees, but a quarter of the time they get whatever was drawn.
        if (tree && r.nextDouble() < 0.75) s.family = r.nextBoolean() ? "tree" : (r.nextBoolean() ? "caterpillar" : "star");
        s.ports = r.nextDouble() < (s.algorithm.endsWith("layered") ? 0.35 : 0.08);
        s.nodeLabels = s.sizeModel.equals("labels") || r.nextDouble() < 0.3;
        s.edgeLabels = r.nextDouble() < 0.2;
        s.hierarchy = !tree && s.requestedNodes >= 6 && r.nextDouble() < 0.18;
        s.presetPositions = r.nextDouble() < (s.algorithm.contains("spore") || s.algorithm.endsWith("fixed") ? 0.9 : 0.1);

        ElkNode g = EdgeCases.root(s.algorithm);
        if (undirected) EdgeCases.undirected(g);
        int n = s.requestedNodes;
        List<ElkNode> ns = new ArrayList<>();
        for (int i = 0; i < n; i++) ns.add(makeNode(g, s, r, i));
        buildEdges(s, r, ns);

        if (s.hierarchy) nest(g, s, r, ns);
        if (s.edgeLabels) for (ElkEdge e : allEdges(g)) if (r.nextDouble() < 0.5) EdgeCases.label(e, randomText(r, 1 + r.nextInt(r.nextDouble() < 0.1 ? 60 : 12)));
        if (s.presetPositions) for (ElkNode x : ns) x.setLocation(r.nextGaussian() * 300, r.nextGaussian() * 200);

        if (r.nextDouble() < 0.15) injectEdgeCases(g, s, r, ns);
        sampleOptions(g, s, r);
        return g;
    }

    static ElkNode makeNode(ElkNode parent, Spec s, Random r, int i) {
        double w, h;
        switch (s.sizeModel) {
            case "uniform" -> { w = 40; h = 24; }
            case "varied" -> { w = 10 + r.nextInt(100); h = 10 + r.nextInt(70); }
            case "dots" -> { w = 10; h = 10; }
            case "heavy-tail" -> { w = Math.min(2000, 8 * Math.exp(r.nextGaussian() * 1.1)); h = Math.min(2000, 8 * Math.exp(r.nextGaussian() * 1.1)); }
            default -> { w = 0; h = 0; }
        }
        ElkNode x;
        if (s.nodeLabels) {
            String t = randomText(r, 1 + r.nextInt(r.nextDouble() < 0.05 ? 80 : 14));
            x = EdgeCases.box(parent, t);
            if (!s.sizeModel.equals("labels")) x.setDimensions(Math.max(w, 1), Math.max(h, 1));
            else if (r.nextBoolean()) x.setProperty(CoreOptions.NODE_SIZE_CONSTRAINTS, SizeConstraint.free());
        } else {
            x = ElkGraphUtil.createNode(parent);
            x.setDimensions(w == 0 ? 30 : w, h == 0 ? 20 : h);
        }
        x.setIdentifier("n" + i);
        return x;
    }

    static final String ALPHA = "abcdefghijklmnopqrstuvwxyz      0123456789_-.";

    static String randomText(Random r, int len) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < len; i++) sb.append(ALPHA.charAt(r.nextInt(ALPHA.length())));
        String t = sb.toString().trim();
        return t.isEmpty() ? "x" : t;
    }

    /** Connects two nodes, either directly or through freshly created ports. */
    static void connect(Spec s, Random r, ElkNode a, ElkNode b) {
        if (!s.ports || r.nextDouble() < 0.2) { EdgeCases.edge(a, b); return; }
        ElkConnectableShape pa = portFor(s, r, a, true), pb = portFor(s, r, b, false);
        EdgeCases.edge(pa, pb);
    }

    static ElkPort portFor(Spec s, Random r, ElkNode n, boolean out) {
        // Reuse an existing port sometimes, which creates hyperedge-like fan-in/out.
        if (!n.getPorts().isEmpty() && r.nextDouble() < 0.3) return n.getPorts().get(r.nextInt(n.getPorts().size()));
        PortSide side = r.nextDouble() < 0.7 ? (out ? PortSide.EAST : PortSide.WEST) : pick(r, PortSide.values());
        ElkPort p = EdgeCases.port(n, side);
        if (r.nextDouble() < 0.25) EdgeCases.label(p, randomText(r, 1 + r.nextInt(8)));
        return p;
    }

    static void buildEdges(Spec s, Random r, List<ElkNode> ns) {
        int n = ns.size();
        if (n < 2) return;
        switch (s.family) {
            case "gnm" -> { int m = (int) (n * (0.5 + r.nextDouble() * 2.5)); for (int i = 0; i < m; i++) { int a = r.nextInt(n), b = r.nextInt(n); if (a != b) connect(s, r, ns.get(a), ns.get(b)); } }
            case "dag" -> { int m = (int) (n * (0.8 + r.nextDouble() * 2)); for (int i = 0; i < m; i++) { int a = r.nextInt(n - 1); connect(s, r, ns.get(a), ns.get(a + 1 + r.nextInt(n - a - 1))); } }
            case "tree" -> { for (int i = 1; i < n; i++) connect(s, r, ns.get(r.nextInt(i)), ns.get(i)); }
            case "forest" -> { for (int i = 1; i < n; i++) if (r.nextDouble() < 0.85) connect(s, r, ns.get(r.nextInt(i)), ns.get(i)); }
            case "path" -> { for (int i = 1; i < n; i++) connect(s, r, ns.get(i - 1), ns.get(i)); }
            case "cycle" -> { for (int i = 0; i < n; i++) connect(s, r, ns.get(i), ns.get((i + 1) % n)); }
            case "grid" -> { int w = Math.max(1, (int) Math.sqrt(n)); for (int i = 0; i < n; i++) { if ((i + 1) % w != 0 && i + 1 < n) connect(s, r, ns.get(i), ns.get(i + 1)); if (i + w < n) connect(s, r, ns.get(i), ns.get(i + w)); } }
            case "complete" -> { int k = Math.min(n, 30); for (int i = 0; i < k; i++) for (int j = i + 1; j < k; j++) connect(s, r, ns.get(i), ns.get(j)); }
            case "bipartite" -> { int h = n / 2; double p = Math.min(1, 3.0 / Math.max(1, h)); for (int i = 0; i < h; i++) for (int j = h; j < n; j++) if (r.nextDouble() < p) connect(s, r, ns.get(i), ns.get(j)); }
            case "star" -> { for (int i = 1; i < n; i++) if (r.nextBoolean()) connect(s, r, ns.get(0), ns.get(i)); else connect(s, r, ns.get(i), ns.get(0)); if (s.algorithm.endsWith("mrtree") || s.algorithm.endsWith("radial") || s.algorithm.endsWith("vertiflex")) { /* keep tree-shaped */ } }
            case "caterpillar" -> { int spine = Math.max(1, n / 4); for (int i = 1; i < spine; i++) connect(s, r, ns.get(i - 1), ns.get(i)); for (int i = spine; i < n; i++) connect(s, r, ns.get(r.nextInt(spine)), ns.get(i)); }
            case "powerlaw" -> { List<ElkNode> ends = new ArrayList<>(List.of(ns.get(0))); for (int i = 1; i < n; i++) { int k = 1 + r.nextInt(2); for (int j = 0; j < k; j++) { ElkNode t = ends.get(r.nextInt(ends.size())); if (t != ns.get(i)) { connect(s, r, ns.get(i), t); ends.add(t); } } ends.add(ns.get(i)); } }
            case "components" -> { int i = 0; while (i < n) { int k = 1 + r.nextInt(8); for (int j = i + 1; j < Math.min(n, i + k); j++) connect(s, r, ns.get(i + r.nextInt(j - i)), ns.get(j)); i += k; } }
            case "layered-dag" -> { int layers = 2 + r.nextInt(Math.max(1, Math.min(12, n / 2))); for (int i = 0; i < n; i++) { int li = i * layers / n; for (int j = i + 1; j < n; j++) if (j * layers / n == li + 1 && r.nextDouble() < 2.5 / Math.max(1, n / layers)) connect(s, r, ns.get(i), ns.get(j)); } }
            default -> throw new IllegalStateException(s.family);
        }
        // For star on tree algorithms, the in-edges above would break tree shape; that's part of the spectrum.
    }

    static List<ElkEdge> allEdges(ElkNode g) {
        List<ElkEdge> l = new ArrayList<>();
        Deque<ElkNode> st = new ArrayDeque<>(List.of(g));
        while (!st.isEmpty()) { ElkNode p = st.pop(); l.addAll(p.getContainedEdges()); st.addAll(p.getChildren()); }
        return l;
    }

    /** Moves nodes into nested compound nodes, keeping existing edges (which then cross hierarchy levels). */
    static void nest(ElkNode g, Spec s, Random r, List<ElkNode> ns) {
        s.includeChildren = s.algorithm.endsWith("layered") && r.nextDouble() < 0.6;
        if (s.includeChildren) g.setProperty(CoreOptions.HIERARCHY_HANDLING, HierarchyHandling.INCLUDE_CHILDREN);
        s.mixedAlgorithms = r.nextDouble() < 0.25;
        int groups = 1 + r.nextInt(Math.max(1, Math.min(8, ns.size() / 4)));
        List<ElkNode> containers = new ArrayList<>(List.of(g));
        for (int k = 0; k < groups; k++) {
            ElkNode parent = containers.get(r.nextInt(containers.size()));
            ElkNode c = EdgeCases.group(parent, r.nextBoolean() ? "group " + k : null);
            if (s.mixedAlgorithms) c.setProperty(CoreOptions.ALGORITHM, pickAlgorithm(r));
            containers.add(c);
        }
        for (ElkNode x : ns) if (r.nextDouble() < 0.7) x.setParent(containers.get(1 + r.nextInt(containers.size() - 1)));
        // Re-home every edge to its lowest common ancestor so the graph stays valid.
        for (ElkEdge e : allEdges(g)) ElkGraphUtil.updateContainment(e);
    }

    static void injectEdgeCases(ElkNode g, Spec s, Random r, List<ElkNode> ns) {
        int k = 1 + r.nextInt(2);
        for (int i = 0; i < k; i++) {
            String kind = pick(r, new String[] {"self-loops", "multi-edges", "zero-size", "isolated", "huge-node", "thin-nodes",
                "negative-size", "nan-size", "far-position", "long-labels", "spacing-zero", "spacing-negative", "spacing-huge", "duplicate-ports"});
            if (ns.isEmpty() && !kind.startsWith("spacing")) kind = "isolated";
            s.edgeCases.add(kind);
            switch (kind) {
                case "self-loops" -> { for (int j = 0; j < 1 + r.nextInt(5); j++) { ElkNode x = ns.get(r.nextInt(ns.size())); connect(s, r, x, x); } }
                case "multi-edges" -> { if (ns.size() > 1) { ElkNode a = ns.get(r.nextInt(ns.size())), b = ns.get(r.nextInt(ns.size())); for (int j = 0; j < 2 + r.nextInt(8); j++) connect(s, r, a, b); } }
                case "zero-size" -> { for (ElkNode x : ns) if (r.nextDouble() < 0.5) x.setDimensions(0, 0); }
                case "isolated" -> { for (int j = 0; j < 1 + r.nextInt(10); j++) ns.add(makeNode(g, s, r, 10000 + j)); }
                case "huge-node" -> ns.get(r.nextInt(ns.size())).setDimensions(2000 + r.nextInt(50000), 2000 + r.nextInt(50000));
                case "thin-nodes" -> { for (ElkNode x : ns) if (r.nextDouble() < 0.5) { if (r.nextBoolean()) x.setDimensions(300 + r.nextInt(500), 1); else x.setDimensions(1, 300 + r.nextInt(500)); } }
                case "negative-size" -> ns.get(r.nextInt(ns.size())).setDimensions(-1 - r.nextInt(100), -1 - r.nextInt(100));
                case "nan-size" -> ns.get(r.nextInt(ns.size())).setDimensions(Double.NaN, 20);
                case "far-position" -> ns.get(r.nextInt(ns.size())).setLocation(1e7, -1e7);
                case "long-labels" -> { for (int j = 0; j < 3; j++) EdgeCases.label(ns.get(r.nextInt(ns.size())), randomText(r, 200)); }
                case "spacing-zero" -> { g.setProperty(CoreOptions.SPACING_NODE_NODE, 0.0); g.setProperty(CoreOptions.SPACING_EDGE_EDGE, 0.0); g.setProperty(CoreOptions.SPACING_EDGE_NODE, 0.0); }
                case "spacing-negative" -> g.setProperty(CoreOptions.SPACING_NODE_NODE, -20.0 - r.nextInt(50));
                case "spacing-huge" -> g.setProperty(CoreOptions.SPACING_NODE_NODE, 1000.0 + r.nextInt(5000));
                case "duplicate-ports" -> { ElkNode x = ns.get(r.nextInt(ns.size())); ElkPort p = EdgeCases.port(x, PortSide.EAST); for (int j = 0; j < 3 + r.nextInt(15); j++) EdgeCases.edge(p, ns.get(r.nextInt(ns.size()))); }
                default -> throw new IllegalStateException(kind);
            }
        }
    }

    /** Picks a handful of the options the chosen algorithm declares and assigns random values. */
    static void sampleOptions(ElkNode g, Spec s, Random r) {
        LayoutMetaDataService svc = LayoutMetaDataService.getInstance();
        LayoutAlgorithmData alg = svc.getAlgorithmData(s.algorithm);
        if (alg == null) throw new IllegalStateException("algorithm not registered: " + s.algorithm);
        List<LayoutOptionData> parentOpts = new ArrayList<>(), nodeOpts = new ArrayList<>(), edgeOpts = new ArrayList<>(), portOpts = new ArrayList<>();
        List<LayoutOptionData> all = new ArrayList<>(svc.getOptionData());
        all.sort(Comparator.comparing(LayoutOptionData::getId)); // deterministic order for reproducibility
        for (LayoutOptionData o : all) {
            if (SKIP_OPTIONS.contains(o.getId()) || !alg.knowsOption(o.getId())) continue;
            switch (o.getType()) { case BOOLEAN, INT, DOUBLE, ENUM, ENUMSET -> {} default -> { continue; } }
            if (o.getTargets().contains(LayoutOptionData.Target.PARENTS)) parentOpts.add(o);
            if (o.getTargets().contains(LayoutOptionData.Target.NODES)) nodeOpts.add(o);
            if (o.getTargets().contains(LayoutOptionData.Target.EDGES)) edgeOpts.add(o);
            if (o.getTargets().contains(LayoutOptionData.Target.PORTS)) portOpts.add(o);
        }
        int k = r.nextDouble() < 0.15 ? 0 : 1 + (int) Math.min(parentOpts.size(), Math.abs(r.nextGaussian() * 4));
        Collections.shuffle(parentOpts, r);
        for (LayoutOptionData o : parentOpts.subList(0, Math.min(k, parentOpts.size()))) {
            if (!keep(o, r)) continue;
            Object v = sampleValue(o, alg, r, s);
            if (v == null) continue;
            g.setProperty(o, v);
            s.rootOptions.put(o.getId(), String.valueOf(v));
        }
        List<ElkNode> nodes = new ArrayList<>();
        List<ElkPort> ports = new ArrayList<>();
        Deque<ElkNode> st = new ArrayDeque<>(g.getChildren());
        while (!st.isEmpty()) { ElkNode x = st.pop(); nodes.add(x); ports.addAll(x.getPorts()); st.addAll(x.getChildren()); }
        List<ElkEdge> edges = allEdges(g);
        applyElementOptions(r, s, alg, nodeOpts, nodes, 0.35);
        applyElementOptions(r, s, alg, edgeOpts, edges, 0.2);
        applyElementOptions(r, s, alg, portOpts, ports, 0.15);
    }

    static void applyElementOptions(Random r, Spec s, LayoutAlgorithmData alg, List<LayoutOptionData> opts,
            List<? extends ElkGraphElement> targets, double chance) {
        if (opts.isEmpty() || targets.isEmpty() || r.nextDouble() >= chance) return;
        int k = 1 + r.nextInt(Math.min(2, opts.size()));
        for (int i = 0; i < k; i++) {
            LayoutOptionData o = opts.get(r.nextInt(opts.size()));
            if (!keep(o, r)) continue;
            Object v = sampleValue(o, alg, r, s);
            if (v == null) continue;
            int applied = 0;
            double frac = 0.05 + r.nextDouble() * 0.5;
            for (ElkGraphElement t : targets) if (r.nextDouble() < frac) { t.setProperty(o, v); applied++; }
            if (applied > 0) s.elementOptions.put(o.getId(), v + " (on " + applied + ")");
        }
    }

    /**
     * Keeps the corpus only slightly biased toward invalid input: enum sets (mostly invalid when random) and the
     * top-down layout family (which needs a coordinated set of options) are drawn far less often.
     */
    static boolean keep(LayoutOptionData o, Random r) {
        if (o.getId().contains("topdown")) return r.nextDouble() < 0.12;
        if (o.getType() == LayoutOptionData.Type.ENUMSET) return r.nextDouble() < 0.25;
        return true;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    static Object sampleValue(LayoutOptionData o, LayoutAlgorithmData alg, Random r, Spec s) {
        Object def = alg.getDefaultValue(o.getId());
        if (def == null) def = o.getDefault();
        boolean edge = r.nextDouble() < 0.12;
        switch (o.getType()) {
            case BOOLEAN: return r.nextBoolean();
            case ENUM: {
                Object[] cs = o.getOptionClass().getEnumConstants();
                return cs[r.nextInt(cs.length)];
            }
            case ENUMSET: {
                Class<? extends Enum> cls = (Class<? extends Enum>) o.getOptionClass();
                EnumSet set = EnumSet.noneOf(cls);
                for (Object c : cls.getEnumConstants()) if (r.nextDouble() < 0.35) set.add(c);
                return set;
            }
            case INT: {
                int d = def instanceof Number nd ? nd.intValue() : 1;
                if (edge) { s.edgeCases.add("option-extreme:" + o.getId()); return pick(r, new Integer[] {0, -1, -d, Math.max(1, d) * 50, Integer.MAX_VALUE / 2}); }
                int v = (int) Math.round(Math.max(1, Math.abs(d)) * Math.exp(r.nextGaussian() * 0.7));
                // Iteration counts and similar stay bounded so the corpus finishes.
                return Math.min(v, 5000);
            }
            case DOUBLE: {
                double d = def instanceof Number nd ? nd.doubleValue() : 1.0;
                if (edge) { s.edgeCases.add("option-extreme:" + o.getId()); return pick(r, new Double[] {0.0, -1.0, -Math.abs(d) - 10, Math.max(1, Math.abs(d)) * 1000, 1e-9}); }
                double base = d == 0 ? 1 : Math.abs(d);
                double v = base * Math.exp(r.nextGaussian() * 0.8);
                if (o.getLowerBound() instanceof Number lb && v < lb.doubleValue()) v = lb.doubleValue();
                if (o.getUpperBound() instanceof Number ub && v > ub.doubleValue()) v = ub.doubleValue();
                return Math.round(v * 1000) / 1000.0;
            }
            default: return null;
        }
    }
}
