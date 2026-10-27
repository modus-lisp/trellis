import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Supplier;

import org.eclipse.elk.core.RecursiveGraphLayoutEngine;
import org.eclipse.elk.core.data.ILayoutMetaDataProvider;
import org.eclipse.elk.core.data.LayoutMetaDataService;
import org.eclipse.elk.core.data.LayoutOptionData;
import org.eclipse.elk.core.math.ElkPadding;
import org.eclipse.elk.core.math.KVector;
import org.eclipse.elk.core.options.*;
import org.eclipse.elk.core.util.BasicProgressMonitor;
import org.eclipse.elk.graph.*;
import org.eclipse.elk.graph.util.ElkGraphUtil;

/**
 * Stress-tests ELK with a catalog of edge-case graphs. Each case is laid out with a timeout, checked for
 * obvious defects (exceptions, non-finite coordinates, overlapping siblings, detached edges) and rendered to SVG.
 */
public class EdgeCases {

    static final String[] PALETTE = {"#4C78A8", "#F58518", "#54A24B", "#B279A2", "#E45756", "#72B7B2", "#EECA3B", "#9D755D"};
    static final String LAYERED = "org.eclipse.elk.layered", MRTREE = "org.eclipse.elk.mrtree",
            RADIAL = "org.eclipse.elk.radial", FORCE = "org.eclipse.elk.force", STRESS = "org.eclipse.elk.stress",
            RECTPACKING = "org.eclipse.elk.rectpacking", TOPDOWN = "org.eclipse.elk.topdownpacking",
            SPORE_OVERLAP = "org.eclipse.elk.sporeOverlap", SPORE_COMPACTION = "org.eclipse.elk.sporeCompaction",
            DISCO = "org.eclipse.elk.disco", VERTIFLEX = "org.eclipse.elk.vertiflex", FIXED = "org.eclipse.elk.fixed",
            BOX = "org.eclipse.elk.box", RANDOM = "org.eclipse.elk.random";

    record Case(String id, String category, String title, String note, Supplier<ElkNode> graph) {}

    static final List<Case> CASES = new ArrayList<>();

    static void add(String id, String category, String title, String note, Supplier<ElkNode> g) {
        CASES.add(new Case(id, category, title, note, g));
    }

    // =====================================================================================================
    // Graph construction helpers
    // =====================================================================================================

    static final Set<ElkNode> UNDIRECTED = Collections.newSetFromMap(new IdentityHashMap<>());
    static final Set<ElkNode> GROUPS = Collections.newSetFromMap(new IdentityHashMap<>());

    static ElkNode root(String algorithm) {
        ElkNode g = ElkGraphUtil.createGraph();
        g.setProperty(CoreOptions.ALGORITHM, algorithm);
        g.setProperty(CoreOptions.PADDING, new ElkPadding(20));
        return g;
    }

    static ElkNode undirected(ElkNode g) {
        UNDIRECTED.add(g);
        return g;
    }

    static ElkNode box(ElkNode parent, String text) {
        return box(parent, text, Math.max(40, textWidth(text, 12) + 16), 28);
    }

    static ElkNode box(ElkNode parent, String text, double w, double h) {
        ElkNode n = ElkGraphUtil.createNode(parent);
        n.setDimensions(w, h);
        if (text != null && !text.isEmpty()) {
            label(n, text);
            n.setProperty(CoreOptions.NODE_LABELS_PLACEMENT, NodeLabelPlacement.insideCenter());
        }
        return n;
    }

    static ElkNode dot(ElkNode parent) {
        ElkNode n = ElkGraphUtil.createNode(parent);
        n.setDimensions(12, 12);
        return n;
    }

    static ElkNode group(ElkNode parent, String text) {
        ElkNode n = ElkGraphUtil.createNode(parent);
        GROUPS.add(n);
        if (text != null) {
            label(n, text);
            n.setProperty(CoreOptions.NODE_LABELS_PLACEMENT,
                    EnumSet.of(NodeLabelPlacement.INSIDE, NodeLabelPlacement.V_TOP, NodeLabelPlacement.H_CENTER));
        }
        n.setProperty(CoreOptions.NODE_SIZE_CONSTRAINTS, EnumSet.of(SizeConstraint.NODE_LABELS, SizeConstraint.MINIMUM_SIZE));
        n.setProperty(CoreOptions.PADDING, new ElkPadding(26, 12, 12, 12));
        n.setDimensions(40, 40);
        return n;
    }

    static ElkLabel label(ElkGraphElement e, String text) {
        ElkLabel l = ElkGraphUtil.createLabel(text, e);
        double fs = e instanceof ElkNode ? 12 : 10;
        l.setDimensions(textWidth(text, fs), fs + 3);
        return l;
    }

    static ElkPort port(ElkNode n, PortSide side) {
        ElkPort p = ElkGraphUtil.createPort(n);
        p.setDimensions(7, 7);
        p.setProperty(CoreOptions.PORT_SIDE, side);
        return p;
    }

    static ElkEdge edge(ElkConnectableShape a, ElkConnectableShape b) {
        return ElkGraphUtil.createSimpleEdge(a, b);
    }

    static ElkEdge edge(ElkConnectableShape a, ElkConnectableShape b, String text) {
        ElkEdge e = edge(a, b);
        label(e, text);
        return e;
    }

    static double textWidth(String s, double fs) {
        return Math.ceil(s.length() * fs * 0.58);
    }

    /** Sets an option by its full id, parsing the value the same way ELK's JSON importer does. */
    static void opt(ElkGraphElement e, String id, Object value) {
        LayoutOptionData d = LayoutMetaDataService.getInstance().getOptionData(id);
        if (d == null) throw new IllegalArgumentException("unknown layout option " + id);
        Object v = value instanceof String s ? d.parseValue(s) : value;
        if (v == null) throw new IllegalArgumentException("bad value " + value + " for " + id);
        e.setProperty(d, v);
    }

    static List<ElkNode> boxes(ElkNode g, int n, String prefix) {
        List<ElkNode> l = new ArrayList<>();
        for (int i = 0; i < n; i++) l.add(box(g, prefix + i));
        return l;
    }

    static List<ElkNode> dots(ElkNode g, int n) {
        List<ElkNode> l = new ArrayList<>();
        for (int i = 0; i < n; i++) l.add(dot(g));
        return l;
    }

    static void randomDag(ElkNode g, List<ElkNode> ns, int edges, long seed) {
        Random r = new Random(seed);
        for (int i = 0; i < edges; i++) {
            int a = r.nextInt(ns.size() - 1);
            int b = a + 1 + r.nextInt(Math.min(ns.size() - a - 1, 12));
            edge(ns.get(a), ns.get(b));
        }
    }

    static void randomGraph(List<ElkNode> ns, int edges, long seed) {
        Random r = new Random(seed);
        for (int i = 0; i < edges; i++) {
            int a = r.nextInt(ns.size()), b = r.nextInt(ns.size());
            if (a != b) edge(ns.get(a), ns.get(b));
        }
    }

    static List<ElkNode> randomTree(ElkNode g, int n, long seed, boolean labeled) {
        Random r = new Random(seed);
        List<ElkNode> ns = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            ElkNode x = labeled ? box(g, "n" + i) : dot(g);
            if (i > 0) edge(ns.get(r.nextInt(i)), x);
            ns.add(x);
        }
        return ns;
    }

    static ElkNode demoDag(String algorithm) {
        ElkNode g = root(algorithm);
        List<ElkNode> ns = boxes(g, 14, "v");
        randomDag(g, ns, 22, 5);
        return g;
    }

    // =====================================================================================================
    // The catalog
    // =====================================================================================================

    static void defineCases() {
        // ---------------------------------------------------------------- degenerate inputs
        add("empty", "Degenerate", "Empty graph", "No nodes at all. Should produce a padding-only box, not a crash.",
                () -> root(LAYERED));
        add("single", "Degenerate", "Single node", "One node, no edges.", () -> {
            ElkNode g = root(LAYERED);
            box(g, "alone");
            return g;
        });
        add("zero-size", "Degenerate", "Zero-size nodes", "Nodes of 0×0 connected in a small DAG. Spacing must still keep them apart.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = dots(g, 10);
            for (ElkNode n : ns) n.setDimensions(0, 0);
            randomDag(g, ns, 14, 2);
            return g;
        });
        add("negative-size", "Degenerate", "Negative node size", "A node with width −80 and height −30. Garbage in — what comes out?", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "a"), b = box(g, "neg", -80, -30), c = box(g, "c");
            edge(a, b); edge(b, c); edge(a, c);
            return g;
        });
        add("nan-size", "Degenerate", "NaN node size", "A node whose width is NaN.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "a"), b = box(g, "nan", Double.NaN, 20), c = box(g, "c");
            edge(a, b); edge(b, c);
            return g;
        });
        add("huge-coords", "Degenerate", "Enormous node", "One 1e6 × 1e6 node next to normal ones.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "a"), b = box(g, "huge", 1e6, 1e6), c = box(g, "c");
            edge(a, b); edge(b, c);
            return g;
        });

        // ---------------------------------------------------------------- self loops & multi-edges
        add("self-loop", "Loops & multi-edges", "Single self-loop", "The simplest self-loop.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "loop");
            edge(a, a);
            return g;
        });
        add("self-loops-many", "Loops & multi-edges", "Eight labeled self-loops", "Eight self-loops with labels on one node between two neighbours.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode in = box(g, "in"), a = box(g, "busy", 80, 50), out = box(g, "out");
            edge(in, a); edge(a, out);
            for (int i = 0; i < 8; i++) edge(a, a, "loop " + i);
            return g;
        });
        add("self-loops-ports", "Loops & multi-edges", "Self-loops between fixed ports", "Self-loops connecting ports on all four sides of a node (FIXED_SIDE).", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "ports", 90, 70);
            a.setProperty(CoreOptions.PORT_CONSTRAINTS, PortConstraints.FIXED_SIDE);
            PortSide[] sides = {PortSide.NORTH, PortSide.EAST, PortSide.SOUTH, PortSide.WEST};
            for (int i = 0; i < 4; i++) for (int j = 0; j < 4; j++) if (i != j && (i + j) % 2 == 1) edge(port(a, sides[i]), port(a, sides[j]));
            edge(port(a, PortSide.NORTH), port(a, PortSide.NORTH));
            edge(port(a, PortSide.EAST), port(a, PortSide.WEST));
            return g;
        });
        add("multi-edges", "Loops & multi-edges", "Twelve parallel edges", "Twelve labeled edges between the same two nodes, plus four going back.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "A"), b = box(g, "B");
            for (int i = 0; i < 12; i++) edge(a, b, "e" + i);
            for (int i = 0; i < 4; i++) edge(b, a, "back" + i);
            return g;
        });
        add("two-cycles", "Loops & multi-edges", "Bidirectional chain", "Every consecutive pair is connected in both directions — maximal 2-cycles.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = boxes(g, 8, "n");
            for (int i = 0; i + 1 < ns.size(); i++) { edge(ns.get(i), ns.get(i + 1)); edge(ns.get(i + 1), ns.get(i)); }
            return g;
        });
        add("ring", "Loops & multi-edges", "Directed ring of 20", "One big cycle: cycle breaking must reverse exactly one edge.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = boxes(g, 20, "r");
            for (int i = 0; i < ns.size(); i++) edge(ns.get(i), ns.get((i + 1) % ns.size()));
            return g;
        });
        add("tournament", "Loops & multi-edges", "Complete digraph K7", "Every ordered pair connected — 42 edges, cycles everywhere.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = boxes(g, 7, "k");
            for (ElkNode a : ns) for (ElkNode b : ns) if (a != b) edge(a, b);
            return g;
        });

        // ---------------------------------------------------------------- degree extremes
        add("star-out", "Degree extremes", "Fan-out: 1 → 80", "A single hub with 80 children.", () -> {
            ElkNode g = root(LAYERED);
            g.setProperty(CoreOptions.DIRECTION, Direction.DOWN);
            ElkNode hub = box(g, "hub");
            for (int i = 0; i < 80; i++) edge(hub, dot(g));
            return g;
        });
        add("star-in", "Degree extremes", "Fan-in: 80 → 1 via one port", "80 sources all targeting the same port, with edge merging enabled.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode sink = box(g, "sink", 60, 60);
            sink.setProperty(CoreOptions.PORT_CONSTRAINTS, PortConstraints.FIXED_SIDE);
            ElkPort p = port(sink, PortSide.WEST);
            opt(g, "org.eclipse.elk.layered.mergeEdges", true);
            for (int i = 0; i < 80; i++) edge(dot(g), p);
            return g;
        });
        add("bipartite", "Degree extremes", "Complete bipartite K6,6", "Crossing minimization's worst case: every left node to every right node.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> l = boxes(g, 6, "L"), r = boxes(g, 6, "R");
            for (ElkNode a : l) for (ElkNode b : r) edge(a, b);
            return g;
        });
        add("comb", "Degree extremes", "Crossed comb", "Two columns wired in reverse order — a perfect permutation to untangle.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> l = boxes(g, 12, "a"), r = boxes(g, 12, "b");
            for (int i = 0; i < 12; i++) edge(l.get(i), r.get(11 - i));
            return g;
        });

        // ---------------------------------------------------------------- scale & shape
        add("chain-wrapped", "Scale & shape", "100-node chain, wrapped", "A long path with graph wrapping (MULTI_EDGE) and target aspect ratio 1.4.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = boxes(g, 100, "");
            for (int i = 0; i + 1 < ns.size(); i++) edge(ns.get(i), ns.get(i + 1));
            opt(g, "org.eclipse.elk.layered.wrapping.strategy", "MULTI_EDGE");
            g.setProperty(CoreOptions.ASPECT_RATIO, 1.4);
            return g;
        });
        add("big-dag", "Scale & shape", "Random DAG: 400 nodes, 700 edges", "Performance and readability at scale.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = dots(g, 400);
            randomDag(g, ns, 700, 42);
            return g;
        });
        add("big-cyclic", "Scale & shape", "Random cyclic graph: 150 nodes, 300 edges", "Unstructured input with many cycles, spline routing.", () -> {
            ElkNode g = root(LAYERED);
            g.setProperty(CoreOptions.EDGE_ROUTING, EdgeRouting.SPLINES);
            List<ElkNode> ns = dots(g, 150);
            randomGraph(ns, 300, 9);
            return g;
        });
        add("components", "Scale & shape", "40 disconnected components", "Many small components of different shapes, packed together.", () -> {
            ElkNode g = root(LAYERED);
            Random r = new Random(4);
            for (int c = 0; c < 40; c++) {
                int k = 1 + r.nextInt(6);
                List<ElkNode> ns = dots(g, k);
                for (int i = 1; i < k; i++) edge(ns.get(r.nextInt(i)), ns.get(i));
            }
            return g;
        });
        add("size-variance", "Scale & shape", "Extreme size variance", "One 400×260 node among 4×4 nodes.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode big = box(g, "giant", 400, 260);
            for (int i = 0; i < 12; i++) {
                ElkNode s = box(g, null, 4, 4);
                if (i % 2 == 0) edge(s, big); else edge(big, s);
            }
            return g;
        });
        add("aspect-strips", "Scale & shape", "Extremely thin nodes", "Nodes of 600×4 and 4×300 in a cycle.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, null, 600, 4), b = box(g, null, 4, 300), c = box(g, null, 600, 4), d = box(g, null, 4, 300);
            edge(a, b); edge(b, c); edge(c, d); edge(d, a); edge(a, c);
            return g;
        });

        // ---------------------------------------------------------------- labels
        add("long-labels", "Labels", "Very long edge labels", "Edge labels of 60–90 characters on short edges.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "a"), b = box(g, "b"), c = box(g, "c");
            edge(a, b, "this edge label is absurdly long and keeps going and going and going");
            edge(b, c, "short");
            edge(a, c, "another very long label that pushes the layers apart a great deal more than needed");
            return g;
        });
        add("many-labels", "Labels", "Six labels on one edge", "Head, center and tail labels stacked on a single edge.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "source"), b = box(g, "target");
            ElkEdge e = edge(a, b);
            String[] t = {"head 1", "head 2", "center 1", "center 2", "tail 1", "tail 2"};
            EdgeLabelPlacement[] p = {EdgeLabelPlacement.HEAD, EdgeLabelPlacement.HEAD, EdgeLabelPlacement.CENTER,
                    EdgeLabelPlacement.CENTER, EdgeLabelPlacement.TAIL, EdgeLabelPlacement.TAIL};
            for (int i = 0; i < t.length; i++) label(e, t[i]).setProperty(CoreOptions.EDGE_LABELS_PLACEMENT, p[i]);
            return g;
        });
        add("node-sized-by-labels", "Labels", "Nodes sized by labels and ports", "Nodes start at 0×0 and must grow to fit their labels and labeled ports.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode prev = null;
            for (int i = 0; i < 4; i++) {
                ElkNode n = box(g, "component number " + i, 0, 0);
                n.setProperty(CoreOptions.NODE_SIZE_CONSTRAINTS, SizeConstraint.free());
                n.setProperty(CoreOptions.PORT_CONSTRAINTS, PortConstraints.FIXED_SIDE);
                n.setProperty(CoreOptions.PORT_LABELS_PLACEMENT, EnumSet.of(PortLabelPlacement.INSIDE));
                for (int k = 0; k <= i; k++) label(port(n, PortSide.WEST), "in_" + k);
                for (int k = 0; k <= i; k++) label(port(n, PortSide.EAST), "out_" + k);
                if (prev != null) for (int k = 0; k < i; k++) edge(prev.getPorts().get(prev.getPorts().size() - 1 - k), n.getPorts().get(k));
                prev = n;
            }
            return g;
        });

        // ---------------------------------------------------------------- ports
        add("ports-four-sides", "Ports", "Ports on all four sides (FIXED_ORDER)", "Edges must leave north/south ports and wrap around nodes.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = new ArrayList<>();
            for (int i = 0; i < 4; i++) {
                ElkNode n = box(g, "N" + i, 70, 60);
                n.setProperty(CoreOptions.PORT_CONSTRAINTS, PortConstraints.FIXED_ORDER);
                ns.add(n);
            }
            PortSide[] sides = PortSide.values();
            Random r = new Random(1);
            for (int i = 0; i < 14; i++) {
                ElkNode a = ns.get(r.nextInt(4)), b = ns.get(r.nextInt(4));
                if (a == b) continue;
                edge(port(a, sides[1 + r.nextInt(4)]), port(b, sides[1 + r.nextInt(4)]));
            }
            return g;
        });
        add("inverted-ports", "Ports", "Inverted ports", "Outputs fixed on the WEST side and inputs on the EAST — against the flow direction.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = boxes(g, 5, "inv");
            for (ElkNode n : ns) n.setProperty(CoreOptions.PORT_CONSTRAINTS, PortConstraints.FIXED_SIDE);
            for (int i = 0; i + 1 < ns.size(); i++) edge(port(ns.get(i), PortSide.WEST), port(ns.get(i + 1), PortSide.EAST));
            edge(port(ns.get(0), PortSide.WEST), port(ns.get(4), PortSide.EAST));
            return g;
        });
        add("same-side-ports", "Ports", "Edges between same-side ports", "Port pairs on the same side of different nodes — north-to-north and south-to-south.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = boxes(g, 4, "s");
            for (ElkNode n : ns) n.setProperty(CoreOptions.PORT_CONSTRAINTS, PortConstraints.FIXED_SIDE);
            edge(port(ns.get(0), PortSide.NORTH), port(ns.get(1), PortSide.NORTH));
            edge(port(ns.get(1), PortSide.SOUTH), port(ns.get(2), PortSide.SOUTH));
            edge(port(ns.get(2), PortSide.EAST), port(ns.get(3), PortSide.WEST));
            edge(port(ns.get(0), PortSide.SOUTH), port(ns.get(3), PortSide.NORTH));
            return g;
        });
        add("fixed-pos-ports", "Ports", "Ports at fixed positions", "FIXED_POS ports placed at odd coordinates, including corners.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode a = box(g, "A", 80, 80), b = box(g, "B", 80, 80);
            for (ElkNode n : List.of(a, b)) n.setProperty(CoreOptions.PORT_CONSTRAINTS, PortConstraints.FIXED_POS);
            double[][] pos = {{-7, 10}, {80, 70}, {36, -7}, {80, -7}, {-7, 80}};
            List<ElkPort> pa = new ArrayList<>(), pb = new ArrayList<>();
            for (double[] p : pos) {
                ElkPort x = port(a, PortSide.UNDEFINED); x.setLocation(p[0], p[1]); pa.add(x);
                ElkPort y = port(b, PortSide.UNDEFINED); y.setLocation(p[0], p[1]); pb.add(y);
            }
            for (int i = 0; i < pos.length; i++) edge(pa.get(i), pb.get((i + 2) % pos.length));
            return g;
        });

        // ---------------------------------------------------------------- hierarchy
        add("deep-nesting", "Hierarchy", "Six levels of nesting", "Compound nodes nested six deep with edges that jump across levels (INCLUDE_CHILDREN).", () -> {
            ElkNode g = root(LAYERED);
            g.setProperty(CoreOptions.HIERARCHY_HANDLING, HierarchyHandling.INCLUDE_CHILDREN);
            ElkNode outside = box(g, "outside");
            ElkNode cur = g;
            List<ElkNode> inner = new ArrayList<>();
            for (int d = 0; d < 6; d++) {
                cur = group(cur, "level " + d);
                ElkNode n = box(cur, "x" + d);
                inner.add(n);
            }
            edge(outside, inner.get(5));
            for (int i = 0; i + 1 < inner.size(); i++) edge(inner.get(i + 1), inner.get(i));
            edge(inner.get(2), outside);
            return g;
        });
        add("parent-child-edge", "Hierarchy", "Edges into and out of own parent", "Edges from a compound node to its own children and back.", () -> {
            ElkNode g = root(LAYERED);
            g.setProperty(CoreOptions.HIERARCHY_HANDLING, HierarchyHandling.INCLUDE_CHILDREN);
            ElkNode p = group(g, "parent");
            ElkNode a = box(p, "child a"), b = box(p, "child b");
            ElkNode o = box(g, "outer");
            edge(p, a); edge(b, p); edge(a, b); edge(o, p); edge(o, b);
            return g;
        });
        add("empty-compounds", "Hierarchy", "Empty compound nodes & compound self-loop", "Compound nodes with no children, plus a self-loop on a compound.", () -> {
            ElkNode g = root(LAYERED);
            g.setProperty(CoreOptions.HIERARCHY_HANDLING, HierarchyHandling.INCLUDE_CHILDREN);
            ElkNode e1 = group(g, "empty 1"), e2 = group(g, "empty 2");
            ElkNode full = group(g, "full");
            ElkNode a = box(full, "a"), b = box(full, "b");
            edge(a, b); edge(e1, full); edge(full, e2); edge(full, full); edge(e1, e2);
            return g;
        });
        add("separate-children", "Hierarchy", "Cross-hierarchy edges without INCLUDE_CHILDREN", "Same idea as nesting, but each level laid out separately (default SEPARATE_CHILDREN). Cross-level edges are not routed by layered.", () -> {
            ElkNode g = root(LAYERED);
            ElkNode g1 = group(g, "left"), g2 = group(g, "right");
            g1.setProperty(CoreOptions.ALGORITHM, LAYERED);
            g2.setProperty(CoreOptions.ALGORITHM, LAYERED);
            List<ElkNode> l = boxes(g1, 3, "l"), r = boxes(g2, 3, "r");
            edge(l.get(0), l.get(1)); edge(r.get(0), r.get(2));
            edge(l.get(1), r.get(0)); edge(l.get(2), r.get(1));
            return g;
        });
        add("mixed-algorithms", "Hierarchy", "Different algorithm per compound", "Layered at the top; children use mrtree, rectpacking, stress and radial.", () -> {
            ElkNode g = root(LAYERED);
            String[] algs = {MRTREE, RECTPACKING, STRESS, RADIAL};
            List<ElkNode> gs = new ArrayList<>();
            for (int i = 0; i < algs.length; i++) {
                ElkNode c = group(g, algs[i].substring(algs[i].lastIndexOf('.') + 1));
                c.setProperty(CoreOptions.ALGORITHM, algs[i]);
                if (algs[i].equals(RECTPACKING)) {
                    Random r = new Random(i);
                    for (int k = 0; k < 14; k++) box(c, null, 10 + r.nextInt(30), 10 + r.nextInt(30));
                } else {
                    randomTree(c, 18, i, false);
                    if (algs[i].equals(RADIAL)) {
                        for (ElkNode n : c.getChildren()) n.setDimensions(1, 1);
                        opt(c, "org.eclipse.elk.radial.radius", 30.0);
                    }
                }
                gs.add(c);
            }
            for (int i = 0; i + 1 < gs.size(); i++) edge(gs.get(i), gs.get(i + 1));
            return g;
        });

        // ---------------------------------------------------------------- layered options
        for (Direction d : new Direction[] {Direction.RIGHT, Direction.DOWN, Direction.LEFT, Direction.UP}) {
            add("direction-" + d.name().toLowerCase(), "Layered options", "Direction " + d, "The same random DAG with layout direction " + d + ".", () -> {
                ElkNode g = demoDag(LAYERED);
                g.setProperty(CoreOptions.DIRECTION, d);
                return g;
            });
        }
        for (EdgeRouting er : new EdgeRouting[] {EdgeRouting.ORTHOGONAL, EdgeRouting.POLYLINE, EdgeRouting.SPLINES}) {
            add("routing-" + er.name().toLowerCase(), "Layered options", er + " routing, dense cyclic graph", "30 nodes, 70 random edges, routed " + er + ".", () -> {
                ElkNode g = root(LAYERED);
                g.setProperty(CoreOptions.EDGE_ROUTING, er);
                List<ElkNode> ns = boxes(g, 30, "");
                randomGraph(ns, 70, 77);
                return g;
            });
        }
        add("layer-constraints", "Layered options", "Layer constraints", "Some nodes forced to FIRST / LAST layers even though edges point the other way.", () -> {
            ElkNode g = root(LAYERED);
            List<ElkNode> ns = boxes(g, 10, "n");
            randomDag(g, ns, 14, 6);
            opt(ns.get(9), "org.eclipse.elk.layered.layering.layerConstraint", "FIRST");
            opt(ns.get(0), "org.eclipse.elk.layered.layering.layerConstraint", "LAST");
            opt(ns.get(5), "org.eclipse.elk.layered.layering.layerConstraint", "FIRST_SEPARATE");
            label(ns.get(9), "FIRST"); label(ns.get(0), "LAST");
            return g;
        });
        add("partitioning", "Layered options", "Partitions in reverse order", "Partition ids assigned against the edge direction.", () -> {
            ElkNode g = root(LAYERED);
            opt(g, "org.eclipse.elk.partitioning.activate", true);
            List<ElkNode> ns = boxes(g, 9, "p");
            for (int i = 0; i + 1 < ns.size(); i++) edge(ns.get(i), ns.get(i + 1));
            for (int i = 0; i < ns.size(); i++) opt(ns.get(i), "org.eclipse.elk.partitioning.partition", 8 - i);
            return g;
        });
        add("model-order", "Layered options", "Model order preserved", "considerModelOrder = NODES_AND_EDGES on a graph whose order conflicts with crossing minimization.", () -> {
            ElkNode g = root(LAYERED);
            opt(g, "org.eclipse.elk.layered.considerModelOrder.strategy", "NODES_AND_EDGES");
            List<ElkNode> l = boxes(g, 6, "a"), r = boxes(g, 6, "b");
            for (int i = 0; i < 6; i++) edge(l.get(i), r.get(5 - i));
            return g;
        });
        add("interactive", "Layered options", "Interactive strategies with scrambled positions", "Interactive cycle breaking/layering/crossing min using random initial coordinates.", () -> {
            ElkNode g = root(LAYERED);
            opt(g, "org.eclipse.elk.layered.cycleBreaking.strategy", "INTERACTIVE");
            opt(g, "org.eclipse.elk.layered.layering.strategy", "INTERACTIVE");
            opt(g, "org.eclipse.elk.layered.crossingMinimization.strategy", "INTERACTIVE");
            List<ElkNode> ns = boxes(g, 14, "i");
            Random r = new Random(3);
            for (ElkNode n : ns) n.setLocation(r.nextInt(800), r.nextInt(500));
            randomGraph(ns, 22, 8);
            return g;
        });
        add("compaction", "Layered options", "Post-compaction (EDGE_LENGTH)", "Layered with the one-dimensional post compaction step.", () -> {
            ElkNode g = demoDag(LAYERED);
            opt(g, "org.eclipse.elk.layered.compaction.postCompaction.strategy", "EDGE_LENGTH");
            return g;
        });
        add("zero-spacing", "Layered options", "All spacings set to 0", "Node, edge and layer spacing all zero.", () -> {
            ElkNode g = demoDag(LAYERED);
            g.setProperty(CoreOptions.SPACING_NODE_NODE, 0.0);
            g.setProperty(CoreOptions.SPACING_EDGE_EDGE, 0.0);
            g.setProperty(CoreOptions.SPACING_EDGE_NODE, 0.0);
            opt(g, "org.eclipse.elk.layered.spacing.nodeNodeBetweenLayers", 0.0);
            opt(g, "org.eclipse.elk.layered.spacing.edgeNodeBetweenLayers", 0.0);
            opt(g, "org.eclipse.elk.layered.spacing.edgeEdgeBetweenLayers", 0.0);
            return g;
        });
        add("negative-spacing", "Layered options", "Negative node spacing", "Node-node spacing −30.", () -> {
            ElkNode g = demoDag(LAYERED);
            g.setProperty(CoreOptions.SPACING_NODE_NODE, -30.0);
            return g;
        });

        // ---------------------------------------------------------------- trees
        add("tree-forest", "Trees (mrtree)", "Forest of 6 trees", "Multiple roots — mrtree must pick and arrange several trees.", () -> {
            ElkNode g = root(MRTREE);
            for (int t = 0; t < 6; t++) randomTree(g, 4 + t * 3, t, false);
            return g;
        });
        add("tree-not-a-tree", "Trees (mrtree)", "Non-tree input", "A tree algorithm given a graph with cycles and cross edges.", () -> {
            ElkNode g = root(MRTREE);
            List<ElkNode> ns = randomTree(g, 25, 8, false);
            Random r = new Random(1);
            for (int i = 0; i < 10; i++) edge(ns.get(r.nextInt(25)), ns.get(r.nextInt(25)));
            return g;
        });
        add("tree-deep", "Trees (mrtree)", "Path of 60 (depth 60)", "A degenerate tree with no branching.", () -> {
            ElkNode g = root(MRTREE);
            List<ElkNode> ns = dots(g, 60);
            for (int i = 0; i + 1 < ns.size(); i++) edge(ns.get(i), ns.get(i + 1));
            return g;
        });
        add("tree-wide", "Trees (mrtree)", "Root with 120 children", "A degenerate tree with no depth.", () -> {
            ElkNode g = root(MRTREE);
            ElkNode r = dot(g);
            for (int i = 0; i < 120; i++) edge(r, dot(g));
            return g;
        });
        add("tree-sizes", "Trees (mrtree)", "Tree with wildly varying node sizes", "Node sizes from 6 to 140 px.", () -> {
            ElkNode g = root(MRTREE);
            List<ElkNode> ns = randomTree(g, 30, 12, false);
            Random r = new Random(2);
            for (ElkNode n : ns) n.setDimensions(6 + r.nextInt(135), 6 + r.nextInt(80));
            return g;
        });

        // ---------------------------------------------------------------- radial
        add("radial-star", "Radial", "Star of 150", "One centre, 150 leaves on a single ring.", () -> {
            ElkNode g = root(RADIAL);
            ElkNode c = dot(g);
            for (int i = 0; i < 150; i++) edge(c, dot(g));
            return g;
        });
        add("radial-cyclic", "Radial", "Radial on a cyclic graph", "Random graph with cycles; radial must extract a spanning tree.", () -> {
            ElkNode g = root(RADIAL);
            List<ElkNode> ns = randomTree(g, 40, 3, false);
            randomGraph(ns, 20, 4);
            opt(g, "org.eclipse.elk.radial.radius", 80.0);
            return g;
        });
        add("radial-default-overlap", "Radial", "Radial defaults with crowded rings", "Default radius (0) on a 60-node tree — relies entirely on overlap removal. (Known to look broken.)", () -> {
            ElkNode g = root(RADIAL);
            randomTree(g, 60, 7, false);
            return g;
        });
        add("radial-caterpillar", "Radial", "Caterpillar tree", "A long spine with 3 leaves on every spine node.", () -> {
            ElkNode g = root(RADIAL);
            List<ElkNode> spine = dots(g, 12);
            for (int i = 0; i + 1 < spine.size(); i++) edge(spine.get(i), spine.get(i + 1));
            for (ElkNode s : spine) for (int k = 0; k < 3; k++) edge(s, dot(g));
            for (ElkNode n : g.getChildren()) n.setDimensions(1, 1);
            opt(g, "org.eclipse.elk.radial.radius", 50.0);
            return g;
        });

        // ---------------------------------------------------------------- force-directed
        add("force-components", "Force & stress", "Force: disconnected components", "Eight separate clusters with no edges between them.", () -> {
            ElkNode g = undirected(root(FORCE));
            for (int c = 0; c < 8; c++) {
                List<ElkNode> ns = dots(g, 3 + c);
                for (int i = 1; i < ns.size(); i++) edge(ns.get(i - 1), ns.get(i));
                edge(ns.get(ns.size() - 1), ns.get(0));
            }
            return g;
        });
        add("force-complete", "Force & stress", "Force: complete graph K14", "91 edges, every node attracts every other.", () -> {
            ElkNode g = undirected(root(FORCE));
            List<ElkNode> ns = dots(g, 14);
            for (int i = 0; i < ns.size(); i++) for (int j = i + 1; j < ns.size(); j++) edge(ns.get(i), ns.get(j));
            return g;
        });
        add("force-big-labels", "Force & stress", "Force: big labeled boxes", "Force layout with large boxes — does it avoid overlaps?", () -> {
            ElkNode g = undirected(root(FORCE));
            List<ElkNode> ns = new ArrayList<>();
            for (int i = 0; i < 16; i++) ns.add(box(g, "a rather wide label " + i));
            randomGraph(ns, 24, 5);
            return g;
        });
        add("stress-grid", "Force & stress", "Stress: 14×14 grid", "Should recover a near-perfect grid.", () -> {
            ElkNode g = undirected(root(STRESS));
            int n = 14;
            ElkNode[][] m = new ElkNode[n][n];
            for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) m[i][j] = dot(g);
            for (int i = 0; i < n; i++) for (int j = 0; j < n; j++) {
                if (i + 1 < n) edge(m[i][j], m[i + 1][j]);
                if (j + 1 < n) edge(m[i][j], m[i][j + 1]);
            }
            return g;
        });
        add("stress-ring", "Force & stress", "Stress: ring of 60", "Should recover a circle.", () -> {
            ElkNode g = undirected(root(STRESS));
            List<ElkNode> ns = dots(g, 60);
            for (int i = 0; i < ns.size(); i++) edge(ns.get(i), ns.get((i + 1) % ns.size()));
            return g;
        });
        add("stress-torus", "Force & stress", "Stress: torus 16×8", "A grid with wrap-around edges in both directions.", () -> {
            ElkNode g = undirected(root(STRESS));
            int a = 16, b = 8;
            ElkNode[][] m = new ElkNode[a][b];
            for (int i = 0; i < a; i++) for (int j = 0; j < b; j++) m[i][j] = dot(g);
            for (int i = 0; i < a; i++) for (int j = 0; j < b; j++) { edge(m[i][j], m[(i + 1) % a][j]); edge(m[i][j], m[i][(j + 1) % b]); }
            return g;
        });
        add("stress-barbell", "Force & stress", "Stress: barbell", "Two K8 cliques joined by a 10-node path.", () -> {
            ElkNode g = undirected(root(STRESS));
            List<ElkNode> a = dots(g, 8), b = dots(g, 8), p = dots(g, 10);
            for (List<ElkNode> k : List.of(a, b)) for (int i = 0; i < 8; i++) for (int j = i + 1; j < 8; j++) edge(k.get(i), k.get(j));
            edge(a.get(0), p.get(0));
            for (int i = 0; i + 1 < p.size(); i++) edge(p.get(i), p.get(i + 1));
            edge(p.get(9), b.get(0));
            return g;
        });
        add("stress-disconnected", "Force & stress", "Stress: disconnected input", "Stress majorization needs graph distances — what happens with infinite ones?", () -> {
            ElkNode g = undirected(root(STRESS));
            for (int c = 0; c < 5; c++) {
                List<ElkNode> ns = dots(g, 6);
                for (int i = 0; i < 6; i++) edge(ns.get(i), ns.get((i + 1) % 6));
            }
            return g;
        });
        add("stress-binary-tree", "Force & stress", "Stress: complete binary tree (depth 7)", "127 nodes.", () -> {
            ElkNode g = undirected(root(STRESS));
            List<ElkNode> ns = dots(g, 127);
            for (int i = 1; i < ns.size(); i++) edge(ns.get((i - 1) / 2), ns.get(i));
            return g;
        });

        // ---------------------------------------------------------------- packing
        add("pack-identical", "Packing", "Rectpacking: 100 identical squares", "Should give a clean grid.", () -> {
            ElkNode g = root(RECTPACKING);
            for (int i = 0; i < 100; i++) box(g, String.valueOf(i), 30, 30);
            return g;
        });
        add("pack-giant", "Packing", "Rectpacking: one giant, 80 tiny", "A 300×300 box among 80 small ones.", () -> {
            ElkNode g = root(RECTPACKING);
            box(g, "giant", 300, 300);
            Random r = new Random(1);
            for (int i = 0; i < 80; i++) box(g, null, 8 + r.nextInt(20), 8 + r.nextInt(20));
            return g;
        });
        add("pack-strips", "Packing", "Rectpacking: extreme aspect ratios", "Long horizontal strips and tall vertical strips.", () -> {
            ElkNode g = root(RECTPACKING);
            for (int i = 0; i < 6; i++) box(g, null, 500, 10);
            for (int i = 0; i < 12; i++) box(g, null, 10, 220);
            return g;
        });
        add("pack-tall-target", "Packing", "Rectpacking: aspect ratio 0.25", "Random boxes packed into a tall, narrow target.", () -> {
            ElkNode g = root(RECTPACKING);
            g.setProperty(CoreOptions.ASPECT_RATIO, 0.25);
            Random r = new Random(5);
            for (int i = 0; i < 50; i++) box(g, String.valueOf(i), 20 + r.nextInt(70), 20 + r.nextInt(50));
            return g;
        });
        add("pack-with-edges", "Packing", "Rectpacking ignores edges", "Edges are present but the packer doesn't route them.", () -> {
            ElkNode g = root(RECTPACKING);
            List<ElkNode> ns = boxes(g, 12, "e");
            randomGraph(ns, 10, 2);
            return g;
        });
        add("topdown-13", "Packing", "Top-down packing: 13 boxes", "Topdownpacking expands boxes to fill whitespace; 13 is prime.", () -> {
            ElkNode g = root(TOPDOWN);
            for (int i = 0; i < 13; i++) box(g, String.valueOf(i), 60, 40);
            return g;
        });

        // ---------------------------------------------------------------- other algorithms
        add("spore-overlap", "Other algorithms", "SPOrE overlap removal", "60 nodes dumped at random, heavily overlapping positions.", () -> {
            ElkNode g = root(SPORE_OVERLAP);
            Random r = new Random(6);
            for (int i = 0; i < 60; i++) {
                ElkNode n = box(g, String.valueOf(i), 30 + r.nextInt(50), 20 + r.nextInt(30));
                n.setLocation(r.nextInt(250), r.nextInt(180));
            }
            return g;
        });
        add("spore-compaction", "Other algorithms", "SPOrE compaction", "Nodes scattered far apart at random; compaction should pull them together.", () -> {
            ElkNode g = root(SPORE_COMPACTION);
            Random r = new Random(7);
            for (int i = 0; i < 40; i++) {
                ElkNode n = box(g, String.valueOf(i), 30 + r.nextInt(40), 20 + r.nextInt(30));
                n.setLocation(r.nextInt(3000), r.nextInt(2000));
            }
            return g;
        });
        add("disco", "Other algorithms", "DisCo component packing", "15 components laid out by force, packed as polyominoes by DisCo.", () -> {
            ElkNode g = undirected(root(DISCO));
            opt(g, "org.eclipse.elk.disco.componentCompaction.componentLayoutAlgorithm", FORCE);
            Random r = new Random(8);
            for (int c = 0; c < 15; c++) {
                int k = 2 + r.nextInt(9);
                List<ElkNode> ns = dots(g, k);
                for (int i = 1; i < k; i++) edge(ns.get(r.nextInt(i)), ns.get(i));
            }
            return g;
        });
        add("vertiflex", "Other algorithms", "VertiFlex tree", "Tree layout where some nodes are pinned to a y-coordinate below their parent.", () -> {
            ElkNode g = root(VERTIFLEX);
            List<ElkNode> ns = randomTree(g, 25, 9, true);
            Map<ElkNode, Integer> depth = new IdentityHashMap<>();
            depth.put(ns.get(0), 0);
            for (ElkNode n : ns) for (ElkEdge e : ElkGraphUtil.allOutgoingEdges(n)) depth.put(ElkGraphUtil.connectableShapeToNode(e.getTargets().get(0)), depth.get(n) + 1);
            for (int i = 3; i < ns.size(); i += 4) opt(ns.get(i), "org.eclipse.elk.vertiflex.verticalConstraint", depth.get(ns.get(i)) * 110.0 + 40);
            return g;
        });
        add("vertiflex-invalid", "Other algorithms", "VertiFlex with contradictory constraints", "Random vertical constraints, some placing a child above its parent.", () -> {
            ElkNode g = root(VERTIFLEX);
            List<ElkNode> ns = randomTree(g, 25, 9, true);
            Random r = new Random(1);
            for (int i = 1; i < ns.size(); i += 3) opt(ns.get(i), "org.eclipse.elk.vertiflex.verticalConstraint", (double) (60 + r.nextInt(300)));
            return g;
        });
        add("box", "Other algorithms", "Box layout of mixed sizes", "ELK's simple built-in box packer.", () -> {
            ElkNode g = root(BOX);
            Random r = new Random(9);
            for (int i = 0; i < 30; i++) box(g, String.valueOf(i), 20 + r.nextInt(80), 20 + r.nextInt(60));
            return g;
        });
        add("random", "Other algorithms", "Random layout", "ELK's random placer — the baseline every other algorithm should beat.", () -> {
            ElkNode g = demoDag(RANDOM);
            return g;
        });
    }

    // =====================================================================================================
    // Running & checking
    // =====================================================================================================

    static class Result {
        Case c;
        String status = "ok"; // ok | warn | error | timeout
        String error;
        long millis;
        int nodes, edges;
        String algorithm = "";
        double width, height;
        List<String> issues = new ArrayList<>();
        String svg;
    }

    public static void main(String[] args) throws Exception {
        registerProviders();
        defineCases();
        Path out = Paths.get(args.length > 0 ? args[0] : "edge-cases");
        Files.createDirectories(out);
        String only = args.length > 1 ? args[1] : null;

        ExecutorService ex = Executors.newCachedThreadPool(r -> { Thread t = new Thread(r); t.setDaemon(true); return t; });
        List<Result> results = new ArrayList<>();
        for (Case c : CASES) {
            if (only != null && !c.id().matches(only)) continue;
            Result res = new Result();
            res.c = c;
            ElkNode[] holder = new ElkNode[1];
            Future<?> f = ex.submit(() -> {
                holder[0] = c.graph().get();
                long t0 = System.nanoTime();
                new RecursiveGraphLayoutEngine().layout(holder[0], new BasicProgressMonitor());
                res.millis = (System.nanoTime() - t0) / 1_000_000;
                return null;
            });
            try {
                f.get(60, TimeUnit.SECONDS);
                check(holder[0], res);
                res.svg = render(holder[0], c);
            } catch (TimeoutException e) {
                f.cancel(true);
                res.status = "timeout";
                res.error = "layout did not finish within 60 s";
            } catch (ExecutionException e) {
                res.status = "error";
                Throwable cause = e.getCause();
                StringWriter sw = new StringWriter();
                cause.printStackTrace(new PrintWriter(sw));
                res.error = sw.toString().lines().limit(8).reduce((a, b) -> a + "\n" + b).orElse(cause.toString());
            }
            if (holder[0] != null) {
                String a = holder[0].getProperty(CoreOptions.ALGORITHM);
                res.algorithm = a == null ? "" : a.substring(a.lastIndexOf('.') + 1);
                res.nodes = countNodes(holder[0]);
                res.edges = countEdges(holder[0]);
            }
            if (res.svg == null) res.svg = errorSvg(c, res);
            Files.writeString(out.resolve(c.id() + ".svg"), res.svg);
            System.out.printf(Locale.ROOT, "%-24s %-8s %6d ms  %s%s%n", c.id(), res.status, res.millis,
                    String.join("; ", res.issues), res.error != null ? res.error.lines().findFirst().orElse("") : "");
            results.add(res);
        }
        writeJson(out.resolve("results.json"), results);
    }

    static void registerProviders() {
        String[] providers = {
            "org.eclipse.elk.alg.layered.options.LayeredMetaDataProvider",
            "org.eclipse.elk.alg.mrtree.options.MrTreeMetaDataProvider",
            "org.eclipse.elk.alg.radial.options.RadialMetaDataProvider",
            "org.eclipse.elk.alg.force.options.ForceMetaDataProvider",
            "org.eclipse.elk.alg.force.options.StressMetaDataProvider",
            "org.eclipse.elk.alg.rectpacking.options.RectPackingMetaDataProvider",
            "org.eclipse.elk.alg.topdownpacking.options.TopdownpackingMetaDataProvider",
            "org.eclipse.elk.alg.spore.options.SporeMetaDataProvider",
            "org.eclipse.elk.alg.disco.options.DisCoMetaDataProvider",
            "org.eclipse.elk.alg.vertiflex.options.VertiFlexMetaDataProvider",
        };
        for (String p : providers) {
            try {
                LayoutMetaDataService.getInstance().registerLayoutMetaDataProviders(
                        (ILayoutMetaDataProvider) Class.forName(p).getDeclaredConstructor().newInstance());
            } catch (ReflectiveOperationException e) {
                System.err.println("provider not available: " + p);
            }
        }
    }

    static int countNodes(ElkNode n) {
        int c = 0;
        for (ElkNode k : n.getChildren()) c += 1 + countNodes(k);
        return c;
    }

    static int countEdges(ElkNode n) {
        int c = n.getContainedEdges().size();
        for (ElkNode k : n.getChildren()) c += countEdges(k);
        return c;
    }

    static boolean bad(double... vs) {
        for (double v : vs) if (!Double.isFinite(v)) return true;
        return false;
    }

    static void check(ElkNode g, Result res) {
        res.width = g.getWidth();
        res.height = g.getHeight();
        int nonFinite = 0, overlaps = 0, detached = 0, unrouted = 0;
        Deque<ElkNode> stack = new ArrayDeque<>(List.of(g));
        while (!stack.isEmpty()) {
            ElkNode p = stack.pop();
            List<ElkNode> ch = p.getChildren();
            for (ElkNode n : ch) {
                if (bad(n.getX(), n.getY(), n.getWidth(), n.getHeight())) nonFinite++;
                for (ElkPort pt : n.getPorts()) if (bad(pt.getX(), pt.getY())) nonFinite++;
                stack.push(n);
            }
            for (int i = 0; i < ch.size(); i++) for (int j = i + 1; j < ch.size(); j++) {
                ElkNode a = ch.get(i), b = ch.get(j);
                double ox = Math.min(a.getX() + a.getWidth(), b.getX() + b.getWidth()) - Math.max(a.getX(), b.getX());
                double oy = Math.min(a.getY() + a.getHeight(), b.getY() + b.getHeight()) - Math.max(a.getY(), b.getY());
                if (ox > 1 && oy > 1) overlaps++;
            }
            for (ElkEdge e : p.getContainedEdges()) {
                if (e.getSections().isEmpty()) { unrouted++; continue; }
                KVector off = absolute(e.getContainingNode());
                ElkEdgeSection first = e.getSections().get(0), last = e.getSections().get(e.getSections().size() - 1);
                if (bad(first.getStartX(), first.getStartY(), last.getEndX(), last.getEndY())) { nonFinite++; continue; }
                if (!touches(e.getSources().get(0), off.x + first.getStartX(), off.y + first.getStartY())
                        || !touches(e.getTargets().get(0), off.x + last.getEndX(), off.y + last.getEndY())) detached++;
            }
        }
        if (bad(g.getWidth(), g.getHeight())) nonFinite++;
        if (nonFinite > 0) res.issues.add(nonFinite + " non-finite coordinate(s)");
        if (overlaps > 0) res.issues.add(overlaps + " overlapping node pair(s)");
        if (detached > 0) res.issues.add(detached + " edge(s) not touching their endpoints");
        if (unrouted > 0) res.issues.add(unrouted + " edge(s) without a route");
        if (!res.issues.isEmpty()) res.status = "warn";
    }

    static boolean touches(ElkConnectableShape s, double x, double y) {
        KVector o = absolute(s instanceof ElkPort p ? p.getParent() : ((ElkNode) s).getParent());
        double sx = o.x + s.getX(), sy = o.y + s.getY();
        double tol = 3;
        return x >= sx - tol && x <= sx + s.getWidth() + tol && y >= sy - tol && y <= sy + s.getHeight() + tol;
    }

    /** Absolute position of a node's content area origin (i.e. where its children's coordinates start). */
    static KVector absolute(ElkNode n) {
        KVector v = new KVector();
        while (n != null && n.getParent() != null) { v.x += n.getX(); v.y += n.getY(); n = n.getParent(); }
        return v;
    }

    // =====================================================================================================
    // Rendering
    // =====================================================================================================

    static String render(ElkNode g, Case c) {
        double w = Math.max(g.getWidth(), 120), h = Math.max(g.getHeight(), 60);
        if (bad(w, h)) return null;
        // Keep pathological sizes (e.g. 1e6 nodes) drawable.
        StringBuilder sb = new StringBuilder();
        sb.append(f("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 %.1f %.1f' width='%.0f' height='%.0f' "
                + "font-family='Helvetica, Arial, sans-serif'>\n", w, h, Math.min(w, 4000), Math.min(h, 4000)));
        sb.append("<defs><marker id='arrow' viewBox='0 0 10 10' refX='9' refY='5' markerWidth='6' markerHeight='6' "
                + "orient='auto-start-reverse'><path d='M0,0 L10,5 L0,10 z' fill='#555'/></marker></defs>\n");
        sb.append("<rect width='100%' height='100%' fill='#fff'/>\n");
        boolean directed = !UNDIRECTED.contains(g);
        renderLevel(g, 0, 0, 0, sb, directed);
        sb.append("</svg>\n");
        return sb.toString();
    }

    static String f(String fmt, Object... args) {
        return String.format(Locale.ROOT, fmt, args);
    }

    static void renderLevel(ElkNode parent, double ox, double oy, int depth, StringBuilder sb, boolean directed) {
        int i = 0;
        for (ElkNode n : parent.getChildren()) {
            double x = ox + n.getX(), y = oy + n.getY(), w = n.getWidth(), h = n.getHeight();
            if (bad(x, y, w, h)) continue;
            boolean compound = !n.getChildren().isEmpty() || GROUPS.contains(n);
            String fill = PALETTE[(depth * 3 + i++) % PALETTE.length];
            if (compound) {
                sb.append(f("<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='6' fill='%s' fill-opacity='0.08' stroke='#9CA3AF'/>\n",
                        x, y, Math.abs(w), Math.abs(h), PALETTE[depth % PALETTE.length]));
            } else if (n.getLabels().isEmpty() && w <= 14 && h <= 14 && w == h) {
                sb.append(f("<circle cx='%.1f' cy='%.1f' r='%.1f' fill='%s'/>\n", x + w / 2, y + h / 2, Math.max(3, w / 2), fill));
            } else {
                sb.append(f("<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='3' fill='%s' fill-opacity='0.85'/>\n",
                        Math.min(x, x + w), Math.min(y, y + h), Math.max(Math.abs(w), 1), Math.max(Math.abs(h), 1), fill));
            }
            for (ElkLabel l : n.getLabels()) {
                if (bad(l.getX(), l.getY())) continue;
                sb.append(f("<text x='%.1f' y='%.1f' font-size='12' text-anchor='middle' fill='%s'%s>%s</text>\n",
                        x + l.getX() + l.getWidth() / 2, y + l.getY() + l.getHeight() - 3,
                        compound ? "#374151" : "#fff", compound ? " font-weight='bold'" : "", esc(l.getText())));
            }
            for (ElkPort p : n.getPorts()) {
                if (bad(p.getX(), p.getY())) continue;
                sb.append(f("<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' fill='#222'/>\n", x + p.getX(), y + p.getY(), p.getWidth(), p.getHeight()));
                for (ElkLabel l : p.getLabels()) {
                    sb.append(f("<text x='%.1f' y='%.1f' font-size='9' fill='#fff'>%s</text>\n",
                            x + p.getX() + l.getX(), y + p.getY() + l.getY() + l.getHeight() - 3, esc(l.getText())));
                }
            }
        }
        for (ElkNode n : parent.getChildren()) {
            if (!n.getChildren().isEmpty() && !bad(n.getX(), n.getY())) renderLevel(n, ox + n.getX(), oy + n.getY(), depth + 1, sb, directed);
        }
        for (ElkEdge e : parent.getContainedEdges()) renderEdge(e, sb, directed);
    }

    static void renderEdge(ElkEdge e, StringBuilder sb, boolean directed) {
        KVector off = absolute(e.getContainingNode());
        boolean spline = false;
        for (ElkNode n = e.getContainingNode(); n != null; n = n.getParent()) {
            if (n.getProperty(CoreOptions.EDGE_ROUTING) == EdgeRouting.SPLINES) { spline = true; break; }
            if (n.hasProperty(CoreOptions.EDGE_ROUTING)) break;
        }
        for (ElkEdgeSection s : e.getSections()) {
            List<KVector> pts = new ArrayList<>();
            pts.add(new KVector(s.getStartX(), s.getStartY()));
            for (ElkBendPoint b : s.getBendPoints()) pts.add(new KVector(b.getX(), b.getY()));
            pts.add(new KVector(s.getEndX(), s.getEndY()));
            if (pts.stream().anyMatch(p -> bad(p.x, p.y))) continue;
            StringBuilder d = new StringBuilder(f("M%.1f,%.1f", off.x + pts.get(0).x, off.y + pts.get(0).y));
            int nb = pts.size() - 2;
            if (spline && nb > 0 && nb % 3 == 2) {
                for (int i = 1; i + 2 <= pts.size() - 1; i += 3) {
                    d.append(f(" C%.1f,%.1f %.1f,%.1f %.1f,%.1f", off.x + pts.get(i).x, off.y + pts.get(i).y,
                            off.x + pts.get(i + 1).x, off.y + pts.get(i + 1).y, off.x + pts.get(i + 2).x, off.y + pts.get(i + 2).y));
                }
            } else {
                for (int i = 1; i < pts.size(); i++) d.append(f(" L%.1f,%.1f", off.x + pts.get(i).x, off.y + pts.get(i).y));
            }
            sb.append(f("<path d='%s' fill='none' stroke='#555' stroke-width='1.2' stroke-opacity='0.85'%s/>\n",
                    d, directed ? " marker-end='url(#arrow)'" : ""));
        }
        for (ElkLabel l : e.getLabels()) {
            if (bad(l.getX(), l.getY())) continue;
            sb.append(f("<text x='%.1f' y='%.1f' font-size='10' text-anchor='middle' fill='#444'>%s</text>\n",
                    off.x + l.getX() + l.getWidth() / 2, off.y + l.getY() + l.getHeight() - 3, esc(l.getText())));
        }
    }

    static String errorSvg(Case c, Result r) {
        StringBuilder sb = new StringBuilder("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 520 200' width='520' height='200' "
                + "font-family='Menlo, monospace'><rect width='100%' height='100%' fill='#FEF2F2'/>\n");
        sb.append(f("<text x='16' y='30' font-size='15' font-family='Helvetica, Arial, sans-serif' font-weight='bold' fill='#B91C1C'>%s</text>\n",
                r.status.toUpperCase()));
        int y = 56;
        String msg = r.error != null ? r.error : String.join("\n", r.issues) + "\n(graph size is not finite, nothing to draw)";
        for (String line : msg.lines().limit(8).toList()) {
            sb.append(f("<text x='16' y='%d' font-size='10' fill='#7F1D1D'>%s</text>\n", y, esc(line.length() > 90 ? line.substring(0, 90) + "…" : line)));
            y += 16;
        }
        return sb.append("</svg>\n").toString();
    }

    static String esc(String s) {
        return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("'", "&apos;");
    }

    static String js(String s) {
        if (s == null) return "null";
        StringBuilder b = new StringBuilder("\"");
        for (char ch : s.toCharArray()) {
            switch (ch) {
                case '"' -> b.append("\\\"");
                case '\\' -> b.append("\\\\");
                case '\n' -> b.append("\\n");
                case '\t' -> b.append("\\t");
                case '\r' -> {}
                default -> { if (ch < 0x20) b.append(f("\\u%04x", (int) ch)); else b.append(ch); }
            }
        }
        return b.append('"').toString();
    }

    static void writeJson(Path p, List<Result> rs) throws IOException {
        StringBuilder sb = new StringBuilder("[\n");
        for (int i = 0; i < rs.size(); i++) {
            Result r = rs.get(i);
            sb.append(f("  {\"id\":%s,\"algorithm\":%s,\"category\":%s,\"title\":%s,\"note\":%s,\"status\":%s,\"millis\":%d,\"nodes\":%d,\"edges\":%d,"
                    + "\"width\":%s,\"height\":%s,\"issues\":[%s],\"error\":%s}%s\n",
                    js(r.c.id()), js(r.algorithm), js(r.c.category()), js(r.c.title()), js(r.c.note()), js(r.status), r.millis, r.nodes, r.edges,
                    Double.isFinite(r.width) ? f("%.1f", r.width) : "null", Double.isFinite(r.height) ? f("%.1f", r.height) : "null",
                    String.join(",", r.issues.stream().map(EdgeCases::js).toList()), js(r.error), i + 1 < rs.size() ? "," : ""));
        }
        Files.writeString(p, sb.append("]\n").toString());
    }
}
