# ELK random corpus — summary

1000 cases · 68,254 nodes · 93,950 edges · 119.0 s total layout time · 200 distinct layout options exercised

## Outcomes

| status | cases |
|---|---|
| ok | 515 |
| warn | 381 |
| error | 96 |
| timeout | 8 |

`ok` = laid out and passed every check; `warn` = laid out but a sanity check flagged something (see Issues); `error` = ELK threw; `timeout` = no result within 30 s. Other `*-error` statuses would mean the harness itself failed at that stage.

## By algorithm

| algorithm | ok | warn | error | timeout | total |
|---|---|---|---|---|---|
| layered | 299 | 60 | 26 | 1 | 386 |
| stress | 37 | 40 | 1 | 2 | 80 |
| mrtree | 51 | 10 | 13 | 0 | 74 |
| force | 50 | 13 | 6 | 2 | 71 |
| rectpacking | 1 | 57 | 13 | 0 | 71 |
| radial | 16 | 22 | 17 | 3 | 58 |
| disco | 4 | 39 | 1 | 0 | 44 |
| sporeOverlap | 29 | 12 | 1 | 0 | 42 |
| vertiflex | 14 | 4 | 17 | 0 | 35 |
| sporeCompaction | 8 | 22 | 0 | 0 | 30 |
| random | 2 | 27 | 0 | 0 | 29 |
| topdownpacking | 2 | 27 | 0 | 0 | 29 |
| box | 0 | 26 | 0 | 0 | 26 |
| fixed | 2 | 22 | 1 | 0 | 25 |

## By graph family

| family | ok | warn | error | timeout | total |
|---|---|---|---|---|---|
| bipartite | 33 | 26 | 4 | 0 | 63 |
| caterpillar | 63 | 37 | 12 | 1 | 113 |
| complete | 28 | 33 | 4 | 2 | 67 |
| components | 46 | 24 | 5 | 0 | 75 |
| cycle | 26 | 16 | 4 | 0 | 46 |
| dag | 27 | 27 | 9 | 1 | 64 |
| forest | 22 | 21 | 3 | 0 | 46 |
| gnm | 40 | 27 | 6 | 2 | 75 |
| grid | 34 | 22 | 9 | 1 | 66 |
| layered-dag | 24 | 27 | 2 | 0 | 53 |
| path | 39 | 26 | 2 | 0 | 67 |
| powerlaw | 25 | 21 | 5 | 0 | 51 |
| star | 43 | 33 | 15 | 0 | 91 |
| tree | 65 | 41 | 16 | 1 | 123 |

## Exceptions, grouped

| count | exception | innermost ELK frame | example cases |
|---|---|---|---|
| 24 | IllegalArgumentException: Label and node size calculator can only be used with ports that have port sides assigned. | `org.eclipse.elk.alg.common.nodespacing.internal.algorithm.PortContextCreator.createPortContexts(PortContextCreator.java:51)` | 0043, 0079, 0140, 0142, 0149, 0156 … |
| 17 | UnsupportedConfigurationException: The given graph is not an acyclic tree! | `org.eclipse.elk.alg.vertiflex.VertiFlexLayoutProvider.layout(VertiFlexLayoutProvider.java:72)` | 0189, 0370, 0411, 0439, 0485, 0621 … |
| 5 | UnsupportedConfigurationException: A target width has to be set if the TargetWidthWidthApproximator should be used. | `org.eclipse.elk.alg.rectpacking.p1widthapproximation.TargetWidthWidthApproximator.process(TargetWidthWidthApproximator.java:47)` | 0035, 0212, 0701, 0712, 0725 |
| 5 | NullPointerException: Cannot invoke "java.lang.Integer.intValue()" because the return value of "java.util.Map.get(Object)" is null | `org.eclipse.elk.alg.layered.intermediate.greedyswitch.BetweenLayerEdgeTwoNodeCrossingsCounter$AdjacencyList.addAdjacencyOf(BetweenLayerEdgeTwoNodeCrossingsCounter.java:283)` | 0275, 0310, 0324, 0364, 0610 |
| 3 | UnsupportedConfigurationException: null has not been assigned a top-down node type. | `org.eclipse.elk.core.RecursiveGraphLayoutEngine.layoutRecursively(RecursiveGraphLayoutEngine.java:231)` | 0169, 0522, 0980 |
| 3 | IllegalArgumentException: The given graph is not a tree! | `org.eclipse.elk.alg.radial.RadialLayoutProvider.layout(RadialLayoutProvider.java:56)` | 0251, 0441, 0845 |
| 3 | NullPointerException: Cannot read field "x" because "position" is null | `org.eclipse.elk.alg.radial.intermediate.optimization.EdgeLengthPositionOptimization.evaluate(EdgeLengthPositionOptimization.java:42)` | 0428, 0449, 0678 |
| 3 | NullPointerException: Cannot read field "x" because "positionN" is null | `org.eclipse.elk.alg.radial.intermediate.optimization.CrossingMinimizationPosition.isCrossing(CrossingMinimizationPosition.java:72)` | 0529, 0608, 0886 |
| 2 | ArrayIndexOutOfBoundsException: Index N out of bounds for length N | `org.eclipse.elk.alg.layered.p3order.counting.BinaryIndexedTree.add(BinaryIndexedTree.java:53)` | 0000, 0331 |
| 2 | NullPointerException: Cannot read field "x" because "positionN" is null | `org.eclipse.elk.alg.radial.RadialUtil.lambda$0(RadialUtil.java:154)` | 0138, 0726 |
| 2 | UnsupportedConfigurationException: Invalid port label placement: [INSIDE, NEXT_TO_PORT_IF_POSSIBLE, ALWAYS_SAME_SIDE, SPACE_EFFICIENT] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:153)` | 0157, 0464 |
| 2 | StackOverflowError | `org.eclipse.elk.graph.properties.MapPropertyHolder.setProperty(MapPropertyHolder.java:42)` | 0230, 0702 |
| 2 | IllegalArgumentException: The vector chain must contain at least a source and a target point. | `org.eclipse.elk.core.util.ElkUtil.applyVectorChain(ElkUtil.java:938)` | 0328, 0968 |
| 2 | IllegalStateException: Invalid hitboxes for scanline constraint calculation. | `org.eclipse.elk.alg.common.compaction.oned.ScanlineConstraintCalculator$ConstraintsScanlineHandler.insert(ScanlineConstraintCalculator.java:177)` | 0637, 0675 |
| 2 | UnsupportedConfigurationException: Invalid port label placement: [ALWAYS_SAME_SIDE, SPACE_EFFICIENT] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:153)` | 0899, 0982 |
| 1 | UnsupportedConfigurationException: Invalid port label placement: [ALWAYS_SAME_SIDE, ALWAYS_OTHER_SAME_SIDE, SPACE_EFFICIENT] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:153)` | 0082 |
| 1 | UnsupportedConfigurationException: Invalid port label placement: [OUTSIDE, INSIDE, ALWAYS_SAME_SIDE] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:153)` | 0089 |
| 1 | UnsupportedConfigurationException: Invalid node label placement: [H_LEFT, H_CENTER, V_TOP] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:159)` | 0204 |
| 1 | UnsupportedConfigurationException: Node 'N zNoNl-sr-' has its layer constraint set to LAST, but has at least one outgoing edge that  does not go to a LAST_SEPAR | `org.eclipse.elk.alg.layered.intermediate.LayerConstraintPostprocessor.throwUpUnlessNoOutgoingEdges(LayerConstraintPostprocessor.java:244)` | 0205 |
| 1 | UnsupportedConfigurationException: Invalid node label placement: [V_TOP, INSIDE, OUTSIDE] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:159)` | 0214 |
| 1 | UnsupportedGraphException: The source or the target of edge ElkEdge ElkNode "nN" (N.N,-N.N \| N.N,N.N) -> ElkNode "nN" (-N.N,-N.N \| N.N,N.N) could not be found.  | `org.eclipse.elk.alg.layered.graph.transform.ElkGraphImporter.transformEdge(ElkGraphImporter.java:1130)` | 0292 |
| 1 | UnsupportedGraphException: The source or the target of edge ElkEdge "_xh" ElkNode "nN" (N.N,N.N \| N.N,N.N) -> ElkNode "nN" (N.N,N.N \| N.N,N.N) could not be foun | `org.eclipse.elk.alg.layered.graph.transform.ElkGraphImporter.transformEdge(ElkGraphImporter.java:1130)` | 0376 |
| 1 | UnsupportedConfigurationException: Topdown layout cannot be used together with hierarchy handling. | `org.eclipse.elk.core.RecursiveGraphLayoutEngine.layoutRecursively(RecursiveGraphLayoutEngine.java:180)` | 0382 |
| 1 | NullPointerException: Cannot read field "y" because "portNodeSize" is null | `org.eclipse.elk.alg.layered.graph.LGraphUtil.createExternalPortDummy(LGraphUtil.java:944)` | 0412 |
| 1 | UnsupportedConfigurationException: Invalid node label placement: [H_RIGHT, INSIDE, OUTSIDE] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:159)` | 0567 |
| 1 | NegativeArraySizeException: -N | `org.eclipse.elk.alg.common.polyomino.structures.TwoBitGrid.reinitialize(TwoBitGrid.java:117)` | 0613 |
| 1 | StringIndexOutOfBoundsException: Range [N, N) out of bounds for length N |  | 0674 |
| 1 | UnsupportedConfigurationException: Invalid node label placement: [H_LEFT, H_CENTER, V_BOTTOM, OUTSIDE, H_PRIORITY] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:159)` | 0715 |
| 1 | UnsupportedConfigurationException: Invalid vertical constraints. Node nN has a vertical constraint that is too low for its ancestors. | `org.eclipse.elk.alg.vertiflex.VertiFlexLayoutProvider.checkVerticalConstraintValidity(VertiFlexLayoutProvider.java:151)` | 0765 |
| 1 | UnsupportedGraphException: The source or the target of edge ElkEdge ElkNode "nN" (N.N,N.N \| N.N,N.N) -> ElkNode "nN" (N.N,N.N \| N.N,N.N) could not be found. Thi | `org.eclipse.elk.alg.layered.graph.transform.ElkGraphImporter.transformEdge(ElkGraphImporter.java:1130)` | 0798 |
| 1 | UnsupportedConfigurationException: Invalid node label placement: [H_CENTER, V_CENTER, V_BOTTOM, OUTSIDE] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:159)` | 0823 |
| 1 | StackOverflowError | `org.eclipse.elk.alg.radial.RadialUtil.getSuccessors(RadialUtil.java:57)` | 0883 |
| 1 | StackOverflowError | `org.eclipse.elk.alg.vertiflex.p4edgerouting.StraightEdgeRouter.routeEdges(StraightEdgeRouter.java:59)` | 0937 |
| 1 | UnsupportedConfigurationException: Invalid port label placement: [NEXT_TO_PORT_IF_POSSIBLE, ALWAYS_SAME_SIDE, ALWAYS_OTHER_SAME_SIDE] | `org.eclipse.elk.alg.common.nodespacing.internal.NodeContext.<init>(NodeContext.java:153)` | 0988 |

## Timeouts

| case | algorithm | family | nodes | edges | root options |
|---|---|---|---|---|---|
| 0004 | radial | dag | 226 | 516 | radius=0.494, nodeNode=28.532 |
| 0070 | force | gnm | 247 | 665 | repulsion=5000.0, separateConnectedComponents=false |
| 0180 | force | caterpillar | 1049 | 1048 | omitNodeMicroLayout=false, edgeLabel=9.629, nodeNode=167.798 |
| 0191 | stress | grid | 70 | 123 | interactive=false, desiredEdgeLength=134.581, dimension=Y, epsilon=0.001 |
| 0243 | radial | tree | 226 | 225 | omitNodeMicroLayout=true, compactionStepSize=0, compactor=RADIAL_COMPACTION, optimizationCriteria=EDGE_LENGTH, radius=0.887 |
| 0262 | layered | gnm | 1430 | 1819 | aspectRatio=0.883, interactiveLayout=false, forceNodeModelOrder=true, mode=SLOPPY, feedbackEdges=false, default=NONE, componentComponent=150.144, edgeLabel=2000.0, labelPortVertical=0.232, nodeNode=6.217 |
| 0321 | radial | complete | 24 | 276 | — |
| 0785 | stress | complete | 41 | 435 | desiredEdgeLength=0.0, dimension=Y, epsilon=0.001 |

## Sanity-check issues (cases flagged)

| issue | cases |
|---|---|
| edge(s) without a route | 196 |
| overlapping node pair(s) | 161 |
| edge(s) not touching their endpoints | 91 |
| non-finite coordinate(s) | 16 |
| child node(s) outside their parent | 7 |

Some of these are expected by design: packing and fixed/random layouts don't route edges, and the fixed layout keeps whatever overlaps the input had.

## Edge-case injections

| injection | cases |
|---|---|
| option-extreme | 170 |
| empty-or-single | 31 |
| zero-size | 23 |
| huge-node | 20 |
| far-position | 20 |
| duplicate-ports | 19 |
| long-labels | 19 |
| large | 18 |
| spacing-negative | 16 |
| nan-size | 16 |
| self-loops | 16 |
| thin-nodes | 16 |
| spacing-huge | 15 |
| isolated | 13 |
| spacing-zero | 13 |
| multi-edges | 13 |
| negative-size | 11 |

## Slowest successful layouts

| case | algorithm | family | nodes | edges | ms |
|---|---|---|---|---|---|
| 0990 | stress | dag | 1397 | 3446 | 23440 |
| 0578 | stress | layered-dag | 136 | 336 | 5400 |
| 0235 | layered | gnm | 221 | 559 | 3748 |
| 0479 | force | grid | 125 | 227 | 3236 |
| 0351 | force | dag | 153 | 251 | 3125 |
| 0920 | layered | layered-dag | 538 | 692 | 2857 |
| 0573 | force | caterpillar | 238 | 237 | 2749 |
| 0888 | stress | gnm | 250 | 693 | 2719 |
| 0023 | layered | complete | 91 | 435 | 2667 |
| 0266 | force | complete | 15 | 105 | 2349 |
| 0053 | sporeOverlap | components | 245 | 191 | 2008 |
| 0363 | layered | complete | 180 | 435 | 1846 |
| 0095 | sporeCompaction | cycle | 137 | 137 | 1703 |
| 0814 | layered | complete | 88 | 435 | 1689 |
| 0544 | sporeOverlap | star | 126 | 125 | 1412 |
