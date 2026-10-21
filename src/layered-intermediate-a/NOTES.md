# layered-intermediate-a -- notes

Ports (org.eclipse.elk.alg.layered.intermediate, top-level files only):
BiLinkedHashMultiMap, CommentNodeMarginCalculator, CommentPostprocessor, CommentPreprocessor,
ConstraintsPostprocessor, DummySelfLoopProcessor, EdgeAndLayerConstraintEdgeReverser,
EndLabelPostprocessor, EndLabelPreprocessor, EndLabelSorter, FinalSplineBendpointsCalculator,
GraphTransformer, HierarchicalNodeResizingProcessor, HierarchicalPortConstraintProcessor,
HierarchicalPortDummySizeProcessor, HierarchicalPortOrthogonalEdgeRouter,
HierarchicalPortPositionProcessor, HighDegreeNodeLayeringProcessor, HyperedgeDummyMerger,
HypernodesProcessor, InLayerConstraintProcessor, IntermediateProcessorStrategy.

Files (load order in files.txt): classes, multimap-comments, reversers, end-labels, hierarchical,
hierarchical-constraint, hierarchical-router, misc, high-degree, final-spline, graph-transformer,
intermediate-strategy.

## Deviations and non-obvious points

- Java overloads that differ only by argument type (GraphTransformer.mirrorX/mirrorY/transpose/
  transposePortSide, HierarchicalPortOrthogonalEdgeRouter.fixCoordinates/correctSlantedEdgeSegments)
  are separate Lisp methods, distinguished by name or specialised on the parameter class.
- Private helpers that are not Java names are declared with DEFINE-JCALLS at the top of their file,
  because the call sites are compiled before the DEFJMETHOD that defines them.
- Java `return` inside a DEFJMETHOD is written as an explicit BLOCK; DEFJMETHOD does not establish a
  block named after the method.
- HighDegreeNodeLayeringProcessor: the ListIterator over layers is modelled with an index CURSOR
  that prependLayer/appendLayer advance exactly as ListIterator.add does.  Pairs are conses.
- HierarchicalPortConstraintProcessor: Arrays.sort with NodeComparator is done with JL-SORT
  (stable merge sort over a predicate).  NodeComparator is not a consistent comparator (two
  non-port nodes compare -1 both ways), so the resulting order can differ from TimSort's in that
  case; no test is known to depend on it.
- HyperedgeDummyMerger.MergeState is a plain class with a constructor (classes.lisp).
- HierarchicalPortOrthogonalEdgeRouter uses JSET (insertion-ordered in SBCL hash tables) for the
  LinkedHashSets of source and target layers.
- FinalSplineBendpointsCalculator: `new KVector(x, y)` etc. map to NEW-K-VECTOR/2; the Java
  NubSpline varargs call is NEW-NUB-SPLINE with a KVectorChain.  ElkMath.approximateBezierSegment
  receives a list of points.
- IntermediateProcessorStrategy.CREATE is keyed on the enum constants of
  LAYERED-INTERMEDIATE-PROCESSOR-STRATEGY in gen-enums.  Each Java `new X(...)` is the mechanical
  NEW-<lisp-class-name>, with class names taken from tools/classnames.tsv.
- EndLabelSorter: the LABEL-GROUP-COMPARATOR defjconst was moved above its first use (it was a
  forward reference that compiled with an undefined-variable warning).

## Cross-module names this module relies on that are NOT defined anywhere yet

- PortConstraints.isOrderFixed / isSideFixed (`is-order-fixed`, `is-side-fixed`): called from
  hierarchical-constraint.lisp, reversers.lisp, multimap-comments.lisp and layered-intermediate-b
  but defined in no module (core options owns the enum).
- Layered-p5 (later module): SPLINE-EDGE-ROUTER.IS-QUALIFIED-AS-STARTING-NODE,
  SPLINE-EDGE-ROUTER.SPLINE-DIMENSION, SPLINES-MATH.PORT-SIDE-TO-DIRECTION, NEW-NUB-SPLINE/3,
  GET-BEZIER-CP, IS-HYPER-EDGE on SPLINE-SEGMENT.
- Constructors of the intermediate-b/c/d and layered-misc processors listed in
  intermediate-strategy.lisp (NEW-<class>/0), and NEW-LABEL-MANAGEMENT-PROCESSOR/1.

## Verification

tools/check-module.lisp cannot run as shipped: its core-module step fails on `core/files.txt`
entries that include the `.lisp` extension and the earlier modules exhaust SBCL's default heap.
The module was checked with a private copy (/tmp/check-lia.lisp) that strips the extension and
runs with --dynamic-space-size 16384: all 12 files compile, 0 warnings in this module, and the only
undefined names are the cross-module ones listed above.  No runtime test was run.
