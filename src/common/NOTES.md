# common (org.eclipse.elk.alg.common) -- port notes

Status: polyomino/ ported (PolyominoCompactor, ProfileFill, Successor*, comparators,
structures/* incl. PlanarGrid, TwoBitGrid, Polyomino, Polyominoes, Direction).
Ported: root, utils, networksimplex, overlaps, spore (cspore), compaction (incl. oned),
polyomino. Tests (4 JUnit files) not ported.
nodespacing/ is excluded (another agent).

Deviations
- networksimplex: NNode's ChangeAwareArrayList cache is replaced by a fresh
  getConnectedEdges() list each call. Same contents and order; only object identity
  differs (Java returns the cached list while incoming/outgoing are unchanged).
- Sets and maps: LinkedHashSet -> insertion-ordered jlist with membership checks
  (CGroup.cNodes). HashSet/HashMap keyed by identity -> jset / hash table; Java
  HashSet iteration order is not reproduced.
- TreeSet uses (overlaps scanline, spore scanline, ConstraintsScanlineHandler) are
  sorted adjustable vectors; comparator-equal add is rejected as in TreeSet.
- SVG and debug output (DebugUtil, SVGImage, getDebugSVG, DepthFirstCompaction debugOut)
  are no-ops; signatures kept.
- ScanlineOverlapCheck's dbg argument is ignored.
- common-internal-properties.fuzziness (0.0001) is defined in root.lisp (TTriangle needs
  it before spore loads); spore must not redefine it.
- Scanline.execute has two Java overloads of arity 3; dispatched on whether the third
  argument is a list/jlist (iterable) or a single handler.
- CompareFuzzy lives at the top of compaction-scanline.lisp (overlap checks use it).

Cross-module assumptions (undefined in this module at check time)
- core-direction.isHorizontal: is-horizontal on core-direction (core.options.Direction)
  must be provided by the core owner.
- elk-graph-adapters.adapt/1 (used by spore/, not common).
- Lisp names from root/utils by other agents: utils.get-rect-edges/1.

Polyomino deviations
- TwoBitGrid word layout: Java's long[][] (32 cells x 2 bits per long) is stored as
  rows of 32-bit halves (element 2w holds cells 32w..32w+15, element 2w+1 cells
  32w+16..32w+31). Each two-bit field lies inside one half, so every Java operation
  (>>>, &, |, ~mask) is reproduced exactly with masks, and no value leaves the fixnum
  range (no bignums on the 64-bit path).
- Out-of-range access: Java's ArrayIndexOutOfBounds (wrapped as IndexOutOfBounds) is
  signalled as index-out-of-bounds-exception for negative/overflowing rows and words;
  set() also rejects x >= width as Java does (cells in the padding of the last word are
  readable/writable, as in Java).
- The center-based wrappers do not append Java's " Given center based coordinates
  were (x, y)." to the exception message (message text only).
- toString() of TwoBitGrid is ported (cheap); debug/SVG output is not.
- Comparators are small classes with a COMPARE/2 method; PolyominoCompactor wraps them
  as closures for JL-SORT. Java's Comparator.reversed() is REVERSED-COMPARATOR.
- SuccessorQuadrantsGeneric compares polyominoes by identity (Polyomino has no equals
  override), as Java does.
- Intersects/addFilledCellsFrom overloads: the PlanarGrid bodies live in
  %PLANAR-INTERSECTS-GRID / %PLANAR-ADD-FILLED-CELLS-FROM, so the (PlanarGrid) cast in
  Java's Polyomino overloads calls the generic body, not the Polyomino method.
- NEW-TWO-BIT-GRID/0, NEW-PLANAR-GRID/0 and NEW-POLYOMINO/0 are defined by DEFJCLASS and
  again by DEFJCTOR (the explicit package-private no-arg constructors); the redefinition
  warning at build time is expected.
