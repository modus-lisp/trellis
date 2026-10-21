;;;; common/polyomino-classes.lisp -- org.eclipse.elk.alg.common.polyomino (see tools/classnames.tsv)
;;;; Class definitions for the polyomino package.  Enums (common-direction,
;;;; traversal-strategy, low/high-level-sorting-criterion) are generated in
;;;; src/gen-enums/enums.lisp.

(in-package #:elk)

;;; structures/IThreeValueGrid
(defjinterface i-three-value-grid)

;;; structures/TwoBitGrid: the cell grid.  Rows are simple-vectors of 32-bit
;;; halves; see polyomino-structures.lisp for the layout.
(defjclass two-bit-grid (i-three-value-grid)
  ((grid nil)
   (x-size 0)
   (y-size 0)))

;;; structures/PlanarGrid: adds the center-based coordinate system.
(defjclass planar-grid (two-bit-grid)
  ((x-center 0)
   (y-center 0)))

;;; structures/Polyomino: a planar grid with an offset (x, y) and a list of
;;; extensions, each a UniqueTriple<Direction, Integer, Integer>.
(defjclass polyomino (planar-grid)
  ((x 0)
   (y 0)
   (polyomino-extensions nil)))

;;; structures/Polyominoes: the holder the compactor works on.
(defjclass polyominoes (map-property-holder)
  ((polys (new-jlist))
   (grid nil)))

;;; Comparators of PolyominoCompactor (private inner classes in Java).
(defjclass min-perimeter-comparator () ())
(defjclass min-perimeter-comparator-with-shape () ())
(defjclass min-num-of-extensions-comparator () ())
(defjclass min-num-of-extension-directions-comparator () ())
(defjclass single-extension-side-greater-than-rest-comparator () ())
(defjclass corner-cases-greater-than-rest-comparator () ())

;;; Successor functions: BiFunction<Pair<Integer,Integer>, Polyomino, Pair<Integer,Integer>>.
(defjclass successor-combination ()
  ((normal-fun nil)
   (external-fun nil)))
(defjclass successor-line-by-line () ())
(defjclass successor-manhattan () ())
(defjclass successor-jitter () ())
(defjclass successor-max-norm-winding-in-math-pos-sense () ())
(defjclass successor-quadrants-generic ()
  ((last-poly nil)
   (pos-x t)
   (pos-y t)
   (neg-x t)
   (neg-y t)
   (cost-fun nil)))

(defjclass polyomino-compactor () ())
(defjclass profile-fill () ())
