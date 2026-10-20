# core-math — org.eclipse.elk.core.math

Files: classes.lisp, vectors.lisp (KVector, KVectorChain, Spacing/ElkPadding/ElkMargin,
ElkRectangle), elk-math.lisp, splines.lisp (BezierSpline, BezierCurve, ISplineInterpolator,
CubicSplineInterpolator).

## Deviations / non-obvious
- `i-data-object` interface and `parse/1` generic are defined here (graph module had none).
- Overloads of the same arity are dispatched by `typep`:
  `new-k-vector/1` (KVector copy | angle), `new-k-vector/2` (doubles | start,end vectors),
  `new-k-vector-chain/1` (collection | varargs list | single KVector),
  chain `add/2` (index,KVector | x,y), chain `j-remove/1` (int index | object),
  `new-elk-padding/1` (any | other), `elk-math.contains/2` (rect,KVector | rect,chain),
  `elk-math.approximate-bezier-segment/2` (int,points | two points).
- Varargs are one parameter (list/vector) as per PORTING.md, BUT convenience arities exist:
  `k-vector.sum/2,/3`, `elk-math.approximate-bezier-segment/3,/4,/5` (first arg integer =
  resultSize), `elk-math.{max,min}{i,l,f,d}/2,/3`.
- Arrays returned (`to-array`, `get-inner-points`, `approximate-bezier-segment`, ...) are
  simple vectors.  KVectorChain keeps its elements in `.items` (an adjustable jlist);
  `jiterable-items` returns that vector itself (no copy).
- `jclone` of a KVectorChain is shallow (LinkedList.clone); of KVector/ElkPadding/ElkMargin a copy.
- Constructors/setters coerce numbers to double (`jd`), so `(new-k-vector 5 50)` holds doubles.
- `point-on-line` / `angle-on-line` with negative distance reproduce Java's ListIterator
  quirk: `listIterator(size()-1).previous()` starts at element size()-2 (and a 2-point chain
  throws NoSuchElementException there, as in Java).
- Double.parseDouble is reimplemented (`%cm-parse-double`) strictly; malformed numbers raise
  illegal-argument-exception.  String.split semantics (trailing empties dropped) in `%cm-split`.
- hashCode methods reproduce Java's values (Double.hashCode, Integer.reverse, Objects.hash).
- `powf` uses single-float arithmetic; float-typed methods otherwise use doubles.
- Guava DoubleMath.fuzzyEquals/fuzzyCompare are local helpers `%cm-fuzzy-equals/compare`.
