# layered-p5 (org.eclipse.elk.alg.layered.p5edges) -- status NOTES

## Status: splines PORTED (2026-10-21)

Load order (files.txt): classes, util, edges, orthogonal, splines-math,
rectangle, nub-spline, nubs-self-loop, spline-edge-router, spline-segment.

Splines (org.eclipse.elk.alg.layered.p5edges.splines), all ported:
- splines-math.lisp   -- SplinesMath (overloads folded into one dispatching function)
- rectangle.lisp      -- Rectangle (ctors, union, margins)
- nub-spline.lisp     -- NubSpline + PolarCP
- nubs-self-loop.lisp -- NubsSelfLoop (varargs KVector... taken as a jlist)
- spline-edge-router.lisp -- SplineEdgeRouter + Dependency + configuration
- spline-segment.lisp -- SplineSegment (two 3-arg ctors dispatch on the first arg)

Known deviations, all deliberate:
- Rectangle.union(Collection) keeps Java's bug: it unions into the first
  rectangle, so the returned copy is only that rectangle.
- NubSpline(clamped, dim, list) copies the caller's list before padding it;
  Java pads the caller's list in place.
- Pair<SideToProcess, LEdge> is a cons (side . edge); Sets of pairs are
  jlists with value dedupe.

Cross-module defect found (NOT fixed here, owned by layered-intermediate-a):
- final-spline.lisp, FINAL-SPLINE-BENDPOINTS-CALCULATOR PROCESS ends with
  (done progress-monitor) and never begins the monitor.  Java's processor does
  not touch the monitor at all.  Every spline layout therefore raises
  ILLEGAL-STATE-EXCEPTION "The task has not begun yet."  Remove that call.

Still NOT PORTED: nothing in the p5edges Java list.  Not verified: the
spline output geometry against ELK reference models (tests only check that
no exception is raised).
