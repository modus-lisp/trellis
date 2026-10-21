# layered-p5 (org.eclipse.elk.alg.layered.p5edges) -- status NOTES

## Status: PARTIAL (resumed port, not finished)

Files present, in load order (files.txt):
- classes.lisp   -- defjclass for all p5edges classes, incl. splines (fields only)
- util.lisp      -- ListIterator over a jlist (for NubSpline knot insertion)
- edges.lisp     -- EdgeRouterFactory, OrthogonalEdgeRouter, PolylineEdgeRouter
- orthogonal.lisp -- orthogonal/ and orthogonal/direction/ (HyperEdgeSegment*,
                    HyperEdgeCycleDetector, OrthogonalRoutingGenerator, strategies)

NOT PORTED (only classes.lisp has their fields):
- splines/SplineEdgeRouter.java (1049 lines)
- splines/NubSpline.java (1134), splines/NubsSelfLoop.java (316)
- splines/Rectangle.java (337), splines/SplineSegment.java (304), splines/SplinesMath.java (310)

## Not verified
The module was NOT compile-checked. tools/check-module.lisp (working tree and
HEAD) cannot run in this environment:
- HEAD version expects extensionless names in files.txt, but src/core/files.txt
  lists "classes.lisp" (the working tree was being edited concurrently by another porter).
- Loading the earlier modules with EVAL exhausts the SBCL heap even with
  --dynamic-space-size 8192 (SB-C::COPY-CONSET during constraint propagation,
  inside core.lisp's loader); core/core.lisp also has an unmatched ')' at line 277.
A /tmp copy with the name fix still dies on the heap, so this needs a fix in tools/.
