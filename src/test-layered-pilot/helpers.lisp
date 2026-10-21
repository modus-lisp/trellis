;;;; helpers.lisp -- the parts of the Java test helpers that the pilot tests use.
;;;;
;;;; org.eclipse.elk.alg.test.GraphTestUtils (allNodes, haveOverlaps) is not in
;;;; the test-framework module, so it is ported here under its mechanical name.
;;;; FILTER-PATH is ModelResourcePath.withFilter(...), which the registry's
;;;; resource clause receives as a plain path structure.

(in-package #:elk)

(defun %pilot-with-filter (path &rest extensions)
  "new ModelResourcePath(p).withFilter(new FileExtensionFilter(exts...))."
  (setf (abstract-resource-path-filter path) (apply #'file-extension-filter extensions))
  path)

(defjstatic graph-test-utils.all-nodes (graph include-graph)
  ;; Walks the list while it grows, as the Java does.
  (let ((nodes (new-jlist)))
    (if include-graph
        (jl-add nodes graph)
        (jdo (child (get-children graph)) (jl-add nodes child)))
    (loop for i from 0
          while (< i (length nodes))
          do (jdo (child (get-children (aref nodes i))) (jl-add nodes child)))
    nodes))

(defun %pilot-rect-intersects-p (x0 y0 w0 h0 x1 y1 w1 h1)
  "java.awt.geom.Rectangle2D.intersects: empty rectangles never intersect."
  (and (> w0 0) (> h0 0) (> w1 0) (> h1 0)
       (< x1 (+ x0 w0)) (< y1 (+ y0 h0))
       (> (+ x1 w1) x0) (> (+ y1 h1) y0)))

(defjstatic graph-test-utils.have-overlaps (shapes)
  (let ((rects (new-jlist)))
    (jdo (shape shapes)
      (let ((pos (elk-util.absolute-position shape)))
        (jl-add rects (list (.x pos) (.y pos) (get-width shape) (get-height shape)))))
    (loop for i from 0 below (length rects)
          thereis (loop for j from (1+ i) below (length rects)
                        thereis (destructuring-bind (x0 y0 w0 h0) (aref rects i)
                                  (destructuring-bind (x1 y1 w1 h1) (aref rects j)
                                    (%pilot-rect-intersects-p x0 y0 w0 h0 x1 y1 w1 h1)))))))
