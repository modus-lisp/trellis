;;;; rectlinear-convex-hull-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.compaction.recthull.RectlinearConvexHullTest.
;;;; A plain JUnit class (no LayoutTestRunner), so its tests are ordinary
;;;; functions, run with (rectlinear-convex-hull-test.run-all).

(in-package #:elk)

(defun %rch-contains-in-any-order (needles haystack)
  (loop for needle across needles
        do (unless (find needle haystack :test #'jequals)
             (fail "Expected hull rectangle not found.")))
  t)

(defun rectlinear-convex-hull-test.test-cross ()
  (let* ((points (jlist (new-layered-point 0d0 1d0) (new-layered-point 1d0 1d0)
                        (new-layered-point 1d0 0d0) (new-layered-point 2d0 0d0)
                        (new-layered-point 2d0 1d0) (new-layered-point 3d0 1d0)
                        (new-layered-point 3d0 2d0) (new-layered-point 2d0 2d0)
                        (new-layered-point 2d0 3d0) (new-layered-point 1d0 3d0)
                        (new-layered-point 1d0 2d0) (new-layered-point 0d0 2d0)))
         (rch (layered-rectilinear-convex-hull.of points)))
    (assert-equals points (get-hull rch))
    (let ((expected (jlist (new-elk-rectangle 0 1 1 1)
                           (new-elk-rectangle 1 0 1 3)
                           (new-elk-rectangle 2 1 1 1)))
          (actual (split-into-rectangles rch)))
      (assert-true (%rch-contains-in-any-order expected actual)))))

(defun rectlinear-convex-hull-test.test-little-robot ()
  (let* ((points (jlist (new-layered-point 0d0 2d0) (new-layered-point 1d0 2d0)
                        (new-layered-point 1d0 1d0) (new-layered-point 2d0 1d0)
                        (new-layered-point 2d0 0d0) (new-layered-point 2d0 0d0)
                        (new-layered-point 2d0 1d0) (new-layered-point 3d0 1d0)
                        (new-layered-point 3d0 2d0) (new-layered-point 4d0 2d0)
                        (new-layered-point 4d0 2d0) (new-layered-point 3d0 2d0)
                        (new-layered-point 3d0 3d0) (new-layered-point 2d0 3d0)
                        (new-layered-point 2d0 4d0) (new-layered-point 2d0 4d0)
                        (new-layered-point 2d0 3d0) (new-layered-point 1d0 3d0)
                        (new-layered-point 1d0 2d0) (new-layered-point 0d0 2d0)))
         (rch (layered-rectilinear-convex-hull.of points)))
    (assert-equals points (get-hull rch))
    (let ((actual (split-into-rectangles rch))
          (expected (jlist (new-elk-rectangle 0 2 1 0)
                           (new-elk-rectangle 1 1 1 2)
                           (new-elk-rectangle 2 0 0 4)
                           (new-elk-rectangle 2 1 1 2)
                           (new-elk-rectangle 3 2 1 0))))
      (assert-true (%rch-contains-in-any-order expected actual)))))

(defun rectlinear-convex-hull-test.run-all ()
  (rectlinear-convex-hull-test.test-cross)
  (rectlinear-convex-hull-test.test-little-robot))
