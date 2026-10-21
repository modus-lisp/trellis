;;;; rectilinear-convex-hull-test.lisp -- port of
;;;; org.eclipse.elk.alg.common.RectilinearConvexHullTest.

(in-package #:elk)

(define-plain-test-class rectilinear-convex-hull-test)

(deftest test-cross ()
  (let ((points (jlist (new-common-point 0d0 1d0) (new-common-point 1d0 1d0)
                       (new-common-point 1d0 0d0) (new-common-point 2d0 0d0)
                       (new-common-point 2d0 1d0) (new-common-point 3d0 1d0)
                       (new-common-point 3d0 2d0) (new-common-point 2d0 2d0)
                       (new-common-point 2d0 3d0) (new-common-point 1d0 3d0)
                       (new-common-point 1d0 2d0) (new-common-point 0d0 2d0))))
    (let* ((rch (common-rectilinear-convex-hull.of points)))
      ;; test that the hull consists of the specified points in the exact order
      (assert-true "Java List.equals" (%elements-equal-p points (get-hull rch)))
      (let ((expected-rects (jlist (new-elk-rectangle 0d0 1d0 1d0 1d0)
                                   (new-elk-rectangle 1d0 0d0 1d0 3d0)
                                   (new-elk-rectangle 2d0 1d0 1d0 1d0)))
            (actual-rects (split-into-rectangles rch)))
        ;; we don't care about order here
        (assert-true (%elements-equal-p actual-rects expected-rects))))))

(deftest test-little-robot ()
  (let ((points (jlist (new-common-point 0d0 2d0) (new-common-point 1d0 2d0)
                       (new-common-point 1d0 1d0) (new-common-point 2d0 1d0)
                       (new-common-point 2d0 0d0)
                       (new-common-point 2d0 0d0) (new-common-point 2d0 1d0)
                       (new-common-point 3d0 1d0) (new-common-point 3d0 2d0)
                       (new-common-point 4d0 2d0)
                       (new-common-point 4d0 2d0) (new-common-point 3d0 2d0)
                       (new-common-point 3d0 3d0) (new-common-point 2d0 3d0)
                       (new-common-point 2d0 4d0)
                       (new-common-point 2d0 4d0) (new-common-point 2d0 3d0)
                       (new-common-point 1d0 3d0) (new-common-point 1d0 2d0)
                       (new-common-point 0d0 2d0))))
    ;; test hull
    (let ((rch (common-rectilinear-convex-hull.of points)))
      (assert-true "Java List.equals" (%elements-equal-p points (get-hull rch)))
      ;; and rects
      (let ((actual-rects (split-into-rectangles rch))
            (expected-rects (jlist (new-elk-rectangle 0d0 2d0 1d0 0d0)
                                   (new-elk-rectangle 1d0 1d0 1d0 2d0)
                                   (new-elk-rectangle 2d0 0d0 0d0 4d0)
                                   (new-elk-rectangle 2d0 1d0 1d0 2d0)
                                   (new-elk-rectangle 3d0 2d0 1d0 0d0))))
        (assert-true (%elements-equal-p actual-rects expected-rects))))))
