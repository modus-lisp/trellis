;;;; cubic-spline-interpolator-test.lisp -- port of
;;;; org.eclipse.elk.core.math.CubicSplineInterpolatorTest.

(in-package #:elk)

(defcore-class cubic-spline-interpolator-test
  ("calculateClosedBezierSplineTest"
   (let* ((v0 (new-k-vector 5 50)) (v1 (new-k-vector 10 70)) (v2 (new-k-vector 30 100))
          (v3 (new-k-vector 60 70)) (v4 (new-k-vector 70 40))
          (vectors (vector v0 v1 v2 v3 v4))
          (b (calculate-closed-bezier-spline (new-cubic-spline-interpolator) vectors)))
     (assert-true (jequals v0 (get-end-point b))))
   ;; horizontal aligned points
   (let* ((vectors-h (vector (new-k-vector 5 50) (new-k-vector 10 50) (new-k-vector 30 50)
                             (new-k-vector 60 50) (new-k-vector 70 50)))
          (b (calculate-closed-bezier-spline (new-cubic-spline-interpolator) vectors-h)))
     (assert-near 50 (.y (get-start-point b)) 0)
     (assert-near 50 (.y (get-end-point b)) 0)
     (jdo (v (get-inner-points b))
       (assert-near 50 (.y v) 0)))
   ;; vertical aligned points
   (let* ((vectors-v (vector (new-k-vector 100 10) (new-k-vector 100 50) (new-k-vector 100 100)
                             (new-k-vector 100 120) (new-k-vector 100 200)))
          (b (calculate-closed-bezier-spline (new-cubic-spline-interpolator) vectors-v)))
     (assert-near 100 (.x (get-start-point b)) 0)
     (assert-near 100 (.x (get-end-point b)) 0)
     (jdo (v (get-inner-points b))
       (assert-near 100 (.x v) 0)))))
