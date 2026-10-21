;;;; elk-math-test.lisp -- port of org.eclipse.elk.core.math.ElkMathTest.
;;;; Java varargs (ElkMath.maxi(1, 7, 5, 6)) are one list parameter here:
;;;; (elk-math.maxi (list 1 7 5 6)).

(in-package #:elk)

(defcore-class elk-math-test
  ("testPointContains"
   (let ((rect (new-elk-rectangle 23 14 20 20)))
     (assert-true (elk-math.contains rect (new-k-vector 26 19)))
     (assert-true (not (elk-math.contains rect (new-k-vector 10 9))))
     (assert-true (not (elk-math.contains rect (new-k-vector 23 20))))
     (assert-true (not (elk-math.contains rect (new-k-vector 23 14))))))
  ("testLineContains"
   (let ((rect (new-elk-rectangle 23 14 20 20)))
     (assert-true (elk-math.contains rect (new-k-vector 24 20) (new-k-vector 40 32)))
     (assert-true (not (elk-math.contains rect (new-k-vector 10 10) (new-k-vector 40 32))))))
  ("testPathContains"
   (let* ((rect (new-elk-rectangle 23 14 20 20))
          (path (new-k-vector-chain)))
     (add path 24 15) (add path 27 20) (add path 39 30) (add path 29 19)
     (assert-true (elk-math.contains rect path))
     ;; on border
     (let ((path2 (new-k-vector-chain path)))
       (add path2 23 14)
       (assert-true (not (elk-math.contains rect path2))))
     ;; outside
     (add path 10 10)
     (assert-true (not (elk-math.contains rect path)))))
  ("testLineLineIntersect"
   (let ((l11 (new-k-vector 10 10)) (l12 (new-k-vector 20 20)))
     ;; cross
     (assert-true (elk-math.intersects l11 l12 (new-k-vector 11 21) (new-k-vector 21 11)))
     ;; touch
     (assert-true (not (elk-math.intersects l11 l12 (new-k-vector 10 10) (new-k-vector 15 10))))
     ;; no cross
     (assert-true (not (elk-math.intersects l11 l12 (new-k-vector 1 2) (new-k-vector 2 1))))
     ;; same line
     (assert-true (not (elk-math.intersects l11 l12 (clone l11) (clone l12))))
     (assert-true (not (elk-math.intersects l11 l12 (clone l12) (clone l11))))
     ;; parallel
     (assert-true (not (elk-math.intersects l11 l12 (new-k-vector 11 1) (new-k-vector 21 21))))))
  ("testPathIntersects"
   (let* ((rect (new-elk-rectangle 23 14 20 20))
          (path (new-k-vector-chain)))
     (add path 24 15) (add path 27 20) (add path 39 30) (add path 29 19)
     (assert-true (not (elk-math.intersects rect path)))
     ;; on border
     (let ((path2 (new-k-vector-chain path)))
       (add path2 23 14)
       (assert-true (not (elk-math.intersects rect path2))))
     ;; cross
     (add path 10 10)
     (assert-true (elk-math.intersects rect path))))
  ("testFactl"
   (assert-equals 1 (elk-math.factl 0))
   (assert-equals 1 (elk-math.factl 1))
   (assert-equals 2432902008176640000 (elk-math.factl 20)))
  ("testFactlLittleIllegalArgumentException"
   (assert-throws error (elk-math.factl -50)))
  ("testFactlBigIllegalArgumentException"
   (assert-throws error (elk-math.factl 21)))
  ("testFactd"
   (assert-near 1 (elk-math.factd 0) 0.0)
   (assert-near 1 (elk-math.factd 1) 0.0))
  ("testFacdlLittleIllegalArgumentException"
   (assert-throws error (elk-math.factd -1)))
  ("testBinomiall"
   (assert-equals 1 (elk-math.binomiall 2 0))
   (assert-equals 1 (elk-math.binomiall 20 20))
   (assert-equals 2 (elk-math.binomiall 2 1)))
  ("testBinomialllLittleIllegalArgumentException"
   (assert-throws error (elk-math.binomiall -1 1)))
  ("testBinomiald"
   (assert-near 1 (elk-math.binomiald 2 0) 0)
   (assert-near 1 (elk-math.binomiald 20 20) 0)
   (assert-near 2 (elk-math.binomiald 2 1) 0))
  ("testBinomialdLittleIllegalArgumentException"
   (assert-throws error (elk-math.binomiald -1 1)))
  ("testPow"
   (let ((ad 10d0) (af 10f0))
     (assert-near 1 (elk-math.powd ad 0) 0)
     (assert-near 1 (elk-math.powf af 0) 0)
     (assert-near 100 (elk-math.powd ad 2) 0)
     (assert-near 100 (elk-math.powf af 2) 0)))
  ("testCalcBezierPoints"
   (let* ((k1 (new-k-vector 10 10)) (k2 (new-k-vector 20 20))
          (k3 (new-k-vector 30 30)) (k4 (new-k-vector 50 50))
          (result (elk-math.approximate-bezier-segment 20 k1 k2 k3 k4)))
     (assert-near (.x k4) (.x (aref result (1- (length result)))) 0.000000001)
     (assert-near (.y k4) (.y (aref result (1- (length result)))) 0.000000001))
   (let* ((k1 (new-k-vector 50 10)) (k2 (new-k-vector 70 10))
          (k3 (new-k-vector 80 10)) (k4 (new-k-vector 100 10))
          (result (elk-math.approximate-bezier-segment 20 k1 k2 k3 k4)))
     (jdo (k result)
       (assert-near 10 (.y k) 0.000000001))))
  ("testAppoximateSpline"
   (let* ((k1 (new-k-vector 10 10)) (k2 (new-k-vector 20 20))
          (k3 (new-k-vector 30 30)) (k4 (new-k-vector 50 50))
          (vectors (elk-math.approximate-bezier-segment 20 k1 k2 k3 k4))
          (control (new-k-vector-chain vectors))
          (result (elk-math.approximate-bezier-spline control))
          (k (j-get result (1- (size result)))))
     (assert-near (.x k4) (.x k) 0.000000001)
     (assert-near (.y k4) (.y k) 0.000000001))
   (let* ((k1 (new-k-vector 50 10)) (k2 (new-k-vector 70 10))
          (k3 (new-k-vector 80 10)) (k4 (new-k-vector 100 10))
          (vectors (elk-math.approximate-bezier-segment 20 k1 k2 k3 k4))
          (control (new-k-vector-chain vectors))
          (result (elk-math.approximate-bezier-spline control)))
     (jdo (kv result)
       (assert-near 10 (.y kv) 0.000000001))))
  ("testDistanceFromSpline"
   (let* ((k1 (new-k-vector 10 10)) (k2 (new-k-vector 20 20))
          (k3 (new-k-vector 30 30)) (k4 (new-k-vector 50 50)))
     (dolist (needle (list k4 k3 k2 k1))
       (assert-near 0 (elk-math.distance-from-bezier-segment k1 k2 k3 k4 needle) 0.01))))
  ("testMax"
   (assert-equals 7 (elk-math.maxi (list 1 7 5 6)))
   (assert-near 7 (elk-math.maxf (list 1 7 5 6)) 0)
   (assert-near 7 (elk-math.maxd (list 1 7 5 6)) 0))
  ("testMin"
   (assert-equals 1 (elk-math.mini (list 1 7 5 6)))
   (assert-equals 0 (elk-math.mini (list 8 1 9 0)))
   (assert-equals 8 (elk-math.mini (list 8 8 8 8)))
   (assert-near 1 (elk-math.minf (list 1 7 5 6)) 0)
   (assert-near 0 (elk-math.minf (list 8 1 9 0)) 0)
   (assert-near 8 (elk-math.minf (list 8 8 8 8)) 0)
   (assert-near 1 (elk-math.mind (list 1 7 5 6)) 0)
   (assert-near 0 (elk-math.mind (list 8 1 9 0)) 0)
   (assert-near 8 (elk-math.mind (list 8 8 8 8)) 0))
  ("testAverage"
   (assert-equals 4 (elk-math.averagel (list 5 8 2 1)))
   (assert-equals 2 (elk-math.averagel (list 5 0 2 1)))
   (assert-near 4 (elk-math.averagef (list 5 8 2 1)) 0)
   (assert-near 2 (elk-math.averagef (list 5 0 2 1)) 0)
   (assert-near 4 (elk-math.averaged (list 5 8 2 1)) 0)
   (assert-near 2 (elk-math.averaged (list 5 0 2 1)) 0)))
