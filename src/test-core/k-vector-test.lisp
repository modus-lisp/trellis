;;;; k-vector-test.lisp -- port of org.eclipse.elk.core.math.KVectorTest.
;;;; The commented-out testApplyBounds body in Java is not ported (it is
;;;; commented out upstream).

(in-package #:elk)

(defcore-class k-vector-test
  ("testEquals"
   (let ((kvector1 (new-k-vector 10 10))
         (kvector2 (new-k-vector)))
     (setf (.x kvector2) 10)
     (setf (.y kvector2) 10)
     (assert-true (jequals kvector1 kvector2))
     (setf kvector1 (new-k-vector))
     (assert-false (jequals kvector1 (list :object)))))
  ("testAddAndSub"
   (let ((kvector1 (new-k-vector 12 70)) (kvector2 (new-k-vector 15 17)))
     (assert-true (jequals kvector1 (sub (add kvector1 kvector2) kvector2)))))
  ("testScale"
   (let* ((a (new-k-vector 12 70)) (b (new-k-vector 12 70)) (a-temp (new-k-vector 12 70)))
     (setf a (add (add a a-temp) a-temp))
     (setf b (scale b 3))
     (assert-true (jequals a b))))
  ("testTranslate"
   (let ((v (new-k-vector 10 30)) (b (new-k-vector 50 50)))
     (assert-true (jequals b (add v 40 20)))))
  ("testNormalize"
   (let ((v (new-k-vector 2 0)) (n (new-k-vector 1 0)))
     (assert-true (jequals n (normalize v)))
     (setf v (new-k-vector 0 2))
     (setf n (new-k-vector 0 1))
     (assert-true (jequals n (normalize v)))))
  ("testToDegrees"
   (loop for (x y deg) in '((10 0 0) (10 10 45) (0 10 90) (-10 10 135) (-10 0 180)
                            (-10 -10 225) (0 -10 270) (10 -10 315))
         do (assert-near deg (to-degrees (new-k-vector x y)) 0.00001)))
  ("testDistance"
   (let ((v1 (new-k-vector 5 50)) (v2 (new-k-vector 5 50)))
     (assert-near 0 (distance v1 v2) 0)
     (setf v1 (new-k-vector 0 20))
     (setf v2 (new-k-vector 0 50))
     (assert-near 30 (distance v1 v2) 0)))
  ("testParse"
   (let ((v1 (new-k-vector 5 50)) (v2 (new-k-vector)))
     (parse v2 "(5,50)")
     (parse v2 "{5,50}")
     (assert-true (jequals v1 v2))
     (parse v2 "[5,50]")
     (assert-true (jequals v1 v2))
     (parse v2 "{(5,50)}")
     (assert-true (jequals v1 v2))
     (parse v2 "[(5,50)]")
     (assert-true (jequals v1 v2))
     (parse v2 "[{5,50}]")
     (assert-true (jequals v1 v2)))))
