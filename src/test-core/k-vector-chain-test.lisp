;;;; k-vector-chain-test.lisp -- port of org.eclipse.elk.core.math.KVectorChainTest.

(in-package #:elk)

(defcore-class k-vector-chain-test
  ("testParse"
   (let ((v0 (new-k-vector 5 50)) (v1 (new-k-vector 10 50)) (v2 (new-k-vector 30 50))
         (kv (new-k-vector-chain)))
     (parse kv "{(5,50),(10,50),(30,50)}")
     (assert-true (jequals v0 (j-get kv 0)))
     (assert-true (jequals v1 (j-get kv 1)))
     (assert-true (jequals v2 (j-get kv 2)))
     ;; ignore a dangling element
     (setf kv (new-k-vector-chain))
     (parse kv "{(5,50),(10,50),(30,)}")
     (assert-true (jequals v0 (j-get kv 0)))
     (assert-true (jequals v1 (j-get kv 1)))
     (assert-true (= (size kv) 2))
     ;; some weird syntax
     (setf kv (new-k-vector-chain))
     (parse kv "{(5; 50 ], [10 , 50 ),(30,,,)}")
     (assert-true (jequals v0 (j-get kv 0)))
     (assert-true (jequals v1 (j-get kv 1)))
     (assert-true (= (size kv) 2))))
  ("testParseIllegalArgumentException"
   (let ((kv (new-k-vector-chain)))
     (assert-throws error (parse kv "{(5,a),(10,50),(30,50)}"))))
  ("testGetLength"
   (let ((kv (new-k-vector-chain)))
     (parse kv "{(10,50),(10,50),(10,50)}")
     (assert-near 0 (total-length kv) 0)
     (parse kv "{(10,0),(10,20),(10,30)}")
     (assert-near 30 (total-length kv) 0)))
  ("testGetPointOnLine"
   (let* ((v0 (new-k-vector 5 50)) (v1 (new-k-vector 10 50)) (v2 (new-k-vector 30 50))
          (kv (new-k-vector-chain (list v0 v1 v2))))
     (assert-true (jequals v0 (point-on-line kv 0)))
     (assert-true (jequals v1 (point-on-line kv 5)))
     (assert-true (jequals v2 (point-on-line kv 40))))))
