;;;; polyline-self-loop-router-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.loops.routing.PolylineSelfLoopRouterTest.
;;;; Plain JUnit class; run with (polyline-self-loop-router-test.run-all).

(in-package #:elk)

(defun %pslrt-chain (&rest points)
  (let ((chain (new-k-vector-chain)))
    (dolist (p points)
      (add chain (new-k-vector (first p) (second p))))
    chain))

(defun polyline-self-loop-router-test.test-usual-case ()
  (let* ((router (new-polyline-self-loop-router))
         (input (%pslrt-chain '(0 0) '(100 0) '(100 100) '(-100 100) '(-100 -100) '(0 -100)))
         (expected (%pslrt-chain '(90 0) '(100 10) '(100 90) '(90 100)
                                 '(-90 100) '(-100 90) '(-100 -90) '(-90 -100))))
    (assert-equals expected (cut-corners router input 10))))

(defun polyline-self-loop-router-test.test-smaller-distance ()
  (let* ((router (new-polyline-self-loop-router))
         (input (%pslrt-chain '(0 0) '(100 0) '(100 100) '(-100 100) '(-100 -100) '(0 -100)))
         (expected (%pslrt-chain '(50 0) '(100 50) '(100 50) '(50 100)
                                 '(-20 100) '(-100 20) '(-100 -50) '(-50 -100))))
    (assert-equals expected (cut-corners router input 80))))

(defun polyline-self-loop-router-test.run-all ()
  (polyline-self-loop-router-test.test-usual-case)
  (polyline-self-loop-router-test.test-smaller-distance))
