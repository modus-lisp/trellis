;;;; classes.lisp -- test-only classes of org.eclipse.elk.alg.layered.TestGraphCreator
;;;; (the MockRandom inner class; the creator itself is a defjclass in
;;;; test-graph-creator.lisp).

(in-package #:elk)

;; protected final class MockRandom extends Random.  nextBoolean/nextDouble/
;; nextFloat are overridden to return fixed, incrementing values.
(defjclass mock-random (java-random)
  ((next-boolean-value t)
   (change-by 0.01d0)
   (current-double 0.01d0)))

(defjctor mock-random ()
  (random-set-seed this 0))

(defjmethod set-next-boolean ((this mock-random) b) (setf (.next-boolean-value this) b))
(defjmethod set-change-by ((this mock-random) d) (setf (.change-by this) d))

(defmethod next-boolean/0 ((this mock-random)) (.next-boolean-value this))
(defmethod next-double/0 ((this mock-random))
  (incf (.current-double this) (.change-by this)))
(defmethod next-float/0 ((this mock-random))
  (coerce (next-double/0 this) 'double-float))
