;;;; binary-indexed-tree-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p3order.counting.BinaryIndexedTreeTest.
;;;; Plain JUnit (no layout involved).

(in-package #:elk)

(defplaintest binary-indexed-tree-test sum-before ()
  (let ((ft (new-binary-indexed-tree 5)))
    (add ft 1)
    (add ft 2)
    (add ft 1)
    (assert-equals "rank(1)" 0 (rank ft 1))
    (assert-equals "rank(2)" 2 (rank ft 2))))

(defplaintest binary-indexed-tree-test size ()
  (let ((ft (new-binary-indexed-tree 5)))
    (add ft 2)
    (add ft 1)
    (add ft 1)
    (assert-equals "size" 3 (size ft))))

(defplaintest binary-indexed-tree-test remove-all ()
  (let ((ft (new-binary-indexed-tree 5)))
    (add ft 0)
    (add ft 2)
    (add ft 1)
    (add ft 1)
    (remove-all ft 1)
    (assert-equals "size after removeAll" 2 (size ft))
    (assert-equals "rank(2) after removeAll" 1 (rank ft 2))
    (remove-all ft 1)
    (assert-equals "size after second removeAll" 2 (size ft))
    (assert-equals "rank(2) after second removeAll" 1 (rank ft 2))))
