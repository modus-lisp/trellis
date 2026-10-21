;;;; network-simplex-test.lisp -- port of
;;;; org.eclipse.elk.alg.common.networksimplex.NetworkSimplexTest.

(in-package #:elk)

(define-plain-test-class network-simplex-test)

(defun %generate-random-graph (random)
  (let ((graph (new-n-graph))
        (n 4000)
        (e 8000))
    ;; create nodes
    (dotimes (i n)
      (create (id (n-node.of) i) graph))
    ;; create edges
    (dotimes (i e)
      (let* ((src (next-int random n))
             (tgt (next-int random n)))
        ;; no self loops
        (loop while (= src tgt)
              do (setf tgt (next-int random n)))
        (%ns-add-random-edge graph random src tgt)))
    ;; assert connectedness
    (dotimes (i (- n 1))
      (%ns-add-random-edge graph random i (+ i 1)))
    ;; assert acyclic
    (jdo (node (.nodes graph))
      (jdo (edge (jlist-from (get-outgoing-edges node)))
        (when (> (.id (.source edge)) (.id (.target edge)))
          (j-reverse edge))))
    graph))

(defun %ns-add-random-edge (graph random src tgt)
  "NEdge.of().delta(random.nextInt(50)).weight(random.nextDouble() * 50)
.source(...).target(...).create(): delta is drawn before weight, as in Java."
  (let* ((delta (next-int random 50))
         (weight (* (next-double random) 50d0))
         (edge (delta (n-edge.of) delta)))
    (setf edge (weight edge weight))
    (setf edge (source edge (jl-get (.nodes graph) src)))
    (setf edge (target edge (jl-get (.nodes graph) tgt)))
    (create edge)))

(deftest test-deltas ()
  (let ((random (new-random 1)))
    (dotimes (j 5)
      (dotimes (i 5)
        (let ((graph (%generate-random-graph random)))
          (assert-true (is-acyclic graph))
          (execute (network-simplex.for-graph graph) (new-basic-progress-monitor))
          (jdo (node (.nodes graph))
            (jdo (e (get-outgoing-edges node))
              (assert-true "Valid delta"
               (>= (- (.layer (get-target e)) (.layer (get-source e))) (.delta e))))))))))
