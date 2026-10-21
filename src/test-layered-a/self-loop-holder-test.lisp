;;;; self-loop-holder-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.loops.SelfLoopHolderTest.  Plain JUnit
;;;; class; run with (self-loop-holder-test.run-all).

(in-package #:elk)

(defun %slht-same-set-p (a b)
  "HashSet equality for identity-compared elements."
  (and (subsetp a b :test #'eq) (subsetp b a :test #'eq)))

(defun self-loop-holder-test.test-non-self-loop-graph ()
  (let ((l-graph (self-loop-test-graph-creator.basic-graph-without-self-loops)))
    (jdo (l-node (get-layerless-nodes l-graph))
      (assert-false (self-loop-holder.needs-self-loop-processing l-node))
      ;; try { install; fail(); } catch (AssertionError e) {}: Java's fail() is an
      ;; AssertionError too, so this catches both outcomes, exactly as in Java.
      (handler-case (progn (self-loop-holder.install l-node)
                           (fail "install should have failed"))
        (error () nil)))))

(defun self-loop-holder-test.test-basic-self-loops ()
  (let* ((l-graph (self-loop-test-graph-creator.basic-graph-with-self-loops))
         (l-node (j-get (get-layerless-nodes l-graph) 1)))
    (assert-true (self-loop-holder.needs-self-loop-processing l-node))
    (let ((sl-holder (self-loop-holder.install l-node)))
      (assert-not-null sl-holder)
      (assert-equals sl-holder (get-property l-node layered-internal-properties.self-loop-holder))
      (assert-equals (length (get-ports l-node)) (size (get-sl-port-map sl-holder)))
      (assert-equals (floor (length (get-ports l-node)) 2)
                     (size (get-sl-hyper-loops sl-holder))))))

(defun self-loop-holder-test.test-different-self-hyper-loops ()
  (let* ((l-graph (self-loop-test-graph-creator.basic-graph-without-self-loops))
         (l-node (j-get (get-layerless-nodes l-graph) 1))
         (first-loops '()) (second-loops '()))
    (self-loop-test-graph-creator.ports l-node 2 2 2 1)
    (loop for i from 1 below (floor (length (get-ports l-node)) 2)
          do (pushnew (self-loop-test-graph-creator.edge (j-get (get-ports l-node) (- i 1))
                                                         (j-get (get-ports l-node) i))
                      first-loops))
    (loop for i from (+ (floor (length (get-ports l-node)) 2) 1) below (length (get-ports l-node))
          do (pushnew (self-loop-test-graph-creator.edge (j-get (get-ports l-node) (- i 1))
                                                         (j-get (get-ports l-node) i))
                      second-loops))
    (let ((sl-holder (self-loop-holder.install l-node)))
      (assert-equals 2 (size (get-sl-hyper-loops sl-holder)))
      (let ((first-hl-edges (mapcar (jfun get-l-edge 0)
                                    (coerce (jiterable-items (get-sl-edges (j-get (get-sl-hyper-loops sl-holder) 0))) 'list)))
            (second-hl-edges (mapcar (jfun get-l-edge 0)
                                     (coerce (jiterable-items (get-sl-edges (j-get (get-sl-hyper-loops sl-holder) 1))) 'list))))
        (let ((match (and (%slht-same-set-p first-hl-edges first-loops)
                          (%slht-same-set-p second-hl-edges second-loops))))
          (setf match (or match
                          (and (%slht-same-set-p first-hl-edges second-loops)
                               (%slht-same-set-p second-hl-edges first-loops))))
          (assert-true match))))))

(defun self-loop-holder-test.run-all ()
  (self-loop-holder-test.test-non-self-loop-graph)
  (self-loop-holder-test.test-basic-self-loops)
  (self-loop-holder-test.test-different-self-hyper-loops))
