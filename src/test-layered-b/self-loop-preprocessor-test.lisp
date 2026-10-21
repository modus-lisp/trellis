;;;; self-loop-preprocessor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.SelfLoopPreprocessorTest.
;;;;
;;;; A plain JUnit class (no LayoutTestRunner), so it is not a DEFLAYOUTTEST.
;;;; Run with (run-self-loop-preprocessor-test).

(in-package #:elk)

(defstruct %b-sl-test-graph
  graph node regular-ports only-self-loop-ports unconnected-ports)

(defun %b-sl-add-ports (node)
  (self-loop-test-graph-creator.ports node 2 2 2 1)
  ;; The ports need proper coordinates (the first will be placed at 10, the second at 40)
  (let ((north 10d0) (east 10d0) (south 10d0) (west 10d0))
    (jdo (l-port (get-ports node))
      (let ((side (get-side l-port)))
        (cond ((eq side port-side.north)
               (j-set (get-position l-port) north 0d0)
               (incf north 30d0))
              ((eq side port-side.east)
               (j-set (get-position l-port) (.x (get-size node)) east)
               (incf east 30d0))
              ((eq side port-side.south)
               (j-set (get-position l-port) south (.y (get-size node)))
               (incf south 30d0))
              ((eq side port-side.west)
               (j-set (get-position l-port) 0d0 west)
               (incf west 30d0))
              (t (fail nil)))))))

(defun %b-sl-add-edges (tg)
  (let* ((node (%b-sl-test-graph-node tg))
         (ports (get-ports node)))
    (self-loop-test-graph-creator.edge (j-get ports 0) (j-get ports 1))
    (self-loop-test-graph-creator.edge (j-get ports 1) (j-get ports 2))
    (self-loop-test-graph-creator.edge (j-get ports 3) (j-get ports 4))
    (self-loop-test-graph-creator.edge (j-get ports 4) (j-get ports 5))
    (add (%b-sl-test-graph-regular-ports tg) (j-get ports 0))
    (dolist (i '(1 2 3 4 5))
      (add (%b-sl-test-graph-only-self-loop-ports tg) (j-get ports i)))
    (add (%b-sl-test-graph-unconnected-ports tg) (j-get ports 6))
    (add (%b-sl-test-graph-unconnected-ports tg) (j-get ports 7))))

(defun %b-sl-make-test-graph (port-constraints)
  (let* ((graph (self-loop-test-graph-creator.basic-graph-without-self-loops))
         (node (j-get (get-layerless-nodes graph) 1))
         (tg (make-%b-sl-test-graph :graph graph :node node
                                    :regular-ports (new-jset)
                                    :only-self-loop-ports (new-jset)
                                    :unconnected-ports (new-jset))))
    (set-property node layered-options.port-constraints port-constraints)
    (j-set (get-size node) 50d0 50d0)
    (%b-sl-add-ports node)
    (%b-sl-add-edges tg)
    tg))

(defun %b-sl-test-hidden-edges (tg)
  (let ((node (%b-sl-test-graph-node tg)))
    ;; Not a single self loop edge should be left
    (jdo (l-edge (get-connected-edges node))
      (assert-false (is-self-loop l-edge)))
    ;; A regular edge should be left
    (assert-true (not (is-empty (get-connected-edges node))))))

(defun %b-sl-test-ports-hidden (port-constraints)
  (let* ((tg (%b-sl-make-test-graph port-constraints))
         (node (%b-sl-test-graph-node tg)))
    (process (new-self-loop-pre-processor) (%b-sl-test-graph-graph tg)
             (new-null-elk-progress-monitor))
    ;; Only the regular and unconnected ports should be left
    (assert-equals (+ (size (%b-sl-test-graph-regular-ports tg))
                      (size (%b-sl-test-graph-unconnected-ports tg)))
                   (size (get-ports node)))
    (assert-true (%b-contains-all-p (get-ports node) (%b-sl-test-graph-regular-ports tg)))
    (assert-true (%b-contains-all-p (get-ports node) (%b-sl-test-graph-unconnected-ports tg)))
    (%b-sl-test-hidden-edges tg)))

(defun %b-sl-test-ports-untouched (port-constraints)
  (let* ((tg (%b-sl-make-test-graph port-constraints))
         (node (%b-sl-test-graph-node tg)))
    (process (new-self-loop-pre-processor) (%b-sl-test-graph-graph tg)
             (new-null-elk-progress-monitor))
    ;; All ports should be left
    (assert-equals (+ (size (%b-sl-test-graph-regular-ports tg))
                      (size (%b-sl-test-graph-only-self-loop-ports tg))
                      (size (%b-sl-test-graph-unconnected-ports tg)))
                   (size (get-ports node)))
    (%b-sl-test-hidden-edges tg)))

(defun run-self-loop-preprocessor-test ()
  (%b-run-plain-tests
   "self-loop-preprocessor-test"
   (list (cons "testFreePorts" (lambda () (%b-sl-test-ports-hidden port-constraints.free)))
         (cons "testFixedSide" (lambda () (%b-sl-test-ports-hidden port-constraints.fixed-side)))
         (cons "testFixedOrder" (lambda () (%b-sl-test-ports-untouched port-constraints.fixed-order)))
         (cons "testFixedRatio" (lambda () (%b-sl-test-ports-untouched port-constraints.fixed-ratio)))
         (cons "testFixedPos" (lambda () (%b-sl-test-ports-untouched port-constraints.fixed-pos))))))
