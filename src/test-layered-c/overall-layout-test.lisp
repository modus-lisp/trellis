;;;; overall-layout-test.lisp -- port of org.eclipse.elk.alg.layered.OverallLayoutTest.
;;;; Plain JUnit: @Before lays out a simple graph with LayeredLayoutProvider.
;;;; Each DEFPLAINTEST lays out its own copy, which is what @Before does per test.

(in-package #:elk)

(defun %olt-create-simple-graph ()
  "Two fixed-size nodes joined by one edge; no coordinates are set (all 0)."
  (let* ((graph (elk-graph-util.create-graph))
         (node1 (elk-graph-util.create-node graph))
         (node2 (elk-graph-util.create-node graph)))
    (set-width node1 30) (set-height node1 30)
    (set-property node1 layered-options.node-size-constraints (size-constraint.fixed))
    (set-width node2 30) (set-height node2 30)
    (set-property node2 layered-options.node-size-constraints (size-constraint.fixed))
    (elk-graph-util.create-simple-edge node1 node2)
    graph))

(defun %olt-laid-out-graph ()
  (let ((graph (%olt-create-simple-graph)))
    (with-java-floats (layout (new-layered-layout-provider) graph (new-basic-progress-monitor)))
    graph))

(defplaintest overall-layout-test test-node-coordinates ()
  (let ((graph (%olt-laid-out-graph)))
    (jdo (node (get-children graph))
      (assert-true (> (get-x node) 0))
      (assert-true (> (get-y node) 0)))))

(defplaintest overall-layout-test test-edge-coordinates ()
  (let ((graph (%olt-laid-out-graph)))
    (jdo (edge (get-contained-edges graph))
      (jdo (section (get-sections edge))
        (assert-true (> (get-start-x section) 0))
        (assert-true (> (get-start-y section) 0))
        (assert-true (> (get-end-x section) 0))
        (assert-true (> (get-end-y section) 0))))))

(defplaintest overall-layout-test test-graph-size ()
  (let ((graph (%olt-laid-out-graph)))
    (assert-true (> (get-width graph) 0))
    (assert-true (> (get-height graph) 0))))

(defplaintest overall-layout-test test-edge-orthogonality ()
  ;; This test only makes sense if the orthogonal edge router is used.
  (let ((graph (%olt-laid-out-graph)))
    (jdo (edge (get-contained-edges graph))
      (jdo (section (get-sections edge))
        ;; Assemble list of bend points
        (let ((bend-points (new-jlist)))
          (jl-add bend-points (new-k-vector (get-start-x section) (get-start-y section)))
          (jdo (bp (get-bend-points section))
            (jl-add bend-points (new-k-vector (.x bp) (.y bp))))
          (jl-add bend-points (new-k-vector (get-end-x section) (get-end-y section)))
          (when (>= (size bend-points) 2)
            ;; Check whether the bend points run orthogonally
            (let ((prev (jl-get bend-points 0)))
              (loop for i from 1 below (size bend-points)
                    for curr = (jl-get bend-points i)
                    do (assert-true (or (= (.x prev) (.x curr))
                                        (= (.y prev) (.y curr))))
                       (setf prev curr)))))))))
