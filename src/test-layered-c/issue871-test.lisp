;;;; issue871-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue871Test.
;;;; Plain JUnit: each test builds its own graph and calls LayeredLayoutProvider
;;;; directly, so the tests are DEFPLAINTEST (run with RUN-PLAIN-JUNIT).

(in-package #:elk)

(defun %issue871-feedback-parent (feedback-edges)
  "The options shared by the three graphs of this class.  FEEDBACK-EDGES is the
FEEDBACK_EDGES option, which only the first two tests set."
  (let ((parent (elk-graph-util.create-graph)))
    (set-property parent core-options.algorithm layered-options.algorithm-id)
    (set-property parent core-options.direction core-direction.right)
    (set-property parent layered-options.cycle-breaking-strategy cycle-breaking-strategy.model-order)
    (set-property parent layered-options.consider-model-order-strategy ordering-strategy.prefer-edges)
    (set-property parent layered-options.crossing-minimization-strategy crossing-minimization-strategy.none)
    (set-property parent layered-options.crossing-minimization-greedy-switch-type greedy-switch-type.off)
    (set-property parent core-options.padding (new-elk-padding 0.0d0))
    (set-property parent core-options.spacing-node-node 10.0d0)
    (set-property parent layered-options.spacing-node-node-between-layers 20.0d0)
    (set-property parent layered-options.feedback-edges feedback-edges)
    parent))

(defun %issue871-layout (parent)
  (with-java-floats (layout (new-layered-layout-provider) parent (new-basic-progress-monitor))))

(defplaintest issue871-test test-feedback-edge-below ()
  ;; Desired layout
  ;; n1 -> n2 -> n4 -
  ;;    -> n3       |
  ;;   |____________|
  (let* ((parent (%issue871-feedback-parent t))
         (n1 (elk-graph-util.create-node parent))
         (n2 (elk-graph-util.create-node parent))
         (n3 (elk-graph-util.create-node parent))
         (n4 (elk-graph-util.create-node parent)))
    (set-dimensions n1 30 30) (set-dimensions n2 30 30)
    (set-dimensions n3 30 30) (set-dimensions n4 30 30)
    (elk-graph-util.create-simple-edge n1 n2)
    (elk-graph-util.create-simple-edge n1 n3)
    (elk-graph-util.create-simple-edge n2 n4)
    (elk-graph-util.create-simple-edge n4 n3) ; Feedback edge
    (elk-graph-util.create-label "n1" n1)
    (elk-graph-util.create-label "n2" n2)
    (elk-graph-util.create-label "n3" n3)
    (elk-graph-util.create-label "n4" n4)
    (%issue871-layout parent)
    ;; Check whether n4 aligns with n2.
    (tlc-assert-equals-delta "n4 and n2 should align" (get-y n2) (get-y n4) 0.1d0)))

(defplaintest issue871-test test-feedback-edge-basic ()
  ;; Desired layout
  ;; n1 -> n2 -> n3-
  ;;   |____________|
  (let* ((parent (%issue871-feedback-parent t))
         (n1 (elk-graph-util.create-node parent))
         (n2 (elk-graph-util.create-node parent))
         (n3 (elk-graph-util.create-node parent)))
    (set-dimensions n1 30 30) (set-dimensions n2 30 30) (set-dimensions n3 30 30)
    (elk-graph-util.create-simple-edge n1 n2)
    (elk-graph-util.create-simple-edge n2 n3)
    (elk-graph-util.create-simple-edge n3 n2) ; Feedback edge
    (elk-graph-util.create-label "n1" n1)
    (elk-graph-util.create-label "n2" n2)
    (elk-graph-util.create-label "n3" n3)
    (%issue871-layout parent)
    ;; Check whether n3 aligns with n2.
    (tlc-assert-equals-delta "n3 and n2 should align" (get-y n2) (get-y n3) 0.1d0)))

(defplaintest issue871-test test-no-feedback-edges-still-working ()
  ;; Desired layout
  ;; n1 -1-> n2 ------>
  ;;           <- n3 -> n4
  ;;   --2------------>
  (let* ((parent (%issue871-feedback-parent nil))
         (n1 (elk-graph-util.create-node parent))
         (n2 (elk-graph-util.create-node parent))
         (n3 (elk-graph-util.create-node parent))
         (n4 (elk-graph-util.create-node parent)))
    (set-dimensions n1 30 30) (set-dimensions n2 30 30)
    (set-dimensions n3 30 30) (set-dimensions n4 30 30)
    (let ((e1 (elk-graph-util.create-simple-edge n1 n2)))
      (elk-graph-util.create-label "1" e1))
    (let ((e2 (elk-graph-util.create-simple-edge n1 n4)))
      (elk-graph-util.create-label "2" e2))
    (elk-graph-util.create-simple-edge n2 n4)
    (elk-graph-util.create-simple-edge n3 n2)
    (elk-graph-util.create-simple-edge n3 n4)
    (elk-graph-util.create-label "n1" n1)
    (elk-graph-util.create-label "n2" n2)
    (elk-graph-util.create-label "n3" n3)
    (elk-graph-util.create-label "n4" n4)
    (%issue871-layout parent)
    ;; Check whether nodes are on the expected positions.
    (tlc-assert-equals-delta "n1 X coordinate is wrong" 0 (get-x n1) 0.1d0)
    (tlc-assert-equals-delta "n1 Y coordinate is wrong" 31 (get-y n1) 0.1d0)
    (tlc-assert-equals-delta "n2 X coordinate is wrong" 70.0d0 (get-x n2) 0.1d0)
    (tlc-assert-equals-delta "n2 Y coordinate is wrong" 6 (get-y n2) 0.1d0)
    (tlc-assert-equals-delta "n3 X coordinate is wrong" 120 (get-x n3) 0.1d0)
    (tlc-assert-equals-delta "n3 Y coordinate is wrong" 11 (get-y n3) 0.1d0)
    (tlc-assert-equals-delta "n4 X coordinate is wrong" 170 (get-x n4) 0.1d0)
    (tlc-assert-equals-delta "n4 Y coordinate is wrong" 11 (get-y n4) 0.1d0)))
