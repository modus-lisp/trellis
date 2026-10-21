;;;; switch-decider-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.greedyswitch.SwitchDeciderTest.
;;;; A @RunWith(Parameterized) class that extends the greedy-switch TestGraphCreator,
;;;; so the test class is a subclass of layered-greedyswitch-test-graph-creator and
;;;; each test function creates its own instance (THIS).  Every test runs once per
;;;; parameter row: (switch-decider-test.run-all) loops over the rows.
;;;; Hamcrest assertThat(x, is(v)) is assert-equals with the expected value.

(in-package #:elk)

(defjclass switch-decider-test (layered-greedyswitch-test-graph-creator)
  ((greedy-type nil)
   (decider nil)
   (current-node-order nil)
   (free-layer-index 0)))

(defjctor switch-decider-test (gt)
  (setf (.greedy-type this) gt))

(defparameter *switch-decider-test-greedy-types*
  (list cross-min-type.one-sided-greedy-switch
        cross-min-type.two-sided-greedy-switch))

(defun %sd-new (greedy-type)
  "Each test method's fresh instance (JUnit creates one per test)."
  (initialize-plain-java-layout)
  (new-switch-decider-test greedy-type))

(defun %sd-assert-is (message expected actual)
  "assertThat(message, actual, is(expected)) for a boolean."
  (if message
      (assert-equals/2 message expected actual)
      (assert-equals/1 expected actual)))

(defun %sd-given-decider-for-free-layer (this layer-index direction)
  (setf (.free-layer-index this) layer-index)
  (setf (.current-node-order this) (to-node-array (.graph this)))
  (let ((crossing-matrix-filler
          (new-crossing-matrix-filler (.greedy-type this) (.current-node-order this)
                                      layer-index direction)))
    (new-switch-decider layer-index (.current-node-order this)
                        crossing-matrix-filler
                        (make-array (%sd-n-ports (.current-node-order this)) :initial-element 0)
                        (new-graph-info-holder (.graph this) cross-min-type.one-sided-greedy-switch
                                               (new-jlist))
                        (eq (.greedy-type this) cross-min-type.one-sided-greedy-switch))))

(defun %sd-n-ports (current-order)
  (let ((n-ports 0))
    (loop for layer across current-order
          do (loop for n across layer
                   do (incf n-ports (size (get-ports n)))))
    n-ports))

(defun %sd-switch-nodes (this upper-index lower-index)
  (let* ((row (aref (.current-node-order this) (.free-layer-index this)))
         (upper-node (aref row upper-index)))
    (setf (aref row upper-index) (aref row lower-index))
    (setf (aref row lower-index) upper-node)))

(defun %sd-is-one-sided (this)
  (eq (.greedy-type this) cross-min-type.one-sided-greedy-switch))

(defun switch-decider-test.cross-formed (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-cross-formed-graph this))
    (set-up-ids this)
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.east))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))))

(defun switch-decider-test.one-node (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-one-node-graph this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.west))
    (%sd-assert-is nil nil (does-switch-reduce-crossings (.decider this) 0 0))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.east))
    (%sd-assert-is nil nil (does-switch-reduce-crossings (.decider this) 0 0))))

(defun switch-decider-test.in-layer-switchable (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-in-layer-edges-graph this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.east))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))))

(defun switch-decider-test.multiple-edges-between-same-nodes (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-multiple-edges-between-same-nodes-graph this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.east))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))))

(defun switch-decider-test.self-loops (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-cross-with-many-self-loops-graph this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.east))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))))

(defun switch-decider-test.north-south-port-crossing (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-three-layer-north-south-crossing-graph
                         (new-north-south-edge-test-graph-creator)))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (if (%sd-is-one-sided this)
        (assert-true (does-switch-reduce-crossings (.decider this) 1 2))
        (assert-false (does-switch-reduce-crossings (.decider this) 1 2)))))

(defun switch-decider-test.more-complex (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-more-complex-three-layer-graph this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-false (does-switch-reduce-crossings (.decider this) 0 1))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 2 crossing-count-side.west))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
    (assert-false (does-switch-reduce-crossings (.decider this) 1 2))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.east))
    (assert-false (does-switch-reduce-crossings (.decider this) 0 1))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.east))
    (assert-false (does-switch-reduce-crossings (.decider this) 0 1))
    (assert-true (does-switch-reduce-crossings (.decider this) 1 2))))

(defun switch-decider-test.switch-only-true-for-one-sided (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-switch-only-one-sided this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (if (%sd-is-one-sided this)
        (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
        (assert-false (does-switch-reduce-crossings (.decider this) 0 1)))))

(defun switch-decider-test.switch-only-true-for-one-sided-eastern-side (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-switch-only-east-one-sided this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.east))
    (if (%sd-is-one-sided this)
        (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
        (assert-false (does-switch-reduce-crossings (.decider this) 0 1)))))

(defun switch-decider-test.constraints-prevent-switch (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-cross-formed-graph-with-constraints-in-second-layer this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-false (does-switch-reduce-crossings (.decider this) 0 1))))

(defun switch-decider-test.in-layer-unit-constraints-prevent-switch (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-graph-where-layout-unit-prevents-switch
                         (new-north-south-edge-test-graph-creator)))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.west))
    (assert-false (does-switch-reduce-crossings (.decider this) 1 2))))

(defun switch-decider-test.switch-and-recount (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-cross-formed-graph this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.east))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
    (%sd-switch-nodes this 0 1)
    (notify-of-switch (.decider this)
                      (jl-get (copy-of-nodes-in-layer this 0) 0)
                      (jl-get (copy-of-nodes-in-layer this 0) 1))
    (assert-false (does-switch-reduce-crossings (.decider this) 0 1))))

(defun switch-decider-test.switch-and-recount-counter-bug (greedy-type)
  (let* ((this (%sd-new greedy-type))
         (left-nodes (add-nodes-to-layer this 2 (make-layer this (get-graph this))))
         (right-nodes (add-nodes-to-layer this 4 (make-layer this (get-graph this))))
         (left-top-port (add-port-on-side this (aref left-nodes 0) port-side.east))
         (left-lower-port (add-port-on-side this (aref left-nodes 1) port-side.east))
         (right-top-port (add-port-on-side this (aref right-nodes 0) port-side.west)))
    (add-edge-between-ports this left-lower-port right-top-port)
    (east-west-edge-from-to this left-lower-port (aref right-nodes 2))
    (add-edge-between-ports this left-top-port right-top-port)
    (east-west-edge-from-to this left-top-port (aref right-nodes 1))
    (east-west-edge-from-to this left-top-port (aref right-nodes 3))
    (set-up-ids this)
    (setf (.graph this) (get-graph this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (let ((order (.current-node-order this))
          (free (.free-layer-index this)))
      (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
      (assert-false (does-switch-reduce-crossings (.decider this) 1 2))
      (assert-true (does-switch-reduce-crossings (.decider this) 2 3))
      (notify-of-switch (.decider this) (aref (aref order free) 0) (aref (aref order free) 1))
      (%sd-switch-nodes this 0 1)
      (assert-false (does-switch-reduce-crossings (.decider this) 0 1))
      (assert-false (does-switch-reduce-crossings (.decider this) 1 2))
      (assert-true (does-switch-reduce-crossings (.decider this) 2 3))
      (notify-of-switch (.decider this) (aref (aref order free) 2) (aref (aref order free) 3))
      (%sd-switch-nodes this 2 3)
      (assert-false (does-switch-reduce-crossings (.decider this) 0 1))
      (assert-true (does-switch-reduce-crossings (.decider this) 1 2))
      (assert-false (does-switch-reduce-crossings (.decider this) 2 3))
      (notify-of-switch (.decider this) (aref (aref order free) 1) (aref (aref order free) 2))
      (%sd-switch-nodes this 1 2)
      (assert-false (does-switch-reduce-crossings (.decider this) 0 1))
      (assert-false (does-switch-reduce-crossings (.decider this) 1 2))
      (assert-false (does-switch-reduce-crossings (.decider this) 2 3)))))

(defun switch-decider-test.switch-and-recount-reduced-counter-bug (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-switched-problem-graph this))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (dotimes (i (- (size (copy-of-nodes-in-layer this 1)) 1))
      (%sd-assert-is (format nil "attempted switch ~A with ~A" i (+ i 1)) nil
                     (does-switch-reduce-crossings (.decider this) i (+ i 1))))))

(defun switch-decider-test.should-switch-with-long-edge-dummies (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-northern-north-south-dummy-edge-crossing-graph
                         (new-north-south-edge-test-graph-creator)))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-true (does-switch-reduce-crossings (.decider this) 1 2))
    (when (%sd-is-one-sided this)
      (assert-true (does-switch-reduce-crossings (.decider this) 0 1)))
    (setf (.graph this) (get-southern-north-south-dummy-edge-crossing-graph
                         (new-north-south-edge-test-graph-creator)))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.west))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))
    (when (%sd-is-one-sided this)
      (assert-true (does-switch-reduce-crossings (.decider this) 1 2)))))

(defun switch-decider-test.layout-unit-constraint-prevents-switch-with-node-with-northern-ports (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-graph-layout-unit-prevents-switch-with-node-with-node-with-northern-edges
                         (new-north-south-edge-test-graph-creator)))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.east))
    (assert-false (does-switch-reduce-crossings (.decider this) 1 2))))

(defun switch-decider-test.layout-unit-constraint-prevents-switch-with-node-with-southern-ports (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-graph-layout-unit-prevents-switch-with-node-with-node-with-southern-edges
                         (new-north-south-edge-test-graph-creator)))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 0 crossing-count-side.east))
    (assert-false (does-switch-reduce-crossings (.decider this) 0 1))))

(defun switch-decider-test.layout-unit-constraint-does-not-prevent-switch-with-when-other-node-is-long-edge-dummy (greedy-type)
  (let ((this (%sd-new greedy-type)))
    (setf (.graph this) (get-graph-layout-unit-does-not-prevent-switch-with-long-edge-dummy
                         (new-north-south-edge-test-graph-creator)))
    (setf (.decider this) (%sd-given-decider-for-free-layer this 1 crossing-count-side.east))
    (assert-true (does-switch-reduce-crossings (.decider this) 0 1))))

(defun switch-decider-test.switching-dummy-nodes-notifies-port-switch (greedy-type)
  (let* ((this (%sd-new greedy-type))
         (left-node (add-node-to-layer this (make-layer this)))
         (right-nodes (add-nodes-to-layer this 2 (make-layer this)))
         (left-ports (add-ports-on-side this 2 left-node port-side.east))
         (nested-graph (nested-graph this left-node))
         (nested-layer (make-layer this nested-graph))
         (dummies (add-external-port-dummies-to-layer this nested-layer left-ports)))
    (east-west-edge-from-to this (aref left-ports 0) (aref right-nodes 1))
    (east-west-edge-from-to this (aref left-ports 1) (aref right-nodes 0))
    (let* ((crossing-matrix-filler
             (new-crossing-matrix-filler (.greedy-type this) (to-node-array nested-graph)
                                         0 crossing-count-side.east))
           (parent-graph-data (new-graph-info-holder (.graph this) (.greedy-type this) (new-jlist)))
           (graph-data (new-graph-info-holder nested-graph (.greedy-type this)
                                              (jlist parent-graph-data)))
           (switch-decider (new-switch-decider 0 (to-node-array nested-graph)
                                               crossing-matrix-filler
                                               (make-array (%sd-n-ports (to-node-array nested-graph))
                                                           :initial-element 0)
                                               graph-data
                                               nil)))
      (when (eq (.greedy-type this) cross-min-type.two-sided-greedy-switch)
        (assert-true (does-switch-reduce-crossings switch-decider 0 1))
        (notify-of-switch switch-decider (aref dummies 0) (aref dummies 1))
        (assert-false (does-switch-reduce-crossings switch-decider 0 1))))))

(defun switch-decider-test.run-all ()
  "Every test method, once per parameter row (Parameterized)."
  (dolist (gt *switch-decider-test-greedy-types*)
    (switch-decider-test.cross-formed gt)
    (switch-decider-test.one-node gt)
    (switch-decider-test.in-layer-switchable gt)
    (switch-decider-test.multiple-edges-between-same-nodes gt)
    (switch-decider-test.self-loops gt)
    (switch-decider-test.north-south-port-crossing gt)
    (switch-decider-test.more-complex gt)
    (switch-decider-test.switch-only-true-for-one-sided gt)
    (switch-decider-test.switch-only-true-for-one-sided-eastern-side gt)
    (switch-decider-test.constraints-prevent-switch gt)
    (switch-decider-test.in-layer-unit-constraints-prevent-switch gt)
    (switch-decider-test.switch-and-recount gt)
    (switch-decider-test.switch-and-recount-counter-bug gt)
    (switch-decider-test.switch-and-recount-reduced-counter-bug gt)
    (switch-decider-test.should-switch-with-long-edge-dummies gt)
    (switch-decider-test.layout-unit-constraint-prevents-switch-with-node-with-northern-ports gt)
    (switch-decider-test.layout-unit-constraint-prevents-switch-with-node-with-southern-ports gt)
    (switch-decider-test.layout-unit-constraint-does-not-prevent-switch-with-when-other-node-is-long-edge-dummy gt)
    (switch-decider-test.switching-dummy-nodes-notifies-port-switch gt)))
