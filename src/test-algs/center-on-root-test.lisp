;;;; center-on-root-test.lisp -- port of org.eclipse.elk.alg.radial.test.CenterOnRootTest.

(in-package #:elk)

(defun %cor-check (parent root)
  (let* ((margins (get-property root core-options.margins))
         (h-expected (/ (get-width parent) 2))
         (v-expected (/ (get-height parent) 2)))
    (assert-near h-expected (+ (get-x root) (.left margins) (/ (get-width root) 2)) 0.1
                 "Horizontal centering")
    (assert-near v-expected (+ (get-y root) (.top margins) (/ (get-height root) 2)) 0.1
                 "Vertical centering")))

(defun %cor-run (parent)
  (set-property parent core-options.algorithm radial-options.algorithm-id)
  (set-property parent radial-options.center-on-root t)
  (layout (make-instance (quote radial-layout-provider)) parent (new-basic-progress-monitor)))

(defalgtest center-on-root-test test-simple-centering
  (let* ((parent (elk-graph-util.create-graph))
         (root (elk-graph-util.create-node parent))
         (n1 (elk-graph-util.create-node parent))
         (n2 (elk-graph-util.create-node parent))
         (n3 (elk-graph-util.create-node parent)))
    (elk-graph-util.create-simple-edge root n1)
    (elk-graph-util.create-simple-edge root n2)
    (elk-graph-util.create-simple-edge root n3)
    (%cor-run parent)
    (%cor-check parent root)))

(defalgtest center-on-root-test test-larger-graph-centering
  (let* ((parent (elk-graph-util.create-graph))
         (root (elk-graph-util.create-node parent))
         (n1 (elk-graph-util.create-node parent))
         (n2 (elk-graph-util.create-node parent))
         (n3 (elk-graph-util.create-node parent))
         (n11 (elk-graph-util.create-node parent))
         (n12 (elk-graph-util.create-node parent))
         (n13 (elk-graph-util.create-node parent)))
    (elk-graph-util.create-simple-edge root n1)
    (elk-graph-util.create-simple-edge root n2)
    (elk-graph-util.create-simple-edge root n3)
    (elk-graph-util.create-simple-edge n1 n11)
    (elk-graph-util.create-simple-edge n1 n12)
    (elk-graph-util.create-simple-edge n1 n13)
    (%cor-run parent)
    (%cor-check parent root)))
