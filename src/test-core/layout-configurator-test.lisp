;;;; layout-configurator-test.lisp -- port of org.eclipse.elk.core.LayoutConfiguratorTest.

(in-package #:elk)

(defparameter *lc-epsilon* 0.0001d0)

(defstruct lc-graph root n1 n2)

(defun %lc-new-graph ()
  (let* ((root (elk-graph-util.create-graph))
         (n1 (elk-graph-util.create-node root))
         (n2 (elk-graph-util.create-node root)))
    (make-lc-graph :root root :n1 n1 :n2 n2)))

(defcore-class layout-configurator-test
  ("testElementOverridesClass"
   (let ((g (%lc-new-graph)) (lc (new-layout-configurator)))
     (set-property (configure lc 'elk-node) core-options.spacing-node-node 22d0)
     (set-property (configure lc (lc-graph-n1 g)) core-options.spacing-node-node 12d0)
     (set-property (configure lc (lc-graph-n2 g)) core-options.spacing-node-node 32d0)
     (elk-util.apply-visitors (lc-graph-root g) (vector lc))
     (assert-near 22d0 (get-property (lc-graph-root g) core-options.spacing-node-node) *lc-epsilon*)
     (assert-near 12d0 (get-property (lc-graph-n1 g) core-options.spacing-node-node) *lc-epsilon*)
     (assert-near 32d0 (get-property (lc-graph-n2 g) core-options.spacing-node-node) *lc-epsilon*)))
  ("testCombineElementAndClass"
   (let ((g (%lc-new-graph)) (lc (new-layout-configurator)))
     (set-property (configure lc 'elk-node) core-options.spacing-node-node 42d0)
     (set-property (configure lc (lc-graph-n1 g)) core-options.spacing-edge-node 42d0)
     (elk-util.apply-visitors (lc-graph-root g) (vector lc))
     (assert-near 42d0 (get-property (lc-graph-n1 g) core-options.spacing-node-node) *lc-epsilon*)
     (assert-near 42d0 (get-property (lc-graph-n1 g) core-options.spacing-edge-node) *lc-epsilon*)))
  ("testCombineMultipleClass"
   (let ((g (%lc-new-graph)) (lc (new-layout-configurator)))
     (set-property (configure lc 'elk-node) core-options.spacing-node-node 42d0)
     (set-property (configure lc 'elk-graph-element) core-options.spacing-edge-node 42d0)
     (elk-util.apply-visitors (lc-graph-root g) (vector lc))
     (assert-near 42d0 (get-property (lc-graph-n1 g) core-options.spacing-node-node) *lc-epsilon*)
     (assert-near 42d0 (get-property (lc-graph-n1 g) core-options.spacing-edge-node) *lc-epsilon*)))
  ("testOverridingExistingOptions"
   (let ((g (%lc-new-graph)) (lc (new-layout-configurator)))
     (set-property (lc-graph-root g) core-options.spacing-node-node 13d0)
     (set-property (configure lc 'elk-node) core-options.spacing-node-node 42d0)
     (elk-util.apply-visitors (lc-graph-root g) (vector lc))
     (assert-near 42d0 (get-property (lc-graph-root g) core-options.spacing-node-node) *lc-epsilon*)))
  ("testPreventOverridingExistingOptions"
   (let ((g (%lc-new-graph)) (lc (new-layout-configurator)))
     (set-property (lc-graph-root g) core-options.spacing-node-node 13d0)
     (set-property (configure lc 'elk-node) core-options.spacing-node-node 42d0)
     (add-filter lc layout-configurator.no-overwrite)
     (elk-util.apply-visitors (lc-graph-root g) (vector lc))
     (assert-near 13d0 (get-property (lc-graph-root g) core-options.spacing-node-node) *lc-epsilon*))))
