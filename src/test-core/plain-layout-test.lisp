;;;; plain-layout-test.lisp -- port of org.eclipse.elk.shared.test.PlainLayoutTest.
;;;; GraphTestUtils.createHierarchicalGraph is not in the port (BLOCKERS.md).
;;;; Iterators.filter(eAllContents, ElkNode.class) is a breadth-first walk
;;;; over the node tree, which is what %plt-all-nodes does.

(in-package #:elk)

(defun %plt-all-nodes (parent)
  (let ((nodes (new-jlist)))
    (jdo (child (get-children parent)) (jl-add nodes child))
    (loop for i from 0 while (< i (length nodes))
          do (jdo (child (get-children (aref nodes i))) (jl-add nodes child)))
    nodes))

(defcore-class plain-layout-test
  ("testPlainLayout"
   (let* ((parent (graph-test-utils.create-hierarchical-graph))
          (monitor (new-basic-progress-monitor)))
     (jdo (node (%plt-all-nodes parent))
       (when (plusp (size (get-children node)))
         (set-property node core-options.algorithm layered-options.algorithm-id)))
     ;; service.registerLayoutMetaDataProviders(new LayeredOptions()): the port's
     ;; options are defined at load time (DEFINE-LAYOUT-OPTION), nothing to register.
     (layout (new-recursive-graph-layout-engine) parent monitor)
     (jdo (node (%plt-all-nodes parent))
       (format t "~&~A: x = ~A, y = ~A~%" (get-text (j-get (get-labels node) 0))
               (get-x node) (get-y node))))))
