;;;; issue316-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue316Test.

(in-package #:elk)

(deflayouttest issue316-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/316_wrongGraphSizeWithIncludeChildren.elkt")
                          (model-resource-path "tickets/layered/316_wrongGraphSizeWithIncludeChildren2.elkt")))
  (:test ensure-ports-dont-overlap () (graph)
    (let ((overlaps (some (lambda (node) (graph-test-utils.have-overlaps (get-ports node)))
                          (graph-test-utils.all-nodes graph nil))))
      (when overlaps
        (fail "Port overlaps detected!"))))
  (:test ensure-nodes-big-enough-for-labels () (graph)
    (jdo (node (graph-test-utils.all-nodes graph nil))
      ;; Only hierarchical nodes matter.
      (unless (zerop (length (get-children node)))
        (jdo (label (get-labels node))
          (when (or (< (get-x label) 0)
                    (< (get-y label) 0)
                    (> (+ (get-x label) (get-width label)) (get-width node))
                    (> (+ (get-y label) (get-height label)) (get-height node)))
            (fail "Node label not fully contained in node.")))))))
