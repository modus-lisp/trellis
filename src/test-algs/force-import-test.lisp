;;;; force-import-test.lisp -- port of org.eclipse.elk.alg.force.test.ForceImportTest.

(in-package #:elk)

(defun %fit-create-elk-graph ()
  (let* ((elk-graph (elk-graph-util.create-graph))
         (n1 (elk-graph-util.create-node elk-graph))
         (n2 (elk-graph-util.create-node elk-graph))
         (n3 (elk-graph-util.create-node elk-graph))
         (edge (elk-graph-util.create-simple-edge n1 n2)))
    (declare (ignorable n3))
    (elk-graph-util.create-label "test" edge)
    (let ((edge2 (elk-graph-util.create-simple-edge n1 n3)))
      (elk-graph-util.create-label "test2" edge2))
    elk-graph))

(defun %fit-create-simple-graph ()
  (let ((importer (new-force-elk-graph-importer)))
    (import-graph importer (%fit-create-elk-graph))))

(defun %fit-create-two-components-graph ()
  (let* ((g1 (%fit-create-elk-graph))
         (g2 (%fit-create-elk-graph))
         (graph (elk-graph-util.create-graph)))
    (jdo (c (get-children g1)) (add (get-children graph) c))
    (jdo (c (get-children g2)) (add (get-children graph) c))
    (import-graph (new-force-elk-graph-importer) graph)))

(defun %fit-check-simple-graph (f)
  (assert-equals 3 (size (get-nodes f)))
  (assert-equals 2 (size (get-edges f)))
  (assert-equals 2 (size (get-labels f))))

(defalgtest force-import-test test-import
  (%fit-check-simple-graph (%fit-create-simple-graph)))

(defalgtest force-import-test test-separate-connected-components
  (let* ((cp (new-force-components-processor))
         (graph (%fit-create-two-components-graph)))
    (set-property graph force-options.separate-connected-components t)
    (let ((graphs (split cp graph)))
      (assert-equals 2 (size graphs))
      (jdo (g graphs) (%fit-check-simple-graph g)))))

(defalgtest force-import-test test-do-not-separate-connected-components
  (let* ((cp (new-force-components-processor))
         (graph (%fit-create-two-components-graph)))
    (set-property graph force-options.separate-connected-components nil)
    (let ((graphs (split cp graph)))
      (assert-equals 1 (size graphs))
      (let ((f (j-get graphs 0)))
        (assert-equals 6 (size (get-nodes f)))
        (assert-equals 4 (size (get-edges f)))
        (assert-equals 4 (size (get-labels f)))))))
