;;;; elk-graph-transformer.lisp -- org.eclipse.elk.alg.layered.graph.transform.ElkGraphTransformer

(in-package #:elk)

(defjmethod import-graph ((this layered-elk-graph-transformer) graph)
  (import-graph (new-layered-elk-graph-importer) graph))

(defjmethod apply-layout ((this layered-elk-graph-transformer) layered-graph)
  (apply-layout (new-elk-graph-layout-transferrer) layered-graph)
  nil)

;;; ------------------------------------------------------------ utility

(defjstatic layered-elk-graph-transformer.get-origin-identifier (element)
  (let ((origin (get-property element layered-internal-properties.origin)))
    (when (typep origin 'elk-graph-element)
      (layered-elk-graph-transformer.get-identifier origin))))

(defjstatic layered-elk-graph-transformer.get-identifier (element)
  (let ((id (get-identifier element)))
    (when (not (%lg-null-or-empty-p id))
      (let* ((container (e-container element))
             (parent-id (and (typep container 'elk-graph-element)
                             (layered-elk-graph-transformer.get-identifier container))))
        (if parent-id (jstr parent-id "." id) id)))))
