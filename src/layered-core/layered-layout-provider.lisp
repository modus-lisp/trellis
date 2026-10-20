;;;; layered-layout-provider.lisp -- org.eclipse.elk.alg.layered.LayeredLayoutProvider

(in-package #:elk)

(defjmethod layout ((this layered-layout-provider) elkgraph progress-monitor)
  ;; import the graph (the transformer always returns an LGraph)
  (let* ((graph-transformer (new-layered-elk-graph-transformer))
         (layered-graph (import-graph graph-transformer elkgraph)))
    (if (eq (get-property elkgraph layered-options.hierarchy-handling)
            hierarchy-handling.include-children)
        ;; layout for all hierarchy levels
        (do-compound-layout (.elk-layered this) layered-graph progress-monitor)
        ;; only the top-level graph
        (do-layout (.elk-layered this) layered-graph progress-monitor))
    (unless (is-canceled progress-monitor)
      ;; apply the layout results to the original graph
      (apply-layout graph-transformer layered-graph))))

(defjmethod start-layout-test ((this layered-layout-provider) elkgraph)
  (let* ((graph-importer (new-layered-elk-graph-transformer))
         (layered-graph (import-graph graph-importer elkgraph)))
    (prepare-layout-test (.elk-layered this) layered-graph)))

(defjmethod get-layout-algorithm ((this layered-layout-provider))
  (.elk-layered this))

(defjmethod set-test-controller ((this layered-layout-provider) controller)
  (set-test-controller (.elk-layered this) controller))
