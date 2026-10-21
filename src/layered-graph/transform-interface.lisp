;;;; transform-interface.lisp -- org.eclipse.elk.alg.layered.graph.transform.IGraphTransformer

(in-package #:elk)

;; IGraphTransformer<T>: importGraph(T) -> LGraph, applyLayout(LGraph).
(defjgeneric import-graph 1)
(defjgeneric apply-layout 1)
