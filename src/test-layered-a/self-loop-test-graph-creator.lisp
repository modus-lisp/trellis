;;;; self-loop-test-graph-creator.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.loops.SelfLoopTestGraphCreator (a helper,
;;;; not a test class).  Static methods keep their Java names under the class name.

(in-package #:elk)

(defjstatic self-loop-test-graph-creator.basic-graph-without-self-loops ()
  (let* ((l-graph (new-l-graph))
         (n1 (self-loop-test-graph-creator.node l-graph)))
    (self-loop-test-graph-creator.ports n1 0 1 0 0)
    (let ((n2 (self-loop-test-graph-creator.node l-graph)))
      (self-loop-test-graph-creator.ports n2 0 0 0 1)
      (self-loop-test-graph-creator.edge (j-get (get-ports n1) 0) (j-get (get-ports n2) 0)))
    l-graph))

(defjstatic self-loop-test-graph-creator.basic-graph-with-self-loops ()
  (let* ((l-graph (self-loop-test-graph-creator.basic-graph-without-self-loops))
         (l-node (j-get (get-layerless-nodes l-graph) 1)))
    (self-loop-test-graph-creator.ports l-node 2 2 2 1)
    (loop for i from 1 below (length (get-ports l-node)) by 2
          do (self-loop-test-graph-creator.edge (j-get (get-ports l-node) (- i 1))
                                                (j-get (get-ports l-node) i)))
    l-graph))

(defjstatic self-loop-test-graph-creator.node (l-graph)
  (let ((l-node (new-l-node l-graph)))
    (jl-add (get-layerless-nodes l-graph) l-node)
    l-node))

(defjstatic self-loop-test-graph-creator.edge (source target)
  (let ((l-edge (new-l-edge)))
    (set-source l-edge source)
    (set-target l-edge target)
    l-edge))

(defjstatic self-loop-test-graph-creator.ports (l-node north east south west)
  (flet ((add-ports (n side)
           (dotimes (i n)
             (let ((l-port (new-l-port)))
               (set-node l-port l-node)
               (set-side l-port side)))))
    (add-ports north port-side.north)
    (add-ports east port-side.east)
    (add-ports south port-side.south)
    (add-ports west port-side.west)))
