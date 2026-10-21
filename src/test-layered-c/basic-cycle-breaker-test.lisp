;;;; basic-cycle-breaker-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p1cycles.BasicCycleBreakerTest.

(in-package #:elk)

(defconstant +bcb-interactive-random-seed+ 0)
(defconstant +bcb-interactive-max-pos+ 100)

(defun %bcb-configurator-for (strategy)
  (let ((config (new-layout-configurator)))
    (set-property (configure config 'elk-node) layered-options.cycle-breaking-strategy strategy)
    config))

(deflayouttest basic-cycle-breaker-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter (model-resource-path "realworld/ptolemy/**/") "elkg")
                          (model-resource-path "tickets/layered/600_outgoingEdgeInLastSeparateNode.elkt")))
  (:configurator-provider depth-first-configurator ()
    (%bcb-configurator-for cycle-breaking-strategy.depth-first))
  (:configurator-provider greedy-configurator ()
    (%bcb-configurator-for cycle-breaking-strategy.greedy))
  (:configurator-provider greedy-model-order-configurator ()
    (%bcb-configurator-for cycle-breaking-strategy.greedy-model-order))
  (:configurator-provider model-order-prefer-edges-configurator ()
    (let ((config (%bcb-configurator-for cycle-breaking-strategy.model-order)))
      (set-property (configure config 'elk-node) layered-options.consider-model-order-strategy
                    ordering-strategy.prefer-edges)
      config))
  (:configurator-provider model-order-nodes-and-edges-configurator ()
    (let ((config (%bcb-configurator-for cycle-breaking-strategy.model-order)))
      (set-property (configure config 'elk-node) layered-options.consider-model-order-strategy
                    ordering-strategy.nodes-and-edges)
      config))
  ;; This is a bit more involved. We not only have to set the proper cycle breaking strategy, but we also
  ;; have to assign bogus coordinates to the nodes
  (:configurator interactive-configurator (elk-graph)
    (let ((node-queue (new-jlist))
          ;; Fixed seed to keep tests repeatable
          (rand (new-random +bcb-interactive-random-seed+)))
      (jl-add node-queue elk-graph)
      (loop until (is-empty node-queue)
            do (let ((node (jl-poll-first node-queue)))
                 (jdo (child (get-children node)) (jl-add node-queue child))
                 (set-property node layered-options.cycle-breaking-strategy
                               cycle-breaking-strategy.interactive)
                 (when (= (get-x node) 0)
                   (set-x node (next-int rand +bcb-interactive-max-pos+)))
                 (when (= (get-y node) 0)
                   (set-y node (next-int rand +bcb-interactive-max-pos+)))))))
  (:after-processor test-is-acyclic
    (:processors (depth-first-cycle-breaker greedy-cycle-breaker interactive-cycle-breaker
                  model-order-cycle-breaker))
    (graph)
    (let ((dfs-unvisited 0)
          (next-dfs-number 1))
      (labels ((on-current-stack-p (node) (< (.id node) 0))
               (put-on-current-stack (node) (setf (.id node) (* -1 (abs (.id node)))))
               (remove-from-current-stack (node) (setf (.id node) (abs (.id node))))
               (dfs (node)
                 (unless (/= (.id node) dfs-unvisited)
                   (setf (.id node) next-dfs-number)
                   (incf next-dfs-number)
                   (put-on-current-stack node)
                   (jdo (l-edge (get-outgoing-edges node))
                     (let ((target (get-node (get-target l-edge))))
                       (cond ((= (.id target) dfs-unvisited)
                              (dfs target))
                             ((on-current-stack-p target)
                              (fail "Cycle detected!")))))
                   (remove-from-current-stack node))))
        ;; Find all nodes that don't have incoming edges
        (let ((sources (new-jlist)))
          (jdo (node (get-layerless-nodes graph))
            (when (is-empty (get-incoming-edges node))
              (jl-add sources node)))
          ;; Reset all node IDs
          (jdo (node (get-layerless-nodes graph))
            (setf (.id node) dfs-unvisited))
          ;; Start a DFS at each source
          (jdo (source sources)
            (when (/= (.id source) dfs-unvisited)
              (fail "Should never happen"))
            (dfs source)))))))
