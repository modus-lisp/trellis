;;;; basic-layer-assignment-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p2layers.BasicLayerAssignmentTest.

(in-package #:elk)

(defconstant +bla-interactive-random-seed+ 0)
(defconstant +bla-interactive-max-pos+ 10000)

(defun %bla-configurator-for (strategy)
  (let ((config (new-layout-configurator)))
    (set-property (configure config 'elk-node) layered-options.layering-strategy strategy)
    config))

(deflayouttest basic-layer-assignment-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter (model-resource-path "realworld/ptolemy/**/") "elkg")))
  (:configurator-provider coffman-graham-configurator ()
    (%bla-configurator-for layering-strategy.coffman-graham))
  (:configurator-provider longest-path-configurator ()
    (%bla-configurator-for layering-strategy.longest-path))
  (:configurator-provider longest-path-source-configurator ()
    (%bla-configurator-for layering-strategy.longest-path-source))
  (:configurator-provider min-width-configurator ()
    (%bla-configurator-for layering-strategy.min-width))
  (:configurator-provider network-simplex-configurator ()
    (%bla-configurator-for layering-strategy.network-simplex))
  (:configurator-provider stretch-width-configurator ()
    (%bla-configurator-for layering-strategy.stretch-width))
  ;; This is a bit more involved. We not only have to set the proper layering strategy, but we also
  ;; have to assign bogus coordinates to the nodes
  (:configurator interactive-configurator (elk-graph)
    (let ((node-queue (new-jlist))
          (rand (new-random +bla-interactive-random-seed+)))
      (jl-add node-queue elk-graph)
      (loop until (is-empty node-queue)
            do (let ((node (jl-poll-first node-queue)))
                 (jdo (child (get-children node)) (jl-add node-queue child))
                 (set-property node layered-options.layering-strategy layering-strategy.interactive)
                 (when (= (get-x node) 0)
                   (set-x node (next-int rand +bla-interactive-max-pos+)))
                 (when (= (get-y node) 0)
                   (set-y node (next-int rand +bla-interactive-max-pos+)))))))
  (:after-processor test-no-layerless-nodes
    (:processors (coffman-graham-layerer interactive-layerer longest-path-layerer
                  min-width-layerer network-simplex-layerer stretch-width-layerer))
    (graph)
    (assert-true (is-empty (get-layerless-nodes graph))))
  (:after-processor test-no-empty-layers
    (:processors (coffman-graham-layerer interactive-layerer longest-path-layerer
                  min-width-layerer network-simplex-layerer stretch-width-layerer))
    (graph)
    (jdo (layer graph)
      (assert-false (is-empty (get-nodes layer)))))
  (:after-processor test-edges-point-towards-next-layers
    (:processors (coffman-graham-layerer interactive-layerer longest-path-layerer
                  min-width-layerer network-simplex-layerer stretch-width-layerer))
    (graph)
    ;; Assign increasing IDs to the layers
    (let ((next-layer-id 0))
      (jdo (layer graph)
        (setf (.id layer) next-layer-id)
        (incf next-layer-id)))
    (jdo (layer graph)
      (jdo (node layer)
        (let ((node-layer-id (.id (get-layer node))))
          (jdo (edge (get-outgoing-edges node))
            (assert-true (< node-layer-id (.id (get-layer (get-node (get-target edge)))))))))))
  )
