;;;; issue583-test.lisp -- port of org.eclipse.elk.alg.rectpacking.issues.Issue583Test.

(in-package #:elk)

(defalgtest issue583-test test-decrease-approximated-width
  (let* ((parent (elk-graph-util.create-graph)) (n1 (elk-graph-util.create-node parent)) (n2 (elk-graph-util.create-node parent)) (n3 (elk-graph-util.create-node parent)) (n4 (elk-graph-util.create-node parent)))
    (set-property parent core-options.algorithm rect-packing-options.algorithm-id)
    (set-property parent core-options.spacing-node-node 0.0)
    (set-property parent core-options.padding (new-elk-padding 0.0))
    (set-dimensions n1 30 30)
    (set-dimensions n2 10 10)
    (set-dimensions n3 30 30)
    (set-dimensions n4 40 10)
    (set-property parent rect-packing-options.packing-strategy packing-strategy.none)
    (layout (new-rect-packing-layout-provider) parent (new-basic-progress-monitor))
    (assert-near 60.0 (get-width parent) 1)
    (set-property parent rect-packing-options.packing-strategy packing-strategy.compaction)
    (layout (new-rect-packing-layout-provider) parent (new-basic-progress-monitor))
    (assert-near 40.0 (get-width parent) 1)))

