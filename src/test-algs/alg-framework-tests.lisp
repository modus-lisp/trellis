;;;; alg-framework-tests.lisp -- ports of the five LayoutTestRunner classes in
;;;; org.eclipse.elk.alg.test.tests (the test framework's own self-tests):
;;;; ConfiguratorProviderTest, ConfiguratorTest, IgnoredTest,
;;;; PlainJavaInitializationTest and WhiteBoxTest.

(in-package #:elk)

;;; ConfiguratorProviderTest

(defvar %cp-test-property (new-property "org.eclipse.elk.alg.common.test" ""))

(deflayouttest configurator-provider-test
  (:algorithm force-options.algorithm-id)
  (:graph basic-graph () (elk-graph-util.create-graph))
  (:configurator-provider configurator-provider ()
    (let ((layout-config (new-layout-configurator)))
      (set-property (configure layout-config 'elk-node)
                    %cp-test-property "Test")
      layout-config))
  (:test test-configurator-provider () (graph)
    (assert-equals "Property is not set properly." "Test" (get-property graph %cp-test-property))))

;;; ConfiguratorTest

(deflayouttest configurator-test
  (:algorithm force-options.algorithm-id)
  (:graph basic-graph () (elk-graph-util.create-graph))
  (:configurator configure-stuff (graph)
    (set-property graph %cp-test-property "Test"))
  (:test test-configurator () (graph)
    (assert-equals "Property is not set properly." "Test" (get-property graph %cp-test-property))))

;;; IgnoredTest

(deflayouttest ignored-test
  (:algorithm force-options.algorithm-id)
  (:graph first-graph () (elk-graph-util.create-graph))
  (:graph second-graph () (elk-graph-util.create-graph))
  (:test success () (graph)
    (assert-true t))
  (:test fail (:ignore t) () (graph)
    (assert-true nil)))

;;; PlainJavaInitializationTest

(deflayouttest plain-java-initialization-test
  (:algorithm box-layouter-options.algorithm-id)
  (:algorithm dis-co-options.algorithm-id)
  (:algorithm fixed-layouter-options.algorithm-id)
  (:algorithm force-options.algorithm-id)
  (:algorithm layered-options.algorithm-id)
  (:algorithm mr-tree-options.algorithm-id)
  (:algorithm radial-options.algorithm-id)
  (:algorithm random-layouter-options.algorithm-id)
  (:algorithm rect-packing-options.algorithm-id)
  (:algorithm spore-compaction-options.algorithm-id)
  (:algorithm spore-overlap-removal-options.algorithm-id)
  (:algorithm stress-options.algorithm-id)
  (:graph basic-graph () (elk-graph-util.create-graph))
  (:test test-algorithm-present () (graph)
    (assert-true (has-property graph core-options.algorithm))))

;;; WhiteBoxTest

(deflayouttest white-box-test
  (:algorithm layered-options.algorithm-id)
  (:graph compound-graph ()
    ;; A graph with a compound node in it.
    (let* ((top-level (elk-graph-util.create-graph))
           (compound-child (elk-graph-util.create-node top-level)))
      (elk-graph-util.create-node compound-child)
      top-level))
  (:configurator configure (graph)
    (set-property graph core-options.hierarchy-handling hierarchy-handling.include-children)
    (set-property graph layered-options.layering-strategy layering-strategy.network-simplex))
  (:after-processor test-no-layerless-nodes
    (:processors (network-simplex-layerer) :fail-if-not-executed t) (graph)
    (assert-true "There are layerless nodes left!" (is-empty (get-layerless-nodes graph))))
  (:after-processor test-proper-layering
    (:processors (network-simplex-layerer) :fail-if-not-executed t) (graph)
    (jdo (layer graph)
      (let ((source-layer-index (get-index layer)))
        (jdo (lnode layer)
          (jdo (ledge (get-outgoing-edges lnode))
            (let ((target-layer-index (get-index (get-layer (get-node (get-target ledge))))))
              (assert-true "Edge points leftwards!" (<= source-layer-index target-layer-index)))))))))
