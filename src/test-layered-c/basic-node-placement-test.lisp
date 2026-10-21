;;;; basic-node-placement-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.p4nodes.BasicNodePlacementTest (extends
;;;; TestGraphCreator in Java; none of its members are used by the tests here).

(in-package #:elk)

(defun %bnp-configurator-for (strategy &optional straightening alignment)
  (let ((config (new-layout-configurator)))
    (let ((target (configure config 'elk-node)))
      (set-property target layered-options.node-placement-strategy strategy)
      (set-property target layered-options.node-placement-bk-fixed-alignment alignment)
      (set-property target layered-options.node-placement-bk-edge-straightening straightening))
    config))

(deflayouttest basic-node-placement-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%pilot-with-filter (model-resource-path "realworld/ptolemy/**/") "elkg")
                          (model-resource-path "tests/layered/node_placement/**/")))
  (:configurator-provider linear-segments-configurator ()
    (%bnp-configurator-for node-placement-strategy.linear-segments))
  (:configurator-provider network-simplex-configurator ()
    (%bnp-configurator-for node-placement-strategy.network-simplex))
  (:configurator-provider simple-configurator ()
    (%bnp-configurator-for node-placement-strategy.simple))
  (:configurator-provider bk-none-none-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.none fixed-alignment.none))
  (:configurator-provider bk-none-left-up-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.none fixed-alignment.leftup))
  (:configurator-provider bk-none-left-down-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.none fixed-alignment.leftdown))
  (:configurator-provider bk-none-right-up-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.none fixed-alignment.rightup))
  (:configurator-provider bk-none-right-down-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.none fixed-alignment.rightdown))
  (:configurator-provider bk-none-balanced-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.none fixed-alignment.balanced))
  (:configurator-provider bk-improve-straightness-left-up-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.improve-straightness fixed-alignment.leftup))
  (:configurator-provider bk-improve-straightness-left-down-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.improve-straightness fixed-alignment.leftdown))
  (:configurator-provider bk-improve-straightness-right-up-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.improve-straightness fixed-alignment.rightup))
  (:configurator-provider bk-improve-straightness-right-down-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.improve-straightness fixed-alignment.rightdown))
  (:configurator-provider bk-improve-straightness-balanced-configurator ()
    (%bnp-configurator-for node-placement-strategy.brandes-koepf
                           edge-straightening-strategy.improve-straightness fixed-alignment.balanced))
  (:after-processor test-proper-coordinates
    (:processors (bk-node-placer linear-segments-node-placer network-simplex-placer simple-node-placer))
    (graph)
    (jdo (layer graph)
      (let* ((nodes (get-nodes layer))
             (node (j-get nodes 0))
             (last-bottom-y (+ (.y (get-position node)) (.y (get-size node)))))
        (loop for i from 1 below (size nodes)
              do (setf node (j-get nodes i))
                 (assert-true (< last-bottom-y (.y (get-position node))))
                 (setf last-bottom-y (+ (.y (get-position node)) (.y (get-size node)))))))))
