;;;; elk-live-examples-test.lisp -- port of org.eclipse.elk.shared.test.ElkLiveExamplesTest.

(in-package #:elk)

(deflayouttest elk-live-examples-test
  (:default-configuration)
  (:graph-resources (list (let ((path (model-resource-path "examples/**/")))
                            (setf (abstract-resource-path-filter path)
                                  (file-extension-filter "elkt"))
                            path)))
  (:test no-exception () (graph)
    (assert-true t)))
