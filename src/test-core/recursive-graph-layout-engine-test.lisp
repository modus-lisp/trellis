;;;; recursive-graph-layout-engine-test.lisp -- port of
;;;; org.eclipse.elk.core.RecursiveGraphLayoutEngineTest.  The @BeforeClass
;;;; PlainJavaInitialization is done by the module loader.

(in-package #:elk)

(defstruct rgle-graph root n1 n2)

(defun %rgle-new-graph ()
  (let* ((root (elk-graph-util.create-graph))
         (n1 (elk-graph-util.create-node root))
         (n2 (elk-graph-util.create-node root)))
    (set-dimensions n1 10 10)
    (set-dimensions n2 10 10)
    (make-rgle-graph :root root :n1 n1 :n2 n2)))

(defcore-class recursive-graph-layout-engine-test
  ("testUnresolvedGraph"
   (let ((g (%rgle-new-graph)) (engine (new-recursive-graph-layout-engine)))
     (set-property (rgle-graph-root g) core-options.algorithm "org.eclipse.elk.box")
     (layout engine (rgle-graph-root g) (new-basic-progress-monitor))
     (assert-true (> (get-width (rgle-graph-root g)) 0))
     (assert-true (> (get-height (rgle-graph-root g)) 0))))
  ("testResolvedGraph"
   (let* ((g (%rgle-new-graph))
          (engine (new-recursive-graph-layout-engine))
          (algorithm-data (get-algorithm-data (layout-meta-data-service.get-instance)
                                              "org.eclipse.elk.box")))
     (set-property (rgle-graph-root g) core-options.resolved-algorithm algorithm-data)
     (layout engine (rgle-graph-root g) (new-basic-progress-monitor))
     (assert-true (> (get-width (rgle-graph-root g)) 0))
     (assert-true (> (get-height (rgle-graph-root g)) 0))))
  ("testUnknownAlgorithmId"
   (let ((g (%rgle-new-graph)) (engine (new-recursive-graph-layout-engine)))
     (set-property (rgle-graph-root g) core-options.algorithm "foo.Bar")
     (assert-throws error
       (layout engine (rgle-graph-root g) (new-basic-progress-monitor)))))
  ("testEmptyAlgorithmId"
   (let ((g (%rgle-new-graph)) (engine (new-recursive-graph-layout-engine)))
     (layout engine (rgle-graph-root g) (new-basic-progress-monitor))
     (assert-equals "org.eclipse.elk.layered"
                    (get-id (get-property (rgle-graph-root g) core-options.resolved-algorithm))))))
