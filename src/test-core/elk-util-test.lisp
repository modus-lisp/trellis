;;;; elk-util-test.lisp -- port of org.eclipse.elk.core.util.ElkUtilTest.

(in-package #:elk)

(defun %eut-content-alignment-graph (content-alignment)
  "createContentAlignmentTestGraph: a parent holding one node with one child."
  (let* ((parent (elk-graph-util.create-graph))
         (node (elk-graph-util.create-node parent)))
    (set-property node core-options.content-alignment content-alignment)
    (elk-graph-util.create-node node)
    (set-dimensions node 100 100)
    (set-dimensions (j-get (get-children node) 0) 80 80)
    parent))

(defun %eut-check-translate (content-alignment expected-x expected-y)
  (let* ((parent (%eut-content-alignment-graph content-alignment))
         (node (j-get (get-children parent) 0))
         (inner (j-get (get-children node) 0)))
    (elk-util.translate (j-get (get-children parent) 0)
                        (new-k-vector 120 120) (new-k-vector 100 100))
    (assert-near expected-x (get-x inner) 1)
    (assert-near expected-y (get-y inner) 1)))

(defcore-class elk-util-test
  ("translateWithContentAlignmentTest"
   (%eut-check-translate nil 0 0)
   (%eut-check-translate (content-alignment.top-left) 0 0)
   (%eut-check-translate (content-alignment.top-center) 10 0)
   (%eut-check-translate (enum-set.of content-alignment.v-top content-alignment.h-right) 20 0)
   (%eut-check-translate (enum-set.of content-alignment.v-center content-alignment.h-left) 0 10)
   (%eut-check-translate (content-alignment.center-center) 10 10)
   (%eut-check-translate (enum-set.of content-alignment.v-center content-alignment.h-right) 20 10)
   (%eut-check-translate (enum-set.of content-alignment.v-bottom content-alignment.h-left) 0 20)
   (%eut-check-translate (enum-set.of content-alignment.v-bottom content-alignment.h-center) 10 20)
   (%eut-check-translate (content-alignment.bottom-right) 20 20)))
