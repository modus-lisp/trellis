;;;; id-test.lisp -- port of org.eclipse.elk.graph.json.test.IdTest.

(in-package #:elk)

(defun %id-import (text)
  (to-elk (elk-graph-json.for-graph text)))

(defcore-class id-test
  ("testNoId"
   (assert-throws json-import-exception (%id-import "{}")))
  ("testWrongIdTypeNumber"
   (assert-throws json-import-exception (%id-import "{ id: 1.2 }")))
  ("testWrongIdTypeObject"
   (assert-throws json-import-exception (%id-import "{ id: {} }")))
  ("testWrongIdTypeArray"
   (assert-throws json-import-exception (%id-import "{ id: [] }")))
  ("testWrongIdTypeBoolean"
   (assert-throws json-import-exception (%id-import "{ id: true }")))
  ("testGoodIdString"
   (%id-import "{ id: 'foo' }"))
  ("testGoodIdInt"
   (%id-import "{ id: 3 }")))
