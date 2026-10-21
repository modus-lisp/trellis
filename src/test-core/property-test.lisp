;;;; property-test.lisp -- port of org.eclipse.elk.core.graph.properties.PropertyTest
;;;; (the Java file lives under org.eclipse.elk.core.test).

(in-package #:elk)

(defun %pt-primitive (default)
  "testPropertyPrimitive: the default is returned as the very same object."
  (let* ((p (make-property "dummy" default))
         (copy (get-default p)))
    (assert-true (eq default copy))
    (assert-true (jequals default copy))))

(defun %pt-object (default)
  "testPropertyObject: the default is copied, and the copy is equal to it."
  (let* ((p (make-property "dummy" default))
         (copy (get-default p)))
    (assert-true (not (eq default copy)))
    (assert-true (jequals default copy))))

(defstruct pt-plain-value slot)

(defcore-class property-test
  ("testPropertyDefaultPrimitive"
   (%pt-primitive 43)
   (%pt-primitive 23.3f0)
   (%pt-primitive 32.3d0)
   (%pt-primitive "foo"))
  ("testPropertyDefaultIDataObject"
   (let ((v (new-k-vector 2 3)))
     (%pt-object v)
     (%pt-object (new-k-vector-chain (list v v)))
     (%pt-object (new-elk-padding 2 3))
     (%pt-object (new-elk-margin 3 2))))
  ("testPropertyDefaultObject"
   (let* ((v (new-k-vector 3 2))
          (al (jlist-from (list v (normalize v) (negate v)))))
     (%pt-object al)
     (%pt-object (jlist-from al))
     (%pt-object (jset-from (list v (normalize v) (negate v))))
     (%pt-object (jset-from al))
     (%pt-object (jset-from (list 1 2 3 4)))))
  ("testPropertyDefaultEnum"
   (%pt-primitive port-side.east))
  ("testPropertyDefaultEnumSet"
   (%pt-object (enum-set.all-of 'size-constraint)))
  ("testUnknownPropertyGetDefault"
   (assert-throws error
     (get-default (make-property "o" (make-pt-plain-value))))))
