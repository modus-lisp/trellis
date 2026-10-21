;;;; direct-layout-test.lisp -- port of org.eclipse.elk.shared.test.DirectLayoutTest.
;;;; GraphTestUtils.createSimpleGraph is the shared test helper; it is not in
;;;; the port (see BLOCKERS.md), so this test errors until it is.

(in-package #:elk)

(defun %dlt-add-layered-options (parent)
  (set-property parent core-options.direction core-direction.right)
  (jdo (child (get-children parent))
    (set-width child 30.0d0)
    (set-height child 30.0d0)
    (set-property child layered-options.node-size-constraints (size-constraint.fixed))
    (set-property child layered-options.port-constraints port-constraints.fixed-pos)
    (let ((i 0))
      (jdo (port (get-ports child))
        (incf i)
        (set-y port (/ (* i 30.0d0) (+ (size (get-ports child)) 1)))
        (if (equals (get-text (j-get (get-labels child) 0)) "node1")
            (progn (set-x port 30.0d0)
                   (set-property port layered-options.port-side port-side.east))
            (progn (set-x port 0.0d0)
                   (set-property port layered-options.port-side port-side.west)))))))

(defcore-class direct-layout-test
  ("testELKLayered"
   (layout-meta-data-service.init-elk-reflect)
   (let* ((parent (graph-test-utils.create-simple-graph))
          (monitor (new-basic-progress-monitor))
          (provider (new-layered-layout-provider)))
     (%dlt-add-layered-options parent)
     (initialize provider nil)
     (layout provider parent monitor)
     (jdo (child (get-children parent))
       (when (or (<= (get-x child) 0) (<= (get-y child) 0))
         (fail "Not all node coordinates have been set properly.")))
     (jdo (child (get-children parent))
       (format t "~&~A: x = ~A, y = ~A~%" (get-text (j-get (get-labels child) 0))
               (get-x child) (get-y child))))))
