;;;; issue405-2-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue405_2Test.

(in-package #:elk)

(defparameter %b-405-2-epsilon 10e-4)

(defun %b-405-2-check-position-equals (first second)
  (let* ((ps (if (typep first 'elk-label)
                 (get-property (get-parent first) layered-options.port-side)
                 (get-property first layered-options.port-side))))
    (if (contains port-side.sides-east-west ps)
        (%b-fuzzy-equals (get-x first) (get-x second) %b-405-2-epsilon)
        (%b-fuzzy-equals (get-y first) (get-y second) %b-405-2-epsilon))))

(deflayouttest issue405-2-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  ;; Further #405 cases cannot be tested here (see Issue405Test).
  (:graph-resources (list (model-resource-path "tickets/layered/405_differentPortLabelPositionsNSWEOutside.elkt")
                          (model-resource-path "tickets/layered/405_differentPortLabelPositionsTwoWestOutsideNextToPort.elkt")))
  (:test test-port-and-label-positions-equal () (graph)
    ;; The reduce(false, a ^ b) in the Java is an XOR fold: true iff the count is odd.
    (let ((previous nil))
      (dolist (name (list "Right" "Down" "Left" "Up"))
        (let ((current (issue405-test.get-relevant-ports-and-port-labels graph name)))
          (when previous
            (jdo (port (car previous))
              (assert-true (format nil "Unexpected port position ~A" port)
                           (oddp (%b-count-matching (lambda (other) (%b-405-2-check-position-equals port other))
                                           (car current)))))
            (jdo (label (cdr previous))
              (assert-true (format nil "Unexpected port label position ~A" label)
                           (oddp (%b-count-matching (lambda (other) (%b-405-2-check-position-equals label other))
                                           (cdr current))))))
          (setf previous current))))))
