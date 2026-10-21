;;;; issue405-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue405Test.

(in-package #:elk)

(defparameter %b-405-epsilon 10e-4)

(defjstatic issue405-test.get-element-with-id (container id)
  (%b-element-with-id container id))

(defjstatic issue405-test.get-relevant-ports-and-port-labels (graph root-node-name)
  "Pair.of(ports, labels) of node N1 inside the root node ROOT-NODE-NAME."
  (let* ((root (issue405-test.get-element-with-id (get-children graph) root-node-name))
         (n1 (issue405-test.get-element-with-id (get-children root) "N1"))
         (labels (new-jlist)))
    (jdo (port (get-ports n1))
      (jdo (label (get-labels port))
        (jl-add labels label)))
    (cons (get-ports n1) labels)))

(defun %b-405-check-position-equals (first second)
  (and (%b-fuzzy-equals (get-x first) (get-x second) %b-405-epsilon)
       (%b-fuzzy-equals (get-y first) (get-y second) %b-405-epsilon)))

(deflayouttest issue405-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/405_differentPortLabelPositionsNSWEInside.elkt")
                          (model-resource-path "tickets/layered/405_differentPortLabelPositionsNSWEOutsideNextToPort.elkt")
                          (model-resource-path "tickets/layered/405_differentPortLabelPositionsTwoWestOutside.elkt")))
  (:test test-port-and-label-positions-equal () (graph)
    (let ((previous nil))
      (dolist (name (list "Right" "Down" "Left" "Up"))
        (let ((current (issue405-test.get-relevant-ports-and-port-labels graph name)))
          (when previous
            (jdo (port (car previous))
              (let ((matching (%b-count-matching (lambda (other) (%b-405-check-position-equals port other))
                                        (car current))))
                (unless (= matching 1)
                  (fail (format nil "~A has not exactly one matching element" port)))))
            (jdo (label (cdr previous))
              (let ((matching (%b-count-matching (lambda (other) (%b-405-check-position-equals label other))
                                        (cdr current))))
                (unless (= matching 1)
                  (fail (format nil "~A has not exactly one matching element" label))))))
          (setf previous current))))))
