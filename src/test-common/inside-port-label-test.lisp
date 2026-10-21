;;;; inside-port-label-test.lisp -- port of
;;;; org.eclipse.elk.alg.common.nodespacing.InsidePortLabelTest.

(in-package #:elk)

(defun %inside-port-label-assemble-labels (node)
  (let ((labels (new-jlist)))
    (jdo (label (get-labels node)) (jl-add labels label))
    (jdo (port (get-ports node))
      (jdo (label (get-labels port))
        (jl-add labels label)))
    labels))

(deflayouttest inside-port-label-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration :edges nil :nodes nil :ports t)
  (:graph-resources (list (model-resource-path "tests/core/node_size/inside_port_labels.elkt")))
  (:test test-no-label-overlaps () (graph)
    (let ((overlaps (some (lambda (node)
                            (graph-test-utils.have-overlaps
                             (%inside-port-label-assemble-labels node)))
                          (jlist-from (get-children graph)))))
      (when overlaps
        (fail "Overlaps between labels detected!")))))
