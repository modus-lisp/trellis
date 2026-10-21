;;;; layout-data-content-assist-test.lisp -- port of
;;;; org.eclipse.elk.core.data.LayoutDataContentAssistTest.  The Enclosed
;;;; runner's Parameterized class becomes one test per row, named
;;;; "testLayoutOption[Prefix: <prefix>]" as JUnit's {2} name does.

(in-package #:elk)

(defun %ldca-proposals-contain (graph-element prefix expected)
  (let ((ps (layout-data-content-assist.get-layout-option-proposals graph-element prefix)))
    (assert-true (some (lambda (p) (jequals (.data p) expected)) ps))))

(defun %ldca-new-element (class-name)
  (cond ((string= class-name "ElkNode") (new-elk-node))
        ((string= class-name "ElkEdge") (new-elk-edge))
        ((string= class-name "ElkPort") (new-elk-port))
        ((string= class-name "ElkLabel") (new-elk-label))
        (t (error "no factory for ~A" class-name))))

(defparameter *ldca-prefix-rows*
  '(("ElkNode" core-options.port-constraints "org.eclipse.elk.portConstraints")
    ("ElkNode" core-options.port-constraints "eclipse.elk.portConstraints")
    ("ElkNode" core-options.port-constraints "elk.portConstraints")
    ("ElkNode" core-options.port-constraints "portConstraints")
    ("ElkNode" core-options.port-constraints "portCo")
    ("ElkNode" core-options.port-constraints "rt Constr")
    ("ElkEdge" core-options.priority "prio")
    ("ElkPort" core-options.port-side "port.sid")
    ("ElkLabel" core-options.node-labels-placement "nodeLabels.pla")
    ("ElkNode" core-options.padding "")
    ("ElkNode" core-options.alignment "")))

(core-register
 'layout-data-content-assist-prefixes-test
 (loop for (class-name option prefix) in *ldca-prefix-rows*
       collect (cons (format nil "testLayoutOption[Prefix: ~A]" prefix)
                     (let ((class-name class-name) (option (symbol-value option)) (prefix prefix))
                       (lambda ()
                         (%core-register-core-options)
                         (%ldca-proposals-contain (%ldca-new-element class-name) prefix option))))))

(defcore-class layout-data-content-assist-other-test
  ("testParentNodes"
   (%core-register-core-options)
   (let* ((root (elk-graph-util.create-graph))
          (child (elk-graph-util.create-node root)))
     (let ((parent-ps (layout-data-content-assist.get-layout-option-proposals
                       root (get-id core-options.algorithm))))
       (assert-true (some (lambda (p) (jequals (.data p) core-options.algorithm)) parent-ps)))
     (let ((child-ps (layout-data-content-assist.get-layout-option-proposals
                      child (get-id core-options.algorithm))))
       (assert-false (some (lambda (p) (jequals (.data p) core-options.algorithm)) child-ps)))))
  ("testAlgorithmSpecificKnown"
   (%core-register-core-options)
   (let ((root (elk-graph-util.create-graph)))
     (elk-graph-util.create-node root)
     (set-property root core-options.algorithm box-layouter-options.algorithm-id)
     (let ((parent-ps (layout-data-content-assist.get-layout-option-proposals
                       root (get-id core-options.expand-nodes))))
       (assert-true (some (lambda (p) (jequals (.data p) core-options.expand-nodes)) parent-ps)))))
  ("testAlgorithmSpecificUnknown"
   (%core-register-core-options)
   (let ((root (elk-graph-util.create-graph)))
     (elk-graph-util.create-node root)
     (set-property root core-options.algorithm fixed-layouter-options.algorithm-id)
     (let ((parent-ps (layout-data-content-assist.get-layout-option-proposals
                       root (get-id core-options.expand-nodes))))
       (assert-false (some (lambda (p) (jequals (.data p) core-options.expand-nodes)) parent-ps)))))
  ("testOptionGroupIsAdded"
   (%core-register-core-options)
   (let ((root (elk-graph-util.create-graph)))
     (elk-graph-util.create-node root)
     (let ((ps (layout-data-content-assist.get-layout-option-proposals root "nodeN")))
       (assert-true (some (lambda (p)
                            (and (jequals (.data p) core-options.spacing-node-node)
                                 (search (get-group (.data p)) (.proposal p))))
                          ps))))))
