;;;; spacing-configuration-test.lisp -- port of
;;;; org.eclipse.elk.core.alg.SpacingConfigurationTest.  JUnit's Enclosed
;;;; runner and its two Parameterized inner classes become three plain classes;
;;;; each parameterized method is one test per option, named "method[option]"
;;;; as JUnit's {0} name does.

(in-package #:elk)

(defvar *spacing-epsilon* 10e-5)

(defun %spacing-create-simple-graph ()
  (let ((graph (elk-graph-util.create-graph))
        (node1 nil) (node2 nil))
    (setf node1 (elk-graph-util.create-node graph))
    (set-width node1 30) (set-height node1 30)
    (setf node2 (elk-graph-util.create-node graph))
    (set-width node2 30) (set-height node2 30)
    (elk-graph-util.create-simple-edge node1 node2)
    graph))

(defun %spacing-create-and-configure (visitor)
  (let ((graph (%spacing-create-simple-graph)))
    (elk-util.apply-visitors graph (vector visitor))
    graph))

(defun %spacing-check-option-value (graph clazz option value)
  (jdo (e (elk-graph-util.properties-skipping-iterator-for graph t))
    (when (typep e clazz)
      (assert-near value (get-property e option) *spacing-epsilon*))))

(defun %spacing-option-name (option) (get-id option))

(defcore-class spacing-configuration-basics-test
  ("testCreateVisitor"
   (let* ((node (elk-graph-util.create-node nil))
          (visitor (to-visitor (elk-spacings.with-base-value 3.0))))
     (visit visitor node)
     (assert-near 3.0 (get-property node elk-core-spacings-builder.base-spacing-option)
                  *spacing-epsilon*)))
  ("testApplyTo"
   (let ((node (elk-graph-util.create-node nil)))
     (j-apply (elk-spacings.with-base-value 3.0) node)
     (assert-near 3.0 (get-property node elk-core-spacings-builder.base-spacing-option)
                  *spacing-epsilon*)))
  ("testSpacingWithNegativeFactor"
   (assert-throws error
     (%spacing-create-and-configure
      (to-visitor (with-factor (elk-spacings.with-base-value 33.0)
                               core-options.spacing-edge-edge (- double.min-value))))))
  ("testSpacingWithNegativeValue"
   (assert-throws error
     (%spacing-create-and-configure
      (to-visitor (with-value (elk-spacings.with-base-value 33.0)
                              core-options.spacing-edge-edge (- double.min-value)))))))

(defparameter *spacing-positive-options*
  (list 'core-options.spacing-component-component
        'core-options.spacing-edge-edge
        'core-options.spacing-edge-label
        'core-options.spacing-edge-node
        'core-options.spacing-label-label
        'core-options.spacing-label-node
        'core-options.spacing-label-port-horizontal
        'core-options.spacing-label-port-vertical
        'core-options.spacing-node-self-loop
        'core-options.spacing-port-port))

(defun %spacing-with-factor-and-value-tests (option-symbol)
  (let* ((opt (symbol-value option-symbol)) (suffix (format nil "[~A]" (%spacing-option-name opt))))
    (flet ((n (name) (concatenate 'string name suffix)))
      (list
       (cons (n "testDefaultFactors")
             (lambda ()
               (let* ((builder (elk-spacings.with-base-value 33.0))
                      (value (j-get (get-factors builder) opt)))
                 (assert-near (/ (get-default opt)
                                 (get-default elk-core-spacings-builder.base-spacing-option))
                              value *spacing-epsilon*))))
       (cons (n "testSpacingWithFactor")
             (lambda ()
               (let ((graph (%spacing-create-and-configure
                             (to-visitor (with-factor (elk-spacings.with-base-value 33.0) opt 2.0)))))
                 (%spacing-check-option-value graph 'elk-node opt 66.0))))
       (cons (n "testSpacingWithValue")
             (lambda ()
               (let ((graph (%spacing-create-and-configure
                             (to-visitor (with-value (elk-spacings.with-base-value 33.0) opt 24.0)))))
                 (%spacing-check-option-value graph 'elk-node opt 24.0))))
       (cons (n "testOverwriteIfRequested")
             (lambda ()
               (let* ((graph (%spacing-create-simple-graph))
                      (first-child (j-get (get-children graph) 0))
                      (second-child (j-get (get-children graph) 1)))
                 (set-property first-child opt 22.0)
                 (elk-util.apply-visitors
                  graph
                  (vector (to-visitor (with-overwrite
                                       (with-value (elk-spacings.with-base-value 1) opt 3)
                                       t))))
                 (assert-near 3.0 (get-property first-child opt) *spacing-epsilon*)
                 (assert-near 3.0 (get-property second-child opt) *spacing-epsilon*))))
       (cons (n "testDontOverwriteIfNotRequested")
             (lambda ()
               (let* ((graph (%spacing-create-simple-graph))
                      (first-child (j-get (get-children graph) 0))
                      (second-child (j-get (get-children graph) 1)))
                 (set-property first-child opt 22.0)
                 (elk-util.apply-visitors
                  graph
                  (vector (to-visitor (with-overwrite
                                       (with-value (elk-spacings.with-base-value 1) opt 3)
                                       nil))))
                 (assert-near 22.0 (get-property first-child opt) *spacing-epsilon*)
                 (assert-near 3.0 (get-property second-child opt) *spacing-epsilon*))))))))

(core-register
 'spacing-configuration-with-factor-and-value-test
 (loop for s in *spacing-positive-options*
       append (%spacing-with-factor-and-value-tests s)))

(defun %spacing-invalid-option-tests (option)
  (let ((suffix (format nil "[~A]" (%spacing-option-name option))))
    (flet ((n (name) (concatenate 'string name suffix)))
      (list
       (cons (n "testSpacingWithFactor")
             (lambda ()
               (assert-throws error
                 (%spacing-create-and-configure
                  (to-visitor (with-factor (elk-spacings.with-base-value 33.0) option 2.0))))))
       (cons (n "testSpacingWithValue")
             (lambda ()
               (assert-throws error
                 (%spacing-create-and-configure
                  (to-visitor (with-value (elk-spacings.with-base-value 33.0) option 24.0))))))))))

(core-register
 'spacing-configuration-invalid-option-configuration-test
 (append (%spacing-invalid-option-tests elk-core-spacings-builder.base-spacing-option)
         (%spacing-invalid-option-tests core-options.aspect-ratio)))
