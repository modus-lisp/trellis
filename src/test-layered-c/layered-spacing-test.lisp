;;;; layered-spacing-test.lisp -- port of org.eclipse.elk.alg.layered.LayeredSpacingTest.
;;;;
;;;; JUnit's Enclosed/Parameterized nesting is flattened: WithFactorAndValueTest
;;;; runs once per spacing option (registered below, the option is the
;;;; parameter), AfterLayoutTest and SpacingOverrides are plain tests.  All are
;;;; DEFPLAINTEST, under the name of the outer Java class.

(in-package #:elk)

(defconstant +lst-double-eq-epsilon+ 10d-5)

(defun %lst-create-simple-graph ()
  "Two 30x30 nodes joined by one edge; no fixed size constraint."
  (let* ((graph (elk-graph-util.create-graph))
         (node1 (elk-graph-util.create-node graph))
         (node2 (elk-graph-util.create-node graph)))
    (set-width node1 30) (set-height node1 30)
    (set-width node2 30) (set-height node2 30)
    (elk-graph-util.create-simple-edge node1 node2)
    graph))

(defun %lst-create-and-configure-test-graph (visitor)
  (let ((graph (%lst-create-simple-graph)))
    (elk-util.apply-visitors graph visitor)
    graph))

(defparameter *lst-spacing-options*
  (list layered-options.spacing-edge-edge
        layered-options.spacing-edge-label
        layered-options.spacing-edge-node
        layered-options.spacing-label-label
        layered-options.spacing-label-node
        layered-options.spacing-label-port-horizontal
        layered-options.spacing-label-port-vertical
        layered-options.spacing-node-self-loop
        layered-options.spacing-port-port
        layered-options.spacing-edge-edge-between-layers
        layered-options.spacing-edge-node-between-layers
        layered-options.spacing-node-node-between-layers))

;; WithFactorAndValueTest: testDefaultFactorsProperlyOverwritten, once per option.
;; "Check that in the (potentially overwritten) default values of the layered
;; options and not the core options are used."
(dolist (option *lst-spacing-options*)
  (let ((option option))
    (%register-plain-junit
     'layered-spacing-test
     (format nil "test-default-factors-properly-overwritten[~A]" (.id option))
     (lambda ()
       (let* ((builder (layered-spacings.with-base-value 33.0d0))
              (value (jm-get (get-factors builder) option)))
         (tlc-assert-equals-delta nil
                                  (/ (get-default option)
                                     (get-default layered-spacings-builder.base-spacing-option))
                                  value
                                  +lst-double-eq-epsilon+))))))

;; AfterLayoutTest
(defplaintest layered-spacing-test test-spacing-node-node-between-layers ()
  (let* ((visitor (to-visitor (with-factor (layered-spacings.with-base-value 33.0d0)
                                           layered-options.spacing-node-node-between-layers 2.0d0)))
         (graph (%lst-create-and-configure-test-graph visitor)))
    (with-java-floats (layout (new-layered-layout-provider) graph (new-basic-progress-monitor)))
    (jdo (edge (get-contained-edges graph))
      (let* ((source (elk-graph-util.get-source-node edge))
             (spacing (- (get-x (elk-graph-util.get-target-node edge))
                         (+ (get-x source) (get-width source)))))
        (tlc-assert-equals-delta nil 66 spacing +lst-double-eq-epsilon+)))))

;; SpacingOverrides
(defplaintest layered-spacing-test test-extracts-correct-default-for-layered-property ()
  (let ((snn (get-property (%lst-create-simple-graph) layered-options.spacing-edge-node)))
    (tlc-assert-equals-delta nil (get-default layered-options.spacing-edge-node) snn
                             +lst-double-eq-epsilon+)))

(defun %lst-property-with-different-default ()
  (new-property layered-options.spacing-edge-node
                (+ (get-default layered-options.spacing-edge-node) 3.0d0)))

(defplaintest layered-spacing-test test-extracts-correct-default-for-inheriting-property ()
  (let ((sne (get-property (%lst-create-simple-graph) (%lst-property-with-different-default))))
    (tlc-assert-equals-delta nil (+ (get-default layered-options.spacing-edge-node) 3.0d0) sne
                             +lst-double-eq-epsilon+)))

(defplaintest layered-spacing-test test-extracts-correct-default-value-after-spacing-configuration ()
  (let* ((node (%lst-create-simple-graph))
         (builder (layered-spacings.with-base-value 35.0d0)))
    (j-apply builder node)
    (assert-true (has-property node layered-options.spacing-edge-node))
    (let ((expected (* 35.0d0 (jm-get (get-factors builder) layered-options.spacing-edge-node))))
      (tlc-assert-equals-delta nil expected
                               (get-property node (%lst-property-with-different-default))
                               +lst-double-eq-epsilon+)
      (tlc-assert-equals-delta nil expected (get-property node layered-options.spacing-edge-node)
                               +lst-double-eq-epsilon+))))
