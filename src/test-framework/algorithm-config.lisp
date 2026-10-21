;;;; algorithm-config.lisp -- org.eclipse.elk.alg.test.framework.algorithm.TestAlgorithm
;;;; and org.eclipse.elk.alg.test.framework.config.* (TestConfiguration and its
;;;; two subclasses ConfiguratorTestConfiguration and MethodTestConfiguration).

(in-package #:elk)

;;; ------------------------------------------------------------ TestAlgorithm

(defstruct test-algorithm
  (data nil))   ; LayoutAlgorithmData, or NIL for the identity (algorithm as in the graph)

(defun test-algorithm-for-data (data)
  "new TestAlgorithm(algData): sets the graph's algorithm option."
  (make-test-algorithm :data data))

(defun test-algorithm-identity ()
  (make-test-algorithm :data nil))

(defun test-algorithm-apply (algorithm graph)
  "TestAlgorithm.apply(test, graph)."
  (let ((data (test-algorithm-data algorithm)))
    (when data
      (let ((configurator (make-instance 'layout-configurator)))
        (set-property (configure configurator 'elk-node)
                      core-options.algorithm (get-id data))
        (elk-util.apply-visitors graph (vector configurator))))))

(defun test-algorithm-description (algorithm)
  (let ((data (test-algorithm-data algorithm)))
    (if data
        (format nil "algorithm(~A)" (get-id data))
        "algorithm(as in test graph)")))

;;; ------------------------------------------------------------ TestConfiguration

;;; Flags come from @DefaultConfiguration (all NIL when the annotation is absent).
;;; BASE is the subclass hook applyBaseConfiguration: a function of the graph,
;;; or NIL.  KIND is :plain, :configurator (ConfiguratorProvider) or :method
;;; (Configurator); NAME is the method name for the description.
(defstruct test-configuration
  (kind :plain)
  (name nil)
  (base nil)
  (nodes nil) (ports nil) (edges nil) (edge-labels nil))

(defun test-configuration-description (c)
  (let* ((joined (format nil "~{~A~^, ~}"
                         (remove nil (list (and (test-configuration-nodes c) "nodes")
                                           (and (test-configuration-ports c) "ports")
                                           (and (test-configuration-edges c) "edges")
                                           (and (test-configuration-edge-labels c) "edgeLabels")))))
         (base (format nil "defaults(~A)" joined)))
    (ecase (test-configuration-kind c)
      (:plain base)
      (:configurator (format nil "~A layoutConfigurator(~A)" base (test-configuration-name c)))
      (:method (format nil "~A configMethod(~A)" base (test-configuration-name c))))))

(defun test-configuration-apply (c graph)
  "TestConfiguration.apply(test, graph): base, then defaults, then label sizes."
  (when (test-configuration-base c)
    (funcall (test-configuration-base c) graph))
  (%tc-apply-default-configuration c graph)
  (%tc-ensure-sensible-label-size graph))

(defun %tc-apply-default-configuration (c graph)
  (when (or (test-configuration-nodes c) (test-configuration-ports c)
            (test-configuration-edges c) (test-configuration-edge-labels c))
    (jdo (node (get-children graph))
      (when (test-configuration-nodes c)
        (elk-util.configure-with-default-values node))
      (when (or (test-configuration-ports c) (test-configuration-edges c)
                (test-configuration-edge-labels c))
        (jdo (port (get-ports node))
          (when (test-configuration-ports c)
            (elk-util.configure-with-default-values port))
          (when (test-configuration-edges c)
            (jdo (edge (get-outgoing-edges port))
              (elk-util.configure-with-default-values edge)))
          (when (test-configuration-edge-labels c)
            (jdo (edge (get-outgoing-edges port))
              (when (zerop (size (get-labels edge)))
                (elk-graph-util.create-label (get-identifier edge) edge))))))
      (when (test-configuration-edges c)
        (jdo (edge (get-outgoing-edges node))
          (elk-util.configure-with-default-values edge)))
      (when (test-configuration-edge-labels c)
        (jdo (edge (get-outgoing-edges node))
          (when (zerop (size (get-labels edge)))
            (elk-graph-util.create-label (get-identifier edge) edge))))
      (%tc-apply-default-configuration c node))))

(defvar *label-size-function* nil
  "ComputeLabelSize needs AWT font metrics, which the port does not have.  When
this is a function (text font-size) -> (values width height) it is used for
labels of zero size with text; NIL leaves such labels at zero size (a known
deviation from the Java framework).")

(defun %tc-ensure-sensible-label-size (graph)
  (when *label-size-function*
    (dolist (label (%tf-all-labels graph))
      (when (and (zerop (get-height label)) (zerop (get-width label))
                 (plusp (length (or (get-text label) ""))))
        (multiple-value-bind (w h) (funcall *label-size-function* (get-text label) 10)
          (set-height label h)
          (set-width label w))))))

(defun %tf-all-labels (graph)
  "The ElkLabels of GRAPH and everything it contains (EMF eAllContents)."
  (let ((labels '()))
    (labels ((walk (node)
               (dolist (l (get-labels node)) (push l labels))
               (jdo (port (get-ports node))
                 (dolist (l (get-labels port)) (push l labels)))
               (jdo (edge (get-contained-edges node))
                 (dolist (l (get-labels edge)) (push l labels)))
               (jdo (child (get-children node))
                 (walk child))))
      (walk graph))
    (nreverse labels)))
