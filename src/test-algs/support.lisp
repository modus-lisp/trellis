;;;; support.lisp -- shared helpers for the test-algs port.
;;;;
;;;; PLAIN JUnit classes (no @RunWith(LayoutTestRunner)) are ported as
;;;; DEFALGTEST forms: each test is a named closure registered under its class.
;;;; /tmp/algs-driver runs them and prints the same Summary line as
;;;; run-elk-tests.  Layout-runner classes use DEFLAYOUTTEST as the pilot does.

(in-package #:elk)

(defvar *algs-plain-tests* '()
  "List of (class-name test-name . thunk) in definition order.")

(defmacro defalgtest (class test-name &body body)
  "One @Test method of a plain JUnit class."
  `(progn
     (setf *algs-plain-tests*
           (append (remove-if (lambda (e) (and (eq (first e) ',class)
                                               (eq (second e) ',test-name)))
                              *algs-plain-tests*)
                   (list (list ',class ',test-name (lambda () (with-java-floats ,@body))))))
     ',test-name))

(defun assert-near (expected actual delta &optional (message ""))
  "JUnit assertEquals(double, double, delta)."
  (unless (<= (abs (- expected actual)) delta)
    (%tf-assertion-failed
     (format nil "~A~A expected:<~A> but was:<~A>" (%tf-msg-prefix message) "" expected actual))))

(defun algs-init ()
  "PlainJavaInitialization.initializePlainJavaLayout(): registers the algorithm
metadata.  See BLOCKERS.md -- the Java class is not in the port."
  (let ((service (layout-meta-data-service.get-instance)))
    (declare (ignorable service))
    nil))

(defun algs-layout (graph)
  "new LayoutConfigurator + ElkUtil.applyVisitors(..., new LayoutAlgorithmResolver())
followed by RecursiveGraphLayoutEngine.layout(graph, new BasicProgressMonitor())."
  (let ((config (new-layout-configurator)))
    (elk-util.apply-visitors graph (list config (new-layout-algorithm-resolver))))
  (layout (new-recursive-graph-layout-engine) graph (new-basic-progress-monitor))
  nil)
