;;;; classes.lisp -- the data the test framework passes around, and the
;;;; ILayoutExecutionListener that drives whitebox tests.
;;;;
;;;; Ports org.eclipse.elk.alg.test.framework: ExperimentStatement (the
;;;; listener half), ExperimentalObject, and the records that LayoutTestRunner
;;;; and ExperimentRunner keep.  The JUnit Runner machinery itself is replaced
;;;; by RUN-ELK-TESTS in runner.lisp.

(in-package #:elk)

;;; A failed JUnit assertion (AssertionError).  Any other CL error raised by a
;;; test body is an ERROR outcome, as in JUnit.
(define-condition assertion-failure (error)
  ((message :initarg :message :initform "" :reader assertion-failure-message))
  (:report (lambda (c s) (write-string (assertion-failure-message c) s))))

;;; One @Test, @TestBeforeProcessor or @TestAfterProcessor method.
;;; KIND is :test, :before or :after.  FUNCTION takes the graph (ElkNode for
;;; blackbox tests, the processor's graph for whitebox tests).
(defstruct test-method
  name
  kind
  (processors nil)           ; whitebox: list of processor class symbols
  (only-on-root nil)         ; @OnlyOnRootNode
  (fail-if-not-executed nil) ; @FailIfNotExecuted
  (ignore nil)               ; @Ignore
  (function nil))

(defun test-method-display-name (m)
  (string-downcase (symbol-name-or-string (test-method-name m))))

(defun symbol-name-or-string (x)
  (if (symbolp x) (symbol-name x) (princ-to-string x)))

;;; One ExperimentRunner result, as reported by RUN-ELK-TESTS.
(defstruct test-result
  experiment        ; experiment description string
  test              ; test name string
  id                ; runner id, as in "name (id)"
  status            ; :pass :fail :error :ignored
  message)

;;; ExperimentalObject: algorithm x configuration x graph.
(defstruct experiment
  algorithm         ; test-algorithm
  configuration     ; test-configuration
  graph             ; test-graph
  (id 0)
  runner)

(defvar *experiment-counter* 0
  "ExperimentRunner.nextRunnerId: incremented once per experiment.")

(defun experiment-description (e)
  (format nil "~A ~A ~A"
          (test-algorithm-description (experiment-algorithm e))
          (test-configuration-description (experiment-configuration e))
          (test-graph-description (experiment-graph e))))

;;; ExperimentStatement: the layout listener.  During RecursiveGraphLayoutEngine
;;; it runs the whitebox tests registered for each processor, recording their
;;; outcome in RECORDER (WhiteboxRecorder) so they can be reported after the
;;; blackbox tests, just as ExperimentStatement.simulateWhiteboxTestsForNotifiers
;;; does.
(defjclass experiment-statement (i-layout-execution-listener)
  ((experiment nil)
   (results nil)
   (recorder (make-hash-table :test 'eq))))   ; test-method -> (executed . failures)
