;;;; runner.lisp -- LayoutTestRunner (class initialization and the experiment
;;;; matrix), ExperimentRunner / ExperimentStatement (running one experiment),
;;;; and RUN-ELK-TESTS, which replaces JUnit's notifier with a printed report.
;;;;
;;;; Order of work follows the Java code.  Blackbox tests run once per
;;;; experiment on the realized, laid-out root node.  Whitebox tests run inside
;;;; the layout, when the named processor starts (@TestBeforeProcessor) or
;;;; finishes (@TestAfterProcessor); their outcomes are recorded and only then
;;;; reported, so an experiment that is never laid out does not pretend its
;;;; whitebox tests passed.

(in-package #:elk)

;;; ------------------------------------------------------------ initialization

(defstruct layout-test-runner
  class
  (errors '())               ; init errors, strings, in the order found
  (blackbox '())             ; TEST-METHODs (:test)
  (whitebox '())             ; TEST-METHODs (:before/:after), declaration order
  (before-table (make-hash-table :test 'eq))  ; processor class symbol -> list of TEST-METHOD
  (after-table (make-hash-table :test 'eq))
  (experiments '()))

(defun %init-error (runner fmt &rest args)
  (setf (layout-test-runner-errors runner)
        (append (layout-test-runner-errors runner) (list (apply #'format nil fmt args)))))

(defun %init-catching (runner thunk)
  "Run THUNK during initialization; any error becomes an init error."
  (handler-case (funcall thunk)
    (error (c) (%init-error runner "~A" c) nil)))

(defun %initialize-layout-test (class)
  (let ((runner (make-layout-test-runner :class class)))
    (%initialize-blackbox-tests runner class)
    (%initialize-whitebox-tests runner class)
    (let ((graphs (%initialize-graphs runner class)))
      (let ((algorithms (%initialize-algorithms runner class)))
        (let ((configurations (%initialize-configurations runner class)))
          (when (and (null (layout-test-runner-errors runner)) (null graphs))
            (%init-error runner "No test graphs specified."))
          (%build-experiments runner algorithms configurations graphs))))
    runner))

(defun %initialize-blackbox-tests (runner class)
  (dolist (m (layout-test-class-methods class))
    (when (eq (test-method-kind m) :test)
      (when (test-method-only-on-root m)
        (%init-error runner "Method ~A cannot have annotation OnlyOnRootNode"
                     (test-method-display-name m)))
      (setf (layout-test-runner-blackbox runner)
            (append (layout-test-runner-blackbox runner) (list m))))))

(defun %initialize-whitebox-tests (runner class)
  (dolist (m (layout-test-class-methods class))
    (unless (eq (test-method-kind m) :test)
      (when (null (test-method-processors m))
        (%init-error runner "Method ~A names no layout processor"
                     (test-method-display-name m)))
      (setf (layout-test-runner-whitebox runner)
            (append (layout-test-runner-whitebox runner) (list m)))
      (dolist (p (test-method-processors m))
        (push m (gethash p (if (eq (test-method-kind m) :before)
                               (layout-test-runner-before-table runner)
                               (layout-test-runner-after-table runner)))))))
  ;; push built the tables in reverse; restore declaration order per processor
  (maphash (lambda (k v) (setf (gethash k (layout-test-runner-before-table runner)) (reverse v)))
           (layout-test-runner-before-table runner))
  (maphash (lambda (k v) (setf (gethash k (layout-test-runner-after-table runner)) (reverse v)))
           (layout-test-runner-after-table runner)))

(defun %initialize-graphs (runner class)
  "GraphFromFile, GraphFromMethod, RandomGraphFromFile, RandomGraphFromMethod."
  (let ((graphs '()))
    ;; GraphFromFile
    (dolist (thunk (layout-test-class-resource-thunks class))
      (%init-catching runner
        (lambda ()
          (dolist (path (funcall thunk))
            (unless (abstract-resource-path-filter path)
              (setf (abstract-resource-path-filter path) (apply #'file-extension-filter *graph-file-extensions*)))
            (dolist (abs (abstract-resource-path-list-resources path))
              (push (graph-from-file (abstract-resource-path-file abs)) graphs))))))
    ;; GraphFromMethod
    (dolist (entry (layout-test-class-graph-providers class))
      (push (graph-from-method (car entry) (cdr entry)) graphs))
    ;; RandomGraphFromFile
    (dolist (thunk (layout-test-class-random-file-thunks class))
      (%init-catching runner
        (lambda ()
          (let ((path (funcall thunk)))
            (setf (abstract-resource-path-filter path)
                  (apply #'file-extension-filter *random-graph-file-extensions*))
            (dolist (abs (abstract-resource-path-list-resources path))
              (let ((file (abstract-resource-path-file abs)))
                (handler-case
                    (dolist (g (%tf-random-graphs-of-file file))
                      (push (random-graph-from-file file g) graphs))
                  (error ()
                    (%init-error runner "Unable to load random graphs from ~A" file)))))))))
    ;; RandomGraphFromMethod
    (dolist (entry (layout-test-class-random-option-providers class))
      (%init-catching runner
        (lambda ()
          (push (random-graph-from-method (car entry)
                                          (%tf-random-graphs-of-method (funcall (cdr entry))))
                graphs))))
    (nreverse graphs)))

(defun %initialize-algorithms (runner class)
  (let* ((ids (layout-test-class-algorithms class))
         (data '()))
    (cond ((and ids (layout-test-class-all-algorithms class))
           (%init-error runner "If @AllAlgorithms is specified, @Algorithm cannot be used anymore."))
          ((layout-test-class-all-algorithms class)
           (setf data (copy-list (jlist-to-list
                                  (get-algorithm-data (layout-meta-data-service.get-instance))))))
          (t
           (dolist (id ids)
             (let ((d (get-algorithm-data (layout-meta-data-service.get-instance) id)))
               (if d
                   (setf data (append data (list d)))
                   (%init-error runner "Unknown layout algorithm: ~A" id))))))
    (setf data (sort data #'string< :key (lambda (d) (get-id d))))
    (let ((algorithms (mapcar #'test-algorithm-for-data data)))
      ;; Whitebox tests need explicit, whitebox-testable algorithms
      (when (layout-test-runner-whitebox runner)
        (cond ((or (null algorithms) (null (test-algorithm-data (first algorithms))))
               (%init-error runner "Whitebox tests require explicit @Algorithm annotations."))
              (t
               (dolist (a algorithms)
                 (let* ((d (test-algorithm-data a))
                        (pool (get-instance-pool d))
                        (provider (fetch pool)))
                   (unless (typep provider 'i-white-box-testable)
                     (%init-error runner "Algorithm ~A is not whitebox testable." (get-id d)))
                   (release pool provider))))))
      (or algorithms (list (test-algorithm-identity))))))

(defun %initialize-configurations (runner class)
  (let* ((defaults (layout-test-class-defaults class))
         (flags (lambda (key) (and defaults (getf defaults key))))
         (configs '()))
    (flet ((make (kind name base)
             (make-test-configuration :kind kind :name name :base base
                                      :nodes (funcall flags :nodes)
                                      :ports (funcall flags :ports)
                                      :edges (funcall flags :edges)
                                      :edge-labels (funcall flags :edge-labels))))
      (dolist (entry (layout-test-class-configurator-providers class))
        (%init-catching runner
          (lambda ()
            (let ((cfg (funcall (cdr entry))))
              (push (make :configurator (car entry)
                          (lambda (graph) (elk-util.apply-visitors graph (vector cfg))))
                    configs)))))
      (dolist (entry (layout-test-class-configurators class))
        (push (make :method (car entry) (cdr entry)) configs)))
    (setf configs (nreverse configs))
    (or configs (list (make-test-configuration :kind :plain
                                               :nodes (funcall flags :nodes)
                                               :ports (funcall flags :ports)
                                               :edges (funcall flags :edges)
                                               :edge-labels (funcall flags :edge-labels))))))

(defun %build-experiments (runner algorithms configurations graphs)
  "ExperimentalObject.inflate: algorithms x configurations x graphs."
  (dolist (a algorithms)
    (dolist (c configurations)
      (dolist (g graphs)
        (let ((e (make-experiment :algorithm a :configuration c :graph g
                                  :id (incf *experiment-counter*) :runner runner)))
          (setf (layout-test-runner-experiments runner)
                (append (layout-test-runner-experiments runner) (list e))))))))

(defun jlist-to-list (coll)
  "Any Java-collection value (jlist, jset, CL list) as a CL list."
  (let ((out '())) (jdo (x coll) (push x out)) (nreverse out)))

;;; ------------------------------------------------------------ running one experiment

(defun %run-body (function args)
  "Run FUNCTION on ARGS.  Returns (values status message): :pass, :fail for a
JUnit assertion, :error for anything else."
  ;; Java float semantics for the test body too: NaN comparisons are false, not traps.
  (handler-case (progn (with-java-floats (apply function args)) (values :pass nil))
    (assertion-failure (c) (values :fail (assertion-failure-message c)))
    (error (c) (values :error (princ-to-string c)))))

(defun %experiment-statement-run (experiment)
  "ExperimentStatement.evaluate(): realize, lay out with the listener, then
run the blackbox tests and report the whitebox tests."
  (let* ((runner (experiment-runner experiment))
         (stmt (make-instance 'experiment-statement))
         (results '()))
    (setf (.experiment stmt) experiment)
    (flet ((emit (test status message)
             (push (make-test-result :experiment (experiment-description experiment)
                                     :test (test-method-display-name test)
                                     :id (experiment-id experiment)
                                     :status status :message message)
                   results)))
      (handler-case
          (let* ((graph (test-graph-provide (experiment-graph experiment)))
                 (controller (when (layout-test-runner-whitebox runner)
                               (new-test-controller (get-id (test-algorithm-data
                                                            (experiment-algorithm experiment)))))))
            (test-algorithm-apply (experiment-algorithm experiment) graph)
            (test-configuration-apply (experiment-configuration experiment) graph)
            (when controller
              (add-layout-execution-listener controller stmt))
            (layout (new-recursive-graph-layout-engine) graph controller
                    (new-basic-progress-monitor))
            (when controller
              (remove-layout-execution-listener controller stmt))
            (setf (.results stmt) nil)
            ;; blackbox tests, on the same realized graph
            (dolist (test (layout-test-runner-blackbox runner))
              (if (test-method-ignore test)
                  (emit test :ignored nil)
                  (multiple-value-bind (status message)
                      (%run-body (test-method-function test) (list graph))
                    (emit test status message))))
            ;; whitebox tests, reported from the recorder
            (dolist (test (layout-test-runner-whitebox runner))
              (%report-whitebox stmt test #'emit)))
        (error (c)
          ;; the experiment itself failed: every test of it is an error
          (dolist (test (append (layout-test-runner-blackbox runner)
                                (layout-test-runner-whitebox runner)))
            (emit test :error (format nil "experiment failed: ~A" c))))))
    (nreverse results)))

(defun %whitebox-record (stmt test)
  (or (gethash test (.recorder stmt))
      (setf (gethash test (.recorder stmt)) (cons nil nil))))

(defun %es-run-whitebox (stmt table processor graph is-root)
  "ExperimentStatement.runWhiteboxTests: the tests registered for PROCESSOR's class."
  (let ((runner (experiment-runner (.experiment stmt))))
    (dolist (test (gethash (type-of processor) table))
      (when (or is-root (not (test-method-only-on-root test)))
        (unless (test-method-ignore test)
          (let ((record (%whitebox-record stmt test)))
            (setf (car record) t)
            (multiple-value-bind (status message)
                (%run-body (test-method-function test) (list graph))
              (unless (eq status :pass)
                (setf (cdr record) (append (cdr record) (list (cons status message))))))))))
    runner))

(defjmethod layout-processor-ready ((this experiment-statement) processor graph is-root)
  (%es-run-whitebox this (layout-test-runner-before-table (experiment-runner (.experiment this)))
                    processor graph is-root))

(defjmethod layout-processor-finished ((this experiment-statement) processor graph is-root)
  (%es-run-whitebox this (layout-test-runner-after-table (experiment-runner (.experiment this)))
                    processor graph is-root))

(defun %report-whitebox (stmt test emit)
  "ExperimentStatement.simulateWhiteboxTestsForNotifiers, for one test."
  (cond ((test-method-ignore test) (funcall emit test :ignored nil))
        (t
         (let* ((record (gethash test (.recorder stmt)))
                (executed (and record (car record)))
                (failures (and record (cdr record))))
           (cond ((and executed failures)
                  (funcall emit test
                           (if (some (lambda (f) (eq (car f) :error)) failures) :error :fail)
                           (format nil "~{~A~^; ~}" (mapcar #'cdr failures))))
                 ((and (not executed) (test-method-fail-if-not-executed test))
                  (funcall emit test :fail "Test was not executed, but was supposed to."))
                 (t (funcall emit test :pass nil)))))))

;;; ------------------------------------------------------------ RUN-ELK-TESTS

(defstruct test-run
  (pass 0) (fail 0) (error 0) (ignored 0)
  (results '()))

(defun %tally (run result)
  (ecase (test-result-status result)
    (:pass (incf (test-run-pass run)))
    (:fail (incf (test-run-fail run)))
    (:error (incf (test-run-error run)))
    (:ignored (incf (test-run-ignored run))))
  (push result (test-run-results run)))

(defun run-elk-tests (&optional (class-name :all) (stream *standard-output*))
  "Run the registered layout test class CLASS-NAME (a symbol or string), or
every class for :ALL (also NIL).  Prints one line per test result and a summary,
and returns a TEST-RUN.  Each experiment (algorithm x configuration x graph) is
reported under its own heading."
  (install-label-metrics)
  (let ((classes (if (member class-name '(:all nil))
                     *layout-test-classes*
                     (let ((c (find-layout-test-class class-name)))
                       (unless c (error "No layout test class named ~A" class-name))
                       (list c))))
        (run (make-test-run)))
    (dolist (class classes)
      (format stream "~&== ~A~%" (layout-test-class-name class))
      (let ((runner (handler-case (%initialize-layout-test class)
                      (error (c)
                        (make-layout-test-runner :class class
                                                 :errors (list (format nil "~A" c)))))))
        (cond ((layout-test-runner-errors runner)
               (dolist (e (layout-test-runner-errors runner))
                 (format stream "   INIT ERROR: ~A~%" e)
                 (%tally run (make-test-result :experiment "(class)"
                                               :test (symbol-name (layout-test-class-name class))
                                               :status :error :message e))))
              (t
               (dolist (e (layout-test-runner-experiments runner))
                 (format stream "-- ~A~%" (experiment-description e))
                 (dolist (r (%experiment-statement-run e))
                   (%tally run r)
                   (format stream "   ~A ~A (~A)~@[: ~A~]~%"
                           (ecase (test-result-status r)
                             (:pass "PASS  ") (:fail "FAIL  ") (:error "ERROR ") (:ignored "IGNORED"))
                           (test-result-test r) (test-result-id r)
                           (and (not (eq (test-result-status r) :pass)) (test-result-message r)))))))))
    (setf (test-run-results run) (nreverse (test-run-results run)))
    (format stream "~&Summary: ~D passed, ~D failed, ~D errors, ~D ignored (~D total)~%"
            (test-run-pass run) (test-run-fail run) (test-run-error run) (test-run-ignored run)
            (+ (test-run-pass run) (test-run-fail run) (test-run-error run) (test-run-ignored run)))
    run))
