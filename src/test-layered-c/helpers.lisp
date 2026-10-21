;;;; helpers.lisp -- module-local test helpers for the layered-c batch.
;;;;
;;;; JUnit's assertEquals(double expected, double actual, double delta) and
;;;; assertThat(x, closeTo(...)) are not in test-framework/assert.lisp, so they
;;;; are ported here under explicit names rather than as ASSERT-EQUALS/3.
;;;;
;;;; PLAIN-JUNIT tests (classes with no LayoutTestRunner and no graph
;;;; resource: they build their own graph or call an internal class) are
;;;; registered with DEFPLAINTEST and run by RUN-PLAIN-JUNIT, which prints the
;;;; same PASS/FAIL/ERROR shape as RUN-ELK-TESTS.

(in-package #:elk)

(defun tlc-assert-equals-delta (message expected actual delta)
  "org.junit.Assert.assertEquals(String, double, double, double)."
  (unless (<= (abs (- expected actual)) delta)
    (%tf-assertion-failed
     (format nil "~@[~A: ~]expected ~A but was ~A (delta ~A)" message expected actual delta))))

(defun tlc-assert-equals-delta/3 (expected actual delta)
  (tlc-assert-equals-delta nil expected actual delta))

(defvar *plain-junit-tests* '()
  "Alist of (CLASS-NAME . ((TEST-NAME . THUNK) ...)) in definition order.")

(defmacro defplaintest (class name () &body body)
  "A JUnit @Test of a class that is not run by LayoutTestRunner."
  `(%register-plain-junit ',class ',name (lambda () ,@body)))

(defun %register-plain-junit (class name thunk)
  (let ((entry (assoc class *plain-junit-tests*)))
    (unless entry
      (setf entry (list class))
      (setf *plain-junit-tests* (append *plain-junit-tests* (list entry))))
    (setf (cdr entry)
          (append (remove name (cdr entry) :key #'car)
                  (list (cons name thunk))))))

(defun run-plain-junit (class &optional (stream *standard-output*))
  "Run every DEFPLAINTEST of CLASS.  Returns (values pass fail error)."
  (let ((entry (assoc class *plain-junit-tests*)) (pass 0) (fail 0) (err 0))
    (format stream "~&== ~A~%" class)
    (dolist (test (cdr entry))
      (multiple-value-bind (status message)
          (handler-case (progn (funcall (cdr test)) (values :pass nil))
            (assertion-failure (c) (values :fail (princ-to-string c)))
            (error (c) (values :error (princ-to-string c))))
        (ecase status
          (:pass (incf pass))
          (:fail (incf fail))
          (:error (incf err)))
        (format stream "   ~A ~A~@[: ~A~]~%"
                (ecase status (:pass "PASS ") (:fail "FAIL ") (:error "ERROR"))
                (car test) (and (not (eq status :pass)) message))))
    (format stream "~&Summary: ~D passed, ~D failed, ~D errors (~D total)~%"
            pass fail err (+ pass fail err))
    (values pass fail err)))

;;; GraphTestUtils.getChild / getPort (org.eclipse.elk.alg.test.GraphTestUtils).
;;; Java's varargs `String... childNames` is one parameter: pass a list of names.

(defjstatic graph-test-utils.get-child (parent child-names)
  "Walks the hierarchy by identifier, one name per level."
  (let ((current parent))
    (dolist (name child-names current)
      (setf current
            (or (find-if (lambda (c) (equal (get-identifier c) name)) (jlist-to-list (get-children current)))
                (error "No child ~A in ~A" name (get-identifier current)))))))

(defjstatic graph-test-utils.get-port (node port-name)
  (or (find-if (lambda (p) (equal (get-identifier p) port-name))
               (jlist-to-list (get-ports node)))
      (error "No port ~A on ~A" port-name (get-identifier node))))
