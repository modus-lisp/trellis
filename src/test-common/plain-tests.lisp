;;;; plain-tests.lisp -- DEFTEST for JUnit tests that are plain unit tests (no
;;;; @RunWith(LayoutTestRunner)).  The layout framework has no plain-test form,
;;;; so this module supplies a minimal one.  Each test is a closure; a failed
;;;; assertion is recorded as FAIL, any other condition as ERROR.  The summary
;;;; line has the same shape as RUN-ELK-TESTS.

(in-package #:elk)

(defvar *plain-test-class* nil
  "Name of the plain test class currently being defined.")

(defvar *plain-test-table* (make-hash-table :test 'eq)
  "Class name -> list of (test-name . thunk), in declaration order.")

(defmacro define-plain-test-class (name)
  `(progn (setf *plain-test-class* ',name)
          (setf (gethash ',name *plain-test-table*) '())
          ',name))

(defun %register-plain-test (class name thunk)
  (setf (gethash class *plain-test-table*)
        (append (remove name (gethash class *plain-test-table*) :key #'car)
                (list (cons name thunk))))
  name)

(defmacro deftest (name () &body body)
  "Register the plain JUnit test NAME in the current plain test class."
  `(%register-plain-test *plain-test-class* ',name (lambda () ,@body)))

(defun run-plain-tests (class &optional (stream *standard-output*))
  "Run every test of plain class CLASS; print one line per test and a Summary
line.  Returns (values pass fail error)."
  (let ((pass 0) (fail 0) (err 0))
    (format stream "~&== ~A~%" class)
    (dolist (entry (gethash class *plain-test-table*))
      (let ((status :pass) (message nil))
        (handler-case (funcall (cdr entry))
          (assertion-failure (c)
            (setf status :fail message (format nil "~A" c)))
          (error (c)
            (setf status :error message (format nil "~A" c))))
        (ecase status
          (:pass (incf pass))
          (:fail (incf fail))
          (:error (incf err)))
        (format stream "   ~A ~A~@[: ~A~]~%"
                (ecase status (:pass "PASS  ") (:fail "FAIL  ") (:error "ERROR "))
                (car entry)
                (and (not (eq status :pass)) message))))
    (format stream "~&Summary: ~D passed, ~D failed, ~D errors, 0 ignored (~D total)~%"
            pass fail err (+ pass fail err))
    (values pass fail err)))
