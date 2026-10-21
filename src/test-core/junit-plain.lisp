;;;; junit-plain.lisp -- the part of JUnit that the layout runner does not
;;;; cover: plain unit-test classes (no algorithm, no graph).
;;;;
;;;; DEFCORE-CLASS names a Java test class and its @Test methods.  Each test is
;;;; (NAME BODY...) -- a clause the JUnit method body maps onto one-for-one.
;;;; A test whose Java annotation is @Test(expected = X) is written with
;;;; ASSERT-THROWS (see below).  RUN-CORE-TESTS prints the same PASS/FAIL/ERROR
;;;; lines and "Summary:" line as RUN-ELK-TESTS, so the driver reads both alike.

(in-package #:elk)

(defvar *core-classes* '()
  "Plain test classes: list of (NAME . ((TEST-NAME . THUNK) ...)), in definition order.")

(defun core-register (name tests)
  "Register the plain class NAME.  TESTS: list of (\"javaName\" . thunk), or
(:ignore \"javaName\" . thunk) for a JUnit @Ignore'd method."
  (let ((entry (cons name tests)))
    (setf *core-classes*
          (append (remove name *core-classes* :key #'car) (list entry)))
    entry))

(defmacro defcore-class (name &body tests)
  "Define the plain JUnit class NAME.  TESTS: (\"javaTestName\" form...), or
((:ignore \"javaTestName\") form...) for a @Ignore'd method."
  `(core-register
    ',name
    (list ,@(loop for (tname . body) in tests
                  collect (if (consp tname)
                              `(list* :ignore ,(second tname) (lambda () ,@body))
                              `(cons ,tname (lambda () ,@body)))))))

(defmacro assert-throws (type &body body)
  "JUnit @Test(expected = TYPE): BODY must signal a condition of TYPE."
  `(handler-case (progn ,@body
                        (%tf-assertion-failed (format nil "expected ~A" ',type)))
     (assertion-failure (c) (error c))
     (,type () nil)))

(defun assert-near (expected actual delta)
  "JUnit assertEquals(double expected, double actual, double delta)."
  (unless (or (= expected actual) (<= (abs (- expected actual)) delta))
    (%tf-assertion-failed
     (format nil "expected:<~A> but was:<~A> (delta ~A)" expected actual delta))))

(defun run-core-tests (&optional (class-name :all) (stream *standard-output*))
  "Run the plain classes named CLASS-NAME (a symbol or :ALL).  Returns counts
as (values pass fail error)."
  (let ((pass 0) (fail 0) (err 0) (ignored 0))
    (dolist (entry *core-classes*)
      (when (or (eq class-name :all)
                (string-equal (symbol-name (car entry)) (symbol-name-or-string class-name)))
        (format stream "~&== ~A~%" (car entry))
        (dolist (test (cdr entry))
          (multiple-value-bind (status message)
              (handler-case (if (eq (car test) :ignore)
                                (values :ignored nil)
                                (progn (funcall (cdr test)) (values :pass nil)))
                (assertion-failure (c) (values :fail (assertion-failure-message c)))
                (error (c) (values :error (princ-to-string c))))
            (ecase status
              (:pass (incf pass))
              (:fail (incf fail))
              (:error (incf err))
              (:ignored (incf ignored)))
            (format stream "   ~A ~A~@[: ~A~]~%"
                    (ecase status (:pass "PASS ") (:fail "FAIL ") (:error "ERROR") (:ignored "IGNORED"))
                    (if (eq (car test) :ignore) (cadr test) (car test))
                    (and (not (member status '(:pass :ignored))) message))))))
    (format stream "~&Summary: ~D passed, ~D failed, ~D errors, ~D ignored (~D total)~%"
            pass fail err ignored (+ pass fail err ignored))
    (values pass fail err ignored)))
