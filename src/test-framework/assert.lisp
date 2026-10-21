;;;; assert.lisp -- the JUnit Assert methods that ELK tests use.
;;;;
;;;; Names follow the call-macro convention in jcalls.lisp.  The generator
;;;; registers these names as INSTANCE calls (an unqualified Java call such as
;;;; assertTrue(c) has no receiver), so the suffix is the argument count minus
;;;; one:  (assert-true c) = ASSERT-TRUE/0,  (assert-true msg c) = ASSERT-TRUE/1,
;;;; (assert-equals e a) = ASSERT-EQUALS/1,  (assert-equals msg e a) = ASSERT-EQUALS/2.
;;;; As in JUnit, assertEquals(expected, actual) takes no message.

(in-package #:elk)

(defun %tf-assertion-failed (message)
  (error 'assertion-failure :message (or message "")))

(defun %tf-msg-prefix (message)
  (if (and message (plusp (length message))) (concatenate 'string message " ==> ") ""))

(defun fail/0 (message) (%tf-assertion-failed message))

(defun assert-true/0 (condition) (assert-true/1 nil condition))
(defun assert-true/1 (message condition)
  (unless condition
    (%tf-assertion-failed (format nil "~Aexpected true" (%tf-msg-prefix message)))))

(defun assert-false/0 (condition) (assert-false/1 nil condition))
(defun assert-false/1 (message condition)
  (when condition
    (%tf-assertion-failed (format nil "~Aexpected false" (%tf-msg-prefix message)))))

(defun assert-null/0 (object) (assert-null/1 nil object))
(defun assert-null/1 (message object)
  (when object
    (%tf-assertion-failed (format nil "~Aexpected null" (%tf-msg-prefix message)))))

(defun assert-not-null/0 (object) (assert-not-null/1 nil object))
(defun assert-not-null/1 (message object)
  (unless object
    (%tf-assertion-failed (format nil "~Aexpected not null" (%tf-msg-prefix message)))))

(defun %tf-assert-equal (message expected actual)
  (unless (jequals expected actual)
    (%tf-assertion-failed
     (format nil "~Aexpected:<~A> but was:<~A>" (%tf-msg-prefix message) expected actual))))

(defun assert-equals/1 (expected actual) (%tf-assert-equal nil expected actual))
(defun assert-equals/2 (message expected actual) (%tf-assert-equal message expected actual))
