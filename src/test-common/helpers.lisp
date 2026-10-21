;;;; helpers.lisp -- assertion helpers for the common tests that have no
;;;; mechanical name.  Private helpers are %-prefixed so the call-macro table
;;;; (jcalls.lisp) does not rewrite them.
;;;;
;;;; JUnit's assertEquals(expected, actual, delta) is NOT spelled ASSERT-EQUALS
;;;; here: the arity-based call macros send a 3-argument call to ASSERT-EQUALS/2,
;;;; which is the (message expected actual) form.  See BLOCKERS.md.

(in-package #:elk)

(defun %assert-equals-delta (expected actual delta)
  (unless (<= (abs (- expected actual)) delta)
    (%tf-assertion-failed
     (format nil "expected:<~A> but was:<~A> (delta ~A)" expected actual delta))))

(defun %assert-equals-delta-m (message expected actual delta)
  (unless (<= (abs (- expected actual)) delta)
    (%tf-assertion-failed
     (format nil "~A expected:<~A> but was:<~A> (delta ~A)"
             message expected actual delta))))

(defun %close-to-p (actual expected delta)
  "Hamcrest closeTo: |actual - expected| <= delta."
  (<= (abs (- actual expected)) delta))

(defun %elements-equal-p (a b)
  "Guava Iterables.elementsEqual: same length, pairwise equals."
  (let ((la (coerce a 'list)) (lb (coerce b 'list)))
    (and (= (length la) (length lb))
         (every #'jequals la lb))))

(defmacro %jchain (form &rest calls)
  "Thread FORM through CALLS, each a call form whose first argument is
supplied: (%jchain x (f a) (g)) = (g (f x a))."
  (reduce (lambda (acc call) (list* (first call) acc (rest call)))
          calls :initial-value form))
