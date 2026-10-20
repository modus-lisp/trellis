;;;; util.lisp -- a java.util.ListIterator over a jlist, for the code that
;;;; edits lists while walking them (NubSpline's knot insertion).

(in-package #:elk)

(defstruct (%p5-li (:constructor %p5-list-iterator (list &optional (cursor 0))))
  list cursor (last -1))

(defun %p5-li-has-next (it) (< (%p5-li-cursor it) (length (%p5-li-list it))))
(defun %p5-li-has-previous (it) (> (%p5-li-cursor it) 0))
(defun %p5-li-next (it)
  (let ((i (%p5-li-cursor it)))
    (prog1 (jl-get (%p5-li-list it) i)
      (setf (%p5-li-cursor it) (1+ i) (%p5-li-last it) i))))
(defun %p5-li-previous (it)
  (let ((i (1- (%p5-li-cursor it))))
    (prog1 (jl-get (%p5-li-list it) i)
      (setf (%p5-li-cursor it) i (%p5-li-last it) i))))
(defun %p5-li-next-index (it) (%p5-li-cursor it))
(defun %p5-li-remove (it)
  (let ((last (%p5-li-last it)))
    (when (< last 0) (jthrow 'illegal-state-exception))
    (jl-remove-at (%p5-li-list it) last)
    (when (< last (%p5-li-cursor it)) (decf (%p5-li-cursor it)))
    (setf (%p5-li-last it) -1)))
(defun %p5-li-add (it x)
  (jl-add-at (%p5-li-list it) (%p5-li-cursor it) x)
  (incf (%p5-li-cursor it))
  (setf (%p5-li-last it) -1))

(defun %p5-float (x)
  "Java (float) narrowing of a double, widened back."
  (float (coerce x 'single-float) 1d0))
