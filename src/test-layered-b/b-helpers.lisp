;;;; b-helpers.lisp -- helpers shared by the test-layered-b ports.
;;;;
;;;; JUnit's Assert overloads with a double delta (assertEquals(d, d, delta),
;;;; assertNotEquals(d, d, delta), and the message variants of those) collide
;;;; with the arity-keyed ASSERT-EQUALS/2 and ASSERT-EQUALS/3 names, so they are
;;;; written out as %B-ASSERT-* functions here.  Guava DoubleMath.fuzzyEquals and
;;;; java.awt Rectangle2D containment are also written out.

(in-package #:elk)

(defun %b-with-ext (path &rest extensions)
  "new ModelResourcePath(p).withFilter(new FileExtensionFilter(exts...))."
  (setf (abstract-resource-path-filter path) (apply #'file-extension-filter extensions))
  path)

(defun %b-with-name-filter (path predicate)
  "new ModelResourcePath(p).withFilter(new FileNameFilter(regex)); PREDICATE is on the name."
  (setf (abstract-resource-path-filter path) (file-name-filter predicate))
  path)

(defun %b-name-matches-p (name prefix suffix)
  "Full match of the regex PREFIX.+SUFFIX (or PREFIX.+ when SUFFIX is empty)."
  (let ((n (length name)) (p (length prefix)) (s (length suffix)))
    (and (> n (+ p s))
         (string= prefix name :end2 p)
         (string= suffix name :start2 (- n s)))))

(defun %b-fuzzy-equals (a b tolerance)
  "com.google.common.math.DoubleMath.fuzzyEquals."
  (or (= a b) (<= (abs (- a b)) tolerance)))

(defun %b-assert-equals-delta (message expected actual delta)
  "JUnit assertEquals(message, expected, actual, delta) for doubles."
  (unless (%b-fuzzy-equals expected actual delta)
    (fail (format nil "~@[~A ~]expected:<~A> but was:<~A>" message expected actual))))

(defun %b-assert-not-equals-delta (message unexpected actual delta)
  "JUnit assertNotEquals(message, unexpected, actual, delta) for doubles."
  (when (%b-fuzzy-equals unexpected actual delta)
    (fail (format nil "~@[~A ~]expected not equal to:<~A> but was:<~A>" message unexpected actual))))

(defun %b-contains-all-p (collection items)
  "Collection.containsAll."
  (let ((ok t))
    (jdo (item items)
      (unless (contains collection item) (setf ok nil)))
    ok))

(defun %b-element-with-id (container id)
  "ElkGraphElement lookup by identifier: stream().filter(...).findFirst().get()."
  (block found
    (jdo (e container)
      (when (jequals (get-identifier e) id)
        (return-from found e)))
    (error "NoSuchElementException: no element with identifier ~A" id)))

;;; java.awt.geom.Rectangle2D.Double, represented as (x y w h).

(defun %b-rect2d-empty-p (rect)
  (destructuring-bind (x y w h) rect
    (declare (ignore x y))
    (or (<= w 0) (<= h 0))))

(defun %b-rect2d-contains-rect-p (outer inner)
  "Rectangle2D.contains(Rectangle2D)."
  (destructuring-bind (x0 y0 w0 h0) outer
    (destructuring-bind (x y w h) inner
      (and (not (%b-rect2d-empty-p outer))
           (> w 0) (> h 0)
           (>= x x0) (>= y y0)
           (<= (+ x w) (+ x0 w0))
           (<= (+ y h) (+ y0 h0))))))

(defun %b-rect2d-contains-point-p (rect x y)
  "Rectangle2D.contains(x, y): half-open on the right and bottom edges."
  (destructuring-bind (x0 y0 w h) rect
    (and (not (%b-rect2d-empty-p rect))
         (>= x x0) (>= y y0)
         (< x (+ x0 w)) (< y (+ y0 h)))))


;;; Plain JUnit classes (no LayoutTestRunner) run through this runner.

(defun %b-run-plain-tests (class-name tests)
  "TESTS is a list of (name . thunk).  Prints one line per test and a Summary line."
  (let ((pass 0) (fail-count 0) (error-count 0))
    (dolist (test tests)
      (handler-case
          (progn (funcall (cdr test))
                 (incf pass)
                 (format t "~&~A ~A PASS~%" class-name (car test)))
        (assertion-failure (c)
          (incf fail-count)
          (format t "~&~A ~A FAIL: ~A~%" class-name (car test) c))
        (error (c)
          (incf error-count)
          (format t "~&~A ~A ERROR: ~A~%" class-name (car test) c))))
    (format t "~&Summary ~A: pass=~D failed=~D errors=~D~%"
            class-name pass fail-count error-count)
    (values pass fail-count error-count)))

(defun %b-count-matching (predicate collection)
  "Number of elements of COLLECTION (an E-list, jlist or any jdo-able) satisfying PREDICATE."
  (let ((n 0))
    (jdo (x collection)
      (when (funcall predicate x) (incf n)))
    n))
