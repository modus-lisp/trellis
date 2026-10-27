;;;; util.lisp -- numbers, geometry, a heap, a seeded random source.

(in-package #:trellis)

(deftype real-number () 'double-float)

(declaim (inline d finite-p))
(defun d (x)
  "X as a double-float."
  (coerce x 'double-float))

(defun finite-p (x)
  (and (realp x)
       (or (rationalp x)
           (and (= x x)                 ; not NaN
                (< (abs x) most-positive-double-float)))))

(defun sane (x &optional (default 0d0))
  "X as a finite double, or DEFAULT when X is missing, NaN or infinite."
  (if (and (realp x) (finite-p x)) (d x) (d default)))

(defun clamp (x lo hi) (max lo (min hi x)))

(defun approx= (a b &optional (eps 1d-6)) (<= (abs (- a b)) eps))

;;; ---- points: conses (x . y) ------------------------------------------------

(declaim (inline pt px py))
(defun pt (x y) (cons (d x) (d y)))
(defun px (p) (car p))
(defun py (p) (cdr p))

(defun pt+ (a b) (pt (+ (px a) (px b)) (+ (py a) (py b))))

(defun remove-collinear (points)
  "Drop repeated points and points lying on the straight line between their
neighbours.  POINTS is a list of (x . y)."
  (let ((out '()))
    (dolist (p points)
      (cond ((and out (approx= (px p) (px (first out)) 1d-9)
                  (approx= (py p) (py (first out)) 1d-9)))
            ((and (cdr out)
                  (let ((a (second out)) (b (first out)))
                    (approx= (* (- (px b) (px a)) (- (py p) (py a)))
                             (* (- (py b) (py a)) (- (px p) (px a)))
                             1d-6)))
             ;; b is between a and p on one line -- unless the path turns back
             (let ((a (second out)) (b (first out)))
               (if (>= (+ (* (- (px b) (px a)) (- (px p) (px b)))
                          (* (- (py b) (py a)) (- (py p) (py b))))
                       0)
                   (setf (first out) p)
                   (push p out))))
            (t (push p out))))
    (nreverse out)))

;;; ---- rectangles ------------------------------------------------------------

(defstruct (rect (:constructor rect (x y w h)))
  (x 0d0 :type double-float) (y 0d0 :type double-float)
  (w 0d0 :type double-float) (h 0d0 :type double-float))

(defun rect-right (r) (+ (rect-x r) (rect-w r)))
(defun rect-bottom (r) (+ (rect-y r) (rect-h r)))

(defun rect-union (a b)
  (cond ((null a) b) ((null b) a)
        (t (let ((x (min (rect-x a) (rect-x b))) (y (min (rect-y a) (rect-y b))))
             (rect x y (- (max (rect-right a) (rect-right b)) x)
                   (- (max (rect-bottom a) (rect-bottom b)) y))))))

;;; ---- spacing records: (top left bottom right) ------------------------------

(defstruct (pad (:constructor pad (&optional (top 0d0) (left 0d0) (bottom 0d0) (right 0d0))))
  (top 0d0 :type double-float) (left 0d0 :type double-float)
  (bottom 0d0 :type double-float) (right 0d0 :type double-float))

;;; ---- binary min-heap keyed by a number -------------------------------------

(defstruct (heap (:constructor make-heap ()))
  (items (make-array 16 :adjustable t :fill-pointer 0)))

(defun heap-empty-p (h) (zerop (fill-pointer (heap-items h))))

(defun heap-push (h key item)
  (let ((a (heap-items h)))
    (vector-push-extend (cons key item) a)
    (loop with i = (1- (fill-pointer a))
          while (> i 0)
          do (let ((p (floor (1- i) 2)))
               (if (< (car (aref a i)) (car (aref a p)))
                   (progn (rotatef (aref a i) (aref a p)) (setf i p))
                   (return))))))

(defun heap-pop (h)
  "The item with the least key, and its key."
  (let* ((a (heap-items h)) (top (aref a 0)) (last (vector-pop a)))
    (when (plusp (fill-pointer a))
      (setf (aref a 0) last)
      (loop with i = 0 and n = (fill-pointer a)
            do (let* ((l (+ (* 2 i) 1)) (r (1+ l)) (m i))
                 (when (and (< l n) (< (car (aref a l)) (car (aref a m)))) (setf m l))
                 (when (and (< r n) (< (car (aref a r)) (car (aref a m)))) (setf m r))
                 (if (= m i) (return)
                     (progn (rotatef (aref a i) (aref a m)) (setf i m))))))
    (values (cdr top) (car top))))

;;; ---- deterministic random numbers ------------------------------------------
;;; A 64-bit xorshift*: the same seed gives the same layout on every Lisp.

(defstruct (rng (:constructor %make-rng (state)))
  (state 1 :type (unsigned-byte 64)))

(defun make-rng (seed)
  (%make-rng (let ((s (logand (+ (* (abs (round (sane seed 1))) 2654435761) 88172645463325252)
                              #xFFFFFFFFFFFFFFFF)))
               (if (zerop s) 88172645463325252 s))))

(defun rng-next (r)
  (let ((x (rng-state r)))
    (setf x (logxor x (ash x -12))
          x (logxor x (logand (ash x 25) #xFFFFFFFFFFFFFFFF))
          x (logxor x (ash x -27)))
    (setf (rng-state r) x)
    (logand (* x 2685821657736338717) #xFFFFFFFFFFFFFFFF)))

(defun rng-float (r)
  "A double in [0, 1)."
  (* (d (ash (rng-next r) -11)) (scale-float 1d0 -53)))

(defun rng-int (r n)
  "An integer in [0, N)."
  (if (<= n 1) 0 (mod (ash (rng-next r) -11) n)))

(defun rng-shuffle (r vector)
  (loop for i from (1- (length vector)) downto 1
        do (rotatef (aref vector i) (aref vector (rng-int r (1+ i)))))
  vector)

;;; ---- misc ------------------------------------------------------------------

(defun hash-keys (h) (loop for k being the hash-keys of h collect k))

(defun group-by (key list &key (test #'eql))
  "An alist (key . items) preserving first-seen order of keys."
  (let ((groups '()))
    (dolist (x list)
      (let* ((k (funcall key x)) (g (assoc k groups :test test)))
        (if g (push x (cdr g)) (push (list k x) groups))))
    (mapcar (lambda (g) (cons (car g) (nreverse (cdr g)))) (nreverse groups))))
