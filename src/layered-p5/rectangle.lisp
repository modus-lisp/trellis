;;;; rectangle.lisp -- org.eclipse.elk.alg.layered.p5edges.splines.Rectangle
;;;;
;;;; The rectangle is kept by its four outer coordinates (top, bottom, left,
;;;; right); the field defaults in classes.lisp give Java's empty rectangle.

(in-package #:elk)

(defjctor rectangle (left top right bottom)
  (when (> top bottom)
    (jthrow 'illegal-argument-exception "Top must be smaller or equal to bottom."))
  (when (> left right)
    (jthrow 'illegal-argument-exception "Left must be smaller or equal to right."))
  (setf (.top this) top
        (.right this) right
        (.bottom this) bottom
        (.left this) left))

(defun %rect-include-vector (this v)
  (setf (.top this) (math.min (.top this) (.y v))
        (.right this) (math.max (.right this) (.x v))
        (.bottom this) (math.max (.bottom this) (.y v))
        (.left this) (math.min (.left this) (.x v))))

(defun %rect-include-rectangle (this other)
  (setf (.top this) (math.min (.top this) (.top other))
        (.right this) (math.max (.right this) (.right other))
        (.bottom this) (math.max (.bottom this) (.bottom other))
        (.left this) (math.min (.left this) (.left other))))

;; Rectangle(Rectangle) | Rectangle(KVector position) | Rectangle(LShape) |
;; Rectangle(Iterable<KVector>) and the KVector... varargs form, which
;; shares the iterable body: a sequence of vectors.
(defjctor rectangle (a)
  (cond ((typep a 'rectangle)
         (setf (.top this) (.top a)
               (.right this) (.right a)
               (.bottom this) (.bottom a)
               (.left this) (.left a)))
        ((typep a 'k-vector)
         (setf (.top this) (.y a)
               (.bottom this) (.y a)
               (.right this) (.x a)
               (.left this) (.x a)))
        ((typep a 'l-shape)
         (let* ((position (get-position a))
                (extend (add (clone position) (get-size a))))
           (setf (.top this) (math.min (.y position) (.y extend))
                 (.bottom this) (math.max (.y position) (.y extend))
                 (.left this) (math.min (.x position) (.x extend))
                 (.right this) (math.max (.x position) (.x extend)))))
        (t
         (when (zerop (jsize a))
           (jthrow 'illegal-argument-exception "The list of vectors may not be empty."))
         (jdo (v a) (%rect-include-vector this v)))))

(defjmethod j-union ((this rectangle) other)
  ;; union(KVector) | union(LShape) | union(Rectangle)
  (cond ((typep other 'rectangle) (%rect-include-rectangle this other))
        ((typep other 'k-vector) (%rect-include-vector this other))
        (t (let ((position (get-position other)))
             (%rect-include-vector this position)
             (%rect-include-vector this (add (clone position) (get-size other))))))
  nil)

(defjstatic rectangle.union (rectangle1 rectangle2)
  (let ((ret-val (new-rectangle/1 rectangle1)))
    (j-union ret-val rectangle2)
    ret-val))

(defjstatic rectangle.union (rectangles)
  ;; Rectangle.union(Collection<Rectangle>).  Kept as Java writes it: the
  ;; loop unions into the FIRST rectangle, not into the copy it returns, so
  ;; the result is only the first rectangle.  Reported, not corrected.
  (let ((items (to-cl-list rectangles)))
    (if (null items)
        (jthrow 'illegal-argument-exception "The list of vectors may not be null.")
        (let* ((rectangle (first items))
               (ret-val (new-rectangle/1 rectangle)))
          (dolist (other (rest items))
            (j-union rectangle other))
          ret-val))))

(defjmethod enlarge ((this rectangle) enlargement)
  (setf (.top this) (- (.top this) enlargement)
        (.left this) (- (.left this) enlargement)
        (.right this) (+ (.right this) enlargement)
        (.bottom this) (+ (.bottom this) enlargement))
  nil)

(defjmethod get-height ((this rectangle)) (- (.bottom this) (.top this)))
(defjmethod get-width ((this rectangle)) (- (.right this) (.left this)))
(defjmethod get-top ((this rectangle)) (.top this))
(defjmethod get-right ((this rectangle)) (.right this))
(defjmethod get-bottom ((this rectangle)) (.bottom this))
(defjmethod get-left ((this rectangle)) (.left this))

(defjmethod get-from-port-side ((this rectangle) side)
  (enum-case side
    (port-side.north (.top this))
    (port-side.east (.right this))
    (port-side.south (.bottom this))
    (port-side.west (.left this))
    (t 0.0d0)))

(defjmethod to-node-margins ((this rectangle) shape)
  (let* ((ret-val (new-elk-margin))
         (size (get-size shape))
         (shape-rectangle (new-rectangle/4 0d0 0d0 (.x size) (.y size))))
    (setf (.top ret-val) (math.max 0.0d0 (- (.top shape-rectangle) (.top this)))
          (.left ret-val) (math.max 0.0d0 (- (.left shape-rectangle) (.left this)))
          (.bottom ret-val) (math.max 0.0d0 (- (.bottom this) (.bottom shape-rectangle)))
          (.right ret-val) (math.max 0.0d0 (- (.right this) (.right shape-rectangle))))
    ret-val))
