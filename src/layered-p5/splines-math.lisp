;;;; splines-math.lisp -- org.eclipse.elk.alg.layered.p5edges.splines.SplinesMath
;;;;
;;;; Java overloads that share an arity but differ in parameter types
;;;; (intersect, innerAngle, convertKVectorToString) are one function here
;;;; that dispatches on the argument types.

(in-package #:elk)

(defjconst splines-math.epsilon 0.00000001d0)
(defjconst splines-math.null-string "(null)")
(defjconst splines-math.half-pi (/ math.pi 2))
(defjconst splines-math.quater-pi (/ splines-math.half-pi 2))
(defjconst splines-math.three-half-pi (+ splines-math.half-pi splines-math.half-pi splines-math.half-pi))
(defjconst splines-math.two-pi (* 2.0d0 math.pi))
(defjconst splines-math.three 3)

(defun %splines-intersect-points (pt1 pt2 pt3 pt4)
  (let* ((x1 (.x pt1)) (y1 (.y pt1))
         (x2 (.x pt2)) (y2 (.y pt2))
         (x3 (.x pt3)) (y3 (.y pt3))
         (x4 (.x pt4)) (y4 (.y pt4))
         (divisor (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))))
    (if (< (math.abs divisor) splines-math.epsilon)
        ;; The two lines are parallel or even coincident
        nil
        (let* ((first (- (* x1 y2) (* y1 x2)))
               (second (- (* x3 y4) (* y3 x4)))
               (new-x (/ (- (* first (- x3 x4)) (* second (- x1 x2))) divisor))
               (new-y (/ (- (* first (- y3 y4)) (* second (- y1 y2))) divisor)))
          (new-k-vector/2 new-x new-y)))))

(defjstatic splines-math.intersect (pt1 pt2 a b)
  ;; intersect(KVector, KVector, double dirPT1, double dirPT2) and
  ;; intersect(KVector, KVector, KVector, KVector) share this body.
  (if (typep a 'k-vector)
      (%splines-intersect-points pt1 pt2 a b)
      (%splines-intersect-points pt1
                                 (add (new-k-vector/1 a) pt1)
                                 pt2
                                 (add (new-k-vector/1 b) pt2))))

(defjstatic splines-math.inner-angle (a b)
  (if (typep a 'k-vector)
      (math.acos (/ (+ (* (.x a) (.x b)) (* (.y a) (.y b)))
                    (* (j-length a) (j-length b))))
      ;; innerAngle(double dir1, double dir2)
      (rem (math.abs (- a b)) math.pi)))

(defun %splines-vector-string (v)
  (if (null v)
      splines-math.null-string
      (format nil "(~,1F,~,1F)" (.x v) (.y v))))

(defjstatic splines-math.convert-k-vector-to-string (v)
  ;; Debug output only; the Java list overloads are folded in here.
  (cond ((null v) splines-math.null-string)
        ((typep v 'k-vector) (%splines-vector-string v))
        (t (let ((parts '()))
             (jdo (x v) (push (%splines-vector-string x) parts))
             (if (null parts)
                 splines-math.null-string
                 (format nil "~{~A~^, ~}" (nreverse parts)))))))

(defjstatic splines-math.length-to-orthogonal (direction point)
  (let* ((angle (splines-math.inner-angle direction (to-radians point)))
         (factor 1))
    (when (> angle splines-math.half-pi)
      ;; point lays in the opposite direction
      (setf factor -1)
      (setf angle (- angle splines-math.quater-pi)))
    (* factor (math.cos angle) (j-length point))))

(defjstatic splines-math.port-side-to-direction (side)
  (enum-case side
    (port-side.north splines-math.three-half-pi)
    (port-side.east 0.0d0)
    (port-side.south splines-math.half-pi)
    (port-side.west math.pi)
    (t 0.0d0)))

(defjstatic splines-math.dist-port-to-node-edge (port side)
  (let ((port-pos (add (clone (get-position port)) (get-anchor port)))
        (node-size (get-size (get-node port))))
    (enum-case side
      (port-side.north (- (.y port-pos)))
      (port-side.east (+ (- (.x port-pos)) (.x node-size)))
      (port-side.south (+ (- (.y port-pos)) (.y node-size)))
      (port-side.west (- (.x port-pos)))
      (t 0.0d0))))

(defjstatic splines-math.is-between (value boundary0 boundary1)
  (cond ((or (< (math.abs (- boundary0 value)) splines-math.epsilon)
             (< (math.abs (- boundary1 value)) splines-math.epsilon))
         t)
        ((> (- boundary0 value) splines-math.epsilon)
         (> (- value boundary1) splines-math.epsilon))
        (t (> (- boundary1 value) splines-math.epsilon))))

(defjstatic splines-math.get-margin-on-port-side (node side)
  (let ((margin (get-margin node)))
    (enum-case side
      (port-side.north (.top margin))
      (port-side.east (.right margin))
      (port-side.south (.bottom margin))
      (port-side.west (.left margin))
      (t 0.0d0))))
