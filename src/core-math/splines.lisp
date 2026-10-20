;;;; BezierSpline, BezierCurve, CubicSplineInterpolator

(in-package #:elk)

;;; ------------------------------------------------------------ BezierSpline

(defjmethod add-curve ((this bezier-spline) curve)
  (jl-add (.curves this) curve) nil)

(defjmethod add-spline ((this bezier-spline) spline beginning)
  (if (not beginning)
      (jl-add-all (.curves this) (copy-seq (get-curves/0 spline)))
      (jl-add-all-at (.curves this) 0 (copy-seq (get-curves/0 spline))))
  nil)

(defjmethod add-curve ((this bezier-spline) start-pnt fst-ctr-pnt snd-ctr-pnt end-pnt)
  (jl-add (.curves this) (new-bezier-curve/4 start-pnt fst-ctr-pnt snd-ctr-pnt end-pnt)) nil)

(defjmethod get-start-point ((this bezier-spline)) (.start (jl-first (.curves this))))
(defjmethod get-end-point ((this bezier-spline)) (.end (jl-last (.curves this))))

(defjmethod get-inner-points ((this bezier-spline))
  (let ((curves (.curves this)))
    (if (zerop (length curves))
        (vector)
        (let ((points (make-array (- (* (length curves) 3) 1) :initial-element nil))
              (i 0))
          (loop for ci from 0 below (length curves)
                for cu = (aref curves ci)
                do (setf (aref points i) (.fst-control-pnt cu)) (incf i)
                   (setf (aref points i) (.snd-control-pnt cu)) (incf i)
                   (when (< (1+ ci) (length curves))
                     (setf (aref points i) (.end cu)) (incf i)))
          points))))

(defjmethod get-base-points ((this bezier-spline))
  (let* ((curves (.curves this))
         (ret (make-array (1+ (length curves)) :initial-element nil))
         (i 1))
    (setf (aref ret 0) (.start (jl-first curves)))
    (jdo (curve curves) (setf (aref ret i) (.end curve)) (incf i))
    ret))

(defjmethod get-polyline-apprx ((this bezier-spline) accuracy)
  (let* ((curves (.curves this))
         (apprx (make-array (1+ (* (length curves) accuracy)) :initial-element nil))
         (i 1))
    (setf (aref apprx 0) (.start (jl-first curves)))
    (jdo (curve curves)
      (let ((pts (elk-math.approximate-bezier-segment/2 accuracy (as-array/0 curve))))
        (map nil (lambda (p) (setf (aref apprx i) (clone/0 p)) (incf i)) pts)))
    apprx))

(defjmethod get-curves ((this bezier-spline)) (.curves this))

(defjmethod to-string ((this bezier-spline))
  (with-output-to-string (s)
    (jdo (cu (.curves this))
      (write-string (jstr "" (to-string/0 (.start cu)) " -> " (to-string/0 (.fst-control-pnt cu))
                          " -> " (to-string/0 (.snd-control-pnt cu)) " -> " (to-string/0 (.end cu)))
                    s)
      (terpri s))))

;;; BezierSpline.BezierCurve
(defjctor bezier-curve (start-pnt fst-ctr-pnt snd-ctr-pnt end-pnt)
  (setf (.start this) start-pnt
        (.fst-control-pnt this) fst-ctr-pnt
        (.snd-control-pnt this) snd-ctr-pnt
        (.end this) end-pnt))

(defjmethod as-vector-list ((this bezier-curve))
  (jlist (.start this) (.fst-control-pnt this) (.snd-control-pnt this) (.end this)))

(defjmethod as-array ((this bezier-curve))
  (vector (.start this) (.fst-control-pnt this) (.snd-control-pnt this) (.end this)))

;;; ------------------------------------------------------------ CubicSplineInterpolator

(defparameter *cm-interp-coef-even*
  #(#(0.25)
    #(0.2677 -0.0667) #(0.2679 -0.0714 0.0179) #(0.2679 -0.0718 0.0191 -0.0048)
    #(0.2679 -0.0718 0.0192 -0.0051 0.0013)
    #(0.2679 -0.0718 0.0192 -0.0052 0.0014 -0.0003)
    #(0.2679 -0.0718 0.0192 -0.0052 0.0014 -0.0004 0.0001)))

(defparameter *cm-interp-coef-odd*
  #(#(0.3333)
    #(0.2727 -0.0909) #(0.2683 -0.0732 0.0244) #(0.2680 -0.0719 0.0196 -0.0065)
    #(0.2680 -0.0718 0.0193 -0.0053 0.0018)
    #(0.2679 -0.0718 0.0192 -0.0052 0.0014 -0.0005)
    #(0.2679 -0.0718 0.0192 -0.0052 0.0014 -0.0004 0.0001)))

(defconstant +cm-max-k+ 7)
(defconstant +cm-tangent-scale+ 0.25d0)

(defjmethod calculate-closed-bezier-spline ((this cubic-spline-interpolator) points)
  (let* ((points (coerce (to-jvector points) 'simple-vector))
         (spline (new-bezier-spline/0))
         (n (length points))
         (even (evenp n))
         (m (min (if even (idiv (- n 2) 2) (idiv (- n 1) 2)) +cm-max-k+))
         (a 0.0)
         (d (make-array n)))
    (dotimes (i n)
      (setf (aref d i) (new-k-vector/0))
      (loop for k from 1 to m
            do (setf a (aref (aref (if even *cm-interp-coef-odd* *cm-interp-coef-even*)
                                   (min (- m 1) (- +cm-max-k+ 1)))
                             (min (- k 1) (- +cm-max-k+ 1))))
               (incf (.x (aref d i)) (* a (- (.x (aref points (mod (+ i k) n)))
                                             (.x (aref points (mod (+ (- i k) n) n))))))
               (incf (.y (aref d i)) (* a (- (.y (aref points (mod (+ i k) n)))
                                             (.y (aref points (mod (+ (- i k) n) n))))))))
    (dotimes (i n)
      (add-curve/4 spline (aref points i) (k-vector.sum/1 (list (aref d i) (aref points i)))
                   (sub/1 (clone/0 (aref points (mod (1+ i) n))) (aref d (mod (1+ i) n)))
                   (aref points (mod (1+ i) n))))
    spline))

(defjmethod calculate-open-bezier-spline ((this cubic-spline-interpolator) points)
  (let ((pts (coerce (to-jvector points) 'simple-vector)))
    (calculate-open-bezier-spline/4
     this pts
     (normalize/0 (sub/1 (clone/0 (aref pts 1)) (aref pts 0)))
     (normalize/0 (sub/1 (clone/0 (aref pts (- (length pts) 2))) (aref pts (- (length pts) 1))))
     nil)))

(defjmethod calculate-open-bezier-spline ((this cubic-spline-interpolator) points start-tan end-tan tangent-scale)
  (let* ((points (coerce (to-jvector points) 'simple-vector))
         (spline (new-bezier-spline/0))
         (n (- (length points) 1))
         (tv (make-array (* 2 n) :initial-element nil))
         (d (make-array (1+ n) :initial-element nil))
         (start-scale 1.0)
         (end-scale 1.0))
    (when tangent-scale
      (if (= (length points) 2)
          (setf start-scale (* (distance/1 (aref points 0) (aref points 1)) +cm-tangent-scale+)
                end-scale start-scale)
          (setf start-scale (* (distance/1 (aref points 0) (aref points 1)) +cm-tangent-scale+)
                end-scale (* (distance/1 (aref points (- n 1)) (aref points n)) +cm-tangent-scale+))))
    (setf (aref d 0) (scale/1 (clone/0 start-tan) start-scale))
    (setf (aref d n) (scale/1 (clone/0 end-tan) end-scale))
    (setf (aref tv 0) (k-vector.sum/1 (list (aref points 0) (aref d 0))))
    (setf (aref tv n) (sub/1 (clone/0 (aref points n)) (aref d n)))
    (loop for i from 1 below n
          do (setf (aref tv i) (aref points i))
             (setf (aref tv (- (* 2 n) i)) (aref tv i)))
    (let ((m (min (- n 1) +cm-max-k+)) (a 0.0))
      (loop for i from 1 below n
            do (setf (aref d i) (new-k-vector/0))
               (loop for k from 1 to m
                     do (setf a (aref (aref *cm-interp-coef-even* (- m 1)) (- k 1)))
                        (incf (.x (aref d i)) (* a (- (.x (aref tv (+ i k))) (.x (aref tv (abs (- i k)))))))
                        (incf (.y (aref d i)) (* a (- (.y (aref tv (+ i k))) (.y (aref tv (abs (- i k))))))))))
    (dotimes (i n)
      (let ((bend1 (k-vector.sum/1 (list (aref points i) (aref d i))))
            (bend2 (sub/1 (clone/0 (aref points (1+ i))) (aref d (1+ i)))))
        (add-curve/4 spline (aref points i) bend1 bend2 (aref points (1+ i)))))
    spline))

;; interpolatePoints(KVector[]) / (LinkedList<KVector>) -- both become a sequence
(defjmethod interpolate-points ((this cubic-spline-interpolator) points)
  (calculate-open-bezier-spline/1 this points))
(defjmethod interpolate-points ((this cubic-spline-interpolator) points start-vec end-vec tangend-scale)
  (calculate-open-bezier-spline/4 this points start-vec end-vec tangend-scale))
