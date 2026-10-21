;;;; nub-spline.lisp -- org.eclipse.elk.alg.layered.p5edges.splines.NubSpline
;;;; and its PolarCP.  Knot and control-point lists are jlists; the Java
;;;; ListIterator walks use the %P5-LI helpers from util.lisp.

(in-package #:elk)

(defjconst nub-spline.dim 3)
(defjconst nub-spline.epsilon 0.000001d0)

;;; ------------------------------------------------------------ PolarCP

(defjctor polar-cp (control-point polar-coordinate)
  (setf (.cp this) (clone control-point)
        (.polar-coordinate this) (jlist-from polar-coordinate)))

(defjctor polar-cp (other)
  (setf (.cp this) (clone (.cp other))
        (.polar-coordinate this) (jlist-from (.polar-coordinate other))))

(defjctor polar-cp (first-cp second-cp new-knot)
  (let* ((first-factor (jl-get (.polar-coordinate first-cp) 0))
         (second-factor (jl-last (.polar-coordinate second-cp)))
         (a-scaled (scale (clone (.cp first-cp)) (- second-factor new-knot)))
         (b-scaled (scale (clone (.cp second-cp)) (- new-knot first-factor)))
         (total (add a-scaled b-scaled)))
    (scale total (/ 1.0d0 (- second-factor first-factor)))
    (setf (.cp this) total)
    (setf (.polar-coordinate this) (new-jlist))
    (let ((needs-to-be-added t)
          (old (.polar-coordinate first-cp)))
      ;; iter.next() skips the first element
      (loop for k from 1 below (length old)
            do (let ((next-knot (jl-get old k)))
                 (when (and needs-to-be-added (> (- next-knot new-knot) nub-spline.epsilon))
                   (jl-add (.polar-coordinate this) new-knot)
                   (setf needs-to-be-added nil))
                 (jl-add (.polar-coordinate this) next-knot)))
      (when needs-to-be-added
        (jl-add (.polar-coordinate this) new-knot)))))

(defjmethod get-cp ((this polar-cp)) (.cp this))
(defjmethod set-cp ((this polar-cp) cp) (setf (.cp this) cp))
(defjmethod get-polar-coordinate ((this polar-cp)) (.polar-coordinate this))
(defjmethod set-polar-coordinate ((this polar-cp) polar-coordinate)
  (setf (.polar-coordinate this) polar-coordinate))

;;; ------------------------------------------------------------ NubSpline

;; NubSpline(NubSpline)
(defjctor nub-spline (nub-spline)
  (setf (.dim-nubs this) (.dim-nubs nub-spline)
        (.is-uniform this) (.is-uniform nub-spline)
        (.is-clamped this) (.is-clamped nub-spline)
        (.outer-box this) (.outer-box nub-spline)
        (.is-bezier this) (.is-bezier nub-spline)
        (.knot-vector this) (jlist-from (.knot-vector nub-spline))
        (.min-knot this) (.min-knot nub-spline)
        (.max-knot this) (.max-knot nub-spline)
        (.control-points this) (jlist-from (.control-points nub-spline))))

;; NubSpline(boolean clamped, int dimension, List<KVector> kVectors).  The
;; Java varargs overload has the same body; callers pass a sequence.
(defjctor nub-spline (clamped dimension k-vectors)
  (when (< dimension 1)
    (jthrow 'illegal-argument-exception "The dimension must be at least 1!"))
  ;; Java pads the caller's list in place; a private copy gives the same
  ;; control points without mutating the argument.
  (let ((kvs (jlist-from k-vectors)))
    (loop for i from (1- (length kvs)) below dimension
          do (jl-add-at kvs 0 (jl-get kvs 0)))
    (if (< (length kvs) (1+ dimension))
        (jthrow 'illegal-argument-exception "At (least dimension + 1) control points are necessary!")
        (progn
          (setf (.dim-nubs this) dimension
                (.is-clamped this) clamped
                (.is-uniform this) t
                (.is-bezier this) nil)
          (%ns-create-uniform-knot-vector this clamped (+ (length kvs) (.dim-nubs this) -1))
          (let* ((polar (new-jlist))
                 (knot-iter (%p5-list-iterator (.knot-vector this))))
            (loop repeat (1- (.dim-nubs this)) do
              (jl-add polar (%p5-li-next knot-iter)))
            (jdo (k-vec kvs)
              (jl-add polar (%p5-li-next knot-iter))
              (jl-add (.control-points this) (new-polar-cp k-vec polar))
              (jl-remove-at polar 0)))))))

;; The private raw constructor used by the derivative and inversion
;; generators.
(defjctor nub-spline (clamped uniform bezier dim knot-vec polar-vectors)
  (setf (.is-clamped this) clamped
        (.is-uniform this) uniform
        (.is-bezier this) bezier
        (.dim-nubs this) dim
        (.knot-vector this) knot-vec
        (.control-points this) polar-vectors
        (.min-knot this) (jl-first knot-vec)
        (.max-knot this) (jl-last knot-vec)))

(defjstatic nub-spline.generate-derived-nubs (nub-spline)
  (let* ((new-clamped (.is-clamped nub-spline))
         (new-uniform (.is-uniform nub-spline))
         (new-bezier (.is-bezier nub-spline))
         (old-dim (.dim-nubs nub-spline))
         (new-dim (1- old-dim))
         (old-knot-vector (.knot-vector nub-spline))
         (new-knot-vector (jl-sub-list old-knot-vector 1 (1- (length old-knot-vector))))
         (new-control-points (new-jlist))
         (controls (.control-points nub-spline)))
    (loop for i from 0 below (1- (length controls)) do
      (let ((new-cp (scale (sub (clone (.cp (jl-get controls (1+ i))))
                                (.cp (jl-get controls i)))
                           (/ old-dim (- (jl-get old-knot-vector (+ i old-dim))
                                         (jl-get old-knot-vector i))))))
        (jl-add new-control-points new-cp)))
    (let ((polar-coordinate (new-jlist))
          (knot-iter (%p5-list-iterator new-knot-vector))
          (new-polar-vectors (new-jlist)))
      (loop repeat (1- new-dim) do
        (jl-add polar-coordinate (%p5-li-next knot-iter)))
      (jdo (k-vec new-control-points)
        (jl-add polar-coordinate (%p5-li-next knot-iter))
        (jl-add new-polar-vectors (new-polar-cp k-vec polar-coordinate))
        (jl-remove-at polar-coordinate 0))
      (new-nub-spline new-clamped new-uniform new-bezier new-dim new-knot-vector new-polar-vectors))))

(defjstatic nub-spline.generate-inverted-nubs (nub-spline)
  (let ((new-knot-vector (new-jlist))
        (max-vector (jl-last (.knot-vector nub-spline))))
    (jdo (v (.knot-vector nub-spline))
      (jl-add-at new-knot-vector 0 (- max-vector v)))
    (let* ((new-control-points (k-vector-chain.reverse (get-control-points nub-spline)))
           (polar-coordinate (new-jlist))
           (knot-iter (%p5-list-iterator new-knot-vector))
           (new-polar-vectors (new-jlist)))
      (loop repeat (1- (.dim-nubs nub-spline)) do
        (jl-add polar-coordinate (%p5-li-next knot-iter)))
      (jdo (k-vec new-control-points)
        (jl-add polar-coordinate (%p5-li-next knot-iter))
        (jl-add new-polar-vectors (new-polar-cp k-vec polar-coordinate))
        (jl-remove-at polar-coordinate 0))
      (new-nub-spline (.is-clamped nub-spline)
                      (.is-uniform nub-spline)
                      (.is-bezier nub-spline)
                      (.dim-nubs nub-spline)
                      new-knot-vector
                      new-polar-vectors))))

(defjmethod get-dim ((this nub-spline)) (.dim-nubs this))

(defjmethod get-outer-box ((this nub-spline))
  (unless (.outer-box this)
    (%ns-calculate-outer-box this))
  (.outer-box this))

(defjmethod set-outer-box ((this nub-spline) outer-rectangle)
  (setf (.outer-box this) outer-rectangle)
  nil)

(defjmethod get-control-points ((this nub-spline))
  (let ((ret-val (new-k-vector-chain)))
    (jdo (polar-cp (.control-points this))
      (add ret-val (.cp polar-cp)))
    ret-val))

(defjmethod get-control-point ((this nub-spline) i)
  (.cp (jl-get (.control-points this) i)))

(defjmethod get-control-points-size ((this nub-spline))
  (length (.control-points this)))

(defjmethod get-knot-vector ((this nub-spline))
  (jlist-from (.knot-vector this)))

(defun %ns-index-in-knot-vector (this knot)
  (let ((kv (.knot-vector this)))
    (loop for i from 0 below (length kv)
          when (< (math.abs (- (jl-get kv i) knot)) nub-spline.epsilon)
            do (return-from %ns-index-in-knot-vector i))
    (length kv)))

(defun %ns-multiplicity (this knot-to-check)
  (let ((count 0))
    (jdo (current (.knot-vector this))
      (let ((diff (- current knot-to-check)))
        (cond ((> diff nub-spline.epsilon)
               (return-from %ns-multiplicity count))
              ((> diff (- nub-spline.epsilon))
               (incf count)))))
    count))

(defun %ns-create-uniform-knot-vector (this clamped size)
  (when (< size (* 2 (.dim-nubs this)))
    (jthrow 'illegal-argument-exception
            "The knot vector must have at least two time the dimension elements."))
  (let ((my-size 0d0)
        (kv (.knot-vector this)))
    (cond (clamped
           (setf (.min-knot this) 0.0d0
                 (.max-knot this) 1.0d0)
           (loop repeat (.dim-nubs this) do
             (jl-add kv 0.0d0))
           (setf my-size (float (- (+ size 1) (* 2 (.dim-nubs this))) 1d0)))
          (t
           (setf my-size (float (+ size 1) 1d0))
           (let ((ddim (float (.dim-nubs this) 1d0)))
             (setf (.min-knot this) (/ ddim (+ my-size 1d0)))
             (setf (.max-knot this) (/ (- my-size ddim) my-size)))))
    (let ((fraction my-size))
      (loop for i from 1
            while (< i my-size)
            do (jl-add kv (/ (float i 1d0) fraction))))
    (when (.is-clamped this)
      (loop repeat (.dim-nubs this) do
        (jl-add kv 1.0d0)))))

(defun %ns-calculate-outer-box (this)
  (setf (.outer-box this) (new-rectangle (get-control-points this))))

(defjstatic nub-spline.get-t-from-polar (polar)
  (let ((sum 0.0d0))
    (jdo (val polar) (incf sum val))
    (/ sum (jsize polar))))

(defun %ns-insert-knot (this knot-to-insert insertions)
  (let ((iter-knot (%p5-list-iterator (.knot-vector this)))
        (iter-cp (%p5-list-iterator (.control-points this))))
    (dotimes (i (if (.is-clamped this) (.dim-nubs this) (1- (.dim-nubs this))))
      (%p5-li-next iter-knot))
    (let ((current-knot (%p5-li-next iter-knot)))
      (loop while (< (- current-knot knot-to-insert) nub-spline.epsilon)
            do (setf current-knot (%p5-li-next iter-knot))
               (%p5-li-next iter-cp))
      (%p5-li-previous iter-knot)
      (%ns-insert-knot-at-current-position this insertions knot-to-insert iter-cp iter-knot))))

(defun %ns-insert-knot-at-current-position (this insertions knot-to-insert iter-cp iter-knot)
  (let ((multiplicity (%ns-multiplicity this knot-to-insert))
        (dim (.dim-nubs this)))
    (dotimes (i insertions)
      (%p5-li-add iter-knot knot-to-insert)
      (let ((new-cps (new-jlist))
            (second-cp (%p5-li-next iter-cp)))
        (loop for j from (+ multiplicity i) below dim
              do (let ((first-cp second-cp))
                   (setf second-cp (%p5-li-next iter-cp))
                   (jl-add new-cps (new-polar-cp first-cp second-cp knot-to-insert))))
        (loop for j from (+ multiplicity i) below dim
              do (%p5-li-previous iter-cp)
                 (when (> j (+ multiplicity i))
                   (%p5-li-remove iter-cp)))
        (jdo (cp new-cps)
          (%p5-li-add iter-cp cp))
        (when (< i (1- insertions))
          (loop for j from (+ multiplicity i) below dim
                do (%p5-li-previous iter-cp)))))))

(defun %ns-axis-value (v axis)
  (if (eq axis :x) (.x v) (.y v)))

(defun %ns-first-point (first-derive accuracy max-recursion axis)
  "The loop shared by the four getFirst/LastVertical/HorizontalPoint methods.
FIRST-DERIVE is the (possibly inverted) derivative; AXIS is :x or :y.
Returns the knot where the derivative's AXIS coordinate is zero."
  (let ((current-accuracy double.positive-infinity)
        (knot 0.0d0)
        (loop-count 0))
    (loop while (and (> current-accuracy accuracy) (< loop-count max-recursion))
          do (setf knot (%ns-zero-of-control-poligon first-derive axis))
             (let ((current-vector (get-point-on-curve first-derive knot t)))
               (setf current-accuracy (math.abs (%ns-axis-value current-vector axis))))
             (incf loop-count))
    knot))

(defjstatic nub-spline.get-first-vertical-point (nub-spline accuracy max-recursion)
  (let* ((first-derive (nub-spline.generate-derived-nubs nub-spline))
         (knot (%ns-first-point first-derive accuracy max-recursion :x)))
    (get-point-on-curve nub-spline knot nil)))

(defjstatic nub-spline.get-last-vertical-point (nub-spline accuracy max-recursion)
  (let* ((first-derive (nub-spline.generate-inverted-nubs
                        (nub-spline.generate-derived-nubs nub-spline)))
         (knot (%ns-first-point first-derive accuracy max-recursion :x))
         (max-val (jl-last (get-knot-vector nub-spline))))
    (get-point-on-curve nub-spline (- max-val knot) nil)))

(defjstatic nub-spline.get-first-horizontal-point (nub-spline accuracy max-recursion)
  (let* ((first-derive (nub-spline.generate-derived-nubs nub-spline))
         (knot (%ns-first-point first-derive accuracy max-recursion :y)))
    (get-point-on-curve nub-spline knot nil)))

(defjstatic nub-spline.get-last-horizontal-point (nub-spline accuracy max-recursion)
  (let* ((first-derive (nub-spline.generate-inverted-nubs
                        (nub-spline.generate-derived-nubs nub-spline)))
         (knot (%ns-first-point first-derive accuracy max-recursion :y))
         (max-val (jl-last (get-knot-vector nub-spline))))
    (get-point-on-curve nub-spline (- max-val knot) nil)))

(defjmethod get-point-on-curve ((this nub-spline) tt)
  (get-point-on-curve this tt nil))

(defjmethod get-point-on-curve ((this nub-spline) tt modify)
  (let ((multiplicity (%ns-multiplicity this tt)))
    (cond ((= multiplicity (.dim-nubs this))
           (get-control-point this (%ns-index-in-knot-vector this tt)))
          (modify
           (%ns-insert-knot this tt (- (.dim-nubs this) multiplicity))
           (get-control-point this (%ns-index-in-knot-vector this tt)))
          (t
           (let ((copy (new-nub-spline this)))
             (%ns-insert-knot copy tt (- (.dim-nubs this) multiplicity))
             (get-control-point copy (%ns-index-in-knot-vector copy tt)))))))

(defun %ns-zero-of-control-poligon (ns axis)
  "getZeroXOfControlPoligon (AXIS :x) and getZeroYOfControlPoligon (AXIS :y)."
  (let* ((controls (.control-points ns))
         (second-cp (jl-get controls 0))
         (second-value (%ns-axis-value (.cp second-cp) axis))
         (second-positive (> second-value nub-spline.epsilon))
         (second-negative (< second-value (- nub-spline.epsilon)))
         (first-cp nil)
         (first-value 0.0d0)
         (first-positive nil)
         (first-negative nil))
    (loop for i from 1 below (length controls)
          do (setf first-cp second-cp
                   first-value second-value
                   first-positive second-positive
                   first-negative second-negative)
             (setf second-cp (jl-get controls i)
                   second-value (%ns-axis-value (.cp second-cp) axis)
                   second-positive (> second-value nub-spline.epsilon)
                   second-negative (< second-value (- nub-spline.epsilon)))
             (unless (or second-positive second-negative)
               (return-from %ns-zero-of-control-poligon
                 (nub-spline.get-t-from-polar (.polar-coordinate second-cp))))
             (when (or (and first-positive second-negative)
                       (and first-negative second-positive))
               (let* ((factor (/ first-value (- first-value second-value)))
                      (t1 (nub-spline.get-t-from-polar (.polar-coordinate first-cp)))
                      (t2 (nub-spline.get-t-from-polar (.polar-coordinate second-cp))))
                 (return-from %ns-zero-of-control-poligon
                   (+ (* factor t1) (* (- 1 factor) t2))))))
    0.0d0))

(defjstatic nub-spline.get-zero-x-of-control-poligon (nub-spline)
  (%ns-zero-of-control-poligon nub-spline :x))

(defjstatic nub-spline.get-zero-y-of-control-poligon (nub-spline)
  (%ns-zero-of-control-poligon nub-spline :y))

(defun %ns-to-bezier (this)
  (let ((iter-knot (%p5-list-iterator (.knot-vector this)))
        (iter-cp (%p5-list-iterator (.control-points this))))
    (if (.is-clamped this)
        (loop repeat (.dim-nubs this) do
          (%p5-li-next iter-knot))
        (loop repeat (1- (.dim-nubs this)) do
          (%p5-li-next iter-knot)
          (%p5-li-remove iter-knot)))
    (let ((current-knot (%p5-li-next iter-knot)))
      (loop while (> (- (.max-knot this) current-knot) nub-spline.epsilon)
            do (let ((knot-to-count current-knot)
                     (occurrence 0))
                 (loop while (< (math.abs (- current-knot knot-to-count)) nub-spline.epsilon)
                       do (incf occurrence)
                          (setf current-knot (%p5-li-next iter-knot))
                          (%p5-li-next iter-cp))
                 (when (< occurrence (.dim-nubs this))
                   (%p5-li-previous iter-knot)
                   (%ns-insert-knot-at-current-position this (- (.dim-nubs this) occurrence)
                                                        knot-to-count iter-cp iter-knot)
                   (%p5-li-next iter-knot)))
               (%p5-li-previous iter-cp)))
    (unless (.is-clamped this)
      (loop repeat (1- (.dim-nubs this)) do
        (%p5-li-next iter-knot)
        (%p5-li-remove iter-knot)))
    (setf (.is-clamped this) t
          (.is-bezier this) t)))

(defjmethod to-bezier ((this nub-spline))
  (%ns-to-bezier this)
  nil)

(defjmethod get-bezier-cp ((this nub-spline) with-source-vector with-target-vector)
  (unless (.is-bezier this)
    (%ns-to-bezier this))
  (let ((ret-val (new-k-vector-chain))
        (controls (.control-points this)))
    (loop for i from (if with-source-vector 0 1) below (length controls)
          do (add ret-val (.cp (jl-get controls i))))
    (unless with-target-vector
      (remove-last ret-val))
    ret-val))

(defjmethod get-bezier-cp ((this nub-spline))
  (get-bezier-cp this nil nil))

(defjstatic nub-spline.generate-nice-curve ()
  (let* ((nice-chain (new-k-vector-chain))
         (vector1 (new-k-vector/2 5.5d0 23.0d0))
         (vector2 (new-k-vector/2 2.5d0 12.0d0))
         (vector3 (new-k-vector/2 5.0d0 10.0d0))
         (vector4 (new-k-vector/2 5.0d0 9.0d0))
         (vector5 (new-k-vector/2 3.8d0 5.5d0))
         (vector6 (new-k-vector/2 7.0d0 4.0d0))
         (vector7 (new-k-vector/2 7.0d0 3.5d0))
         (vector8 (new-k-vector/2 6.0d0 2.2d0))
         (vector9 (new-k-vector/2 8.0d0 0.5d0)))
    (add nice-chain vector1)
    (add nice-chain vector1)
    (add nice-chain vector1)
    (add nice-chain vector2)
    (add nice-chain vector3)
    (add nice-chain vector4)
    (add nice-chain vector5)
    (add nice-chain vector6)
    (add nice-chain vector7)
    (add nice-chain vector8)
    (add nice-chain vector9)
    (add nice-chain (nub-spline.mirror-on-x vector8 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector7 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector6 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector5 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector4 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector3 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector2 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector1 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector1 (.x vector9)))
    (add nice-chain (nub-spline.mirror-on-x vector1 (.x vector9)))
    (scale nice-chain 5.0d0)
    (offset nice-chain -50.0d0 -80.0d0)
    (new-nub-spline t 3 nice-chain)))

(defjstatic nub-spline.mirror-on-x (original x-val)
  (new-k-vector/2 (- (+ x-val x-val) (.x original)) (.y original)))
