;;;; ElkMath -- org.eclipse.elk.core.math.ElkMath

(in-package #:elk)

;;; Guava DoubleMath.fuzzyEquals / fuzzyCompare
(defun %cm-fuzzy-equals (a b tolerance)
  (or (<= (abs (- a b)) tolerance) (= a b) (and (nan-p a) (nan-p b))))
(defun %cm-fuzzy-compare (a b tolerance)
  (cond ((%cm-fuzzy-equals a b tolerance) 0)
        ((< a b) -1)
        ((> a b) 1)
        (t (boolean.compare (nan-p a) (nan-p b)))))

(defun %cm-seq (vals)
  "Varargs parameter -> a CL sequence."
  (if (or (listp vals) (vectorp vals)) vals (to-jvector vals)))

(defjconst elk-math.fact-table
  (vector 1 1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800
          87178291200 1307674368000 20922789888000 355687428096000 6402373705728000
          121645100408832000 2432902008176640000))

(defjstatic elk-math.factl (x)
  (when (or (< x 0) (>= x (length elk-math.fact-table)))
    (jthrow 'illegal-argument-exception "The input must be between 0 and ~D"
            (length elk-math.fact-table)))
  (aref elk-math.fact-table x))

(defjstatic elk-math.factd (x)
  (cond ((< x 0) (jthrow 'illegal-argument-exception "The input must be positive"))
        ((< x (length elk-math.fact-table)) (jd (aref elk-math.fact-table x)))
        (t (with-java-floats
             (* (math.sqrt (* 2.0 math.pi x))
                (/ (elk-math.powf/2 x x) (elk-math.powd/2 (exp 1d0) x)))))))

(defjstatic elk-math.binomiall (n k)
  (cond ((or (< n 0) (< k 0)) (jthrow 'illegal-argument-exception "k and n must be positive"))
        ((> k n) (jthrow 'illegal-argument-exception "k must be smaller than n"))
        ((or (= k 0) (= k n)) 1)
        ((= n 0) 0)
        ((< n (length elk-math.fact-table))
         (truncate (elk-math.factl/1 n) (* (elk-math.factl/1 k) (elk-math.factl/1 (- n k)))))
        (t (+ (elk-math.binomiall/2 (- n 1) (- k 1)) (elk-math.binomiall/2 (- n 1) k)))))

(defjstatic elk-math.binomiald (n k)
  (cond ((or (< n 0) (< k 0)) (jthrow 'illegal-argument-exception "k and n must be positive"))
        ((> k n) (jthrow 'illegal-argument-exception "k must be smaller than n"))
        ((or (= k 0) (= k n)) 1.0)
        ((= n 0) 0.0)
        (t (/ (elk-math.factd/1 n) (* (elk-math.factd/1 k) (elk-math.factd/1 (- n k)))))))

(defjstatic elk-math.powd (a b)
  (with-java-floats
    (let ((result 1.0) (base (jd a)) (exp (if (>= b 0) b (- b))))
      (loop while (> exp 0)
            do (if (evenp exp)
                   (setf base (* base base) exp (truncate exp 2))
                   (setf result (* result base) exp (- exp 1))))
      (if (< b 0) (/ 1.0 result) result))))

(defjstatic elk-math.powf (a b)
  ;; float arithmetic, returned widened to double
  (with-java-floats
    (let ((result 1.0f0) (base (coerce a 'single-float)) (exp (if (>= b 0) b (- b))))
      (loop while (> exp 0)
            do (if (evenp exp)
                   (setf base (* base base) exp (truncate exp 2))
                   (setf result (* result base) exp (- exp 1))))
      (jd (if (< b 0) (/ 1.0f0 result) result)))))

(defjstatic elk-math.approximate-bezier-segment (control-points)
  ;; approximateBezierSegment(KVector... controlPoints)
  (let ((cps (%cm-seq control-points)))
    (elk-math.approximate-bezier-segment/2 (+ (length cps) 1) cps)))

(defjstatic elk-math.approximate-bezier-segment (result-size control-points)
  ;; approximateBezierSegment(int resultSize, KVector... controlPoints); a
  ;; non-integer first argument is the varargs form with two points.
  (if (not (integerp result-size))
      (elk-math.approximate-bezier-segment/1 (list result-size control-points))
      (if (<= result-size 0)
          (vector)
          (let ((result (make-array result-size))
                (dt (/ 1.0 result-size))
                (tt 0.0)
                (cps (%cm-seq control-points)))
            (dotimes (i result-size)
              (incf tt dt)
              (setf (aref result i) (elk-math.get-point-on-bezier-segment/2 tt cps)))
            result))))
;; varargs conveniences
(defun elk-math.approximate-bezier-segment/3 (a b c)
  (if (integerp a)
      (elk-math.approximate-bezier-segment/2 a (list b c))
      (elk-math.approximate-bezier-segment/1 (list a b c))))
(defun elk-math.approximate-bezier-segment/4 (a b c d)
  (if (integerp a)
      (elk-math.approximate-bezier-segment/2 a (list b c d))
      (elk-math.approximate-bezier-segment/1 (list a b c d))))
(defun elk-math.approximate-bezier-segment/5 (a b c d e)
  (if (integerp a)
      (elk-math.approximate-bezier-segment/2 a (list b c d e))
      (elk-math.approximate-bezier-segment/1 (list a b c d e))))

(defjstatic elk-math.get-point-on-bezier-segment (tt control-points)
  (let* ((cps (coerce (%cm-seq control-points) 'simple-vector))
         (n (- (length cps) 1))
         (px 0.0) (py 0.0))
    (loop for j from 0 to n
          do (let* ((p (aref cps j))
                    (factor (* (elk-math.binomiald/2 n j) (elk-math.powd/2 (- 1 tt) (- n j))
                               (elk-math.powd/2 tt j))))
               (incf px (* (.x p) factor))
               (incf py (* (.y p) factor))))
    (new-k-vector/2 px py)))

(defjstatic elk-math.approximate-bezier-spline (control-points)
  (let* ((items (coerce (to-jvector control-points) 'simple-vector))
         (ctrl-pt-count (length items))
         (spline (new-k-vector-chain/0))
         (cursor 0)
         (current-point (aref items 0)))
    (flet ((next () (prog1 (aref items cursor) (incf cursor))))
      (next)
      (add/1 spline current-point)
      (loop while (< cursor ctrl-pt-count)
            do (let ((remaining (- ctrl-pt-count cursor)))
                 (cond ((= remaining 1) (add/1 spline (next)))
                       ((= remaining 2)
                        ;; a quadratic bezier curve
                        (let* ((a (next)) (b (next)))
                          (add-all/1 spline (elk-math.approximate-bezier-segment/1
                                             (list current-point a b)))))
                       (t
                        ;; a cubic bezier curve
                        (let* ((control1 (next)) (control2 (next)) (next-point (next)))
                          (add-all/1 spline (elk-math.approximate-bezier-segment/1
                                             (list current-point control1 control2 next-point)))
                          (setf current-point next-point)))))))
    spline))

(defconstant +cm-w-degree+ 5)
(defconstant +cm-degree+ 3)
(defconstant +cm-maxdepth+ 64)
(defparameter *cm-cubic-z* #2A((1.0 0.6 0.3 0.1) (0.4 0.6 0.6 0.4) (0.1 0.3 0.6 1.0)))
(defparameter *cm-epsilon* (* 1.0 (expt 2d0 (- (- +cm-maxdepth+) 1))))

(defjstatic elk-math.distance-from-bezier-segment (start c1 c2 end needle)
  (let* ((t-candidate (make-array +cm-w-degree+ :initial-element 0.0))
         (v (vector start c1 c2 end))
         (w (elk-math.convert-to-bezier-form/2 v needle))
         (n-solutions (elk-math.find-roots/4 w +cm-w-degree+ t-candidate 0))
         (min-distance (distance/1 needle start))
         (tt 0.0))
    (dotimes (i n-solutions)
      (let* ((p (elk-math.bezier/5 v +cm-degree+ (aref t-candidate i) nil nil))
             (d (distance/1 needle p)))
        (when (< d min-distance)
          (setf min-distance d tt (aref t-candidate i)))))
    (let ((d (distance/1 needle end)))
      (when (< d min-distance) (setf tt 1.0)))
    (let ((pn (new-k-vector/1 (elk-math.bezier/5 v +cm-degree+ tt nil nil))))
      (math.sqrt (distance/1 pn needle)))))

(defjstatic elk-math.convert-to-bezier-form (v pa)
  (let ((c (make-array (1+ +cm-degree+)))
        (d (make-array +cm-degree+))
        (cd-table (make-array (list +cm-degree+ (1+ +cm-degree+)) :initial-element 0.0))
        (w (make-array (1+ +cm-w-degree+))))
    (loop for i from 0 to +cm-degree+
          do (setf (aref c i) (new-k-vector/2 (- (.x (aref v i)) (.x pa)) (- (.y (aref v i)) (.y pa)))))
    (let ((s (jd +cm-degree+)))
      (loop for i from 0 to (- +cm-degree+ 1)
            do (setf (aref d i) (new-k-vector/2 (* s (- (.x (aref v (1+ i))) (.x (aref v i))))
                                                (* s (- (.y (aref v (1+ i))) (.y (aref v i))))))))
    (loop for row from 0 to (- +cm-degree+ 1)
          do (loop for column from 0 to +cm-degree+
                   do (setf (aref cd-table row column)
                            (+ (* (.x (aref d row)) (.x (aref c column)))
                               (* (.y (aref d row)) (.y (aref c column)))))))
    (loop for i from 0 to +cm-w-degree+
          do (setf (aref w i) (new-k-vector/2 (/ (jd i) +cm-w-degree+) 0.0)))
    (let ((n +cm-degree+) (m (- +cm-degree+ 1)))
      (loop for k from 0 to (+ n m)
            do (let ((lb (max 0 (- k m))) (ub (min k n)))
                 (loop for i from lb to ub
                       do (let ((j (- k i)))
                            (setf (.y (aref w (+ i j)))
                                  (+ (.y (aref w (+ i j)))
                                     (* (aref cd-table j i) (aref *cm-cubic-z* j i)))))))))
    w))

(defjstatic elk-math.find-roots (w degree tt depth)
  (block find-roots
    (case (elk-math.crossing-count/2 w degree)
      (0 (return-from find-roots 0))
      (1 (when (>= depth +cm-maxdepth+)
           (setf (aref tt 0) (/ (+ (.x (aref w 0)) (.x (aref w +cm-w-degree+))) 2.0))
           (return-from find-roots 1))
         (when (elk-math.control-polygon-flat-enough/2 w degree)
           (setf (aref tt 0) (elk-math.compute-x-intercept/2 w degree))
           (return-from find-roots 1))))
    (let ((left (make-array (1+ +cm-w-degree+) :initial-element nil))
          (right (make-array (1+ +cm-w-degree+) :initial-element nil))
          (left-t (make-array (1+ +cm-w-degree+) :initial-element 0.0))
          (right-t (make-array (1+ +cm-w-degree+) :initial-element 0.0)))
      (elk-math.bezier/5 w degree (/ 1.0 2) left right)
      (let ((left-count (elk-math.find-roots/4 left degree left-t (1+ depth)))
            (right-count (elk-math.find-roots/4 right degree right-t (1+ depth))))
        (dotimes (i left-count) (setf (aref tt i) (aref left-t i)))
        (dotimes (i right-count) (setf (aref tt (+ i left-count)) (aref right-t i)))
        (+ left-count right-count)))))

(defjstatic elk-math.control-polygon-flat-enough (v degree)
  (with-java-floats
    (let* ((a (- (.y (aref v 0)) (.y (aref v degree))))
           (b (- (.x (aref v degree)) (.x (aref v 0))))
           (c (- (* (.x (aref v 0)) (.y (aref v degree))) (* (.x (aref v degree)) (.y (aref v 0)))))
           (ab-squared (+ (* a a) (* b b)))
           (distance (make-array (1+ degree) :initial-element 0.0)))
      (loop for i from 1 below degree
            do (setf (aref distance i) (+ (* a (.x (aref v i))) (* b (.y (aref v i))) c))
               (when (> (aref distance i) 0.0)
                 (setf (aref distance i) (/ (* (aref distance i) (aref distance i)) ab-squared)))
               (when (< (aref distance i) 0.0)
                 (setf (aref distance i) (- (/ (* (aref distance i) (aref distance i)) ab-squared)))))
      (let ((max-above 0.0) (max-below 0.0))
        (loop for i from 1 below degree
              do (when (< (aref distance i) 0.0) (setf max-below (math.min max-below (aref distance i))))
                 (when (> (aref distance i) 0.0) (setf max-above (math.max max-above (aref distance i)))))
        (let* ((a1 0.0) (b1 1.0) (c1 0.0)
               (a2 a) (b2 b) (c2 (+ c max-above))
               (det (- (* a1 b2) (* a2 b1)))
               (d-inv (/ 1.0 det))
               (intercept1 (* (- (* b1 c2) (* b2 c1)) d-inv)))
          (setf a2 a b2 b c2 (+ c max-below)
                det (- (* a1 b2) (* a2 b1))
                d-inv (/ 1.0 det))
          (let* ((intercept2 (* (- (* b1 c2) (* b2 c1)) d-inv))
                 (left-intercept (math.min intercept1 intercept2))
                 (right-intercept (math.max intercept1 intercept2))
                 (err (/ (- right-intercept left-intercept) 2)))
            (< err *cm-epsilon*)))))))

(defjstatic elk-math.compute-x-intercept (v degree)
  (with-java-floats
    (let* ((xnm (- (.x (aref v degree)) (.x (aref v 0))))
           (ynm (- (.y (aref v degree)) (.y (aref v 0))))
           (xmk (.x (aref v 0)))
           (ymk (.y (aref v 0)))
           (det-inv (/ -1.0 ynm)))
      (* (- (* xnm ymk) (* ynm xmk)) det-inv))))

(defjstatic elk-math.crossing-count (v degree)
  (let* ((n-crossings 0)
         (sign (if (< (.y (aref v 0)) 0) -1 1))
         (old-sign sign))
    (loop for i from 1 to degree
          do (setf sign (if (< (.y (aref v i)) 0) -1 1))
             (when (/= sign old-sign) (incf n-crossings))
             (setf old-sign sign))
    n-crossings))

(defjstatic elk-math.bezier (c degree tt left right)
  (let ((p (make-array (list (1+ +cm-w-degree+) (1+ +cm-w-degree+)) :initial-element nil)))
    (loop for j from 0 to degree do (setf (aref p 0 j) (new-k-vector/1 (aref c j))))
    (loop for i from 1 to degree
          do (loop for j from 0 to (- degree i)
                   do (setf (aref p i j)
                            (new-k-vector/2 (+ (* (- 1.0 tt) (.x (aref p (1- i) j))) (* tt (.x (aref p (1- i) (1+ j)))))
                                            (+ (* (- 1.0 tt) (.y (aref p (1- i) j))) (* tt (.y (aref p (1- i) (1+ j)))))))))
    (when left
      (loop for j from 0 to degree do (setf (aref left j) (aref p j 0))))
    (when right
      (loop for j from 0 to degree do (setf (aref right j) (aref p (- degree j) j))))
    (aref p degree 0)))

;;; min / max / average over varargs (ONE parameter: a list or vector).  The
;;; /2 and /3 forms are conveniences for callers that spell the varargs out.
(defmacro %cm-define-extremum (name init test &optional (coerce 'identity))
  `(progn
     (defjstatic ,name (vals)
       (let ((m ,init))
         (map nil (lambda (v) (when (,test v m) (setf m v))) (%cm-seq vals))
         (,coerce m)))
     (defun ,(intern (format nil "~A/2" (symbol-name name)) :elk) (a b) (,(intern (format nil "~A/1" (symbol-name name)) :elk) (list a b)))
     (defun ,(intern (format nil "~A/3" (symbol-name name)) :elk) (a b c) (,(intern (format nil "~A/1" (symbol-name name)) :elk) (list a b c)))))

(%cm-define-extremum elk-math.maxi integer.min-value >)
(%cm-define-extremum elk-math.mini integer.max-value <)
(%cm-define-extremum elk-math.maxl integer.min-value >)   ; sic: Java starts at Integer.MIN_VALUE
(%cm-define-extremum elk-math.minl integer.max-value <)
(%cm-define-extremum elk-math.maxf (- float.max-value) > jd)
(%cm-define-extremum elk-math.minf float.max-value < jd)
(%cm-define-extremum elk-math.maxd (- double.max-value) > jd)
(%cm-define-extremum elk-math.mind double.max-value < jd)

(defjstatic elk-math.averagei (vals)
  (let ((vs (%cm-seq vals))) (idiv (reduce #'+ vs :initial-value 0) (length vs))))
(defjstatic elk-math.averagel (vals)
  (let ((vs (%cm-seq vals))) (idiv (reduce #'+ vs :initial-value 0) (length vs))))
(defjstatic elk-math.averagef (vals)
  (let ((vs (%cm-seq vals)))
    (with-java-floats (/ (jd (reduce #'+ vs :initial-value 0.0)) (length vs)))))
(defjstatic elk-math.averaged (vals)
  (let ((vs (%cm-seq vals)))
    (with-java-floats (/ (jd (reduce #'+ vs :initial-value 0.0)) (length vs)))))

(defjstatic elk-math.boundi (x lower upper) (cond ((<= x lower) lower) ((>= x upper) upper) (t x)))
(defjstatic elk-math.boundl (x lower upper) (cond ((<= x lower) lower) ((>= x upper) upper) (t x)))
(defjstatic elk-math.boundf (x lower upper) (cond ((<= x lower) lower) ((>= x upper) upper) (t x)))
(defjstatic elk-math.boundd (x lower upper) (cond ((<= x lower) lower) ((>= x upper) upper) (t x)))

(defjstatic elk-math.clip-vector (v width height)
  (with-java-floats
    (let ((wh (/ width 2)) (hh (/ height 2))
          (absx (math.abs (.x v))) (absy (math.abs (.y v)))
          (xscale 1.0) (yscale 1.0))
      (when (> absx wh) (setf xscale (/ wh absx)))
      (when (> absy hh) (setf yscale (/ hh absy)))
      (scale/1 v (math.min xscale yscale))
      v)))

(defjstatic elk-math.signum (x) (cond ((< x 0) -1) ((> x 0) 1) (t 0)))

(defjstatic elk-math.intersects (rect path)
  ;; intersects(ElkRectangle, KVectorChain)
  (let ((items (to-jvector path)))
    (if (< (length items) 2)
        nil
        (let* ((fst (aref items 0)) (p1 fst))
          (loop for i from 1 below (length items)
                do (let ((p2 (aref items i)))
                     (when (elk-math.intersects/3 rect p1 p2) (return-from elk-math.intersects/2 t))
                     (setf p1 p2)))
          (if (elk-math.intersects/3 rect p1 fst) t nil)))))

(defjstatic elk-math.intersects (rect p1 p2)
  (if (elk-math.contains/3 rect p1 p2)
      nil
      (or (elk-math.intersects/4 (get-top-left/0 rect) (get-top-right/0 rect) p1 p2)
          (elk-math.intersects/4 (get-top-right/0 rect) (get-bottom-right/0 rect) p1 p2)
          (elk-math.intersects/4 (get-bottom-right/0 rect) (get-bottom-left/0 rect) p1 p2)
          (elk-math.intersects/4 (get-bottom-left/0 rect) (get-top-left/0 rect) p1 p2))))

(defjconst elk-math.double-eq-epsilon 0.00001)

(defjstatic elk-math.intersects (l11 l12 l21 l22)
  (with-java-floats
    (let* ((u0 l11) (v0 (sub/1 (clone/0 l12) l11))
           (u1 l21) (v1 (sub/1 (clone/0 l22) l21))
           (x00 (.x u0)) (y00 (.y u0))
           (x10 (.x u1)) (y10 (.y u1))
           (x01 (.x v0)) (y01 (.y v0))
           (x11 (.x v1)) (y11 (.y v1))
           (d (- (* x11 y01) (* x01 y11))))
      (if (%cm-fuzzy-equals 0 d elk-math.double-eq-epsilon)
          nil
          (let ((s (* (/ 1 d) (- (* (- x00 x10) y01) (* (- y00 y10) x01))))
                (tt (* (/ 1 d) (- (+ (* (- (- x00 x10)) y11) (* (- y00 y10) x11))))))
            (and (< (%cm-fuzzy-compare 0 s elk-math.double-eq-epsilon) 0)
                 (< (%cm-fuzzy-compare s 1 elk-math.double-eq-epsilon) 0)
                 (< (%cm-fuzzy-compare 0 tt elk-math.double-eq-epsilon) 0)
                 (< (%cm-fuzzy-compare tt 1 elk-math.double-eq-epsilon) 0)))))))

(defjstatic elk-math.intersects2 (p r q s)
  (with-java-floats
    (let* ((pq (sub/1 (clone/0 q) p))
           (pq-x-r (k-vector.cross-product/2 pq r))
           (r-x-s (k-vector.cross-product/2 r s))
           (tt (/ (k-vector.cross-product/2 pq s) r-x-s))
           (u (/ pq-x-r r-x-s)))
      (if (= r-x-s 0)
          (if (= pq-x-r 0)
              ;; segments are collinear: return point closest to center of s
              (let* ((center (add/1 (clone/0 q) (scale/1 (clone/0 s) 0.5)))
                     (d1 (distance/1 p center))
                     (d2 (distance/1 (add/1 (clone/0 p) r) center))
                     (l (* (j-length/0 s) 0.5)))
                (cond ((and (< d1 d2) (<= d1 l)) (clone/0 p))
                      ((<= d2 l) (add/1 (clone/0 p) r))
                      (t nil)))
              nil)
          (if (and (>= tt 0) (<= tt 1) (>= u 0) (<= u 1))
              (add/1 (clone/0 p) (scale/1 (clone/0 r) tt))
              nil)))))

(defjstatic elk-math.distance (a1 a2 b1 b2 v)
  (math.min (elk-math.trace-rays/5 a1 a2 b1 b2 v)
            (elk-math.trace-rays/5 b1 b2 a1 a2 (negate/0 (clone/0 v)))))

(defjstatic elk-math.trace-rays (a1 a2 b1 b2 v)
  (let ((result double.positive-infinity)
        (intersection nil)
        (endpoint-hit nil))
    (setf intersection (elk-math.intersects2/4 a1 (sub/1 (clone/0 a2) a1) (add/1 (clone/0 b1) v)
                                               (sub/1 (clone/0 b2) b1)))
    (let ((edge-case (and intersection
                          (not (or (equals-fuzzily/1 intersection a1) (equals-fuzzily/1 intersection a2))))))
      ;; trace ray from point b1 to segment a in direction v
      (setf intersection (elk-math.intersects2/4 a1 (sub/1 (clone/0 a2) a1) b1 v))
      (when intersection
        (if (or (eq (not (equals-fuzzily/1 intersection a1)) (not (equals-fuzzily/1 intersection a2)))
                edge-case)
            (setf result (math.min result (j-length/0 (sub/1 intersection b1))))
            (setf endpoint-hit t)))
      ;; trace ray from point b2 to segment a in direction v
      (setf intersection (elk-math.intersects2/4 a1 (sub/1 (clone/0 a2) a1) b2 v))
      (when intersection
        (when (or endpoint-hit
                  (eq (not (equals-fuzzily/1 intersection a1)) (not (equals-fuzzily/1 intersection a2)))
                  edge-case)
          (setf result (math.min result (j-length/0 (sub/1 intersection b2)))))))
    result))

(defjstatic elk-math.contains (rect x)
  ;; contains(ElkRectangle, KVectorChain) | contains(ElkRectangle, KVector)
  (if (typep x 'k-vector)
      (let ((min-x (.x rect)) (max-x (+ (.x rect) (.width rect)))
            (min-y (.y rect)) (max-y (+ (.y rect) (.height rect))))
        (and (> (.x x) min-x) (< (.x x) max-x) (> (.y x) min-y) (< (.y x) max-y)))
      (let ((items (to-jvector x)))
        (if (< (length items) 2)
            nil
            (let* ((fst (aref items 0)) (p1 fst))
              (loop for i from 1 below (length items)
                    do (let ((p2 (aref items i)))
                         (unless (elk-math.contains/3 rect p1 p2) (return-from elk-math.contains/2 nil))
                         (setf p1 p2)))
              (if (elk-math.contains/3 rect p1 fst) t nil))))))

(defjstatic elk-math.contains (rect p1 p2)
  (and (elk-math.contains/2 rect p1) (elk-math.contains/2 rect p2)))

(defjstatic elk-math.shortest-distance (r1 r2)
  (let* ((right-dist (- (.x r2) (+ (.x r1) (.width r1))))
         (left-dist (- (.x r1) (+ (.x r2) (.width r2))))
         (top-dist (- (.y r1) (+ (.y r2) (.height r2))))
         (bottom-dist (- (.y r2) (+ (.y r1) (.height r1))))
         (horz-dist (math.max left-dist right-dist))
         (vert-dist (math.max top-dist bottom-dist)))
    (cond ((not (eq (>= (%cm-fuzzy-compare horz-dist 0 elk-math.double-eq-epsilon) 0)
                    (>= (%cm-fuzzy-compare vert-dist 0 elk-math.double-eq-epsilon) 0)))
           (math.max vert-dist horz-dist))
          ((> (%cm-fuzzy-compare horz-dist 0 elk-math.double-eq-epsilon) 0)
           (math.sqrt (+ (* vert-dist vert-dist) (* horz-dist horz-dist))))
          (t (- (math.sqrt (+ (* vert-dist vert-dist) (* horz-dist horz-dist))))))))
