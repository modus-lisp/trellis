;;;; KVector, KVectorChain, Spacing / ElkPadding / ElkMargin, ElkRectangle

(in-package #:elk)

;;; ------------------------------------------------------------ helpers

(defun %cm-int32 (x)
  (let ((v (ldb (byte 32 0) x))) (if (logbitp 31 v) (- v (ash 1 32)) v)))

(defun %cm-double-bits (d)
  "Double.doubleToLongBits as an unsigned 64-bit integer (NaN canonical)."
  (let ((d (jd d)))
    (if (nan-p d)
        #x7ff8000000000000
        (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits d)) 32)
                (sb-kernel:double-float-low-bits d)))))

(defun %cm-double-hash (d)
  "Double.hashCode"
  (let ((bits (%cm-double-bits d)))
    (%cm-int32 (logxor (ldb (byte 32 0) bits) (ash bits -32)))))

(defun %cm-integer-reverse (i)
  "Integer.reverse"
  (let ((u (ldb (byte 32 0) i)) (r 0))
    (dotimes (k 32) (setf r (logior (ash r 1) (ldb (byte 1 k) u))))
    (%cm-int32 r)))

(defun %cm-double-equals (a b)
  "Double.equals on boxed doubles (Objects.equals(Double, Double))."
  (= (%cm-double-bits a) (%cm-double-bits b)))

(defun %cm-split (string delims)
  "String.split with a regex that is an alternation of single characters
DELIMS: trailing empty strings are removed; an empty input gives (\"\")."
  (if (zerop (length string))
      (list "")
      (let ((tokens '()) (start 0))
        (loop for i from 0 below (length string)
              when (find (char string i) delims)
                do (push (subseq string start i) tokens)
                   (setf start (1+ i)))
        (push (subseq string start) tokens)
        ;; tokens is reversed: drop the trailing empties from its front
        (loop while (and tokens (zerop (length (first tokens)))) do (pop tokens))
        (nreverse tokens))))

(defun %cm-java-trim (s)
  "String.trim: strip chars <= space."
  (string-trim (loop for c from 0 to 32 collect (code-char c)) s))

(defun %cm-parse-double (s)
  "Double.parseDouble; a malformed string signals ILLEGAL-ARGUMENT-EXCEPTION
(NumberFormatException is an IllegalArgumentException in Java)."
  (let* ((tr (%cm-java-trim s))
         (body (string-left-trim "+-" tr))
         (neg (and (plusp (length tr)) (char= (char tr 0) #\-))))
    (flet ((fail () (jthrow 'illegal-argument-exception "NumberFormatException: For input string: \"~A\"" s)))
      (cond ((string= body "NaN") (if (string= tr "NaN") double.nan (fail)))
            ((string= body "Infinity")
             (if (<= (- (length tr) (length body)) 1)
                 (if neg double.negative-infinity double.positive-infinity)
                 (fail)))
            (t
             (let* ((core (if (and (plusp (length tr)) (find (char tr (1- (length tr))) "dDfF"))
                              (subseq tr 0 (1- (length tr)))
                              tr)))
               (when (or (zerop (length core))
                         (> (- (length core) (length (string-left-trim "+-" core))) 1)
                         (notevery (lambda (c) (find c "0123456789+-.eE")) core)
                         (notany #'digit-char-p core))
                 (fail))
               (let ((x (handler-case
                            (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
                              (read-from-string
                               ;; CL does not read ".5" / "5." / "5.e3" the Java way everywhere
                               (let ((c core))
                                 (when (and (plusp (length c)) (char= (char c (1- (length c))) #\.))
                                   (setf c (concatenate 'string c "0")))
                                 (let ((p (search ".e" c :test #'char-equal)))
                                   (when p (setf c (concatenate 'string (subseq c 0 (1+ p)) "0" (subseq c (1+ p))))))
                                 c)))
                          (error () nil))))
                 (if (realp x) (jd x) (fail)))))))))

(defun %cm-isdelim (c delims) (and (find c delims) t))

(defun %cm-trim-delims (string)
  "The start/end scan shared by KVector.parse and Spacing.parse."
  (let ((start 0) (end (length string)))
    (loop while (and (< start (length string))
                     (%cm-isdelim (char string start) (format nil "([{\"' ~C~C~C" #\Tab #\Return #\Newline)))
          do (incf start))
    (loop while (and (> end 0)
                     (%cm-isdelim (char string (1- end)) (format nil ")]}\"' ~C~C~C" #\Tab #\Return #\Newline)))
          do (decf end))
    (values start end)))

;;; ------------------------------------------------------------ KVector

(defjconst k-vector.default-fuzzyness 0.05)

(defjctor k-vector ()
  (setf (.x this) 0.0 (.y this) 0.0))

(defjctor k-vector (a)
  ;; KVector(KVector v) | KVector(double angle)
  (if (typep a 'k-vector)
      (setf (.x this) (.x a) (.y this) (.y a))
      (setf (.x this) (math.cos a) (.y this) (math.sin a))))

(defjctor k-vector (a b)
  ;; KVector(double, double) | KVector(KVector start, KVector end)
  (if (typep a 'k-vector)
      (setf (.x this) (- (.x b) (.x a)) (.y this) (- (.y b) (.y a)))
      (setf (.x this) (jd a) (.y this) (jd b))))

(defjmethod clone ((this k-vector)) (new-k-vector/2 (.x this) (.y this)))
(defmethod jclone ((this k-vector)) (new-k-vector/2 (.x this) (.y this)))

(defjmethod to-string ((this k-vector)) (jstr "(" (.x this) "," (.y this) ")"))
(defmethod print-object ((v k-vector) s)
  (print-unreadable-object (v s) (format s "KVector ~A,~A" (.x v) (.y v))))

(defjmethod equals ((this k-vector) obj)
  (and (typep obj 'k-vector) (= (.x this) (.x obj)) (= (.y this) (.y obj))))

(defjmethod equals-fuzzily ((this k-vector) other)
  (equals-fuzzily/2 this other k-vector.default-fuzzyness))
(defjmethod equals-fuzzily ((this k-vector) other fuzzyness)
  (and (<= (math.abs (- (.x this) (.x other))) fuzzyness)
       (<= (math.abs (- (.y this) (.y other))) fuzzyness)))

(defjmethod hash-code ((this k-vector))
  (%cm-int32 (+ (%cm-double-hash (.x this)) (%cm-integer-reverse (%cm-double-hash (.y this))))))

(defjmethod j-length ((this k-vector))
  (math.sqrt (+ (* (.x this) (.x this)) (* (.y this) (.y this)))))
(defjmethod square-length ((this k-vector))
  (+ (* (.x this) (.x this)) (* (.y this) (.y this))))

(defjmethod reset ((this k-vector))
  (setf (.x this) 0.0 (.y this) 0.0) this)

(defjmethod j-set ((this k-vector) other)
  (setf (.x this) (.x other) (.y this) (.y other)) this)
(defjmethod j-set ((this k-vector) new-x new-y)
  (setf (.x this) (jd new-x) (.y this) (jd new-y)) this)

(defjmethod add ((this k-vector) v)
  (incf (.x this) (.x v)) (incf (.y this) (.y v)) this)
(defjmethod add ((this k-vector) dx dy)
  (incf (.x this) dx) (incf (.y this) dy) this)

(defjstatic k-vector.sum (vs)
  (let ((sum (new-k-vector/0)))
    (jdo (v vs) (incf (.x sum) (.x v)) (incf (.y sum) (.y v)))
    sum))
;; varargs convenience: (k-vector.sum a b) / (k-vector.sum a b c)
(defun k-vector.sum/2 (a b) (k-vector.sum/1 (list a b)))
(defun k-vector.sum/3 (a b c) (k-vector.sum/1 (list a b c)))

(defjmethod sub ((this k-vector) v)
  (decf (.x this) (.x v)) (decf (.y this) (.y v)) this)
(defjmethod sub ((this k-vector) dx dy)
  (decf (.x this) dx) (decf (.y this) dy) this)

(defjstatic k-vector.diff (v1 v2)
  (new-k-vector/2 (- (.x v1) (.x v2)) (- (.y v1) (.y v2))))

(defjmethod scale ((this k-vector) s)
  (setf (.x this) (* (.x this) s) (.y this) (* (.y this) s)) this)
(defjmethod scale ((this k-vector) sx sy)
  (setf (.x this) (* (.x this) sx) (.y this) (* (.y this) sy)) this)

(defjmethod normalize ((this k-vector))
  (let ((len (j-length/0 this)))
    (when (> len 0)
      (setf (.x this) (/ (.x this) len) (.y this) (/ (.y this) len))))
  this)

(defjmethod scale-to-length ((this k-vector) len)
  (normalize/0 this) (scale/1 this len) this)

(defjmethod negate ((this k-vector))
  (setf (.x this) (- (.x this)) (.y this) (- (.y this))) this)

(defjmethod to-degrees ((this k-vector)) (math.to-degrees (to-radians/0 this)))

(defjmethod to-radians ((this k-vector))
  (let ((len (j-length/0 this)) (x (.x this)) (y (.y this)))
    (cond ((and (>= x 0) (>= y 0)) (math.asin (/ y len)))
          ((< x 0) (- math.pi (math.asin (/ y len))))
          (t (+ (* 2 math.pi) (math.asin (/ y len)))))))

(defjmethod wiggle ((this k-vector) random amount)
  (incf (.x this) (- (* (random-next-double random) amount) (/ amount 2)))
  (incf (.y this) (- (* (random-next-double random) amount) (/ amount 2)))
  nil)

(defjmethod distance ((this k-vector) v2)
  (let ((dx (- (.x this) (.x v2))) (dy (- (.y this) (.y v2))))
    (math.sqrt (+ (* dx dx) (* dy dy)))))

(defjmethod dot-product ((this k-vector) v2)
  (+ (* (.x this) (.x v2)) (* (.y this) (.y v2))))

(defjstatic k-vector.cross-product (v w)
  (- (* (.x v) (.y w)) (* (.y v) (.x w))))

(defjmethod rotate ((this k-vector) angle)
  (let ((new-x (- (* (.x this) (math.cos angle)) (* (.y this) (math.sin angle)))))
    (setf (.y this) (+ (* (.x this) (math.sin angle)) (* (.y this) (math.cos angle))))
    (setf (.x this) new-x))
  this)

(defjmethod angle ((this k-vector) other)
  (with-java-floats
    (math.acos (/ (dot-product/1 this other) (* (j-length/0 this) (j-length/0 other))))))

(defjmethod bound ((this k-vector) lowx lowy highx highy)
  (when (or (< highx lowx) (< highy lowy))
    (jthrow 'illegal-argument-exception
            "The highx must be bigger then lowx and the highy must be bigger then lowy"))
  (cond ((< (.x this) lowx) (setf (.x this) (jd lowx)))
        ((> (.x this) highx) (setf (.x this) (jd highx))))
  (cond ((< (.y this) lowy) (setf (.y this) (jd lowy)))
        ((> (.y this) highy) (setf (.y this) (jd highy))))
  this)

(defjmethod is-na-n ((this k-vector)) (or (nan-p (.x this)) (nan-p (.y this))))
(defjmethod is-infinite ((this k-vector)) (or (infinite-p (.x this)) (infinite-p (.y this))))

(defjmethod parse ((this k-vector) string)
  (multiple-value-bind (start end) (%cm-trim-delims string)
    (when (>= start end)
      (jthrow 'illegal-argument-exception "The given string does not contain any numbers."))
    (let ((tokens (%cm-split (subseq string start end) (format nil ",;~C~C" #\Return #\Newline))))
      (unless (= (length tokens) 2)
        (jthrow 'illegal-argument-exception "Exactly two numbers are expected, ~D were found."
                (length tokens)))
      (setf (.x this) (%cm-parse-double (first tokens))
            (.y this) (%cm-parse-double (second tokens)))))
  nil)

;;; ------------------------------------------------------------ KVectorChain

(defjctor k-vector-chain ())

(defjctor k-vector-chain (coll)
  ;; KVectorChain(Collection<KVector>) | KVectorChain(KVector... vectors)
  (if (typep coll 'k-vector)
      (jl-add (.items this) coll)
      (jdo (v coll) (jl-add (.items this) v))))

(defmethod jiterable-items ((c k-vector-chain)) (.items c))
(defmethod jclone ((c k-vector-chain))
  ;; LinkedList.clone(): a shallow copy
  (new-k-vector-chain/1 c))
(defjmethod clone ((c k-vector-chain)) (jclone c))

(defjmethod to-string ((this k-vector-chain))
  (with-output-to-string (s)
    (write-string "(" s)
    (let ((items (.items this)))
      (loop for i from 0 below (length items)
            for v = (aref items i)
            do (write-string (jstr (.x v) "," (.y v)) s)
               (when (< (1+ i) (length items)) (write-string "; " s))))
    (write-string ")" s)))
(defmethod print-object ((c k-vector-chain) s)
  (print-unreadable-object (c s) (format s "KVectorChain ~A" (to-string/0 c))))

(defjmethod equals ((this k-vector-chain) o)
  ;; AbstractList.equals: any list with equal elements in order
  (cond ((typep o 'k-vector-chain) (jl-equal (.items this) (.items o)))
        ((and (vectorp o) (not (stringp o))) (jl-equal (.items this) o))
        (t nil)))

(defjmethod hash-code ((this k-vector-chain))
  (let ((h 1))
    (jdo (v (.items this)) (setf h (%cm-int32 (+ (* 31 h) (if v (hash-code/0 v) 0)))))
    h))

;;; collection protocol (LinkedList)
(defjmethod size ((c k-vector-chain)) (length (.items c)))
(defjmethod is-empty ((c k-vector-chain)) (zerop (length (.items c))))
(defjmethod j-get ((c k-vector-chain) i) (jl-get (.items c) i))
(defjmethod j-set ((c k-vector-chain) i v) (jl-set (.items c) i v))
(defjmethod contains ((c k-vector-chain) x) (jl-contains (.items c) x))
(defjmethod index-of ((c k-vector-chain) x) (jl-index-of (.items c) x))
(defjmethod last-index-of ((c k-vector-chain) x) (jl-last-index-of (.items c) x))
(defjmethod clear ((c k-vector-chain)) (jl-clear (.items c)))
(defjmethod j-remove ((c k-vector-chain))
  ;; LinkedList.remove() = removeFirst()
  (jl-remove-first (.items c)))
(defjmethod j-remove ((c k-vector-chain) x)
  ;; remove(int index) | remove(Object o)
  (if (integerp x) (jl-remove-at (.items c) x) (jl-remove (.items c) x)))
(defjmethod remove-first ((c k-vector-chain)) (jl-remove-first (.items c)))
(defjmethod remove-last ((c k-vector-chain)) (jl-remove-last (.items c)))
(defjmethod get-first ((c k-vector-chain)) (jl-first (.items c)))
(defjmethod get-last ((c k-vector-chain)) (jl-last (.items c)))
(defjmethod peek ((c k-vector-chain)) (jl-peek-first (.items c)))
(defjmethod peek-first ((c k-vector-chain)) (jl-peek-first (.items c)))
(defjmethod peek-last ((c k-vector-chain)) (jl-peek-last (.items c)))
(defjmethod poll ((c k-vector-chain)) (jl-poll-first (.items c)))
(defjmethod poll-first ((c k-vector-chain)) (jl-poll-first (.items c)))
(defjmethod poll-last ((c k-vector-chain)) (jl-poll-last (.items c)))
(defjmethod element ((c k-vector-chain)) (jl-first (.items c)))
(defjmethod offer ((c k-vector-chain) x) (jl-add (.items c) x))
(defjmethod j-push ((c k-vector-chain) x) (jl-add-first (.items c) x) nil)
(defjmethod j-pop ((c k-vector-chain)) (jl-remove-first (.items c)))
(defjmethod sub-list ((c k-vector-chain) a b) (jl-sub-list (.items c) a b))
(defjmethod for-each ((c k-vector-chain) fn) (jdo (x (.items c)) (funcall fn x)))
(defjmethod contains-all ((c k-vector-chain) other)
  (block nil (jdo (x other) (unless (jl-contains (.items c) x) (return nil))) t))

(defjmethod to-array ((this k-vector-chain))
  (coerce (.items this) 'simple-vector))
(defjmethod to-array ((this k-vector-chain) begin-index)
  (when (or (< begin-index 0) (> begin-index (length (.items this))))
    (jthrow 'index-out-of-bounds-exception))
  (coerce (subseq (.items this) begin-index) 'simple-vector))

(defjmethod parse ((this k-vector-chain) string)
  (let ((tokens (%cm-split string (format nil ",;()[]{} ~C~C" #\Tab #\Newline))))
    (jl-clear (.items this))
    (let ((xy 0) (x 0.0) (y 0.0))
      (dolist (tok tokens)
        (when (plusp (length (%cm-java-trim tok)))
          (if (evenp xy)
              (setf x (%cm-parse-double tok))
              (setf y (%cm-parse-double tok)))
          (when (and (> xy 0) (oddp xy))
            (jl-add (.items this) (new-k-vector/2 x y)))
          (incf xy)))))
  nil)

(defjmethod add ((this k-vector-chain)) (jl-add (.items this) (new-k-vector/0)) nil)
(defjmethod add ((this k-vector-chain) v) (jl-add (.items this) v))
(defjmethod add ((this k-vector-chain) a b)
  ;; add(int index, KVector element) | add(double x, double y)
  (if (typep b 'k-vector)
      (progn (jl-add-at (.items this) a b) nil)
      (progn (jl-add (.items this) (new-k-vector/2 a b)) nil)))

(defjmethod add-first ((this k-vector-chain)) (jl-add-first (.items this) (new-k-vector/0)) nil)
(defjmethod add-first ((this k-vector-chain) v) (jl-add-first (.items this) v) nil)
(defjmethod add-first ((this k-vector-chain) x y) (jl-add-first (.items this) (new-k-vector/2 x y)) nil)
(defjmethod add-last ((this k-vector-chain)) (jl-add (.items this) (new-k-vector/0)) nil)
(defjmethod add-last ((this k-vector-chain) v) (jl-add (.items this) v) nil)
(defjmethod add-last ((this k-vector-chain) x y) (jl-add (.items this) (new-k-vector/2 x y)) nil)

(defjmethod add-all ((this k-vector-chain) coll)
  ;; addAll(Collection) | addAll(KVector...)
  (let ((items (.items this)) (snapshot (if (eq coll this) (copy-seq (.items this)) coll)))
    (if (typep snapshot 'k-vector)
        (jl-add items snapshot)
        (jdo (v snapshot) (jl-add items v))))
  t)
(defjmethod add-all ((this k-vector-chain) index coll)
  (jl-add-all-at (.items this) index (if (eq coll this) (copy-seq (.items this)) coll)))

(defjmethod add-all-as-copies ((this k-vector-chain) index chain)
  (let ((copies (new-jlist)))
    (jdo (v chain) (jl-add copies (new-k-vector/1 v)))
    (jl-add-all-at (.items this) index copies))
  nil)

(defjmethod scale ((this k-vector-chain) s)
  (jdo (v (.items this)) (scale/1 v s)) this)
(defjmethod scale ((this k-vector-chain) sx sy)
  (jdo (v (.items this)) (scale/2 v sx sy)) this)

(defjmethod offset ((this k-vector-chain) off)
  (jdo (v (.items this)) (add/1 v off)) this)
(defjmethod offset ((this k-vector-chain) dx dy)
  (jdo (v (.items this)) (add/2 v dx dy)) this)

(defjmethod offset-non-mutating ((this k-vector-chain) off)
  (let ((result (new-k-vector-chain/0)))
    (jdo (v (.items this)) (jl-add (.items result) (k-vector.sum/1 (list v off))))
    result))

(defjmethod total-length ((this k-vector-chain))
  (let ((len 0.0) (items (.items this)))
    (when (>= (length items) 2)
      (loop for i from 1 below (length items)
            do (incf len (distance/1 (aref items (1- i)) (aref items i)))))
    len))

(defjmethod has-na-n ((this k-vector-chain))
  (and (some (lambda (v) (is-na-n/0 v)) (.items this)) t))
(defjmethod has-infinite ((this k-vector-chain))
  (and (some (lambda (v) (is-infinite/0 v)) (.items this)) t))

(defun %cm-point-between (current next abs-distance old-sum additional)
  (let ((this-relative (/ (- abs-distance old-sum) additional))
        (result (sub/1 (clone/0 next) current)))
    (scale/1 result this-relative)
    (add/1 result current)
    result))

(defjmethod point-on-line ((this k-vector-chain) dist)
  (let* ((items (.items this)) (n (length items)))
    (cond
      ((>= n 2)
       (let ((abs-distance (math.abs dist)) (distance-sum 0.0))
         (if (>= dist 0)
             ;; traverse the points in normal direction
             (let* ((cursor 1) (current (aref items 0)))
               (loop
                 (let* ((old-sum distance-sum)
                        (next (aref items cursor))
                        (additional (distance/1 current next)))
                   (incf cursor)
                   (when (> additional 0)
                     (incf distance-sum additional)
                     (when (>= distance-sum abs-distance)
                       (return-from point-on-line/1
                         (%cm-point-between current next abs-distance old-sum additional))))
                   (setf current next)
                   (unless (< cursor n) (return))))
               ;; iter.previous()
               (aref items (1- cursor)))
             ;; traverse the points in reversed direction; Java's
             ;; listIterator(size() - 1).previous() is element size()-2
             (let* ((cursor (- n 2)) (current (aref items cursor)))
               (loop
                 (let ((old-sum distance-sum))
                   (when (<= cursor 0) (jthrow 'no-such-element-exception))
                   (decf cursor)
                   (let* ((next (aref items cursor))
                          (additional (distance/1 current next)))
                     (when (> additional 0)
                       (incf distance-sum additional)
                       (when (>= distance-sum abs-distance)
                         (return-from point-on-line/1
                           (%cm-point-between current next abs-distance old-sum additional))))
                     (setf current next)
                     (unless (> cursor 0) (return)))))
               ;; iter.next()
               (aref items cursor)))))
      ((= n 1) (aref items 0))
      (t (jthrow 'illegal-state-exception "Cannot determine a point on an empty vector chain.")))))

(defjmethod angle-on-line ((this k-vector-chain) dist)
  (let* ((items (.items this)) (n (length items)))
    (unless (>= n 2)
      (jthrow 'illegal-state-exception "Need at least two points to determine an angle."))
    (let ((abs-distance (math.abs dist)) (distance-sum 0.0) current next)
      (if (>= dist 0)
          (let ((cursor 1))
            (setf next (aref items 0))
            (loop
              (setf current next
                    next (aref items cursor))
              (incf cursor)
              (let ((additional (distance/1 current next)))
                (when (> additional 0)
                  (incf distance-sum additional)
                  (when (>= distance-sum abs-distance) (return))))
              (unless (< cursor n) (return))))
          (let ((cursor (- n 2)))
            (setf next (aref items cursor))
            (loop
              (setf current next)
              (when (<= cursor 0) (jthrow 'no-such-element-exception))
              (decf cursor)
              (setf next (aref items cursor))
              (let ((additional (distance/1 current next)))
                (when (> additional 0)
                  (incf distance-sum additional)
                  (when (>= distance-sum abs-distance) (return))))
              (unless (> cursor 0) (return)))))
      (to-radians/0 (sub/1 (clone/0 next) current)))))

(defjstatic k-vector-chain.reverse (chain)
  (let ((result (new-k-vector-chain/0)))
    (jdo (v chain) (jl-add-first (.items result) (new-k-vector/1 v)))
    result))

;;; ------------------------------------------------------------ Spacing

(defjctor spacing ())
(defjctor spacing (top right bottom left)
  (j-set/4 this top right bottom left))

(defjmethod j-set ((this spacing) other)
  (j-set/4 this (.top other) (.right other) (.bottom other) (.left other)))
(defjmethod j-set ((this spacing) new-top new-right new-bottom new-left)
  (setf (.top this) (jd new-top) (.right this) (jd new-right)
        (.bottom this) (jd new-bottom) (.left this) (jd new-left))
  nil)

(defjmethod get-top ((this spacing)) (.top this))
(defjmethod set-top ((this spacing) v) (setf (.top this) (jd v)) nil)
(defjmethod get-right ((this spacing)) (.right this))
(defjmethod set-right ((this spacing) v) (setf (.right this) (jd v)) nil)
(defjmethod get-bottom ((this spacing)) (.bottom this))
(defjmethod set-bottom ((this spacing) v) (setf (.bottom this) (jd v)) nil)
(defjmethod get-left ((this spacing)) (.left this))
(defjmethod set-left ((this spacing) v) (setf (.left this) (jd v)) nil)
(defjmethod set-left-right ((this spacing) v) (setf (.left this) (jd v) (.right this) (jd v)) nil)
(defjmethod set-top-bottom ((this spacing) v) (setf (.top this) (jd v) (.bottom this) (jd v)) nil)
(defjmethod get-horizontal ((this spacing)) (+ (get-left/0 this) (get-right/0 this)))
(defjmethod get-vertical ((this spacing)) (+ (get-top/0 this) (get-bottom/0 this)))

(defjmethod equals ((this spacing) obj)
  (and (typep obj 'spacing)
       (= (.top this) (.top obj)) (= (.bottom this) (.bottom obj))
       (= (.left this) (.left obj)) (= (.right this) (.right obj))))

(defjmethod hash-code ((this spacing))
  (let* ((code1 (logior (%cm-int32 (ash (%cm-double-hash (.left this)) 16))
                        (logand (%cm-double-hash (.bottom this)) #xffff)))
         (code2 (logior (%cm-int32 (ash (%cm-double-hash (.right this)) 16))
                        (logand (%cm-double-hash (.top this)) #xffff))))
    (%cm-int32 (logxor code1 code2))))

(defjmethod to-string ((this spacing))
  (jstr "[top=" (.top this) ",left=" (.left this) ",bottom=" (.bottom this)
        ",right=" (.right this) "]"))
(defmethod print-object ((sp spacing) s)
  (print-unreadable-object (sp s :type t) (write-string (to-string/0 sp) s)))

(defjmethod parse ((this spacing) string)
  (multiple-value-bind (start end) (%cm-trim-delims string)
    (when (< start end)
      (dolist (token (%cm-split (subseq string start end) ",;"))
        (let ((kv (%cm-split token "=")))
          (unless (= (length kv) 2)
            (jthrow 'illegal-argument-exception "Expecting a list of key-value pairs."))
          (let ((key (%cm-java-trim (first kv)))
                (value (%cm-parse-double (second kv))))
            (cond ((string= key "top") (setf (.top this) value))
                  ((string= key "left") (setf (.left this) value))
                  ((string= key "bottom") (setf (.bottom this) value))
                  ((string= key "right") (setf (.right this) value))))))))
  nil)

(defjmethod copy ((this spacing) other)
  (setf (.left this) (.left other) (.right this) (.right other)
        (.top this) (.top other) (.bottom this) (.bottom other))
  this)

(defjmethod add ((this spacing) other)
  (incf (.left this) (.left other)) (incf (.right this) (.right other))
  (incf (.top this) (.top other)) (incf (.bottom this) (.bottom other))
  this)

(defmethod jclone ((this spacing)) (clone/0 this))

;;; ElkPadding / ElkMargin -- identical constructor sets
(defmacro %cm-define-spacing-ctors (class)
  `(progn
     (defjctor ,class ())
     (defjctor ,class (a)
       ;; (double any) | (ElkPadding other)
       (if (typep a 'spacing)
           (super-ctor spacing this (.top a) (.right a) (.bottom a) (.left a))
           (super-ctor spacing this a a a a)))
     (defjctor ,class (left-right top-bottom)
       (super-ctor spacing this top-bottom left-right top-bottom left-right))
     (defjctor ,class (top right bottom left)
       (super-ctor spacing this top right bottom left))
     (defjmethod clone ((this ,class))
       (,(intern (format nil "NEW-~A/1" (symbol-name class)) :elk) this))))

(%cm-define-spacing-ctors elk-padding)
(%cm-define-spacing-ctors elk-margin)

;;; ------------------------------------------------------------ ElkRectangle

(defjctor elk-rectangle () (this-ctor elk-rectangle this 0 0 0 0))
(defjctor elk-rectangle (x y w h)
  (setf (.x this) (jd x) (.y this) (jd y) (.width this) (jd w) (.height this) (jd h)))
(defjctor elk-rectangle (rect)
  (setf (.x this) (.x rect) (.y this) (.y rect)
        (.width this) (.width rect) (.height this) (.height rect)))

(defjmethod set-rect ((this elk-rectangle) nx ny nw nh)
  (setf (.x this) (jd nx) (.y this) (jd ny) (.width this) (jd nw) (.height this) (jd nh))
  nil)

(defjmethod get-position ((this elk-rectangle)) (new-k-vector/2 (.x this) (.y this)))
(defjmethod get-top-left ((this elk-rectangle)) (get-position/0 this))
(defjmethod get-top-right ((this elk-rectangle)) (new-k-vector/2 (+ (.x this) (.width this)) (.y this)))
(defjmethod get-bottom-left ((this elk-rectangle)) (new-k-vector/2 (.x this) (+ (.y this) (.height this))))
(defjmethod get-bottom-right ((this elk-rectangle))
  (new-k-vector/2 (+ (.x this) (.width this)) (+ (.y this) (.height this))))
(defjmethod get-center ((this elk-rectangle))
  (new-k-vector/2 (+ (.x this) (/ (.width this) 2)) (+ (.y this) (/ (.height this) 2))))

(defjmethod j-union ((this elk-rectangle) other)
  (let ((x1 (math.min (.x this) (.x other)))
        (y1 (math.min (.y this) (.y other)))
        (x2 (math.max (+ (.x this) (.width this)) (+ (.x other) (.width other))))
        (y2 (math.max (+ (.y this) (.height this)) (+ (.y other) (.height other)))))
    (when (< x2 x1) (rotatef x1 x2))
    (when (< y2 y1) (rotatef y1 y2))
    (set-rect/4 this x1 y1 (- x2 x1) (- y2 y1))))

(defjmethod move ((this elk-rectangle) off)
  (incf (.x this) (.x off)) (incf (.y this) (.y off)) nil)

(defjmethod to-string ((this elk-rectangle))
  (jstr "Rect[x=" (.x this) ",y=" (.y this) ",w=" (.width this) ",h=" (.height this) "]"))
(defmethod print-object ((r elk-rectangle) s)
  (print-unreadable-object (r s) (write-string (to-string/0 r) s)))

(defjmethod equals ((this elk-rectangle) obj)
  (and (typep obj 'elk-rectangle)
       (%cm-double-equals (.x this) (.x obj)) (%cm-double-equals (.y this) (.y obj))
       (%cm-double-equals (.width this) (.width obj)) (%cm-double-equals (.height this) (.height obj))))

(defjmethod hash-code ((this elk-rectangle))
  (let ((h 1))
    (dolist (v (list (.x this) (.y this) (.width this) (.height this)) h)
      (setf h (%cm-int32 (+ (* 31 h) (%cm-double-hash v)))))))

(defjmethod get-max-x ((this elk-rectangle)) (+ (.x this) (.width this)))
(defjmethod get-max-y ((this elk-rectangle)) (+ (.y this) (.height this)))

(defjmethod intersects ((this elk-rectangle) rect)
  (let ((r1x1 (.x this)) (r1y1 (.y this))
        (r1x2 (+ (.x this) (.width this))) (r1y2 (+ (.y this) (.height this)))
        (r2x1 (.x rect)) (r2y1 (.y rect))
        (r2x2 (+ (.x rect) (.width rect))) (r2y2 (+ (.y rect) (.height rect))))
    (and (< r1x1 r2x2) (> r1x2 r2x1) (> r1y2 r2y1) (< r1y1 r2y2))))
