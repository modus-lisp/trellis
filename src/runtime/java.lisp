;;;; java.lisp -- the Java semantics the port leans on.
;;;;
;;;;  - call macros: a Java call x.foo(a, b) is (foo x a b), which expands to
;;;;    the generic function FOO/2 chosen by SYNTACTIC argument count, so Java
;;;;    overloading by arity costs nothing at run time and every method keeps
;;;;    an exact lambda list (call-next-method works).
;;;;  - classes, constructors, enums, enum sets
;;;;  - collections on plain CL objects: ArrayList/LinkedList/ArrayDeque =
;;;;    adjustable vector, HashMap = hash-table, HashSet = JSET,
;;;;    PriorityQueue = JPQ
;;;;  - java.util.Random, bit for bit
;;;;  - IEEE semantics: WITH-JAVA-FLOATS masks traps so 1/0 is infinity
;;;;
;;;; Everything here is described for porters in PORTING.md.

(in-package #:elk)

;;; ------------------------------------------------------------ call macros

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun arity-symbol (name n)
  (intern (format nil "~A/~D" (symbol-name name) n) (symbol-package name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %jcall-expander (name offset)
  (lambda (form env)
    (declare (ignore env))
    (let ((args (cdr form)))
      (list* (arity-symbol name (- (length args) offset)) args)))))

(defmacro define-jcall (name kind)
  "KIND :instance -- (NAME this args...) => (NAME/<#args> this args...)
KIND :static   -- (NAME args...)      => (NAME/<#args> args...)
Idempotent: a name already defined keeps its macro."
  (let ((offset (ecase kind (:instance 1) (:static 0))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (macro-function ',name)
         (setf (macro-function ',name) (%jcall-expander ',name ,offset))))))

(defmacro define-jcalls (kind &rest names)
  `(progn ,@(loop for n in names collect `(define-jcall ,n ,kind))))

(defmacro jfun (name arity)
  "The function object behind a call macro, for FUNCALL/MAPCAR: (jfun get-x 0)."
  `(function ,(arity-symbol name arity)))

;;; ------------------------------------------------------------ classes

(defun %slot-spec (class spec)
  (declare (ignore class))
  (destructuring-bind (name &optional initform &rest more) (if (consp spec) spec (list spec))
    (let ((acc (intern (format nil ".~A" (symbol-name name)) :elk)))
      `(,name :accessor ,acc :initform ,initform :initarg ,(intern (symbol-name name) :keyword)
              ,@more))))

(defmacro defjclass (name supers slots &rest options)
  "A Java class.  SLOTS are (field initform) and get accessor .FIELD.  Defines
the default constructor NEW-NAME/0 unless the class has a DEFJCTOR."
  `(progn
     (defclass ,name ,(or supers '(jobject))
       ,(mapcar (lambda (s) (%slot-spec name s)) slots)
       ,@options)
     (define-jcall ,(intern (format nil "NEW-~A" (symbol-name name)) :elk) :static)
     (unless (fboundp ',(intern (format nil "NEW-~A/0" (symbol-name name)) :elk))
       (defun ,(intern (format nil "NEW-~A/0" (symbol-name name)) :elk) ()
         (let ((this (make-instance ',name)))
           (%implicit-super-init this ',name)
           this)))
     (find-class ',name)))

(defclass jobject () ())

(defmacro defjinterface (name &optional supers)
  `(defclass ,name ,(or supers '(jobject)) ()))

(defun init-name (class n)
  (intern (format nil "%INIT-~A/~D" (symbol-name class) n) :elk))

(defun %implicit-super-init (this class)
  "Java's implicit super(): run the superclass's no-arg constructor body, if any."
  (let ((super (find-if (lambda (c) (and (not (eq c (find-class 'jobject)))
                                         (not (eq c (find-class 'standard-object)))))
                        (cdr (sb-mop:class-direct-superclasses (find-class class))))))
    (declare (ignore super))
    (let ((direct (first (sb-mop:class-direct-superclasses (find-class class)))))
      (when direct
        (let ((f (init-name (class-name direct) 0)))
          (when (fboundp f) (funcall f this)))))))

(defmacro defjctor (class params &body body)
  "A constructor.  The body sees THIS.  Call (super-ctor PARENT this args...)
first to run a superclass constructor; if the body does not, the parent's
no-arg constructor runs (Java's implicit super())."
  (multiple-value-bind (decls rest) (split-leading-declares body)
  (setf body rest)
  (let* ((n (length params))
         (init (init-name class n))
         (maker (intern (format nil "NEW-~A/~D" (symbol-name class) n) :elk))
         (explicit (and body (consp (first body)) (member (car (first body)) '(super-ctor this-ctor)))))
    `(progn
       (defun ,init (this ,@params)
         (declare (ignorable this))
         ,@decls
         ,@(unless explicit `((%implicit-super-init this ',class)))
         ,@body
         this)
       (defun ,maker ,params
         (,init (make-instance ',class) ,@params))
       (define-jcall ,(intern (format nil "NEW-~A" (symbol-name class)) :elk) :static)
       ',maker))))

(defmacro super-ctor (parent this &rest args)
  `(,(init-name parent (length args)) ,this ,@args))

(defmacro this-ctor (class this &rest args)
  "this(...) -- another constructor of the same class."
  `(,(init-name class (length args)) ,this ,@args))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun split-leading-declares (body)
  "(values declares rest): the DECLARE forms at the front of BODY, which must stay
outside the BLOCK the Java-style macros add."
  (let ((decls '()))
    (loop while (and (consp (first body)) (eq (car (first body)) 'declare))
          do (push (pop body) decls))
    (values (nreverse decls) body))))

(defmacro defjmethod (name (this-spec &rest params) &body body)
  "An instance method: (defjmethod foo ((this klass) a b) ...) defines FOO/2.
The body is wrapped in (block foo ...), so RETURN-FROM foo works as in a Java return."
  (let ((g (arity-symbol name (length params))))
    (multiple-value-bind (decls rest) (split-leading-declares body)
      `(progn
         (define-jcall ,name :instance)
         (defmethod ,g (,this-spec ,@params)
           ,@decls
           (block ,name ,@rest))))))

(defmacro defjgeneric (name arity)
  "Declare an abstract/interface method."
  (let ((params (loop for i below arity collect (intern (format nil "A~D" i) :elk))))
    `(progn (define-jcall ,name :instance)
            (defgeneric ,(arity-symbol name arity) (this ,@params)))))

(defmacro defjstatic (name params &body body)
  "A static method: (defjstatic l-graph-util.offset-graph (g x y) ...)."
  (multiple-value-bind (decls rest) (split-leading-declares body)
    `(progn
       (define-jcall ,name :static)
       (defun ,(arity-symbol name (length params)) ,params ,@decls (block ,name ,@rest)))))

(defmacro defjconst (name value)
  "A static final field."
  `(defparameter ,name ,value))

;;; ------------------------------------------------------------ enums

(defclass jenum (jobject)
  ((name :initarg :name :reader enum-name)
   (ordinal :initarg :ordinal :reader enum-ordinal)))

(defmethod print-object ((e jenum) s)
  (format s "~A.~A" (string-downcase (class-name (class-of e))) (enum-name e)))

(defgeneric enum-values-of (class-name))

(defun java-const->lisp (s)
  "NETWORK_SIMPLEX -> NETWORK-SIMPLEX"
  (substitute #\- #\_ (string-upcase s)))

(defmacro defjenum (name constants &key fields supers)
  "(defjenum port-side (undefined north east south west))
With FIELDS, each constant is (CONST field-values...)."
  (let ((consts (mapcar (lambda (c) (if (consp c) c (list c))) constants)))
    `(progn
       (defclass ,name (,@supers jenum)
         ,(loop for f in fields
                collect `(,f :initarg ,(intern (symbol-name f) :keyword)
                             :accessor ,(intern (format nil ".~A" (symbol-name f)) :elk))))
       ,@(loop for c in consts for i from 0
               collect `(defparameter ,(intern (format nil "~A.~A" (symbol-name name) (symbol-name (car c))) :elk)
                          (make-instance ',name :name ,(substitute #\_ #\- (symbol-name (car c)))
                                                :ordinal ,i
                                                ,@(loop for f in fields for v in (cdr c)
                                                        append (list (intern (symbol-name f) :keyword) v)))))
       (let ((all (vector ,@(loop for c in consts
                                  collect (intern (format nil "~A.~A" (symbol-name name) (symbol-name (car c))) :elk)))))
         (defmethod enum-values-of ((c (eql ',name))) all)
         (defjstatic ,(intern (format nil "~A.VALUES" (symbol-name name)) :elk) () (copy-seq all))
         (defjstatic ,(intern (format nil "~A.VALUE-OF" (symbol-name name)) :elk) (s)
           (or (find (substitute #\_ #\- (string-upcase s)) all :key #'enum-name :test #'string=)
               (error 'illegal-argument-exception
                      :message (format nil "No enum constant ~A.~A" ',name s)))))
       ',name)))

(defun enum-value-of (enum-class string)
  "Parse an enum constant by name; NIL if there is none."
  (find (substitute #\_ #\- (string-upcase (string-trim " " string)))
        (enum-values-of enum-class) :key #'enum-name :test #'string=))

(defmacro enum-case (keyform &body clauses)
  "Java switch over an enum: clauses are (CONST-or-list . body), (T . body)."
  (let ((k (gensym)))
    `(let ((,k ,keyform))
       (cond ,@(loop for (keys . body) in clauses
                     collect (if (member keys '(t otherwise))
                                 `(t ,@body)
                                 `((or ,@(loop for key in (if (listp keys) keys (list keys))
                                               collect `(eq ,k ,key)))
                                   ,@body)))))))

(defjmethod compare-to ((a jenum) b) (- (enum-ordinal a) (enum-ordinal b)))
(defjmethod ordinal ((a jenum)) (enum-ordinal a))
(defjmethod name ((a jenum)) (enum-name a))

;;; EnumSet: a mutable set of constants of one enum, iterated in ordinal order.

(defstruct (enum-set (:constructor %make-enum-set (enum items)))
  enum items)

(defun %es-normalize (es)
  (setf (enum-set-items es) (sort (remove-duplicates (enum-set-items es)) #'< :key #'enum-ordinal))
  es)

(defun enum-set.none-of (enum) (%make-enum-set enum nil))
(defun enum-set.all-of (enum) (%make-enum-set enum (coerce (enum-values-of enum) 'list)))
(defun enum-set.of (first &rest more)
  (%es-normalize (%make-enum-set (class-name (class-of first)) (cons first more))))
(defun enum-set.copy-of (es)
  (etypecase es
    (enum-set (%make-enum-set (enum-set-enum es) (copy-list (enum-set-items es))))
    (sequence (let ((l (coerce es 'list)))
                (%es-normalize (%make-enum-set (and l (class-name (class-of (first l)))) l))))))
(defun enum-set.complement-of (es)
  (%make-enum-set (enum-set-enum es)
                  (remove-if (lambda (x) (member x (enum-set-items es)))
                             (coerce (enum-values-of (enum-set-enum es)) 'list))))
(defun enum-set.range (from to)
  (%make-enum-set (class-name (class-of from))
                  (remove-if-not (lambda (x) (<= (enum-ordinal from) (enum-ordinal x) (enum-ordinal to)))
                                 (coerce (enum-values-of (class-name (class-of from))) 'list))))
(defun enum-set-of-list (enum list)
  (%es-normalize (%make-enum-set enum (copy-list list))))

;;; ------------------------------------------------------------ conditions

(define-condition java-exception (error)
  ((message :initarg :message :initform nil :reader exception-message))
  (:report (lambda (c s) (format s "~A: ~A" (type-of c) (exception-message c)))))

(defmacro define-java-exceptions (&rest names)
  `(progn ,@(loop for n in names collect `(define-condition ,n (java-exception) ()))))

(define-java-exceptions illegal-argument-exception illegal-state-exception
  unsupported-operation-exception index-out-of-bounds-exception
  null-pointer-exception no-such-element-exception class-cast-exception
  runtime-exception arithmetic-exception concurrent-modification-exception)

(defun jthrow (type &optional (fmt "") &rest args)
  (error type :message (apply #'format nil fmt args)))

;;; ------------------------------------------------------------ numbers

(defmacro with-java-floats (&body body)
  "IEEE semantics: overflow -> infinity, 0/0 -> NaN, no traps."
  `(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
     ,@body))

(defconstant integer.max-value 2147483647)
(defconstant integer.min-value -2147483648)
(defconstant long.max-value 9223372036854775807)
(defconstant long.min-value -9223372036854775808)
(defconstant short.max-value 32767)
(defconstant double.max-value most-positive-double-float)
(defconstant double.min-value 4.9d-324)
(defconstant double.positive-infinity sb-ext:double-float-positive-infinity)
(defconstant double.negative-infinity sb-ext:double-float-negative-infinity)
(defparameter double.nan (with-java-floats (- double.positive-infinity double.positive-infinity)))
(defconstant float.max-value (float most-positive-single-float 1d0))
(defconstant float.positive-infinity sb-ext:double-float-positive-infinity)
(defconstant float.negative-infinity sb-ext:double-float-negative-infinity)

(declaim (inline nan-p infinite-p jd))
(defun nan-p (x) (and (floatp x) (/= x x)))
(defun infinite-p (x) (and (floatp x) (= x x) (> (abs x) most-positive-double-float)))
(defun jd (x) "Java (double) cast." (float x 1d0))

(defun to-int (x)
  "Java (int) cast: truncate toward zero, NaN -> 0, saturate."
  (cond ((integerp x) (if (<= integer.min-value x integer.max-value) x
                          (- (mod (+ x 2147483648) 4294967296) 2147483648)))
        ((nan-p x) 0)
        ((>= x 2147483647d0) integer.max-value)
        ((<= x -2147483648d0) integer.min-value)
        (t (values (truncate x)))))

(defun to-long (x)
  (cond ((integerp x) x)
        ((nan-p x) 0)
        ((>= x 9.223372036854775807d18) long.max-value)
        ((<= x -9.223372036854775808d18) long.min-value)
        (t (values (truncate x)))))

(defun int+ (a b) (to-int (+ a b)))
(defun int* (a b) (to-int (* a b)))
(defun idiv (a b) "Java int division." (values (truncate a b)))
(defun imod (a b) "Java %." (if (and (integerp a) (integerp b)) (rem a b) (jfmod a b)))
(defun jfmod (a b) (with-java-floats (if (or (nan-p a) (nan-p b) (zerop b) (infinite-p a)) double.nan
                                           (if (infinite-p b) (jd a)
                                               (- a (* b (ftruncate (/ a b))))))))

(defun math.abs (x) (abs x))
(defun math.max (a b) (if (or (nan-p a) (nan-p b)) double.nan (if (> b a) b a)))
(defun math.min (a b) (if (or (nan-p a) (nan-p b)) double.nan (if (< b a) b a)))
(defun math.sqrt (x) (with-java-floats (if (or (nan-p x) (< x 0)) double.nan (sqrt (jd x)))))
(defun math.pow (a b)
  (with-java-floats
    (let ((a (jd a)) (b (jd b)))
      (cond ((zerop b) 1d0)
            ((or (nan-p a) (nan-p b)) double.nan)
            ((and (< a 0) (/= b (ffloor b))) double.nan)
            ((and (zerop a) (< b 0)) double.positive-infinity)
            (t (let ((r (expt a b))) (if (complexp r) double.nan (jd r))))))))
(defun math.floor (x) (if (or (nan-p x) (infinite-p x)) x (ffloor (jd x))))
(defun math.ceil (x) (if (or (nan-p x) (infinite-p x)) x (fceiling (jd x))))
(defun math.round (x)
  "Java Math.round(double): floor(x + 0.5) as a long."
  (cond ((integerp x) x) ((nan-p x) 0) (t (to-long (floor (+ (jd x) 0.5d0))))))
(defun math.rint (x) (if (or (nan-p x) (infinite-p x)) x (fround (jd x))))
(defun math.signum (x) (cond ((nan-p x) x) ((> x 0) 1d0) ((< x 0) -1d0) (t (jd x))))
(defun math.atan2 (y x) (with-java-floats (atan (jd y) (jd x))))
(defun math.atan (x) (with-java-floats (atan (jd x))))
(defun math.sin (x) (sin (jd x)))
(defun math.cos (x) (cos (jd x)))
(defun math.tan (x) (tan (jd x)))
(defun math.acos (x) (with-java-floats (if (<= -1 x 1) (jd (acos (jd x))) double.nan)))
(defun math.asin (x) (with-java-floats (if (<= -1 x 1) (jd (asin (jd x))) double.nan)))
(defun math.log (x) (with-java-floats (cond ((nan-p x) x) ((< x 0) double.nan) ((zerop x) double.negative-infinity)
                                             (t (log (jd x))))))
(defun math.log10 (x) (with-java-floats (if (and (realp x) (> x 0)) (log (jd x) 10d0) (math.log x))))
(defun math.exp (x) (with-java-floats (exp (jd x))))
(defun math.hypot (a b) (math.sqrt (+ (* a a) (* b b))))
(defun math.to-radians (d) (* (jd d) (/ pi 180d0)))
(defun math.to-degrees (r) (* (jd r) (/ 180d0 pi)))
(defconstant math.pi pi)

(defun double.compare (a b)
  "Double.compare: total order, NaN largest, -0.0 < 0.0."
  (cond ((< a b) -1) ((> a b) 1)
        (t (let ((na (nan-p a)) (nb (nan-p b)))
             (cond ((and na nb) 0) (na 1) (nb -1)
                   ((and (floatp a) (floatp b))
                    (let ((sa (minusp (float-sign a))) (sb (minusp (float-sign b))))
                      (cond ((eq sa sb) 0) (sa -1) (t 1))))
                   (t 0))))))
(defun integer.compare (a b) (cond ((< a b) -1) ((> a b) 1) (t 0)))
(defun boolean.compare (a b) (cond ((eq (not a) (not b)) 0) (a 1) (t -1)))
(defun double.is-nan (x) (nan-p x))
(defun double.is-infinite (x) (infinite-p x))
(defun double.parse-double (s)
  (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
    (let ((x (read-from-string (string-right-trim "dDfF" (string-trim " " s)))))
      (if (realp x) (jd x) (jthrow 'illegal-argument-exception "NumberFormatException: ~A" s)))))
(defun integer.parse-int (s)
  (handler-case (parse-integer (string-trim " " s))
    (error () (jthrow 'illegal-argument-exception "NumberFormatException: ~A" s))))

;;; ------------------------------------------------------------ equality

(defgeneric equals/1 (a b))
(defmethod equals/1 (a b)
  (cond ((and (stringp a) (stringp b)) (string= a b))
        ((and (numberp a) (numberp b)) (= a b))
        (t (eql a b))))
(define-jcall equals :instance)
(defun jequals (a b)
  "Objects.equals"
  (cond ((null a) (null b)) ((null b) nil) (t (equals/1 a b))))

;;; ------------------------------------------------------------ collections

(declaim (inline jlist-p))
(defun jlist-p (x) (and (vectorp x) (not (stringp x))))

(defmacro jdo ((var coll &optional result) &body body)
  "Java for-each over a list/vector/jset/enum-set/hash-table (values)/collection object.
The body is compiled ONCE, as a local function; each collection branch calls it.
(Copying it into every branch made nested loops grow 6x per level and exhaust
the heap while compiling.)  RETURN inside the body leaves the loop, as in Java's break."
  (let ((c (gensym)) (i (gensym)) (k (gensym)) (v (gensym)) (f (gensym)))
    `(let ((,c ,coll))
       (block nil
         (flet ((,f (,var) ,@body))
           (declare (dynamic-extent #',f))
           (typecase ,c
             (null nil)
             (vector (let ((,i 0))
                       (loop while (< ,i (length ,c))
                             do (,f (aref ,c ,i))
                                (incf ,i))))
             (list (dolist (,var ,c) (,f ,var)))
             (jset (loop for ,k being the hash-keys of (jset-table ,c) do (,f ,k)))
             (enum-set (dolist (,var (copy-list (enum-set-items ,c))) (,f ,var)))
             (hash-table (loop for ,k being the hash-keys of ,c using (hash-value ,v) do (,f ,v)))
             (t (map nil #',f (jiterable-items ,c)))))
         ,result))))

(defmacro jdo-map ((k v map) &body body)
  "for (Map.Entry e : map.entrySet())"
  `(loop for (,k . ,v) in (loop for kk being the hash-keys of ,map using (hash-value vv) collect (cons kk vv))
         do (progn ,@body)))

(defun jlist (&rest items)
  "new ArrayList<>(Arrays.asList(items...))"
  (let ((v (make-array (max 4 (length items)) :adjustable t :fill-pointer 0)))
    (dolist (i items v) (vector-push-extend i v))))

(defun new-jlist (&optional (capacity 8))
  (make-array (max 1 capacity) :adjustable t :fill-pointer 0))

(defun jlist-from (coll)
  "new ArrayList<>(coll)"
  (let ((v (new-jlist (max 4 (jsize coll)))))
    (jdo (x coll) (vector-push-extend x v))
    v))

(defstruct (jset (:constructor %make-jset (table)))
  table)

(defun new-jset (&optional (test 'eql)) (%make-jset (make-hash-table :test test)))
(defun jset-from (coll &optional (test 'eql))
  (let ((s (new-jset test))) (jdo (x coll) (setf (gethash x (jset-table s)) t)) s))

(defun new-jmap (&optional (test 'eql)) (make-hash-table :test test))

;;; Iteration over anything collection-shaped.

(defgeneric jiterable-items (coll)
  (:documentation "A CL sequence of COLL's elements, for collections defined by ported classes."))

(defun jsize (c)
  (etypecase c
    (null 0)
    (vector (length c))
    (list (length c))
    (jset (hash-table-count (jset-table c)))
    (enum-set (length (enum-set-items c)))
    (hash-table (hash-table-count c))
    (t (size/0 c))))

(defun to-jvector (c)
  "A fresh simple vector of C's elements."
  (let ((acc '())) (jdo (x c) (push x acc)) (coerce (nreverse acc) 'vector)))

(defun to-cl-list (c)
  (let ((acc '())) (jdo (x c) (push x acc)) (nreverse acc)))

;;; Explicit list helpers (use these when the Java static type is a List).

(defun jl-add (l x) (vector-push-extend x l) t)
(defun jl-add-at (l i x)
  (vector-push-extend x l)
  (replace l l :start1 (1+ i) :start2 i :end2 (1- (length l)))
  (setf (aref l i) x)
  nil)
(defun jl-get (l i)
  "list.get(i).  Vectors take the fast path; any other list (an EMF EList, say)
dispatches to the generic J-GET."
  (if (vectorp l)
      (progn
        (unless (< -1 i (length l))
          (jthrow 'index-out-of-bounds-exception "Index ~D out of bounds for length ~D" i (length l)))
        (aref l i))
      (j-get l i)))
(defun jl-set (l i x) (prog1 (jl-get l i) (setf (aref l i) x)))
(defun jl-remove-at (l i)
  (let ((x (jl-get l i)))
    (replace l l :start1 i :start2 (1+ i))
    (decf (fill-pointer l))
    x))
(defun jl-remove (l x)
  "remove(Object): first element EQUALS to X; returns T if found."
  (let ((i (position x l :test #'jequals)))
    (when i (jl-remove-at l i) t)))
(defun jl-index-of (l x) (or (position x l :test #'jequals) -1))
(defun jl-last-index-of (l x) (or (position x l :test #'jequals :from-end t) -1))
(defun jl-contains (l x) (and (position x l :test #'jequals) t))
(defun jl-clear (l) (setf (fill-pointer l) 0) nil)
(defun jl-add-all (l coll) (jdo (x coll) (vector-push-extend x l)) t)
(defun jl-add-all-at (l i coll)
  (let ((items (to-jvector coll)))
    (loop for x across items for j from i do (jl-add-at l j x))
    (plusp (length items))))
(defun jl-first (l) (if (zerop (length l)) (jthrow 'no-such-element-exception) (aref l 0)))
(defun jl-last (l) (if (zerop (length l)) (jthrow 'no-such-element-exception) (aref l (1- (length l)))))
(defun jl-remove-first (l) (if (zerop (length l)) (jthrow 'no-such-element-exception) (jl-remove-at l 0)))
(defun jl-remove-last (l) (if (zerop (length l)) (jthrow 'no-such-element-exception) (vector-pop l)))
(defun jl-poll-first (l) (if (zerop (length l)) nil (jl-remove-at l 0)))
(defun jl-poll-last (l) (if (zerop (length l)) nil (vector-pop l)))
(defun jl-peek-first (l) (if (zerop (length l)) nil (aref l 0)))
(defun jl-peek-last (l) (if (zerop (length l)) nil (aref l (1- (length l)))))
(defun jl-add-first (l x) (jl-add-at l 0 x))
(defun jl-sub-list (l from to) (let ((v (new-jlist (- to from)))) (loop for i from from below to do (vector-push-extend (aref l i) v)) v))
(defun jl-copy (l) (jlist-from l))
(defun jl-reverse (l) "Collections.reverse, in place." (let ((r (reverse l))) (replace l r) l))
(defun jl-sort (l cmp)
  "Collections.sort / List.sort: stable, comparator returns an int."
  (let ((sorted (stable-sort (subseq l 0) (lambda (a b) (< (funcall cmp a b) 0)))))
    (replace l sorted) l))
(defun jl-sort-natural (l) (jl-sort l #'jcompare))
(defun jl-remove-if (l pred)
  (let ((keep (remove-if pred l)))
    (prog1 (/= (length keep) (length l))
      (setf (fill-pointer l) 0)
      (loop for x across keep do (vector-push-extend x l)))))
(defun jl-swap (l i j) (rotatef (aref l i) (aref l j)))
(defun jl-shuffle (l random)
  "Collections.shuffle(list, rnd), exactly."
  (loop for i from (length l) above 1
        do (jl-swap l (1- i) (random-next-int random i)))
  l)
(defun jl-equal (a b) (and (= (length a) (length b)) (every #'jequals a b)))
(defun jl-is-empty (l) (zerop (length l)))

(defun jcompare (a b)
  "Comparable.compareTo for the built-in comparables."
  (cond ((and (realp a) (realp b)) (double.compare a b))
        ((and (stringp a) (stringp b)) (cond ((string< a b) -1) ((string> a b) 1) (t 0)))
        ((and (typep a 'jenum) (typep b 'jenum)) (- (enum-ordinal a) (enum-ordinal b)))
        (t (compare-to/1 a b))))

;;; Sets

(defun js-add (s x) (let ((tb (jset-table s))) (if (nth-value 1 (gethash x tb)) nil (setf (gethash x tb) t))))
(defun js-contains (s x) (nth-value 1 (gethash x (jset-table s))))
(defun js-remove (s x) (remhash x (jset-table s)))
(defun js-add-all (s coll) (let ((changed nil)) (jdo (x coll) (when (js-add s x) (setf changed t))) changed))
(defun js-clear (s) (clrhash (jset-table s)))

;;; Maps (CL hash tables)

(defun jm-get (m k &optional default) (multiple-value-bind (v found) (gethash k m) (if found v default)))
(defun jm-put (m k v) (prog1 (gethash k m) (setf (gethash k m) v)))
(defun jm-contains-key (m k) (nth-value 1 (gethash k m)))
(defun jm-remove (m k) (prog1 (gethash k m) (remhash k m)))
(defun jm-keys (m) (let ((v (new-jlist))) (maphash (lambda (k x) (declare (ignore x)) (vector-push-extend k v)) m) v))
(defun jm-values (m) (let ((v (new-jlist))) (maphash (lambda (k x) (declare (ignore k)) (vector-push-extend x v)) m) v))
(defun jm-compute-if-absent (m k fn)
  (multiple-value-bind (v found) (gethash k m)
    (if (and found v) v (setf (gethash k m) (funcall fn k)))))
(defun jm-put-if-absent (m k v) (multiple-value-bind (old found) (gethash k m) (if (and found old) old (progn (setf (gethash k m) v) nil))))

;;; Generic collection methods, so (size x) (add x y) (get x i) ... also work
;;; on built-in collections and on ported classes that extend a collection.

(define-jcalls :instance size is-empty add add-all j-get j-set contains contains-all
  index-of last-index-of j-remove clear put put-all get-or-default contains-key
  contains-value key-set j-values poll poll-first poll-last peek peek-first
  peek-last offer add-first add-last remove-first remove-last get-first get-last
  j-push j-pop sub-list to-array j-sort for-each element iterator j-stream
  put-if-absent compute-if-absent j-first j-last)

(defmethod size/0 ((c t)) (jsize c))
(defmethod is-empty/0 ((c t)) (zerop (jsize c)))
(defmethod add/1 ((l vector) x) (jl-add l x))
(defmethod add/1 ((s jset) x) (js-add s x))
(defmethod add/1 ((s enum-set) x) (if (member x (enum-set-items s)) nil (progn (push x (enum-set-items s)) (%es-normalize s) t)))
(defmethod add/2 ((l vector) i x) (jl-add-at l i x))
(defmethod add-all/1 ((l vector) c) (jl-add-all l c))
(defmethod add-all/1 ((s jset) c) (js-add-all s c))
(defmethod add-all/1 ((s enum-set) c) (let ((ch nil)) (jdo (x c) (when (add/1 s x) (setf ch t))) ch))
(defmethod add-all/2 ((l vector) i c) (jl-add-all-at l i c))
(defmethod j-get/1 ((l vector) i) (jl-get l i))
(defmethod j-get/1 ((l list) i) (nth i l))
(defmethod j-get/1 ((m hash-table) k) (values (gethash k m)))
(defmethod j-set/2 ((l vector) i x) (jl-set l i x))
(defmethod contains/1 ((l vector) x) (jl-contains l x))
(defmethod contains/1 ((l list) x) (and (member x l :test #'jequals) t))
(defmethod contains/1 ((s jset) x) (js-contains s x))
(defmethod contains/1 ((s enum-set) x) (and (member x (enum-set-items s)) t))
(defmethod contains-all/1 ((c t) other) (block nil (jdo (x other) (unless (contains/1 c x) (return nil))) t))
(defmethod index-of/1 ((l vector) x) (jl-index-of l x))
(defmethod last-index-of/1 ((l vector) x) (jl-last-index-of l x))
(defmethod j-remove/1 ((l vector) x) (jl-remove l x))
(defmethod j-remove/1 ((s jset) x) (js-remove s x))
(defmethod j-remove/1 ((s enum-set) x) (prog1 (and (member x (enum-set-items s)) t)
                                          (setf (enum-set-items s) (remove x (enum-set-items s)))))
(defmethod j-remove/1 ((m hash-table) k) (jm-remove m k))
(defmethod clear/0 ((l vector)) (jl-clear l))
(defmethod clear/0 ((s jset)) (js-clear s))
(defmethod clear/0 ((s enum-set)) (setf (enum-set-items s) nil))
(defmethod clear/0 ((m hash-table)) (clrhash m) nil)
(defmethod put/2 ((m hash-table) k v) (jm-put m k v))
(defmethod put-all/1 ((m hash-table) other) (maphash (lambda (k v) (setf (gethash k m) v)) other))
(defmethod get-or-default/2 ((m hash-table) k d) (jm-get m k d))
(defmethod contains-key/1 ((m hash-table) k) (jm-contains-key m k))
(defmethod contains-value/1 ((m hash-table) v) (block nil (maphash (lambda (k x) (declare (ignore k)) (when (jequals x v) (return t))) m) nil))
(defmethod key-set/0 ((m hash-table)) (jset-from (jm-keys m) (hash-table-test m)))
(defmethod j-values/0 ((m hash-table)) (jm-values m))
(defmethod put-if-absent/2 ((m hash-table) k v) (jm-put-if-absent m k v))
(defmethod compute-if-absent/2 ((m hash-table) k fn) (jm-compute-if-absent m k fn))
(defmethod poll/0 ((l vector)) (jl-poll-first l))
(defmethod poll-first/0 ((l vector)) (jl-poll-first l))
(defmethod poll-last/0 ((l vector)) (jl-poll-last l))
(defmethod peek/0 ((l vector)) (jl-peek-first l))
(defmethod peek-first/0 ((l vector)) (jl-peek-first l))
(defmethod peek-last/0 ((l vector)) (jl-peek-last l))
(defmethod add-first/1 ((l vector) x) (jl-add-first l x))
(defmethod add-last/1 ((l vector) x) (jl-add l x) nil)
(defmethod remove-first/0 ((l vector)) (jl-remove-first l))
(defmethod remove-last/0 ((l vector)) (jl-remove-last l))
(defmethod get-first/0 ((l vector)) (jl-first l))
(defmethod get-last/0 ((l vector)) (jl-last l))
(defmethod j-push/1 ((l vector) x) (jl-add-first l x))
(defmethod j-pop/0 ((l vector)) (jl-remove-first l))
(defmethod element/0 ((l vector)) (jl-first l))
(defmethod sub-list/2 ((l vector) a b) (jl-sub-list l a b))
(defmethod to-array/0 ((c t)) (to-jvector c))
(defmethod j-sort/1 ((l vector) cmp) (jl-sort l (or cmp #'jcompare)))
(defmethod for-each/1 ((c t) fn) (jdo (x c) (funcall fn x)))

;; ArrayDeque / Queue: offer appends.
(defmethod offer/1 ((l vector) x) (jl-add l x))

;;; PriorityQueue (binary heap, comparator returns an int)

(defstruct (jpq (:constructor %make-jpq (cmp)))
  cmp (heap (new-jlist)))

(defun new-jpq (&optional cmp) (%make-jpq (or cmp #'jcompare)))

(defun %jpq-less (q a b) (< (funcall (jpq-cmp q) a b) 0))
(defun %jpq-up (q i)
  (let ((h (jpq-heap q)))
    (loop while (> i 0)
          do (let ((p (floor (1- i) 2)))
               (if (%jpq-less q (aref h i) (aref h p))
                   (progn (rotatef (aref h i) (aref h p)) (setf i p))
                   (return))))))
(defun %jpq-down (q i)
  (let* ((h (jpq-heap q)) (n (length h)))
    (loop
      (let* ((l (+ 1 (* 2 i))) (r (+ 2 (* 2 i))) (m i))
        (when (and (< l n) (%jpq-less q (aref h l) (aref h m))) (setf m l))
        (when (and (< r n) (%jpq-less q (aref h r) (aref h m))) (setf m r))
        (if (= m i) (return) (progn (rotatef (aref h i) (aref h m)) (setf i m)))))))
(defmethod add/1 ((q jpq) x) (vector-push-extend x (jpq-heap q)) (%jpq-up q (1- (length (jpq-heap q)))) t)
(defmethod offer/1 ((q jpq) x) (add/1 q x))
(defmethod peek/0 ((q jpq)) (if (zerop (length (jpq-heap q))) nil (aref (jpq-heap q) 0)))
(defmethod poll/0 ((q jpq))
  (let ((h (jpq-heap q)))
    (if (zerop (length h)) nil
        (let ((top (aref h 0)) (last (vector-pop h)))
          (when (plusp (length h)) (setf (aref h 0) last) (%jpq-down q 0))
          top))))
(defmethod size/0 ((q jpq)) (length (jpq-heap q)))
(defmethod is-empty/0 ((q jpq)) (zerop (length (jpq-heap q))))
(defmethod contains/1 ((q jpq) x) (jl-contains (jpq-heap q) x))
(defmethod clear/0 ((q jpq)) (jl-clear (jpq-heap q)))
(defmethod j-remove/1 ((q jpq) x)
  (let* ((h (jpq-heap q)) (i (position x h :test #'jequals)))
    (when i
      (let ((last (vector-pop h)))
        (when (< i (length h))
          (setf (aref h i) last) (%jpq-down q i) (%jpq-up q i)))
      t)))
(defmethod jiterable-items ((q jpq)) (copy-seq (jpq-heap q)))

;;; Comparator helpers

(defun comparing (key &optional (cmp #'jcompare))
  (lambda (a b) (funcall cmp (funcall key a) (funcall key b))))
(defun comparing-double (key) (comparing key #'double.compare))
(defun comparing-int (key) (comparing key #'integer.compare))
(defun then-comparing (c1 c2) (lambda (a b) (let ((r (funcall c1 a b))) (if (zerop r) (funcall c2 a b) r))))
(defun reversed-comparator (c) (lambda (a b) (funcall c b a)))
(defun natural-order () #'jcompare)
(defun reverse-order () (reversed-comparator #'jcompare))

;;; ------------------------------------------------------------ java.util.Random

(defclass java-random (jobject)
  ((seed :initform 0 :accessor .seed)
   (next-next-gaussian :initform 0d0 :accessor .next-next-gaussian)
   (have-next-next-gaussian :initform nil :accessor .have-next-next-gaussian)))

(defconstant +rnd-multiplier+ #x5DEECE66D)
(defconstant +rnd-mask+ (1- (ash 1 48)))
(defvar *seed-uniquifier* 8682522807148012)

(defun random-set-seed (r seed)
  (setf (.seed r) (logand (logxor seed +rnd-multiplier+) +rnd-mask+)
        (.have-next-next-gaussian r) nil))

(defun new-random (&optional seed)
  "new Random(seed); with no seed, a time-based one (as Java)."
  (let ((r (make-instance 'java-random)))
    (random-set-seed r (or seed (logxor (setf *seed-uniquifier* (* *seed-uniquifier* 1181783497276652981))
                                        (get-internal-real-time))))
    r))

(defun %signed (x bits)
  (if (logbitp (1- bits) x) (- x (ash 1 bits)) x))

(defun random-next (r bits)
  (setf (.seed r) (logand (+ (* (.seed r) +rnd-multiplier+) #xB) +rnd-mask+))
  (%signed (ash (.seed r) (- (- 48 bits))) 32))

(defun random-next-int (r &optional bound)
  (if (null bound)
      (random-next r 32)
      (progn
        (when (<= bound 0) (jthrow 'illegal-argument-exception "bound must be positive"))
        (let ((rr (random-next r 31)) (m (1- bound)))
          (if (zerop (logand bound m))
              (%signed (logand (ash (* bound rr) -31) #xFFFFFFFF) 32)
              (loop with u = rr
                    do (setf rr (mod u bound))
                       (if (< (%signed (logand (+ (- u rr) m) #xFFFFFFFF) 32) 0)
                           (setf u (random-next r 31))
                           (return rr))))))))

(defun random-next-long (r)
  (%signed (logand (+ (ash (random-next r 32) 32) (random-next r 32)) #xFFFFFFFFFFFFFFFF) 64))

(defun random-next-boolean (r) (/= 0 (random-next r 1)))
(defun random-next-double (r)
  (* (jd (+ (ash (random-next r 26) 27) (random-next r 27))) (scale-float 1d0 -53)))
(defun random-next-float (r)
  (jd (/ (coerce (random-next r 24) 'single-float) (coerce (ash 1 24) 'single-float))))
(defun random-next-gaussian (r)
  (if (.have-next-next-gaussian r)
      (progn (setf (.have-next-next-gaussian r) nil) (.next-next-gaussian r))
      (loop
        (let* ((v1 (- (* 2 (random-next-double r)) 1))
               (v2 (- (* 2 (random-next-double r)) 1))
               (s (+ (* v1 v1) (* v2 v2))))
          (when (and (< s 1) (/= s 0))
            (let ((mul (sqrt (/ (* -2 (log s)) s))))
              (setf (.next-next-gaussian r) (* v2 mul) (.have-next-next-gaussian r) t)
              (return (* v1 mul))))))))

(define-jcalls :instance next-int next-double next-boolean next-float next-long next-gaussian set-seed)
(defmethod next-int/0 ((r java-random)) (random-next-int r))
(defmethod next-int/1 ((r java-random) b) (random-next-int r b))
(defmethod next-double/0 ((r java-random)) (random-next-double r))
(defmethod next-boolean/0 ((r java-random)) (random-next-boolean r))
(defmethod next-float/0 ((r java-random)) (random-next-float r))
(defmethod next-long/0 ((r java-random)) (random-next-long r))
(defmethod next-gaussian/0 ((r java-random)) (random-next-gaussian r))
(defmethod set-seed/1 ((r java-random) s) (random-set-seed r s))

;;; ------------------------------------------------------------ strings

(defun jstr (&rest parts)
  "Java string concatenation."
  (with-output-to-string (s)
    (dolist (p parts)
      (typecase p
        (string (write-string p s))
        (null (write-string "null" s))
        (float (if (and (= p (ffloor p)) (< (abs p) 1d7)) (format s "~,1F" p) (format s "~A" p)))
        (t (princ p s))))))

(defun java-name->lisp (name)
  "Java identifier -> port name: getNodeSizeConstraints -> GET-NODE-SIZE-CONSTRAINTS,
LGraph -> L-GRAPH, KVector -> K-VECTOR, NETWORK_SIMPLEX -> NETWORK-SIMPLEX."
  (if (every (lambda (c) (or (upper-case-p c) (digit-char-p c) (char= c #\_))) name)
      (substitute #\- #\_ (string-upcase name))
      (string-upcase
       (with-output-to-string (out)
         (loop for i from 0 below (length name)
               for c = (char name i)
               for prev = (and (> i 0) (char name (1- i)))
               for next = (and (< (1+ i) (length name)) (char name (1+ i)))
               do (cond ((char= c #\_) (write-char #\- out))
                        ((and prev (upper-case-p c)
                              (or (lower-case-p prev) (digit-char-p prev)
                                  (and (upper-case-p prev) next (lower-case-p next))))
                         (write-char #\- out) (write-char c out))
                        (t (write-char c out))))))))
