;;;; properties.lisp -- org.eclipse.elk.graph.properties
;;;;
;;;; Property (id + default + bounds), MapPropertyHolder (keyed by id, as Java's
;;;; Property.equals is), and ELK's rule that a mutable default is CLONED on
;;;; read and stored, so mutating a property you never set does not mutate the
;;;; shared default.

(in-package #:elk)

(defjinterface i-property)

(defjclass property (i-property)
  ((id nil)
   (default-value nil)
   (lower-bound :negative-infinity)
   (upper-bound :positive-infinity)
   ;; Option metadata, filled for layout options (see options.lisp).
   (option-type nil)))

(defmethod print-object ((p property) s) (format s "#<property ~A>" (.id p)))

(defun make-property (id &optional default lower upper)
  (let ((p (make-instance 'property :id id :default-value default)))
    (when lower (setf (.lower-bound p) lower))
    (when upper (setf (.upper-bound p) upper))
    p))

(defjctor property (id) (setf (.id this) id))
(defjctor property (id default)
  (setf (.id this) (if (typep id 'property) (.id id) id) (.default-value this) default))
(defjctor property (id default lower) (this-ctor property this id default)
  (when lower (setf (.lower-bound this) lower)))
(defjctor property (id default lower upper) (this-ctor property this id default lower)
  (when upper (setf (.upper-bound this) upper)))

(defun property-with-default (prop default)
  "new Property<>(OTHER, default) -- same id, another default."
  (let ((p (make-property (.id prop) default (.lower-bound prop) (.upper-bound prop))))
    (setf (.option-type p) (.option-type prop))
    p))

;;; Cloning (ElkReflect.clone): anything with a JCLONE method is cloneable.

(defclass jcloneable () ()
  (:documentation "Mixin: the class implements Cloneable; define a JCLONE method."))

(defgeneric jclone (x)
  (:documentation "Object.clone() for the cloneable value types."))
(defmethod jclone ((x enum-set)) (enum-set.copy-of x))
(defmethod jclone ((x vector)) (if (stringp x) x (jlist-from x)))
(defmethod jclone ((x hash-table))
  (let ((h (make-hash-table :test (hash-table-test x))))
    (maphash (lambda (k v) (setf (gethash k h) v)) x) h))
(defmethod jclone ((x jset)) (jset-from x (hash-table-test (jset-table x))))

(defun cloneable-p (x)
  (or (typep x 'jcloneable) (typep x 'enum-set) (typep x 'jset) (hash-table-p x)
      (and (vectorp x) (not (stringp x)))))

(defstruct lazy-value thunk)

(defun force-lazy (x) (if (lazy-value-p x) (funcall (lazy-value-thunk x)) x))

(defun %resolve-default (p)
  (let ((d (.default-value p)))
    (if (lazy-value-p d)
        (setf (.default-value p) (force-lazy d))
        d)))

(defjmethod get-default ((p property))
  (let ((d (%resolve-default p)))
    (if (cloneable-p d) (jclone d) d)))
(defjmethod get-id ((p property)) (.id p))
(defjmethod get-lower-bound ((p property))
  (if (lazy-value-p (.lower-bound p)) (setf (.lower-bound p) (force-lazy (.lower-bound p))) (.lower-bound p)))
(defjmethod get-upper-bound ((p property))
  (if (lazy-value-p (.upper-bound p)) (setf (.upper-bound p) (force-lazy (.upper-bound p))) (.upper-bound p)))
(defjmethod equals ((p property) o) (and (typep o 'property) (string= (.id p) (.id o))))
(defjmethod compare-to ((p property) o) (jcompare (.id p) (.id o)))
(defjmethod to-string ((p property)) (.id p))

;;; Holders.  The table maps id -> (property . value) so getAllProperties can
;;; hand back the property objects themselves.

(defjinterface i-property-holder)

(defjclass map-property-holder (i-property-holder)
  ((property-map nil)))

(defun %pmap (h)
  (or (.property-map h) (setf (.property-map h) (make-hash-table :test 'equal))))

(defgeneric resolve-proxy (value property)
  (:documentation "IPropertyValueProxy.resolveValue; non-proxies are returned as they are."))
(defmethod resolve-proxy ((v t) p) (declare (ignore p)) v)
(defclass property-value-proxy () ())

(defjmethod set-property ((h map-property-holder) property value)
  ;; NIL is both Java's null (clear) and Java's Boolean.FALSE.  Clearing a
  ;; property whose default is NIL is the same answer either way, so remove it.
  ;; Otherwise NIL must be STORED, or a false over a default of TRUE reads back
  ;; as the default.  Clearing a non-NIL-default property to null therefore
  ;; stores NIL rather than restoring the default (a known, accepted difference).
  (if (and (null value) (null (%resolve-default property)))
      (when (.property-map h) (remhash (.id property) (.property-map h)))
      (setf (gethash (.id property) (%pmap h)) (cons property value)))
  h)

(defjmethod get-property ((h map-property-holder) property)
  (let* ((cell (and (.property-map h) (gethash (.id property) (.property-map h))))
         (value (cdr cell)))
    (cond ((typep value 'property-value-proxy)
           (let ((v (resolve-proxy value property)))
             (if v
                 (progn (setf (gethash (.id property) (%pmap h)) (cons property v)) v)
                 (%holder-default h property))))
          (value value)
          ;; A stored FALSE is a stored value too.
          (cell value)
          (t (%holder-default h property)))))

(defun %holder-default (h property)
  (let ((d (get-default/0 property)))
    (when (cloneable-p d) (set-property/2 h property d))
    d))

(defjmethod has-property ((h map-property-holder) property)
  (and (.property-map h) (nth-value 1 (gethash (.id property) (.property-map h)))))

(defjmethod copy-properties ((h map-property-holder) other)
  (when other
    (maphash (lambda (id cell) (setf (gethash id (%pmap h)) (cons (car cell) (cdr cell))))
             (%property-table other)))
  h)

(defgeneric %property-table (holder))
(defmethod %property-table ((h map-property-holder)) (or (.property-map h) (make-hash-table :test 'equal)))

(defjmethod get-all-properties ((h map-property-holder))
  "A fresh map property -> value."
  (let ((m (make-hash-table :test 'eq)))
    (when (.property-map h)
      (maphash (lambda (id cell) (declare (ignore id)) (setf (gethash (car cell) m) (cdr cell)))
               (.property-map h)))
    m))

(defun property-value-cells (h)
  "List of (property . value), for iteration."
  (let ((acc '()))
    (when (.property-map h)
      (maphash (lambda (id cell) (declare (ignore id)) (push (cons (car cell) (cdr cell)) acc))
               (.property-map h)))
    (nreverse acc)))

(defmacro defproperty (name id &optional default lower upper)
  "A static final IProperty field."
  `(defparameter ,name (make-property ,id ,default ,lower ,upper)))
