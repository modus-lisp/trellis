;;;; metadata.lisp -- the registry the generated options (src/gen-options) fill:
;;;; LayoutOptionData and LayoutAlgorithmData records, keyed by id.
;;;;
;;;; The core module ports LayoutMetaDataService's API (getOptionData,
;;;; getOptionDataBySuffix, getAlgorithmData, parseValue, ...) on top of these
;;;; tables; this file only defines the records and how they are created, so
;;;; the option variables exist before any module loads.
;;;;
;;;; Defaults are LAZY: a default like (new-elk-padding 12) needs classes from
;;;; later modules, so it is evaluated the first time it is read.

(in-package #:elk)

(defvar *layout-options* (make-hash-table :test 'equal)
  "id -> LAYOUT-OPTION-DATA, for every option id (including legacy ids).")
(defvar *layout-option-list* '() "All LAYOUT-OPTION-DATA, in definition order.")
(defvar *layout-algorithms* (make-hash-table :test 'equal) "id -> LAYOUT-ALGORITHM-DATA")
(defvar *layout-algorithm-list* '())

;; org.eclipse.elk.core.data.LayoutOptionData -- also an IProperty.
(defjclass layout-option-data (property)
  ((group "")
   (legacy-ids nil)
   (option-class nil)         ; the Lisp type: :double :int :boolean :string (:enum e) (:enum-set e) (:object "Name")
   (type nil)                 ; layout-option-data.type enum constant (set by core)
   (name "")
   (description "")
   (targets nil)              ; list of keywords :parents :nodes :edges :ports :labels
   (dependencies (new-jlist))
   (choices nil)
   (visibility :visible)))

;; org.eclipse.elk.core.data.LayoutAlgorithmData
(defjclass layout-algorithm-data ()
  ((id nil)
   (name nil)
   (description "")
   (provider-class nil)       ; symbol naming the provider's Lisp class
   (category-id nil)
   (bundle-name nil)
   (defining-bundle-id nil)   ; LayoutAlgorithmData.Builder.definingBundleId (core API, below)
   (supported-features nil)   ; GRAPH-FEATURE constants from the melk 'features' line
   (option-defaults (make-hash-table :test 'equal)) ; option id -> lazy default
   (preview-image-path nil)
   (validator-class nil)
   (instance-pool nil)))

(defmacro define-layout-option (var id &key type default lower upper targets legacy-ids visibility)
  "Generated: one layout option.  VAR is bound to its LAYOUT-OPTION-DATA (an IProperty)."
  `(defparameter ,var
     (register-layout-option
      (make-instance 'layout-option-data
                     :id ,id
                     :default-value ,(if default `(make-lazy-value :thunk (lambda () ,default)) nil)
                     :lower-bound ,(if lower `(make-lazy-value :thunk (lambda () ,lower)) :negative-infinity)
                     :upper-bound ,(if upper `(make-lazy-value :thunk (lambda () ,upper)) :positive-infinity)
                     :option-class ',type
                     :name ,(subseq id (1+ (or (position #\. id :from-end t) -1)))
                     :group ,(let ((p (position #\. id :from-end t))) (if p (subseq id 0 p) ""))
                     :targets ',targets
                     :legacy-ids ',legacy-ids
                     :visibility ,visibility))))

(defun register-layout-option (data)
  (setf *layout-option-list* (append (remove (.id data) *layout-option-list* :key #'.id :test #'string=)
                                     (list data)))
  (setf (gethash (.id data) *layout-options*) data)
  (dolist (l (.legacy-ids data)) (setf (gethash l *layout-options*) data))
  data)

(defmacro define-algorithm-option (var source &optional (default nil default-p))
  "Generated: an algorithm's view of an option -- the same id, possibly another default."
  (if default-p
      `(defparameter ,var
         (let ((p (make-instance 'property :id (.id ,source)
                                           :default-value (make-lazy-value :thunk (lambda () ,default))
                                           :lower-bound (.lower-bound ,source)
                                           :upper-bound (.upper-bound ,source))))
           (setf (.option-type p) (.option-class ,source))
           p))
      `(defparameter ,var ,source)))

(defun register-layout-algorithm (id &key name provider category bundle features supports)
  "SUPPORTS is a list of (option-id . algorithm-property)."
  (let ((data (make-instance 'layout-algorithm-data
                             :id id :name name :provider-class provider :category-id category
                             :bundle-name bundle :supported-features features)))
    (loop for (oid . prop) in supports
          do (setf (gethash oid (.option-defaults data)) prop))
    (setf (gethash id *layout-algorithms*) data)
    (setf *layout-algorithm-list*
          (append (remove id *layout-algorithm-list* :key #'.id :test #'string=) (list data)))
    data))

(defun algorithm-option-default (algorithm-data option-id)
  "The default of OPTION-ID for ALGORITHM-DATA, and whether the algorithm supports it."
  (multiple-value-bind (prop found) (gethash option-id (.option-defaults algorithm-data))
    (values (and found (get-default/0 prop)) found)))

;;; ------------------------------------------------------------ Java API
;;;
;;; The methods ELK's Java code calls on LayoutOptionData and
;;; LayoutAlgorithmData (org.eclipse.elk.core.data).  They live here, next to
;;; the records, so that every module reading option metadata sees them as
;;; soon as it loads.  The Java enums they return (LayoutOptionData.Type,
;;; .Target, .Visibility, GraphFeature) are the generated enums of
;;; src/gen-enums/enums.lisp: CORE-DATA-TYPE, CORE-TARGET, VISIBILITY.

(defun %enum-constant (enum-class const)
  "The constant ENUM-CLASS.CONST; CONST is a keyword or a string such as :enumset."
  (symbol-value (intern (format nil "~A.~:@(~A~)" (symbol-name enum-class) (string const)) :elk)))

(defun %java-string-compare (a b)
  "String.compareTo: the difference at the first mismatch, else the length difference."
  (let ((n (min (length a) (length b))))
    (dotimes (i n (- (length a) (length b)))
      (let ((ca (char a i)) (cb (char b i)))
        (unless (char= ca cb)
          (return (- (char-code ca) (char-code cb))))))))

(defun %lod-kind (data)
  "The LayoutOptionData.Type of DATA as a keyword: :BOOLEAN :INT :DOUBLE :STRING :ENUM :ENUMSET :OBJECT :UNDEFINED.
An explicit type (from the builder) wins over the one implied by OPTION-CLASS."
  (if (.type data)
      (intern (name (.type data)) :keyword)
      (let ((c (.option-class data)))
        (cond ((member c '(:boolean :int :double :string)) c)
              ((and (consp c) (eq (car c) :enum)) :enum)
              ((and (consp c) (eq (car c) :enum-set)) :enumset)
              ((and (consp c) (member (car c) '(:object :list))) :object)
              (t :undefined)))))

(defun %lod-class-symbol (data)
  "The enum class symbol (enum options) or the Lisp class symbol (object options), else NIL."
  (let ((c (.option-class data)))
    (cond ((and (consp c) (member (car c) '(:enum :enum-set))) (second c))
          ((and (consp c) (eq (car c) :object))
           (intern (java-name->lisp (second c)) :elk))
          (t nil))))

(defun %lod-check-enum-class (data)
  (let ((cls (%lod-class-symbol data)))
    (unless (and cls (member (%lod-kind data) '(:enum :enumset)))
      (jthrow 'illegal-state-exception "Enumeration class expected for layout option ~A" (.id data)))
    cls))

(defun %lod-data-class-p (data)
  (let ((cls (%lod-class-symbol data)))
    (and cls (find-class cls nil) (subtypep cls 'i-data-object) t)))

(defun %lod-visibility (data)
  (let ((v (.visibility data)))
    (if (keywordp v) (%enum-constant 'visibility v) v)))

(defun %lod-target-set (data)
  (enum-set-of-list 'core-target
                    (mapcar (lambda (k) (if (keywordp k) (%enum-constant 'core-target k) k))
                            (.targets data))))

(defun %lod-split-components (s)
  "Java's split(\"[\\[\\]\\s,]+\") with the empty pieces dropped."
  (let ((parts '()) (start nil))
    (loop for i from 0 to (length s)
          for c = (if (< i (length s)) (char s i) #\,)
          do (if (member c '(#\[ #\] #\Space #\Tab #\Newline #\Return #\Page #\, ))
                 (when start (push (subseq s start i) parts) (setf start nil))
                 (unless start (setf start i))))
    (nreverse parts)))

(defun %lod-parse-java-int (s)
  "Integer.valueOf: an optional sign, then decimal digits.  NIL otherwise."
  (and (plusp (length s))
       (let ((start (if (member (char s 0) '(#\+ #\-)) 1 0)))
         (and (< start (length s))
              (every #'digit-char-p (subseq s start))
              (parse-integer s)))))

(defun %lod-parse-java-double (s)
  "Double.valueOf: NaN, Infinity, or a decimal floating literal.  NIL otherwise."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
    (cond ((string= trimmed "NaN") double.nan)
          ((member trimmed '("Infinity" "+Infinity") :test #'string=) double.positive-infinity)
          ((string= trimmed "-Infinity") double.negative-infinity)
          ((and (plusp (length trimmed))
                (every (lambda (c) (find c "0123456789+-.eE")) trimmed)
                (some #'digit-char-p trimmed))
           (let ((x (handler-case (let ((*read-default-float-format* 'double-float)
                                        (*read-eval* nil))
                                    (read-from-string trimmed))
                      (error () nil))))
             (and (realp x) (coerce x 'double-float))))
          (t nil))))

(defun %lod-enum-for-string (cls s)
  "Enum.valueOf(cls, s), else the constant at the ordinal S spells, else NIL."
  (let* ((consts (enum-values-of cls))
         (exact (find s consts :key #'enum-name :test #'string=)))
    (or exact
        (let ((index (%lod-parse-java-int s)))
          (and index (<= 0 index) (< index (length consts)) (aref consts index))))))

(defun %lod-create-data-instance (data)
  (let ((cls (%lod-class-symbol data)))
    (unless (%lod-data-class-p data)
      (jthrow 'illegal-state-exception "IDataType class expected for layout option ~A" (.id data)))
    (let ((instance (elk-reflect.new-instance cls)))
      (unless instance
        (jthrow 'illegal-state-exception
                "Couldn't create new instance of property '~A'. Make sure its type is registered with the ElkReflect utility class."
                (.id data)))
      instance)))

;;; LayoutOptionData

(defjmethod get-id ((this layout-option-data)) (.id this))
(defjmethod get-group ((this layout-option-data)) (.group this))
(defjmethod get-name ((this layout-option-data)) (.name this))
(defjmethod get-description ((this layout-option-data)) (.description this))
(defjmethod get-type ((this layout-option-data)) (%enum-constant 'core-data-type (%lod-kind this)))
(defjmethod get-targets ((this layout-option-data)) (%lod-target-set this))
(defjmethod get-dependencies ((this layout-option-data)) (.dependencies this))
(defjmethod get-visibility ((this layout-option-data)) (%lod-visibility this))
(defjmethod get-legacy-ids ((this layout-option-data)) (.legacy-ids this))
(defjmethod get-option-class ((this layout-option-data)) (%lod-class-symbol this))

(defjmethod get-default ((this layout-option-data))
  (let ((value (force-lazy (.default-value this))))
    (if (cloneable-p value) (jclone value) value)))

(defjmethod get-default-default ((this layout-option-data))
  (ecase (%lod-kind this)
    (:string "")
    (:boolean nil)
    (:int 0)
    (:double 0d0)
    (:enum (aref (enum-values-of (%lod-check-enum-class this)) 0))
    (:enumset (enum-set.none-of (%lod-check-enum-class this)))
    (:object nil)
    (:undefined (jthrow 'illegal-state-exception "Invalid type set for this layout option."))))

(defjmethod get-choices ((this layout-option-data))
  (or (.choices this)
      (setf (.choices this)
            (ecase (%lod-kind this)
              ((:enum :enumset) (map 'vector #'enum-name (enum-values-of (%lod-check-enum-class this))))
              (:boolean (vector "false" "true"))
              ((:int :double :string :object :undefined) (vector))))))

(defjmethod get-enum-value-count ((this layout-option-data))
  (if (member (%lod-kind this) '(:enum :enumset))
      (length (enum-values-of (%lod-check-enum-class this)))
      0))

(defjmethod get-enum-value ((this layout-option-data) int-value)
  (if (member (%lod-kind this) '(:enum :enumset))
      (aref (enum-values-of (%lod-check-enum-class this)) int-value)
      nil))

(defjmethod can-parse-value ((this layout-option-data))
  (case (%lod-kind this)
    (:undefined nil)
    (:object (%lod-data-class-p this))
    (t t)))

(defjmethod parse-value ((this layout-option-data) value-string)
  (cond ((or (null value-string) (string= value-string "null")) nil)
        ((and (zerop (length value-string)) (not (eq (%lod-kind this) :enumset))) nil)
        (t
         (ecase (%lod-kind this)
           (:boolean (cond ((string-equal value-string "true") t)
                           (t nil)))
           (:int (%lod-parse-java-int value-string))
           (:double (%lod-parse-java-double value-string))
           (:string value-string)
           (:enum (%lod-enum-for-string (%lod-check-enum-class this) value-string))
           (:enumset
            (let* ((cls (%lod-check-enum-class this))
                   (items '()))
              (dolist (component (%lod-split-components value-string)
                                 (enum-set-of-list cls items))
                (when (plusp (length (string-trim '(#\Space #\Tab) component)))
                  (let ((constant (%lod-enum-for-string cls component)))
                    (if constant
                        (push constant items)
                        (return nil)))))))
           (:object
            (handler-case (let ((value (%lod-create-data-instance this)))
                            (parse value value-string)
                            value)
              (illegal-argument-exception () nil)))
           (:undefined (jthrow 'illegal-state-exception "Invalid type set for this layout option."))))))

(defjmethod get-lower-bound ((this layout-option-data))
  (let ((v (force-lazy (.lower-bound this))))
    (if (or (realp v) (stringp v)) v :negative-infinity)))

(defjmethod get-upper-bound ((this layout-option-data))
  (let ((v (force-lazy (.upper-bound this))))
    (if (or (realp v) (stringp v)) v :positive-infinity)))

(defjmethod set-lower-bound ((this layout-option-data) lower-bound)
  (setf (.lower-bound this) lower-bound)
  nil)

(defjmethod set-upper-bound ((this layout-option-data) upper-bound)
  (setf (.upper-bound this) upper-bound)
  nil)

(defjmethod equals ((this layout-option-data) obj)
  (cond ((typep obj 'layout-option-data) (string= (.id this) (.id obj)))
        ((typep obj 'property) (string= (.id this) (.id obj)))
        (t nil)))

(defjmethod compare-to ((this layout-option-data) other)
  (%java-string-compare (.id this) (.id other)))

(defjmethod to-string ((this layout-option-data))
  (jstr "Layout Option: " (.id this)))

;;; LayoutAlgorithmData

(defun %lad-option-id (property)
  (if (stringp property) property (.id property)))

(defjmethod get-id ((this layout-algorithm-data)) (.id this))
(defjmethod get-name ((this layout-algorithm-data)) (.name this))
(defjmethod get-description ((this layout-algorithm-data)) (.description this))
(defjmethod get-category-id ((this layout-algorithm-data)) (.category-id this))
(defjmethod get-bundle-name ((this layout-algorithm-data)) (.bundle-name this))
(defjmethod get-defining-bundle-id ((this layout-algorithm-data)) (.defining-bundle-id this))
(defjmethod get-preview-image-path ((this layout-algorithm-data)) (.preview-image-path this))
(defjmethod get-validator-class ((this layout-algorithm-data)) (.validator-class this))

(defjmethod get-instance-pool ((this layout-algorithm-data))
  (or (.instance-pool this)
      (setf (.instance-pool this)
            (new-instance-pool (new-algorithm-factory (.provider-class this))))))

(defjmethod get-supported-features ((this layout-algorithm-data))
  (enum-set-of-list 'graph-feature (copy-list (.supported-features this))))

(defjmethod supports-feature ((this layout-algorithm-data) feature)
  (and (member feature (.supported-features this)) t))

(defjmethod add-known-option ((this layout-algorithm-data) property default-value)
  (setf (gethash (%lad-option-id property) (.option-defaults this)) default-value)
  nil)

(defjmethod get-known-option-ids ((this layout-algorithm-data))
  (let ((ids (new-jlist)))
    (maphash (lambda (k v) (declare (ignore v)) (jl-add ids k)) (.option-defaults this))
    ids))

(defjmethod knows-option ((this layout-algorithm-data) property)
  (nth-value 1 (gethash (%lad-option-id property) (.option-defaults this))))

(defjmethod get-default-value ((this layout-algorithm-data) property)
  (let ((value (gethash (%lad-option-id property) (.option-defaults this))))
    (if (typep value 'property) (get-default/0 value) value)))

(defjmethod equals ((this layout-algorithm-data) obj)
  (and (typep obj 'layout-algorithm-data) (string= (.id this) (.id obj))))

(defjmethod to-string ((this layout-algorithm-data))
  (jstr "Layout Algorithm: " (.id this)))
