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
   (supported-features nil)   ; keywords from the melk 'features' line
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
