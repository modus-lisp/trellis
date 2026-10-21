;;;; data.lisp -- org.eclipse.elk.core.data: LayoutMetaDataService and the
;;;; Registry its providers write into, LayoutCategoryData, the Builders of
;;;; LayoutOptionData / LayoutAlgorithmData / LayoutCategoryData,
;;;; LayoutAlgorithmResolver and DeprecatedLayoutOptionReplacer.
;;;;
;;;; The records themselves and the LayoutOptionData / LayoutAlgorithmData
;;;; methods live in src/runtime/metadata.lisp.  The service's option and
;;;; algorithm tables are the global registries of that file (*layout-options*,
;;;; *layout-option-list*, *layout-algorithms*, *layout-algorithm-list*): the
;;;; generated options register there at load, and there is only one service.
;;;; Its category table and suffix caches are slots of the service object.

(in-package #:elk)

;;; ---------------------------------------------------------------- ILayoutMetaData
;; getId / getName / getDescription are the generics of the metadata.lisp methods.


;;; ---------------------------------------------------------------- LayoutCategoryData

(defjmethod get-layouters ((this layout-category-data)) (.layouters this))
(defjmethod get-id ((this layout-category-data)) (.id this))
(defjmethod get-name ((this layout-category-data)) (.name this))
(defjmethod get-description ((this layout-category-data)) (.description this))

(defjmethod equals ((this layout-category-data) obj)
  (and (typep obj 'layout-category-data) (jequals (.id this) (.id obj)) t))

(defjmethod to-string ((this layout-category-data))
  (jstr "Layout Type: " (.id this)))

(defjmethod id ((this layout-category-data-builder) aid)
  (setf (.id this) aid)
  this)
(defjmethod name ((this layout-category-data-builder) aname)
  (setf (.name this) aname)
  this)
(defjmethod description ((this layout-category-data-builder) adescription)
  (setf (.description this) adescription)
  this)
(defjmethod create ((this layout-category-data-builder))
  (make-instance 'layout-category-data :id (.id this) :name (.name this)
                                       :description (.description this)))

;;; ---------------------------------------------------------------- LayoutOptionData.Builder

(defun jdo-list (coll)
  "The elements of a Java collection as a CL list."
  (let ((items '()))
    (jdo (x coll) (push x items))
    (nreverse items)))

(defun %keyword-of-enum (constant)
  "The keyword spelling of an enum constant: PARENTS -> :PARENTS (the generated melk keywords)."
  (intern (string-upcase (name constant)) :keyword))

(defjmethod id ((this layout-option-data-builder) aid) (setf (.id this) aid) this)
(defjmethod group ((this layout-option-data-builder) agroup) (setf (.group this) agroup) this)
(defjmethod legacy-ids ((this layout-option-data-builder) alegacy-ids)
  (setf (.legacy-ids this) alegacy-ids)
  this)
(defjmethod default-value ((this layout-option-data-builder) adefault-value)
  (setf (.default-value this) adefault-value)
  this)
(defjmethod option-class ((this layout-option-data-builder) aclazz)
  (setf (.clazz this) aclazz)
  this)
(defjmethod j-type ((this layout-option-data-builder) atype)
  (setf (.type this) atype)
  this)
(defjmethod name ((this layout-option-data-builder) aname) (setf (.name this) aname) this)
(defjmethod description ((this layout-option-data-builder) adescription)
  (setf (.description this) adescription)
  this)
(defjmethod targets ((this layout-option-data-builder) atargets)
  (setf (.targets this) (mapcar #'%keyword-of-enum (jdo-list atargets)))
  this)
(defjmethod visibility ((this layout-option-data-builder) avisibility)
  (setf (.visibility this) (and avisibility (%keyword-of-enum avisibility)))
  this)
(defjmethod lower-bound ((this layout-option-data-builder) alower-bound)
  (setf (.lower-bound this) alower-bound)
  this)
(defjmethod upper-bound ((this layout-option-data-builder) aupper-bound)
  (setf (.upper-bound this) aupper-bound)
  this)

(defjmethod create ((this layout-option-data-builder))
  (make-instance 'layout-option-data
                 :id (.id this) :group (.group this) :legacy-ids (.legacy-ids this)
                 :default-value (.default-value this) :option-class (.clazz this)
                 :type (.type this) :name (.name this) :description (.description this)
                 :targets (.targets this) :visibility (.visibility this)
                 :lower-bound (.lower-bound this) :upper-bound (.upper-bound this)))

;;; ---------------------------------------------------------------- LayoutAlgorithmData.Builder

(defun %as-feature-list (features)
  (cond ((null features) nil)
        ((typep features 'enum-set) (copy-list (enum-set-items features)))
        (t (coerce features 'list))))

(defjmethod id ((this layout-algorithm-data-builder) aid) (setf (.id this) aid) this)
(defjmethod name ((this layout-algorithm-data-builder) aname) (setf (.name this) aname) this)
(defjmethod provider-factory ((this layout-algorithm-data-builder) aprovider-factory)
  (setf (.provider-factory this) aprovider-factory)
  this)
(defjmethod category ((this layout-algorithm-data-builder) acategory)
  (setf (.category this) acategory)
  this)
(defjmethod melk-bundle-name ((this layout-algorithm-data-builder) amelk-bundle-name)
  (setf (.melk-bundle-name this) amelk-bundle-name)
  this)
(defjmethod defining-bundle-id ((this layout-algorithm-data-builder) adefining-bundle-id)
  (setf (.defining-bundle-id this) adefining-bundle-id)
  this)
(defjmethod description ((this layout-algorithm-data-builder) adescription)
  (setf (.description this) adescription)
  this)
(defjmethod image-path ((this layout-algorithm-data-builder) aimage-path)
  (setf (.image-path this) aimage-path)
  this)
(defjmethod supported-features ((this layout-algorithm-data-builder) asupported-features)
  (setf (.supported-features this) asupported-features)
  this)
(defjmethod validator-class ((this layout-algorithm-data-builder) avalidator)
  (setf (.validator-class this) avalidator)
  this)

(defjmethod create ((this layout-algorithm-data-builder))
  (make-instance 'layout-algorithm-data
                 :id (.id this) :name (.name this) :description (.description this)
                 :instance-pool (and (.provider-factory this)
                                     (new-instance-pool (.provider-factory this)))
                 :category-id (.category this) :bundle-name (.melk-bundle-name this)
                 :defining-bundle-id (.defining-bundle-id this)
                 :preview-image-path (.image-path this)
                 :supported-features (%as-feature-list (.supported-features this))
                 :validator-class (.validator-class this)))

;;; ---------------------------------------------------------------- LayoutMetaDataService

(defvar *layout-meta-data-service* nil
  "LayoutMetaDataService.instance")

(defun %suffix-match-p (id suffix)
  "id.endsWith(suffix) at a dot boundary, or equal."
  (let ((il (length id)) (sl (length suffix)))
    (and (>= il sl)
         (string= suffix id :start2 (- il sl))
         (or (= sl il) (char= (char id (- il sl 1)) #\.)))))

(defun %unique-suffix-match (items key suffix)
  "The single item whose KEY matches SUFFIX; NIL when none or several match."
  (let ((found nil) (count 0))
    (dolist (item items)
      (when (%suffix-match-p (funcall key item) suffix)
        (incf count)
        (setf found item)))
    (and (= count 1) found)))

(defun %blank-p (s)
  (or (null s) (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) s)))))

(defun %lmds-register-algorithm (data)
  (setf (gethash (.id data) *layout-algorithms*) data)
  (let ((pos (position (.id data) *layout-algorithm-list* :key #'.id :test #'string=)))
    (if pos
        (setf (nth pos *layout-algorithm-list*) data)
        (setf *layout-algorithm-list* (append *layout-algorithm-list* (list data))))))

(defun %lmds-register-option (data)
  (let ((id (.id data)))
    (setf (gethash id *layout-options*) data)
    (let ((pos (position id *layout-option-list* :key #'.id :test #'string=)))
      (if pos
          (setf (nth pos *layout-option-list*) data)
          (setf *layout-option-list* (append *layout-option-list* (list data))))))
  ;; legacy ids: a real option of the same spelling keeps the id
  (dolist (legacy (.legacy-ids data))
    (let ((existing (gethash legacy *layout-options*)))
      (unless (and existing (string= (.id existing) legacy))
        (setf (gethash legacy *layout-options*) data))))
  data)

(defun %lmds-register-category (service data)
  (let ((id (.id data)))
    (setf (gethash id (.layout-category-map service)) data)
    (let* ((list (.layout-category-list service))
           (pos (position id list :key #'.id :test #'string=)))
      (if pos
          (setf (aref list pos) data)
          (jl-add list data))))
  data)

(defun %lmds-retrieve-backup-category (service)
  (or (gethash "" (.layout-category-map service))
      (%lmds-register-category
       service
       (create (let ((b (new-layout-category-data-builder)))
                 (id b "")
                 (name b "Other")
                 b)))))

(defjstatic layout-meta-data-service.get-instance ()
  (layout-meta-data-service.get-instance/1 nil))

(defjstatic layout-meta-data-service.get-instance (loader)
  loader
  (or *layout-meta-data-service*
      (setf *layout-meta-data-service* (make-instance 'layout-meta-data-service))))

(defjstatic layout-meta-data-service.unload ()
  (when *layout-meta-data-service*
    (jdo (algorithm-data (get-algorithm-data *layout-meta-data-service*))
      (clear (get-instance-pool algorithm-data))))
  (setf *layout-meta-data-service* nil)
  nil)

(defjstatic layout-meta-data-service.init-elk-reflect ()
  ;; The Java copy constructors are clones here (jclone).
  (elk-reflect.register/3 'k-vector (lambda () (new-k-vector)) (lambda (v) (jclone v)))
  (elk-reflect.register/3 'k-vector-chain (lambda () (new-k-vector-chain)) (lambda (v) (jclone v)))
  (elk-reflect.register/3 'elk-margin (lambda () (new-elk-margin)) (lambda (v) (jclone v)))
  (elk-reflect.register/3 'elk-padding (lambda () (new-elk-padding)) (lambda (v) (jclone v)))
  (elk-reflect.register/3 'individual-spacings (lambda () (new-individual-spacings))
                          (lambda (v) (jclone v)))
  ;; ArrayList, LinkedList, HashSet, LinkedHashSet and TreeSet have no ElkReflect entry:
  ;; the Lisp collections are cloned generically by JCLONE.
  nil)

(defjmethod register-layout-meta-data-providers ((this layout-meta-data-service) providers)
  (jdo (provider providers)
    (let ((registry (make-instance 'registry :service this)))
      (j-apply provider registry)
      (apply-dependencies registry)))
  (clrhash (.option-suffix-map this))
  nil)

(defjmethod get-algorithm-data ((this layout-meta-data-service) id)
  (gethash id *layout-algorithms*))

(defjmethod get-algorithm-data ((this layout-meta-data-service))
  (jlist-from *layout-algorithm-list*))

(defjmethod get-algorithm-data-by-suffix ((this layout-meta-data-service) suffix)
  (when (and suffix (plusp (length suffix)))
    (let ((data (gethash suffix (.algorithm-suffix-map this))))
      (unless data
        (setf data (%unique-suffix-match *layout-algorithm-list* #'.id suffix))
        (when data
          (setf (gethash suffix (.algorithm-suffix-map this)) data)))
      data)))

(defjmethod get-algorithm-data-by-suffix-or-default ((this layout-meta-data-service)
                                                     algorithm-id default-id)
  (or (and (not (%blank-p algorithm-id))
           (get-algorithm-data-by-suffix this algorithm-id))
      (and (not (%blank-p default-id))
           (get-algorithm-data-by-suffix this default-id))))

(defjmethod get-option-data ((this layout-meta-data-service) id)
  (gethash id *layout-options*))

(defjmethod get-option-data ((this layout-meta-data-service))
  (jlist-from *layout-option-list*))

(defjmethod get-option-data-by-suffix ((this layout-meta-data-service) suffix)
  (when (and suffix (plusp (length suffix)))
    (let ((data (gethash suffix (.option-suffix-map this))))
      (unless data
        (setf data (%unique-suffix-match *layout-option-list* #'.id suffix))
        (unless data
          ;; legacy ids: one match over all options' legacy ids, counted per id
          (let ((found nil) (count 0))
            (dolist (option *layout-option-list*)
              (dolist (legacy (.legacy-ids option))
                (when (%suffix-match-p legacy suffix)
                  (incf count)
                  (setf found option))))
            (when (= count 1) (setf data found))))
        (when data
          (setf (gethash suffix (.option-suffix-map this)) data)))
      data)))

(defjmethod get-option-data ((this layout-meta-data-service) algorithm-data target-type)
  (let ((result (new-jlist)))
    (dolist (option *layout-option-list* result)
      (when (and (or (knows-option algorithm-data option)
                     (jequals core-options.algorithm option))
                 (contains (get-targets option) target-type))
        (jl-add result option)))))

(defjmethod get-category-data ((this layout-meta-data-service) id)
  (gethash id (.layout-category-map this)))

(defjmethod get-category-data ((this layout-meta-data-service))
  (jlist-from (.layout-category-list this)))

;;; ---------------------------------------------------------------- Registry

(defjmethod register ((this registry) (algorithm-data layout-algorithm-data))
  (%lmds-register-algorithm algorithm-data)
  nil)

(defjmethod register ((this registry) (option-data layout-option-data))
  (%lmds-register-option option-data)
  nil)

(defjmethod register ((this registry) (category-data layout-category-data))
  (%lmds-register-category (.service this) category-data)
  nil)

(defjmethod add-dependency ((this registry) source-option target-option required-value)
  (jl-add (.option-dependencies this)
          (make-instance 'core-data-triple :first-id source-option :second-id target-option
                                           :value required-value))
  nil)

(defjmethod add-option-support ((this registry) algorithm option default-value)
  (jl-add (.option-support this)
          (make-instance 'core-data-triple :first-id algorithm :second-id option
                                           :value default-value))
  nil)

(defjmethod apply-dependencies ((this registry))
  (let ((service (.service this)))
    (jdo (algorithm (get-algorithm-data service))
      (let* ((category-id (or (get-category-id algorithm) ""))
             (category (get-category-data service category-id)))
        (when (and (null category) (string= category-id ""))
          (setf category (%lmds-retrieve-backup-category service)))
        (when (and category (not (jl-contains (get-layouters category) algorithm)))
          (jl-add (get-layouters category) algorithm))))
    (jdo (dep (.option-dependencies this))
      (let ((source (get-option-data service (.first-id dep)))
            (target (get-option-data service (.second-id dep))))
        (when (and source target)
          (jl-add (get-dependencies source) (pair.of target (.value dep))))))
    (jl-clear (.option-dependencies this))
    (jdo (support (.option-support this))
      (let ((algorithm (get-algorithm-data service (.first-id support)))
            (option (get-option-data service (.second-id support))))
        (when (and algorithm option)
          (add-known-option algorithm option (.value support)))))
    (jl-clear (.option-support this))
    nil))

;;; ---------------------------------------------------------------- LayoutAlgorithmResolver

(defjmethod visit ((this layout-algorithm-resolver) element)
  (when (and (typep element 'elk-node)
             (not (get-property element core-options.no-layout)))
    (resolve-algorithm this element))
  nil)

(defjmethod resolve-algorithm ((this layout-algorithm-resolver) node)
  (let ((algorithm-id (get-property node core-options.algorithm)))
    (unless (resolve-and-set-algorithm this algorithm-id node)
      (when (must-resolve this node)
        (if (%blank-p algorithm-id)
            (let* ((default-algorithm-id (get-default-layout-algorithm-id this))
                   (message (new-string-builder)))
              (unless (resolve-and-set-algorithm this (get-default-layout-algorithm-id this) node)
                (%cu-append message "Unable to load default layout algorithm " default-algorithm-id
                            " for unconfigured node ")
                (elk-util.print-element-path node message)
                (jthrow 'unsupported-configuration-exception "~A" (coerce message 'simple-string))))
            (let ((message (new-string-builder)))
              (%cu-append message "Layout algorithm '" algorithm-id "' not found for ")
              (elk-util.print-element-path node message)
              (jthrow 'unsupported-configuration-exception "~A" (coerce message 'simple-string))))))
    nil))

(defjmethod resolve-and-set-algorithm ((this layout-algorithm-resolver) algorithm-id node)
  (let ((algorithm-data (get-algorithm-data-by-suffix (layout-meta-data-service.get-instance)
                                                      algorithm-id)))
    (if algorithm-data
        (progn (set-property node core-options.resolved-algorithm algorithm-data) t)
        nil)))

(defjmethod must-resolve ((this layout-algorithm-resolver) node)
  (and (not (has-property node core-options.resolved-algorithm))
       (or (not (is-empty (get-children node)))
           (get-property node core-options.inside-self-loops-activate))
       t))

(defjmethod get-default-layout-algorithm-id ((this layout-algorithm-resolver))
  "org.eclipse.elk.layered")

;;; ---------------------------------------------------------------- DeprecatedLayoutOptionReplacer

(defun %dlor-next-to-port-if-possible (element)
  (add (get-property element core-options.port-labels-placement)
       port-label-placement.next-to-port-if-possible)
  (set-property element core-options.port-labels-next-to-port-if-possible nil))

(defun %dlor-space-efficient (element)
  (when (contains (get-property element core-options.node-size-options)
                  size-options.space-efficient-port-labels)
    (add (get-property element core-options.port-labels-placement)
         port-label-placement.space-efficient)
    (j-remove (get-property element core-options.node-size-options)
              size-options.space-efficient-port-labels)))

(defparameter deprecated-layout-option-replacer.rules
  (list (cons core-options.port-labels-next-to-port-if-possible #'%dlor-next-to-port-if-possible)
        (cons core-options.node-size-options #'%dlor-space-efficient))
  "RULES: option -> replacer, in insertion order (ImmutableMap).")

(defjmethod visit ((this deprecated-layout-option-replacer) element)
  (dolist (rule deprecated-layout-option-replacer.rules)
    (when (has-property element (car rule))
      (funcall (cdr rule) element)))
  nil)
