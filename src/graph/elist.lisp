;;;; elist.lisp -- EMF's EList for the ElkGraph model, with the inverse
;;;; maintenance of the generated code (eInverseAdd / eInverseRemove /
;;;; eBasicRemoveFromContainer / basicSet*).
;;;;
;;;; EObjectEList is UNIQUE and compares elements by identity: add of a
;;;; contained element answers NIL, add at an index / set of one throws.

(in-package #:elk)

(defun %make-e-list (owner kind feature &optional opposite)
  (make-instance 'e-list :owner owner :kind kind :feature feature :opposite opposite
                         :items (new-jlist)))

;;; --- the container side (BasicEObjectImpl) ---

(defun %graph-feature-list (container feature)
  "The containment list FEATURE of CONTAINER."
  (ecase feature
    (:labels (get-labels/0 container))
    (:ports (get-ports/0 container))
    (:children (get-children/0 container))
    (:contained-edges (get-contained-edges/0 container))
    (:sections (get-sections/0 container))
    (:bend-points (get-bend-points/0 container))))

(defun %el-basic-add (el x)
  (vector-push-extend x (.items el)))

(defun %el-basic-remove (el x)
  (let* ((items (.items el)) (i (position x items :test #'eq)))
    (when i (jl-remove-at items i))))

(defun %e-basic-remove-from-container (obj)
  "eBasicRemoveFromContainer: the container's list forgets OBJ, no inverse."
  (let ((c (.e-internal-container obj)))
    (when c
      (%el-basic-remove (%graph-feature-list c (.e-container-feature-id obj)) obj)
      (setf (.e-internal-container obj) nil
            (.e-container-feature-id obj) nil))))

(defun %e-is-ancestor (ancestor obj)
  "EcoreUtil.isAncestor: OBJ is ANCESTOR or contained in it."
  (loop for o = obj then (.e-internal-container o)
        while o
        when (eq o ancestor) return t))

(defun %e-set-container (obj new-container feature)
  "The generated setParent / setContainingNode."
  (when (or (not (eq new-container (.e-internal-container obj)))
            (and new-container (not (eq feature (.e-container-feature-id obj)))))
    (when (%e-is-ancestor obj new-container)
      (jthrow 'illegal-argument-exception "Recursive containment not allowed for ~A" (to-string/0 obj)))
    (when (.e-internal-container obj)
      (%e-basic-remove-from-container obj))
    (when new-container
      (%el-basic-add (%graph-feature-list new-container feature) obj))
    (setf (.e-internal-container obj) new-container
          (.e-container-feature-id obj) (and new-container feature)))
  nil)

(defun %e-container-via (obj feature)
  "getParent(): the container, if OBJ is contained through FEATURE."
  (and (eq (.e-container-feature-id obj) feature) (.e-internal-container obj)))

;;; --- the list side ---

(defun %el-inverse-add (el x)
  (ecase (.kind el)
    (:containment
     (when (.e-internal-container x)
       (%e-basic-remove-from-container x))
     (setf (.e-internal-container x) (.owner el)
           (.e-container-feature-id x) (.feature el)))
    (:many-inverse
     (%el-basic-add (funcall (.opposite el) x) (.owner el)))))

(defun %el-inverse-remove (el x)
  (ecase (.kind el)
    (:containment
     (setf (.e-internal-container x) nil
           (.e-container-feature-id x) nil))
    (:many-inverse
     (%el-basic-remove (funcall (.opposite el) x) (.owner el)))))

(defun %el-contains (el x) (and (position x (.items el) :test #'eq) t))

(defmethod jiterable-items ((el e-list)) (copy-seq (.items el)))
(defmethod print-object ((el e-list) s)
  (print-unreadable-object (el s :type t) (format s "~A" (coerce (.items el) 'list))))

(defmethod size/0 ((el e-list)) (length (.items el)))
(defmethod is-empty/0 ((el e-list)) (zerop (length (.items el))))
(defmethod contains/1 ((el e-list) x) (%el-contains el x))
(defmethod index-of/1 ((el e-list) x) (or (position x (.items el) :test #'eq) -1))
(defmethod last-index-of/1 ((el e-list) x) (or (position x (.items el) :test #'eq :from-end t) -1))
(defmethod j-get/1 ((el e-list) i) (jl-get (.items el) i))
(defmethod get-first/0 ((el e-list)) (jl-first (.items el)))
(defmethod get-last/0 ((el e-list)) (jl-last (.items el)))
(defmethod to-array/0 ((el e-list)) (copy-seq (.items el)))
(defmethod sub-list/2 ((el e-list) a b) (jl-sub-list (.items el) a b))
(defmethod for-each/1 ((el e-list) fn) (map nil fn (copy-seq (.items el))))
(defmethod contains-all/1 ((el e-list) other)
  (block nil (jdo (x other) (unless (%el-contains el x) (return nil))) t))

(defmethod add/1 ((el e-list) x)
  (if (%el-contains el x)
      nil
      (progn (%el-basic-add el x) (%el-inverse-add el x) t)))

(defmethod add/2 ((el e-list) i x)
  (when (%el-contains el x)
    (jthrow 'illegal-argument-exception "The 'no duplicates' constraint is violated"))
  (jl-add-at (.items el) i x)
  (%el-inverse-add el x)
  nil)

(defmethod add-all/1 ((el e-list) coll)
  (let ((new (remove-duplicates (remove-if (lambda (x) (%el-contains el x)) (to-jvector coll))
                                :test #'eq :from-end t)))
    (loop for x across new do (%el-basic-add el x))
    (loop for x across new do (%el-inverse-add el x))
    (plusp (length new))))

(defmethod add-all/2 ((el e-list) i coll)
  (let ((new (remove-duplicates (remove-if (lambda (x) (%el-contains el x)) (to-jvector coll))
                                :test #'eq :from-end t)))
    (jl-add-all-at (.items el) i new)
    (loop for x across new do (%el-inverse-add el x))
    (plusp (length new))))

(defun %el-remove-at (el i)
  (let ((x (jl-remove-at (.items el) i)))
    (%el-inverse-remove el x)
    x))

(defmethod j-remove/1 ((el e-list) x)
  "remove(int) and remove(Object): elements are never integers."
  (if (integerp x)
      (%el-remove-at el x)
      (let ((i (position x (.items el) :test #'eq)))
        (when i (%el-remove-at el i) t))))

(defmethod remove-all/1 ((el e-list) coll)
  (let ((changed nil))
    (jdo (x coll)
      (loop for i = (position x (.items el) :test #'eq)
            while i do (%el-remove-at el i) (setf changed t)))
    changed))

(define-jcall j-remove-if :instance)
(defmethod j-remove-if/1 ((el e-list) pred)
  (let ((doomed (remove-if-not pred (copy-seq (.items el)))))
    (loop for x across doomed do (j-remove/1 el x))
    (plusp (length doomed))))

(defmethod retain-all/1 ((el e-list) coll)
  (let ((keep (to-cl-list coll)))
    (j-remove-if/1 el (lambda (x) (not (member x keep :test #'eq))))))

(defmethod clear/0 ((el e-list))
  (let ((old (copy-seq (.items el))))
    (jl-clear (.items el))
    (loop for x across old do (%el-inverse-remove el x))
    nil))

(defmethod j-set/2 ((el e-list) i x)
  (let ((old (jl-get (.items el) i)))
    (unless (eq old x)
      (when (%el-contains el x)
        (jthrow 'illegal-argument-exception "The 'no duplicates' constraint is violated"))
      (setf (aref (.items el) i) x)
      (when old (%el-inverse-remove el old))
      (%el-inverse-add el x))
    old))

(defmethod move/2 ((el e-list) new-pos obj-or-old-pos)
  "move(int newPosition, E object) and move(int newPosition, int oldPosition)."
  (let* ((items (.items el))
         (old (if (integerp obj-or-old-pos)
                  obj-or-old-pos
                  (or (position obj-or-old-pos items :test #'eq)
                      (jthrow 'illegal-argument-exception "The object is not contained in the list"))))
         (x (jl-remove-at items old)))
    (jl-add-at items new-pos x)
    x))

;; ECollections.sort: a stable sort that moves elements, no inverse changes.
(defjstatic e-collections.sort (list comparator)
  (jl-sort (if (typep list 'e-list) (.items list) list) (or comparator #'jcompare))
  nil)
(defjstatic e-collections.sort (list)
  (jl-sort (if (typep list 'e-list) (.items list) list) #'jcompare)
  nil)

(defun %el-copy (el)
  "A fresh jlist of EL's elements (runtime JLIST-FROM is compiled before JDO)."
  (let* ((items (.items el)) (v (new-jlist (max 4 (length items)))))
    (loop for x across items do (vector-push-extend x v))
    v))
