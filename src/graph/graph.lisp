;;;; graph.lisp -- org.eclipse.elk.graph + graph.impl: features of the model
;;;; classes, the derived features, toString, and the EObject containment
;;;; queries (eContainer, eContents, eAllContents).

(in-package #:elk)

;;; ------------------------------------------------------------ EObject

(defjmethod e-container ((this e-object)) (.e-internal-container this))

(defgeneric %e-contents (obj)
  (:documentation "The contained objects of OBJ, in EMF feature order, as a CL list.
The property map entries (EMapPropertyHolder.properties) are not modelled as
objects and are not part of the contents."))
(defmethod %e-contents ((obj e-object)) nil)

(defjmethod e-contents ((this e-object)) (apply #'jlist (%e-contents this)))

(defjmethod e-all-contents ((this e-object))
  "eAllContents(): a pre-order traversal of the containment tree, root excluded.
Answered as a fresh jlist (a snapshot) instead of a TreeIterator."
  (let ((out (new-jlist)))
    (labels ((walk (o) (dolist (c (%e-contents o)) (vector-push-extend c out) (walk c))))
      (walk this))
    out))

(defun %graph-hash-string (obj)
  (format nil "~(~x~)" (logand (sb-kernel:get-lisp-obj-address obj) #xffffffff)))

(defjmethod to-string ((this e-object))
  (jstr (%java-impl-name this) "@" (%graph-hash-string this)))

(defun %java-impl-name (obj)
  (typecase obj
    (elk-node "org.eclipse.elk.graph.impl.ElkNodeImpl")
    (elk-port "org.eclipse.elk.graph.impl.ElkPortImpl")
    (elk-label "org.eclipse.elk.graph.impl.ElkLabelImpl")
    (elk-edge "org.eclipse.elk.graph.impl.ElkEdgeImpl")
    (elk-edge-section "org.eclipse.elk.graph.impl.ElkEdgeSectionImpl")
    (elk-bend-point "org.eclipse.elk.graph.impl.ElkBendPointImpl")
    (t (string-downcase (class-name (class-of obj))))))

(defmethod print-object ((o e-object) s)
  (print-unreadable-object (o s) (write-string (to-string/0 o) s)))

;;; ------------------------------------------------------------ EMapPropertyHolder

(defjmethod j-get-properties ((this e-map-property-holder))
  (make-instance 'e-map :holder this))

(defmethod jiterable-items ((m e-map))
  (let ((h (.holder m)))
    (coerce (loop for (p . v) in (property-value-cells h)
                  collect (make-instance 'elk-property-to-value-map-entry :key p :value v :holder h))
            'vector)))
(defmethod size/0 ((m e-map)) (let ((t1 (.property-map (.holder m)))) (if t1 (hash-table-count t1) 0)))
(defmethod is-empty/0 ((m e-map)) (zerop (size/0 m)))
(defmethod j-get/1 ((m e-map) key)
  (let ((t1 (.property-map (.holder m))))
    (and t1 (cdr (gethash (.id key) t1)))))
(defmethod contains-key/1 ((m e-map) key) (has-property/1 (.holder m) key))
(defmethod put/2 ((m e-map) key value)
  (prog1 (j-get/1 m key)
    (setf (gethash (.id key) (%pmap (.holder m))) (cons key value))))
(defmethod put-all/1 ((m e-map) other)
  (if (hash-table-p other)
      (maphash (lambda (k v) (put/2 m k v)) other)
      (jdo (e other) (put/2 m (get-key/0 e) (get-value/0 e)))))
(define-jcall remove-key :instance)
(defmethod remove-key/1 ((m e-map) key)
  (prog1 (j-get/1 m key)
    (let ((t1 (.property-map (.holder m)))) (when t1 (remhash (.id key) t1)))))
(defmethod j-remove/1 ((m e-map) key) (remove-key/1 m key))
(defmethod clear/0 ((m e-map)) (let ((t1 (.property-map (.holder m)))) (when t1 (clrhash t1))) nil)
(defmethod key-set/0 ((m e-map))
  (apply #'jlist (mapcar #'car (property-value-cells (.holder m)))))
(defmethod j-values/0 ((m e-map))
  (apply #'jlist (mapcar #'cdr (property-value-cells (.holder m)))))
(define-jcall j-map :instance)
(defmethod j-map/0 ((m e-map)) (get-all-properties/0 (.holder m)))

(defjmethod get-key ((e elk-property-to-value-map-entry)) (.key e))
(defjmethod get-value ((e elk-property-to-value-map-entry)) (.value e))
(defjmethod set-value ((e elk-property-to-value-map-entry) v)
  (prog1 (.value e)
    (setf (.value e) v)
    (when (.holder e)
      (setf (gethash (.id (.key e)) (%pmap (.holder e))) (cons (.key e) v)))))
(defjmethod set-key ((e elk-property-to-value-map-entry) k) (setf (.key e) k) nil)
(defjmethod get-typed-key ((e elk-property-to-value-map-entry)) (.key e))
(defjmethod set-typed-key ((e elk-property-to-value-map-entry) k) (setf (.key e) k) nil)
(defjmethod to-string ((e elk-property-to-value-map-entry))
  (jstr (if (.key e) (get-id/0 (.key e)) "null") " -> " (let ((v (.value e))) (if (typep v 'jobject) (to-string/0 v) v))))

;;; getProperty / setProperty / hasProperty / copyProperties / getAllProperties
;;; are MapPropertyHolder's (src/runtime/properties.lisp), which has the same
;;; semantics as EMapPropertyHolderImpl (proxy resolution, cloned defaults).

;;; ------------------------------------------------------------ ElkGraphElement

(defjmethod get-labels ((this elk-graph-element))
  (or (.labels this) (setf (.labels this) (%make-e-list this :containment :labels))))
(defjmethod get-identifier ((this elk-graph-element)) (.identifier this))
(defjmethod set-identifier ((this elk-graph-element) id) (setf (.identifier this) id) nil)
(defmethod %e-contents ((obj elk-graph-element))
  (coerce (.items (get-labels/0 obj)) 'list))

(defjmethod to-string ((this elk-graph-element))
  (jstr (call-next-method) " (identifier: " (.identifier this) ")"))

;;; ------------------------------------------------------------ ElkShape

(defjmethod get-height ((this elk-shape)) (.height this))
(defjmethod set-height ((this elk-shape) v) (setf (.height this) (jd v)) nil)
(defjmethod get-width ((this elk-shape)) (.width this))
(defjmethod set-width ((this elk-shape) v) (setf (.width this) (jd v)) nil)
(defjmethod get-x ((this elk-shape)) (.x this))
(defjmethod set-x ((this elk-shape) v) (setf (.x this) (jd v)) nil)
(defjmethod get-y ((this elk-shape)) (.y this))
(defjmethod set-y ((this elk-shape) v) (setf (.y this) (jd v)) nil)
(defjmethod set-dimensions ((this elk-shape) width height)
  (set-width/1 this width)
  (set-height/1 this height)
  nil)
(defjmethod set-location ((this elk-shape) x y)
  (set-x/1 this x)
  (set-y/1 this y)
  nil)

(defjmethod to-string ((this elk-shape))
  (jstr (call-next-method)
        " (height: " (.height this) ", width: " (.width this)
        ", x: " (.x this) ", y: " (.y this) ")"))

;;; ------------------------------------------------------------ ElkConnectableShape

(defjmethod get-outgoing-edges ((this elk-connectable-shape))
  (or (.outgoing-edges this)
      (setf (.outgoing-edges this)
            (%make-e-list this :many-inverse :outgoing-edges (lambda (e) (get-sources/0 e))))))
(defjmethod get-incoming-edges ((this elk-connectable-shape))
  (or (.incoming-edges this)
      (setf (.incoming-edges this)
            (%make-e-list this :many-inverse :incoming-edges (lambda (e) (get-targets/0 e))))))

;;; ------------------------------------------------------------ ElkNode

(defjmethod get-ports ((this elk-node))
  (or (.ports this) (setf (.ports this) (%make-e-list this :containment :ports))))
(defjmethod get-children ((this elk-node))
  (or (.children this) (setf (.children this) (%make-e-list this :containment :children))))
(defjmethod get-contained-edges ((this elk-node))
  (or (.contained-edges this)
      (setf (.contained-edges this) (%make-e-list this :containment :contained-edges))))
(defjmethod get-parent ((this elk-node)) (%e-container-via this :children))
(defjmethod set-parent ((this elk-node) new-parent) (%e-set-container this new-parent :children))
(defjmethod is-hierarchical ((this elk-node)) (> (size/0 (get-children/0 this)) 0))

(defmethod %e-contents ((obj elk-node))
  (append (call-next-method)
          (coerce (.items (get-ports/0 obj)) 'list)
          (coerce (.items (get-children/0 obj)) 'list)
          (coerce (.items (get-contained-edges/0 obj)) 'list)))

(defun %elk-name-part (this)
  "Identifier, or else the first label's text, as ElkNode/Port/EdgeImpl.toString."
  (let ((id (get-identifier/0 this)))
    (cond ((and id (plusp (length id))) (jstr " \"" id "\""))
          ((> (size/0 (get-labels/0 this)) 0)
           (let ((text (get-text/0 (j-get/1 (get-labels/0 this) 0))))
             (if (and text (plusp (length text))) (jstr " \"" text "\"") "")))
          (t ""))))

(defun %elk-geometry-part (this)
  (jstr " (" (.x this) "," (.y this) " | " (.width this) "," (.height this) ")"))

(defjmethod to-string ((this elk-node))
  (jstr "ElkNode" (%elk-name-part this) (%elk-geometry-part this)))

;;; ------------------------------------------------------------ ElkPort

(defjmethod get-parent ((this elk-port)) (%e-container-via this :ports))
(defjmethod set-parent ((this elk-port) new-parent) (%e-set-container this new-parent :ports))

(defjmethod to-string ((this elk-port))
  (jstr "ElkPort" (%elk-name-part this) (%elk-geometry-part this)))

;;; ------------------------------------------------------------ ElkLabel

(defjmethod get-parent ((this elk-label)) (%e-container-via this :labels))
(defjmethod set-parent ((this elk-label) new-parent) (%e-set-container this new-parent :labels))
(defjmethod get-text ((this elk-label)) (.text this))
(defjmethod set-text ((this elk-label) text) (setf (.text this) text) nil)

(defjmethod to-string ((this elk-label))
  (let ((text (.text this)))
    (jstr "ElkLabel" (if (and text (plusp (length text))) (jstr " \"" text "\"") "")
          (%elk-geometry-part this))))

;;; ------------------------------------------------------------ ElkEdge

(defjmethod get-containing-node ((this elk-edge)) (%e-container-via this :contained-edges))
(defjmethod set-containing-node ((this elk-edge) node) (%e-set-container this node :contained-edges))
(defjmethod get-sources ((this elk-edge))
  (or (.sources this)
      (setf (.sources this)
            (%make-e-list this :many-inverse :sources (lambda (s) (get-outgoing-edges/0 s))))))
(defjmethod get-targets ((this elk-edge))
  (or (.targets this)
      (setf (.targets this)
            (%make-e-list this :many-inverse :targets (lambda (s) (get-incoming-edges/0 s))))))
(defjmethod get-sections ((this elk-edge))
  (or (.sections this) (setf (.sections this) (%make-e-list this :containment :sections))))

(defjmethod is-hyperedge ((this elk-edge))
  (not (and (<= (size/0 (get-sources/0 this)) 1) (<= (size/0 (get-targets/0 this)) 1))))

(defjmethod is-hierarchical ((this elk-edge))
  ;; We're basically iterating over all sources and targets and check whether
  ;; they are contained in the same graph. If they are, we're a simple edge.
  (let ((common nil))
    (jdo (shape (elk-graph-util.all-incident-shapes/1 this))
      (let ((shape-node (elk-graph-util.connectable-shape-to-node/1 shape)))
        (cond ((null common) (setf common (get-parent/0 shape-node)))
              ((not (eq common (get-parent/0 shape-node)))
               ;; Different hierarchy levels
               (return-from is-hierarchical/0 t)))))
    nil))

(defjmethod is-selfloop ((this elk-edge))
  (let ((common nil))
    (jdo (shape (elk-graph-util.all-incident-shapes/1 this))
      (let ((shape-node (elk-graph-util.connectable-shape-to-node/1 shape)))
        (cond ((null common) (setf common shape-node))
              ((not (eq common shape-node)) (return-from is-selfloop/0 nil)))))
    t))

(defjmethod is-connected ((this elk-edge))
  (and (not (is-empty/0 (get-sources/0 this))) (not (is-empty/0 (get-targets/0 this)))))

(defmethod %e-contents ((obj elk-edge))
  (append (call-next-method) (coerce (.items (get-sections/0 obj)) 'list)))

(defjmethod to-string ((this elk-edge))
  (let ((hyper (is-hyperedge/0 this))
        (join (lambda (el) (format nil "~{~A~^, ~}" (map 'list #'to-string/0 (.items el))))))
    (jstr "ElkEdge" (%elk-name-part this)
          (if hyper " [" " ")
          (funcall join (get-sources/0 this))
          (if hyper "]" "")
          " -> "
          (if hyper "[" "")
          (funcall join (get-targets/0 this))
          (if hyper "]" ""))))

;;; ------------------------------------------------------------ ElkEdgeSection

(defjmethod get-start-x ((this elk-edge-section)) (.start-x this))
(defjmethod set-start-x ((this elk-edge-section) v) (setf (.start-x this) (jd v)) nil)
(defjmethod get-start-y ((this elk-edge-section)) (.start-y this))
(defjmethod set-start-y ((this elk-edge-section) v) (setf (.start-y this) (jd v)) nil)
(defjmethod get-end-x ((this elk-edge-section)) (.end-x this))
(defjmethod set-end-x ((this elk-edge-section) v) (setf (.end-x this) (jd v)) nil)
(defjmethod get-end-y ((this elk-edge-section)) (.end-y this))
(defjmethod set-end-y ((this elk-edge-section) v) (setf (.end-y this) (jd v)) nil)
(defjmethod get-bend-points ((this elk-edge-section))
  (or (.bend-points this) (setf (.bend-points this) (%make-e-list this :containment :bend-points))))
(defjmethod get-parent ((this elk-edge-section)) (%e-container-via this :sections))
(defjmethod set-parent ((this elk-edge-section) edge) (%e-set-container this edge :sections))
(defjmethod get-outgoing-shape ((this elk-edge-section)) (.outgoing-shape this))
(defjmethod basic-get-outgoing-shape ((this elk-edge-section)) (.outgoing-shape this))
(defjmethod set-outgoing-shape ((this elk-edge-section) s) (setf (.outgoing-shape this) s) nil)
(defjmethod get-incoming-shape ((this elk-edge-section)) (.incoming-shape this))
(defjmethod basic-get-incoming-shape ((this elk-edge-section)) (.incoming-shape this))
(defjmethod set-incoming-shape ((this elk-edge-section) s) (setf (.incoming-shape this) s) nil)
(defjmethod get-outgoing-sections ((this elk-edge-section))
  (or (.outgoing-sections this)
      (setf (.outgoing-sections this)
            (%make-e-list this :many-inverse :outgoing-sections (lambda (s) (get-incoming-sections/0 s))))))
(defjmethod get-incoming-sections ((this elk-edge-section))
  (or (.incoming-sections this)
      (setf (.incoming-sections this)
            (%make-e-list this :many-inverse :incoming-sections (lambda (s) (get-outgoing-sections/0 s))))))
(defjmethod get-identifier ((this elk-edge-section)) (.identifier this))
(defjmethod set-identifier ((this elk-edge-section) id) (setf (.identifier this) id) nil)
(defjmethod set-start-location ((this elk-edge-section) x y)
  (set-start-x/1 this x)
  (set-start-y/1 this y)
  nil)
(defjmethod set-end-location ((this elk-edge-section) x y)
  (set-end-x/1 this x)
  (set-end-y/1 this y)
  nil)

(defmethod %e-contents ((obj elk-edge-section))
  (coerce (.items (get-bend-points/0 obj)) 'list))

(defjmethod to-string ((this elk-edge-section))
  (jstr (call-next-method)
        " (startX: " (.start-x this) ", startY: " (.start-y this)
        ", endX: " (.end-x this) ", endY: " (.end-y this)
        ", identifier: " (.identifier this) ")"))

;;; ------------------------------------------------------------ ElkBendPoint

(defjmethod get-x ((this elk-bend-point)) (.x this))
(defjmethod set-x ((this elk-bend-point) v) (setf (.x this) (jd v)) nil)
(defjmethod get-y ((this elk-bend-point)) (.y this))
(defjmethod set-y ((this elk-bend-point) v) (setf (.y this) (jd v)) nil)
(defjmethod j-set ((this elk-bend-point) x y)
  (set-x/1 this x)
  (set-y/1 this y)
  nil)

(defjmethod to-string ((this elk-bend-point))
  (jstr (call-next-method) " (x: " (.x this) ", y: " (.y this) ")"))

;;; ------------------------------------------------------------ ElkGraphFactory

(defjconst elk-graph-factory.e-instance (make-instance 'elk-graph-factory))

(defjstatic elk-graph-factory-impl.init () elk-graph-factory.e-instance)

(defjmethod create-elk-node ((f elk-graph-factory)) (new-elk-node/0))
(defjmethod create-elk-port ((f elk-graph-factory)) (new-elk-port/0))
(defjmethod create-elk-label ((f elk-graph-factory)) (new-elk-label/0))
(defjmethod create-elk-edge ((f elk-graph-factory)) (new-elk-edge/0))
(defjmethod create-elk-edge-section ((f elk-graph-factory)) (new-elk-edge-section/0))
(defjmethod create-elk-bend-point ((f elk-graph-factory)) (new-elk-bend-point/0))
(defjmethod create-elk-property-to-value-map-entry ((f elk-graph-factory))
  (new-elk-property-to-value-map-entry/0))

;;; ------------------------------------------------------------ ElkPersistentEntry

(defjmethod get-key ((e elk-persistent-entry)) (.key e))
(defjmethod set-key ((e elk-persistent-entry) k) (setf (.key e) k) nil)
(defjmethod get-value ((e elk-persistent-entry)) (.value e))
(defjmethod set-value ((e elk-persistent-entry) v) (setf (.value e) v) nil)
