;;;; util-internal.lisp -- org.eclipse.elk.core.util.internal,
;;;; org.eclipse.elk.core.util.selection, org.eclipse.elk.core.util.persistence
;;;; and org.eclipse.elk.core.labels.

(in-package #:elk)

;;; ------------------------------------------------------------ util.internal.LayoutOptionProxy

(defjctor layout-option-proxy (value)
  (setf (.value this) value))

(defjstatic layout-option-proxy.set-proxy-value (property-holder key value)
  (let ((property (new-property key))
        (proxy (new-layout-option-proxy value)))
    (set-property property-holder property proxy)
    nil))

(defjmethod resolve-value ((this layout-option-proxy) property)
  (when property
    (let ((option-data (if (typep property 'layout-option-data)
                           property
                           (get-option-data/1 (layout-meta-data-service.get-instance)
                                              (get-id property)))))
      (when option-data
        (parse-value/1 option-data (.value this))))))

(defjmethod to-string ((this layout-option-proxy)) (.value this))

(defjmethod equals ((this layout-option-proxy) object)
  (and (typep object 'layout-option-proxy)
       (if (null (.value this))
           (null (.value object))
           (jequals (.value this) (.value object)))))

(defjmethod hash-code ((this layout-option-proxy))
  (if (.value this) (sxhash (.value this)) 0))

;;; ------------------------------------------------------------ util.selection

;;; SelectionIterator extends EMF's AbstractTreeIterator with includeRoot = false.
;;; The iterator is depth first and pre-order: the children of an element are
;;; asked for (GET-CHILDREN/1) when that element is handed out, and the
;;; children it yields are visited before its following siblings.  STACK holds
;;; one pending list per open level.

(defjgeneric get-children 1)

(defjctor selection-iterator (edge)
  (setf (.root this) edge
        (.started this) nil
        (.stack this) nil))

(defjmethod attach-visited-set ((this selection-iterator) visited-set)
  (setf (.visited this) visited-set)
  nil)

(defun %si-start (this)
  (unless (.started this)
    (setf (.started this) t)
    (push (copy-list (to-cl-list (get-children/1 this (.root this)))) (.stack this))))

(defjmethod has-next ((this selection-iterator))
  (%si-start this)
  (loop while (and (.stack this) (null (first (.stack this))))
        do (pop (.stack this)))
  (and (.stack this) t))

(defjmethod next ((this selection-iterator))
  (unless (has-next/0 this)
    (jthrow 'no-such-element-exception))
  (let ((item (pop (first (.stack this)))))
    (push (copy-list (to-cl-list (get-children/1 this item))) (.stack this))
    item))

(defmethod jiterable-items ((this selection-iterator))
  (let ((acc '()))
    (loop while (has-next/0 this)
          do (push (next/0 this) acc))
    (nreverse acc)))

(defjctor default-selection-iterator (edge add-ports follow-edge-direction)
  (this-ctor selection-iterator this edge)
  (setf (.add-ports this) add-ports
        (.follow-edge-direction this) follow-edge-direction))

(defjmethod get-children ((this default-selection-iterator) object)
  (unless (.visited this)
    (setf (.visited this) (new-jset)))
  (if (typep object 'elk-edge)
      (let ((connected-shape (if (.follow-edge-direction this)
                                 (j-get (get-targets object) 0)
                                 (j-get (get-sources object) 0))))
        (if (or (not (typep connected-shape 'elk-port))
                (js-contains (.visited this) connected-shape))
            nil
            (let* ((port connected-shape)
                   (result-edges (to-cl-list (if (.follow-edge-direction this)
                                                 (get-outgoing-edges port)
                                                 (get-incoming-edges port)))))
              (js-add (.visited this) port)
              (if (.add-ports this)
                  (cons port result-edges)
                  result-edges))))
      nil))

;;; ------------------------------------------------------------ labels.ILabelManager

;;; manageLabelSize(Object label, double targetWidth) is implemented by the
;;; application; the port has no default implementation.
(defjgeneric manage-label-size 2)

;;; ------------------------------------------------------------ util.persistence
;;; ELK's graph resources are EMF XMI resources.  The port has no EMF, so these
;;; keep the resource's contents and the two value conversions the XMI helper
;;; performs; the XMI text itself is not produced (see NOTES.md).

(defjctor elk-graph-resource (uri)
  (setf (.uri this) uri
        (.encoding this) "utf-8"))

(defjmethod get-contents ((this elk-graph-resource)) (.contents this))
(defjmethod get-uri ((this elk-graph-resource)) (.uri this))
(defjmethod get-encoding ((this elk-graph-resource)) (.encoding this))
(defjmethod set-encoding ((this elk-graph-resource) encoding)
  (setf (.encoding this) encoding)
  nil)

(defjmethod create-xmi-save ((this elk-graph-resource))
  (new-elk-graph-xmi-save (new-elk-graph-xmi-helper this)))
(defjmethod create-xmi-helper ((this elk-graph-resource))
  (new-elk-graph-xmi-helper this))


(defjmethod create-resource ((this elk-graph-resource-factory) uri)
  (new-elk-graph-resource uri))

(defjctor elk-graph-xmi-helper (resource)
  (setf (.resource this) resource))

(defjmethod create-i-property-from-string ((this elk-graph-xmi-helper) id)
  "The option with this id, or a plain property with that id."
  (or (get-option-data/1 (layout-meta-data-service.get-instance) id)
      (new-property id)))

(defjmethod create-property-value-from-string ((this elk-graph-xmi-helper) value)
  (new-layout-option-proxy value))

(defjctor elk-graph-xmi-save (helper)
  (setf (.helper this) helper))

(defjmethod save-element ((this elk-graph-xmi-save) entry feature)
  "ElkGraphXMISave.saveElement's filter.  A property entry (FEATURE :PROPERTIES,
ENTRY an ELK-PROPERTY-TO-VALUE-MAP-ENTRY) is written only when its option can
parse its value back.  Answers whether the element is kept."
  (if (and (eq feature :properties) (typep entry 'elk-property-to-value-map-entry))
      (let* ((property (.key entry))
             (option-data (get-option-data/1 (layout-meta-data-service.get-instance)
                                             (and property (get-id property)))))
        (not (and option-data (not (can-parse-value/0 option-data)))))
      t))

;;; ElkGraphXMIHelper.createFromString.  EMF is not in the port, so the EDataType
;;; is passed as a keyword: :IPROPERTY or :PROPERTY-VALUE.  The Java super call
;;; for any other data type has no counterpart here and signals.
(defjmethod create-from-string ((this elk-graph-xmi-helper) e-data-type value)
  (cond ((eq e-data-type :iproperty)
         ;; getOptionData(id), or a fresh Property(id) when the id is unknown
         (or (get-option-data (layout-meta-data-service.get-instance) value)
             (make-property value)))
        ((eq e-data-type :property-value)
         (new-layout-option-proxy value))
        (t (jthrow 'illegal-argument-exception
                   "ElkGraphXMIHelper: no EMF data type ~A" e-data-type))))
