;;;; classes.lisp -- org.eclipse.elk.graph: the ElkGraph model classes.
;;;;
;;;; Each EMF interface + Impl pair is ONE class, named after the interface.
;;;; EObject's container bookkeeping (eInternalContainer / eContainerFeatureID)
;;;; lives in E-OBJECT; many-valued features are E-LISTs (elist.lisp), created
;;;; lazily by their getters as in the generated Java.

(in-package #:elk)

(defjclass e-object ()
  ((e-internal-container nil)
   ;; Keyword naming the containing feature of the container (:children,
   ;; :ports, :labels, :contained-edges, :sections, :bend-points), or NIL.
   (e-container-feature-id nil)))

(defjclass e-map-property-holder (map-property-holder e-object) ())

(defjclass elk-graph-element (e-map-property-holder)
  ((labels nil)
   (identifier nil)))

(defjclass elk-shape (elk-graph-element)
  ((height 0.0) (width 0.0) (x 0.0) (y 0.0)))

(defjclass elk-connectable-shape (elk-shape)
  ((outgoing-edges nil)
   (incoming-edges nil)))

(defjclass elk-node (elk-connectable-shape)
  ((ports nil)
   (children nil)
   (contained-edges nil)))

(defjclass elk-port (elk-connectable-shape) ())

(defjclass elk-label (elk-shape)
  ((text "")))

(defjclass elk-edge (elk-graph-element)
  ((sources nil)
   (targets nil)
   (sections nil)))

(defjclass elk-edge-section (e-map-property-holder)
  ((start-x 0.0) (start-y 0.0) (end-x 0.0) (end-y 0.0)
   (bend-points nil)
   (outgoing-shape nil)
   (incoming-shape nil)
   (outgoing-sections nil)
   (incoming-sections nil)
   (identifier nil)))

(defjclass elk-bend-point (e-object)
  ((x 0.0) (y 0.0)))

;;; ElkPropertyToValueMapEntryImpl: an entry of an EMapPropertyHolder's
;;; property map, as handed out by iterating (j-get-properties holder).
(defjclass elk-property-to-value-map-entry ()
  ((key nil) (value nil) (holder nil)))

;;; The EList: owner + feature hooks.  KIND is :containment (the elements'
;;; container is OWNER, with or without an opposite reference -- both behave
;;; the same here) or :many-inverse (OPPOSITE is a function element -> the
;;; element's opposite e-list).
(defjclass e-list ()
  ((owner nil)
   (kind :containment)
   (feature nil)
   (opposite nil)
   (items (new-jlist))))

;;; The EMap view of an EMapPropertyHolder's properties.
(defjclass e-map ()
  ((holder nil)))

(defjclass elk-graph-factory () ())

(defjclass property-holder-comparator ()
  ((property nil)))

(defjclass graph-identifier-generator ()
  ((graph nil)
   (validate nil)
   (generate nil)
   (unique nil)
   (random nil)
   (current-i-ds (make-array 5 :initial-element 0))
   (existing-identifiers (new-jset 'equal))))

(defjinterface i-property-value-proxy (property-value-proxy))
(defjinterface advanced-property-value)
(defjinterface experimental-property-value)

;;; ElkPersistentEntry: a key/value string pair for persisted graphs.  It is an
;;; EObject, but it is not contained by any feature of the model.
(defjclass elk-persistent-entry (e-object)
  ((key nil) (value nil)))
