;;;; properties.lisp -- org.eclipse.elk.graph.properties beyond what
;;;; src/runtime/properties.lisp already provides (Property, IProperty,
;;;; IPropertyHolder, MapPropertyHolder, the proxy hook).

(in-package #:elk)

;;; IPropertyValueProxy: subclasses of I-PROPERTY-VALUE-PROXY define
;;; RESOLVE-VALUE/1; the runtime's holders call RESOLVE-PROXY.
(defjgeneric resolve-value 1)
(defmethod resolve-proxy ((v i-property-value-proxy) property)
  (resolve-value/1 v property))

;;; GraphFeature's description field.
(defjmethod get-description ((this graph-feature))
  (enum-case this
    (graph-feature.self-loops "Edges connecting a node with itself.")
    (graph-feature.inside-self-loops "Self-loops routed through a node instead of around it.")
    (graph-feature.multi-edges "Multiple edges with the same source and target node.")
    (graph-feature.edge-labels "Labels that are associated with edges.")
    (graph-feature.ports "Edges are connected to nodes over ports.")
    (graph-feature.compound (jstr "Edges that connect nodes from different hierarchy levels"
                                  "and are incident to compound nodes."))
    (graph-feature.clusters "Edges that connect nodes from different clusters, but not the cluster parent nodes.")
    (graph-feature.disconnected "Multiple connected components.")
    (t "")))

;;; PropertyHolderComparator: a comparator closure, as all comparators are.
(defjstatic property-holder-comparator.with (property)
  (lambda (ph1 ph2)
    (let ((p1 (get-property/1 ph1 property))
          (p2 (get-property/1 ph2 property)))
      (cond ((and p1 p2) (jcompare p1 p2))
            (p1 -1)
            (p2 1)
            (t 0)))))

;;; ElkReflect.  Classes are designated by their Lisp class name (a symbol) or
;;; class object; the registered functions are closures.  Cloning in the port
;;; normally goes through JCLONE (properties.lisp in the runtime); this
;;; registry is consulted by callers that use ElkReflect explicitly.

(defvar *elk-reflect-registry-new* (make-hash-table :test 'eq))
(defvar *elk-reflect-registry-clone* (make-hash-table :test 'eq))

(defun %elk-reflect-key (clazz)
  (if (typep clazz 'class) (class-name clazz) clazz))

(defjstatic elk-reflect.register (clazz new-fun clone-fun)
  (when (and clazz new-fun)
    (setf (gethash (%elk-reflect-key clazz) *elk-reflect-registry-new*) new-fun))
  (when (and clazz clone-fun)
    (setf (gethash (%elk-reflect-key clazz) *elk-reflect-registry-clone*) clone-fun))
  nil)

(defjstatic elk-reflect.register-new-instance (clazz new-fun)
  (elk-reflect.register/3 clazz new-fun nil))

(defjstatic elk-reflect.register-clone (clazz clone-fun)
  (elk-reflect.register/3 clazz nil clone-fun))

(defjstatic elk-reflect.new-instance (clazz)
  (let ((f (gethash (%elk-reflect-key clazz) *elk-reflect-registry-new*)))
    (and f (funcall f))))

(defjstatic elk-reflect.clone (clonee)
  ;; special treatment for EnumSets
  (if (typep clonee 'enum-set)
      (enum-set.copy-of clonee)
      (let ((f (and clonee (gethash (class-name (class-of clonee)) *elk-reflect-registry-clone*))))
        (and f (funcall f clonee)))))

;;; AdvancedPropertyValue / ExperimentalPropertyValue are annotations on enum
;;; constants.  The annotated constants of the ELK sources are listed here, as
;;; (enum-class . CONSTANT-NAME); ElkGraphUtil.isAdvancedPropertyValue and
;;; isExperimentalPropertyValue consult these tables.

(defparameter *advanced-property-values*
  '((radial-translation-strategy . "EDGE_LENGTH_BY_POSITION")
    (sorting-strategy . "POLAR_COORDINATE")
    (layering-strategy . "COFFMAN_GRAHAM")
    (layering-strategy . "INTERACTIVE")
    (cycle-breaking-strategy . "INTERACTIVE")
    (crossing-minimization-strategy . "INTERACTIVE")
    (node-promotion-strategy . "NIKOLOV_IMPROVED")
    (node-promotion-strategy . "NIKOLOV_IMPROVED_PIXEL")
    (node-promotion-strategy . "DUMMYNODE_PERCENTAGE")
    (node-promotion-strategy . "NODECOUNT_PERCENTAGE")
    (node-promotion-strategy . "NO_BOUNDARY")
    (node-placement-strategy . "INTERACTIVE")))

(defparameter *experimental-property-values*
  '((layering-strategy . "STRETCH_WIDTH")
    (layering-strategy . "MIN_WIDTH")
    (layering-strategy . "BF_MODEL_ORDER")
    (layering-strategy . "DF_MODEL_ORDER")
    (node-flexibility . "NODE_SIZE_WHERE_SPACE_PERMITS")
    (center-edge-label-placement-strategy . "WIDEST_LAYER")
    (center-edge-label-placement-strategy . "CENTER_LAYER")))

(defun %annotated-enum-value-p (enum-value table)
  (and enum-value (typep enum-value 'jenum)
       (find-if (lambda (e) (and (eq (car e) (class-name (class-of enum-value)))
                                 (string= (cdr e) (enum-name enum-value))))
                table)
       t))
