;;;; adapters-elk.lisp -- org.eclipse.elk.core.util.adapters.ElkGraphAdapters
;;;;
;;;; Adapters that let the layout algorithms see an ElkGraph through the
;;;; GraphAdapters interfaces.  The class skeletons live in classes.lisp; this
;;;; file holds the constructors, the static factories and the methods.

(in-package #:elk)

;;; ------------------------------------------------------------ ElkGraphAdapters (static)

(defjstatic elk-graph-adapters.adapt (node)
  (new-elk-graph-adapter node))

(defjstatic elk-graph-adapters.adapt-single-node (node)
  (new-elk-node-adapter (if (null (get-parent node))
                            nil
                            (elk-graph-adapters.adapt (get-parent node)))
                        node))

(defjstatic elk-graph-adapters.adapt-single-port (port)
  (new-elk-port-adapter port))

;; DEFAULT_PORTLIST_SORTER (a PortComparator, defined below)
(defjconst elk-graph-adapters.default-portlist-sorter
  (let ((comparator (make-instance 'core-port-comparator)))
    (lambda (a b) (compare comparator a b))))

;;; ------------------------------------------------------------ AbstractElkGraphElementAdapter

;; new Property<>(CoreOptions.PORT_BORDER_OFFSET, 0.0): the node spacing code
;; needs a default for the offset property.
(defparameter %ega-offset-proxy
  (property-with-default core-options.port-border-offset 0.0d0))

(defjctor abstract-elk-graph-element-adapter (element)
  (setf (.element this) element))

(defjmethod get-property ((this abstract-elk-graph-element-adapter) prop)
  ;; Property.equals compares ids
  (if (equal (.id prop) (.id core-options.port-border-offset))
      (get-property (.element this) %ega-offset-proxy)
      (get-property (.element this) prop)))

(defjmethod has-property ((this abstract-elk-graph-element-adapter) prop)
  (has-property (.element this) prop))

(defjmethod get-position ((this abstract-elk-graph-element-adapter))
  (new-k-vector (get-x (.element this)) (get-y (.element this))))

(defjmethod get-size ((this abstract-elk-graph-element-adapter))
  (new-k-vector (get-width (.element this)) (get-height (.element this))))

(defjmethod set-size ((this abstract-elk-graph-element-adapter) size)
  (set-width (.element this) (.x size))
  (set-height (.element this) (.y size))
  nil)

(defjmethod set-position ((this abstract-elk-graph-element-adapter) pos)
  (set-x (.element this) (.x pos))
  (set-y (.element this) (.y pos))
  nil)

(defjmethod get-padding ((this abstract-elk-graph-element-adapter))
  (let ((padding (get-property (.element this) core-options.padding)))
    (if (null padding)
        (new-elk-padding)
        (new-elk-padding padding))))

(defjmethod set-padding ((this abstract-elk-graph-element-adapter) padding)
  ;; analog to the padding case, the padding object is copied
  (set-property (.element this) core-options.padding (new-elk-padding padding))
  nil)

(defjmethod get-margin ((this abstract-elk-graph-element-adapter))
  (let ((margins (get-property (.element this) core-options.margins)))
    (when (null margins)
      (setf margins (new-elk-margin)))
    margins))

(defjmethod set-margin ((this abstract-elk-graph-element-adapter) margin)
  ;; analog to the padding case, we copy the margins object here
  (set-property (.element this) core-options.margins (new-elk-margin margin))
  nil)

(defjmethod get-volatile-id ((this abstract-elk-graph-element-adapter))
  (.id this))

(defjmethod set-volatile-id ((this abstract-elk-graph-element-adapter) volatile-id)
  (setf (.id this) volatile-id)
  nil)

;;; ------------------------------------------------------------ ElkGraphAdapter

(defjctor elk-graph-adapter (node)
  (super-ctor abstract-elk-graph-element-adapter this node))

(defjmethod get-nodes ((this elk-graph-adapter))
  (when (null (.child-nodes this))
    (let ((adapters (new-jlist)))
      (jdo (n (get-children (.element this)))
        (jl-add adapters (new-elk-node-adapter this n)))
      (setf (.child-nodes this) adapters)))
  (.child-nodes this))

;;; ------------------------------------------------------------ ElkNodeAdapter

(defjctor elk-node-adapter (parent node)
  (super-ctor abstract-elk-graph-element-adapter this node)
  (setf (.parent-graph-adapter this) parent))

(defjmethod get-graph ((this elk-node-adapter))
  (.parent-graph-adapter this))

(defjmethod get-labels ((this elk-node-adapter))
  (when (null (.label-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (l (get-labels (.element this)))
        (jl-add adapters (new-elk-label-adapter l)))
      (setf (.label-adapters this) adapters)))
  (.label-adapters this))

(defjmethod get-ports ((this elk-node-adapter))
  (when (null (.port-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (p (get-ports (.element this)))
        (jl-add adapters (new-elk-port-adapter p)))
      (setf (.port-adapters this) adapters)))
  (.port-adapters this))

(defjmethod get-incoming-edges ((this elk-node-adapter))
  (when (null (.incoming-edge-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (e (elk-graph-util.all-incoming-edges (.element this)))
        (jl-add adapters (new-elk-edge-adapter e)))
      (setf (.incoming-edge-adapters this) adapters)))
  (.incoming-edge-adapters this))

(defjmethod get-outgoing-edges ((this elk-node-adapter))
  (when (null (.outgoing-edge-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (e (elk-graph-util.all-outgoing-edges (.element this)))
        (jl-add adapters (new-elk-edge-adapter e)))
      (setf (.outgoing-edge-adapters this) adapters)))
  (.outgoing-edge-adapters this))

(defjmethod sort-port-list ((this elk-node-adapter))
  (sort-port-list this elk-graph-adapters.default-portlist-sorter))

(defjmethod sort-port-list ((this elk-node-adapter) comparator)
  (when (is-order-fixed (get-property (.element this) core-options.port-constraints))
    ;; ECollections.sort(element.getPorts(), comparator)
    (jl-sort (get-ports (.element this))
             (if (functionp comparator)
                 comparator
                 (lambda (a b) (compare comparator a b)))))
  nil)

(defjmethod is-compound-node ((this elk-node-adapter))
  (or (not (is-empty (get-children (.element this))))
      (get-property (.element this) core-options.inside-self-loops-activate)))

;;; ------------------------------------------------------------ ElkLabelAdapter

(defjctor elk-label-adapter (label)
  (super-ctor abstract-elk-graph-element-adapter this label))

(defjmethod get-side ((this elk-label-adapter))
  (get-property (.element this) label-side.label-side))

(defjmethod get-text ((this elk-label-adapter))
  (get-text (.element this)))

;;; ------------------------------------------------------------ ElkPortAdapter

(defjctor elk-port-adapter (port)
  (super-ctor abstract-elk-graph-element-adapter this port))

(defjmethod get-side ((this elk-port-adapter))
  (get-property (.element this) core-options.port-side))

(defjmethod get-labels ((this elk-port-adapter))
  (when (null (.label-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (l (get-labels (.element this)))
        (jl-add adapters (new-elk-label-adapter l)))
      (setf (.label-adapters this) adapters)))
  (.label-adapters this))

(defjmethod get-incoming-edges ((this elk-port-adapter))
  (when (null (.incoming-edge-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (e (get-incoming-edges (.element this)))
        (jl-add adapters (new-elk-edge-adapter e)))
      (setf (.incoming-edge-adapters this) adapters)))
  (.incoming-edge-adapters this))

(defjmethod get-outgoing-edges ((this elk-port-adapter))
  (when (null (.outgoing-edge-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (e (get-outgoing-edges (.element this)))
        (jl-add adapters (new-elk-edge-adapter e)))
      (setf (.outgoing-edge-adapters this) adapters)))
  (.outgoing-edge-adapters this))

(defjmethod has-compound-connections ((this elk-port-adapter))
  (let ((node (get-parent (.element this))))
    (or (block found
          (jdo (edge (get-outgoing-edges (.element this)))
            (jdo (target (get-targets edge))
              (let ((target-node (elk-graph-util.connectable-shape-to-node target)))
                (cond ((elk-graph-util.is-descendant target-node node)
                       (return-from found t))
                      ((and (eq target-node node)
                            (get-property edge core-options.inside-self-loops-yo))
                       ;; Inside self loops are treated as compound connections, too
                       (return-from found t))))))
          nil)
        (block found
          (jdo (edge (get-incoming-edges (.element this)))
            (jdo (source (get-sources edge))
              (when (elk-graph-util.is-descendant
                     (elk-graph-util.connectable-shape-to-node source) node)
                (return-from found t))))
          nil))))

;;; ------------------------------------------------------------ ElkEdgeAdapter

(defjctor elk-edge-adapter (edge)
  (setf (.element this) edge))

(defjmethod get-labels ((this elk-edge-adapter))
  (when (null (.label-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (l (get-labels (.element this)))
        (jl-add adapters (new-elk-label-adapter l)))
      (setf (.label-adapters this) adapters)))
  (.label-adapters this))

;;; ------------------------------------------------------------ PortComparator

(defjmethod compare ((this core-port-comparator) port1 port2)
  (let ((ordinal-difference (- (ordinal (get-property port1 core-options.port-side))
                               (ordinal (get-property port2 core-options.port-side)))))
    ;; Sort by side first
    (if (/= ordinal-difference 0)
        ordinal-difference
        (block nil
          ;; In case of equal sides, sort by port index property
          (let ((index1 (get-property port1 core-options.port-index))
                (index2 (get-property port2 core-options.port-index)))
            (when (and index1 index2)
              (let ((index-difference (- index1 index2)))
                (when (/= index-difference 0)
                  (return index-difference)))))
          ;; In case of equal index, sort by position
          (enum-case (get-property port1 core-options.port-side)
            (port-side.north (double.compare (get-x port1) (get-x port2)))
            (port-side.east (double.compare (get-y port1) (get-y port2)))
            ;; reversed order
            (port-side.south (double.compare (get-x port2) (get-x port1)))
            (port-side.west (double.compare (get-y port2) (get-y port1)))
            ;; Port sides should not be undefined
            (t (jthrow 'illegal-state-exception "Port side is undefined")))))))
