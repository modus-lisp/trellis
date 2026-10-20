;;;; adapters.lisp -- org.eclipse.elk.alg.layered.graph.LGraphAdapters

(in-package #:elk)

(define-jcalls :instance set-size set-position get-property has-property get-volatile-id
  set-volatile-id get-nodes get-graph get-labels get-ports get-incoming-edges get-outgoing-edges
  sort-port-list is-compound-node get-padding set-padding get-margin set-margin get-side
  has-compound-connections get-text compare)

(defjstatic l-graph-adapters.adapt (x)
  (etypecase x
    (l-graph (l-graph-adapters.adapt x nil))
    (l-label (new-l-label-adapter x))))

(defjstatic l-graph-adapters.adapt (x transparent-north-south-edges)
  (etypecase x
    (l-graph (new-l-graph-adapter x transparent-north-south-edges nil (lambda (n) (declare (ignore n)) t)))
    (l-node (new-l-node-adapter nil x transparent-north-south-edges))))

(defjstatic l-graph-adapters.adapt (graph transparent-north-south-edges transparent-comment-nodes node-filter)
  (new-l-graph-adapter graph transparent-north-south-edges transparent-comment-nodes node-filter))

;;; ------------------------------------------------------------ AbstractLShapeAdapter

(defjctor abstract-l-shape-adapter (element) (setf (.element this) element))

(defjmethod get-size ((this abstract-l-shape-adapter)) (get-size (.element this)))
(defjmethod set-size ((this abstract-l-shape-adapter) size)
  (let ((s (get-size (.element this))))
    (setf (.x s) (.x size) (.y s) (.y size))
    nil))
(defjmethod get-position ((this abstract-l-shape-adapter)) (get-position (.element this)))
(defjmethod set-position ((this abstract-l-shape-adapter) pos)
  (let ((p (get-position (.element this))))
    (setf (.x p) (.x pos) (.y p) (.y pos))
    nil))
(defjmethod get-property ((this abstract-l-shape-adapter) prop) (get-property (.element this) prop))
(defjmethod has-property ((this abstract-l-shape-adapter) prop) (has-property (.element this) prop))
(defjmethod get-volatile-id ((this abstract-l-shape-adapter)) (.id (.element this)))
(defjmethod set-volatile-id ((this abstract-l-shape-adapter) volatile-id)
  (setf (.id (.element this)) volatile-id))

;;; ------------------------------------------------------------ LGraphAdapter

(defjctor l-graph-adapter (element transparent-north-south-edges transparent-comment-nodes node-filter)
  (setf (.element this) element
        (.transparent-north-south-edges this) transparent-north-south-edges
        (.transparent-comment-nodes this) transparent-comment-nodes
        (.node-filter this) node-filter))

(defjmethod get-size ((this l-graph-adapter)) (get-size (.element this)))
(defjmethod set-size ((this l-graph-adapter) size)
  (let ((s (get-size (.element this))))
    (setf (.x s) (.x size) (.y s) (.y size))
    nil))
(defjmethod get-position ((this l-graph-adapter))
  (jthrow 'unsupported-operation-exception "Not supported by LGraph"))
(defjmethod set-position ((this l-graph-adapter) pos)
  (declare (ignore pos))
  (jthrow 'unsupported-operation-exception "Not supported by LGraph"))
(defjmethod get-property ((this l-graph-adapter) prop) (get-property (.element this) prop))
(defjmethod has-property ((this l-graph-adapter) prop) (has-property (.element this) prop))

(defjmethod get-nodes ((this l-graph-adapter))
  (when (null (.node-adapters this))
    (let ((adapters (new-jlist)))
      (setf (.node-adapters this) adapters)
      ;; We completely ignore layerless nodes here since they are currently not of interest
      ;; to anyone using these adapters
      (jdo (l (get-layers (.element this)))
        (jdo (n (get-nodes l))
          (when (funcall (.node-filter this) n)
            (jl-add adapters (new-l-node-adapter this n (.transparent-north-south-edges this)))
            (when (.transparent-comment-nodes this)
              (when (has-property n layered-internal-properties.top-comments)
                (jdo (comment (get-property n layered-internal-properties.top-comments))
                  (jl-add adapters (new-l-node-adapter this comment nil))))
              (when (has-property n layered-internal-properties.bottom-comments)
                (jdo (comment (get-property n layered-internal-properties.bottom-comments))
                  (jl-add adapters (new-l-node-adapter this comment nil))))))))))
  (.node-adapters this))

(defjmethod get-volatile-id ((this l-graph-adapter)) (.id (.element this)))
(defjmethod set-volatile-id ((this l-graph-adapter) volatile-id)
  (setf (.id (.element this)) volatile-id))

;;; ------------------------------------------------------------ LNodeAdapter

(defjctor l-node-adapter (parent element transparent-north-south-edges)
  (super-ctor abstract-l-shape-adapter this element)
  (setf (.parent-graph-adapter this) parent
        (.transparent-north-south-edges this) transparent-north-south-edges))

(defjmethod get-graph ((this l-node-adapter)) (.parent-graph-adapter this))

(defjmethod get-labels ((this l-node-adapter))
  (when (null (.label-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (l (get-labels (.element this)))
        (jl-add adapters (new-l-label-adapter l)))
      (setf (.label-adapters this) adapters)))
  (.label-adapters this))

(defjmethod get-ports ((this l-node-adapter))
  (when (null (.port-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (p (get-ports (.element this)))
        (jl-add adapters (new-l-port-adapter p (.transparent-north-south-edges this))))
      (setf (.port-adapters this) adapters)))
  (.port-adapters this))

;; we have no directly connected edges
(defjmethod get-incoming-edges ((this l-node-adapter)) (new-jlist))
(defjmethod get-outgoing-edges ((this l-node-adapter)) (new-jlist))

(defjmethod sort-port-list ((this l-node-adapter))
  (sort-port-list this l-graph-adapters.default-portlist-sorter))

(defjmethod sort-port-list ((this l-node-adapter) comparator)
  (when (is-order-fixed (get-property (.element this) layered-options.port-constraints))
    ;; We need to sort the port list accordingly
    (jl-sort (get-ports (.element this))
             (if (functionp comparator)
                 comparator
                 (lambda (a b) (compare comparator a b)))))
  nil)

(defjmethod is-compound-node ((this l-node-adapter))
  (get-property (.element this) layered-internal-properties.compound-node))

(defjmethod get-padding ((this l-node-adapter))
  (let ((l-padding (get-padding (.element this))))
    (new-elk-padding (.top l-padding) (.right l-padding) (.bottom l-padding) (.left l-padding))))

(defjmethod set-padding ((this l-node-adapter) padding)
  (let ((p (get-padding (.element this))))
    (setf (.left p) (.left padding)
          (.top p) (.top padding)
          (.right p) (.right padding)
          (.bottom p) (.bottom padding))
    nil))

(defjmethod get-margin ((this l-node-adapter))
  (let ((lmargins (get-margin (.element this))))
    (new-elk-margin (.top lmargins) (.right lmargins) (.bottom lmargins) (.left lmargins))))

(defjmethod set-margin ((this l-node-adapter) margin)
  (let ((m (get-margin (.element this))))
    (setf (.left m) (.left margin)
          (.top m) (.top margin)
          (.right m) (.right margin)
          (.bottom m) (.bottom margin))
    nil))

;;; ------------------------------------------------------------ LPortAdapter

(defjctor l-port-adapter (element transparent-north-south-edges)
  (super-ctor abstract-l-shape-adapter this element)
  (setf (.transparent-north-south-edges this) transparent-north-south-edges))

(defjmethod get-side ((this l-port-adapter)) (get-side (.element this)))

(defjmethod get-labels ((this l-port-adapter))
  (when (null (.label-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (l (get-labels (.element this)))
        (jl-add adapters (new-l-label-adapter l)))
      (setf (.label-adapters this) adapters)))
  (.label-adapters this))

(defjmethod get-margin ((this l-port-adapter))
  (let ((lmargins (get-margin (.element this))))
    (new-elk-margin (.top lmargins) (.right lmargins) (.bottom lmargins) (.left lmargins))))

(defjmethod set-margin ((this l-port-adapter) margin)
  (let ((m (get-margin (.element this))))
    (setf (.left m) (.left margin)
          (.top m) (.top margin)
          (.right m) (.right margin)
          (.bottom m) (.bottom margin))
    nil))

(defun %lg-port-edge-adapters (this incoming)
  (let* ((element (.element this))
         (adapters (new-jlist)))
    (jdo (e (if incoming (get-incoming-edges element) (get-outgoing-edges element)))
      (jl-add adapters (new-l-edge-adapter e)))
    (when (.transparent-north-south-edges this)
      (let ((port-dummy (get-property element layered-internal-properties.port-dummy)))
        (when port-dummy
          (jdo (e (if incoming (get-incoming-edges port-dummy) (get-outgoing-edges port-dummy)))
            (jl-add adapters (new-l-edge-adapter e))))))
    ;; Add the edges from the self loop holder if asked for one.
    (when (has-property (get-node element) layered-internal-properties.self-loop-holder)
      (let* ((slh (get-property (get-node element) layered-internal-properties.self-loop-holder))
             (slp (jm-get (get-sl-port-map slh) element)))
        (when slp
          (jdo (sle (if incoming (get-incoming-sl-edges slp) (get-outgoing-sl-edges slp)))
            (jl-add adapters (new-l-edge-adapter (get-l-edge sle)))))))
    adapters))

(defjmethod get-incoming-edges ((this l-port-adapter))
  (cond ((and (.transparent-north-south-edges this)
              (eq (get-type (get-node (.element this))) node-type.north-south-port))
         (new-jlist))
        (t (when (null (.incoming-edge-adapters this))
             (setf (.incoming-edge-adapters this) (%lg-port-edge-adapters this t)))
           (.incoming-edge-adapters this))))

(defjmethod get-outgoing-edges ((this l-port-adapter))
  (cond ((and (.transparent-north-south-edges this)
              (eq (get-type (get-node (.element this))) node-type.north-south-port))
         (new-jlist))
        (t (when (null (.outgoing-edge-adapters this))
             (setf (.outgoing-edge-adapters this) (%lg-port-edge-adapters this nil)))
           (.outgoing-edge-adapters this))))

(defjmethod has-compound-connections ((this l-port-adapter))
  (get-property (.element this) layered-internal-properties.inside-connections))

;;; ------------------------------------------------------------ LLabelAdapter

(defjctor l-label-adapter (element) (super-ctor abstract-l-shape-adapter this element))

(defjmethod get-side ((this l-label-adapter)) (get-property (.element this) label-side.label-side))
(defjmethod get-text ((this l-label-adapter)) (get-text (.element this)))

;;; ------------------------------------------------------------ LEdgeAdapter

(defjctor l-edge-adapter (edge) (setf (.element this) edge))

(defjmethod get-labels ((this l-edge-adapter))
  (when (null (.label-adapters this))
    (let ((adapters (new-jlist)))
      (jdo (l (get-labels (.element this)))
        (jl-add adapters (new-l-label-adapter l)))
      (setf (.label-adapters this) adapters)))
  (.label-adapters this))

;;; ------------------------------------------------------------ PortComparator

(defjmethod compare ((this layered-port-comparator) port1 port2)
  (let ((ordinal-difference (- (ordinal (get-side port1)) (ordinal (get-side port2)))))
    ;; Sort by side first
    (if (/= ordinal-difference 0)
        ordinal-difference
        (block nil
          ;; In case of equal sides, sort by port index property
          (let ((index1 (get-property port1 layered-options.port-index))
                (index2 (get-property port2 layered-options.port-index)))
            (when (and index1 index2)
              (let ((index-difference (- index1 index2)))
                (when (/= index-difference 0)
                  (return index-difference)))))
          ;; In case of equal index, sort by position
          (let ((p1 (get-position port1)) (p2 (get-position port2)))
            (enum-case (get-side port1)
              (port-side.north (double.compare (.x p1) (.x p2)))
              (port-side.east (double.compare (.y p1) (.y p2)))
              (port-side.south (double.compare (.x p2) (.x p1)))
              (port-side.west (double.compare (.y p2) (.y p1)))
              ;; Port sides should not be undefined
              (t (jthrow 'illegal-state-exception "Port side is undefined"))))))))

(defjconst l-graph-adapters.default-portlist-sorter
  (let ((comparator (make-instance 'layered-port-comparator)))
    (lambda (a b) (compare comparator a b))))
