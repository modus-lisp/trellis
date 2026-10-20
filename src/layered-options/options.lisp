;;;; options.lisp -- org.eclipse.elk.alg.layered.options: enum methods,
;;;; Spacings, LayeredSpacings.  (Enum constants are generated in gen-enums.)

(in-package #:elk)

;;; ------------------------------------------------------------ CenterEdgeLabelPlacementStrategy

(defjmethod uses-label-size-information ((this center-edge-label-placement-strategy))
  (or (eq this center-edge-label-placement-strategy.widest-layer)
      (eq this center-edge-label-placement-strategy.center-layer)
      (eq this center-edge-label-placement-strategy.space-efficient-layer)))

;;; ------------------------------------------------------------ phase factories (ILayoutPhaseFactory.create)

(defjmethod create ((this crossing-minimization-strategy))
  (enum-case this
    (crossing-minimization-strategy.layer-sweep
     (new-layer-sweep-crossing-minimizer cross-min-type.barycenter))
    (crossing-minimization-strategy.median-layer-sweep
     (new-layer-sweep-crossing-minimizer cross-min-type.median))
    (crossing-minimization-strategy.interactive (new-interactive-crossing-minimizer))
    (crossing-minimization-strategy.none (new-no-crossing-minimizer))
    (t (jthrow 'illegal-argument-exception
               "No implementation is available for the crossing minimizer ~A" (name this)))))

(defjmethod create ((this cycle-breaking-strategy))
  (enum-case this
    (cycle-breaking-strategy.greedy (new-greedy-cycle-breaker))
    (cycle-breaking-strategy.depth-first (new-depth-first-cycle-breaker))
    (cycle-breaking-strategy.interactive (new-interactive-cycle-breaker))
    (cycle-breaking-strategy.model-order (new-model-order-cycle-breaker))
    (cycle-breaking-strategy.greedy-model-order (new-greedy-model-order-cycle-breaker))
    (cycle-breaking-strategy.scc-connectivity (new-sc-connectivity))
    (cycle-breaking-strategy.scc-node-type (new-scc-node-type-cycle-breaker))
    (cycle-breaking-strategy.dfs-node-order (new-dfs-node-order-cycle-breaker))
    (cycle-breaking-strategy.bfs-node-order (new-bfs-node-order-cycle-breaker))
    (t (jthrow 'illegal-argument-exception
               "No implementation is available for the cycle breaker ~A" (name this)))))

(defjmethod create ((this layering-strategy))
  (enum-case this
    (layering-strategy.network-simplex (new-network-simplex-layerer))
    (layering-strategy.longest-path (new-longest-path-layerer))
    (layering-strategy.coffman-graham (new-coffman-graham-layerer))
    (layering-strategy.interactive (new-interactive-layerer))
    (layering-strategy.stretch-width (new-stretch-width-layerer))
    (layering-strategy.min-width (new-min-width-layerer))
    (layering-strategy.longest-path-source (new-longest-path-source-layerer))
    (layering-strategy.bf-model-order (new-breadth-first-model-order-layerer))
    (layering-strategy.df-model-order (new-depth-first-model-order-layerer))
    (t (jthrow 'illegal-argument-exception
               "No implementation is available for the layerer ~A" (name this)))))

(defjmethod create ((this node-placement-strategy))
  (enum-case this
    (node-placement-strategy.simple (new-simple-node-placer))
    (node-placement-strategy.interactive (new-interactive-node-placer))
    (node-placement-strategy.linear-segments (new-linear-segments-node-placer))
    (node-placement-strategy.brandes-koepf (new-bk-node-placer))
    (node-placement-strategy.network-simplex (new-network-simplex-placer))
    (t (jthrow 'illegal-argument-exception
               "No implementation is available for the node placer ~A" (name this)))))

;;; ------------------------------------------------------------ EdgeLabelSideSelection

(defjmethod transpose ((this edge-label-side-selection))
  (enum-case this
    (edge-label-side-selection.always-up edge-label-side-selection.always-down)
    (edge-label-side-selection.always-down edge-label-side-selection.always-up)
    (edge-label-side-selection.direction-up edge-label-side-selection.direction-down)
    (edge-label-side-selection.direction-down edge-label-side-selection.direction-up)
    (edge-label-side-selection.smart-up edge-label-side-selection.smart-down)
    (edge-label-side-selection.smart-down edge-label-side-selection.smart-up)
    (t nil)))

;;; ------------------------------------------------------------ LongEdgeOrderingStrategy

(defjmethod return-value ((this long-edge-ordering-strategy))
  (enum-case this
    (long-edge-ordering-strategy.dummy-node-over integer.max-value)
    (long-edge-ordering-strategy.dummy-node-under integer.min-value)
    (t 0)))

;;; ------------------------------------------------------------ NodeFlexibility

(defjmethod is-flexible-size ((this node-flexibility))
  (eq this node-flexibility.node-size))

(defjmethod is-flexible-size-where-space-permits ((this node-flexibility))
  (or (eq this node-flexibility.node-size-where-space-permits)
      (eq this node-flexibility.node-size)))

(defjmethod is-flexible-ports ((this node-flexibility))
  (or (eq this node-flexibility.port-position)
      (eq this node-flexibility.node-size-where-space-permits)
      (eq this node-flexibility.node-size)))

(defjmethod is-at-least ((this node-flexibility) nf)
  ;; Java: the NODE_SIZE_WHERE_SPACE_PERMITS case has no return and falls
  ;; through to PORT_POSITION's.
  (enum-case this
    (node-flexibility.node-size (is-flexible-size nf))
    ((node-flexibility.node-size-where-space-permits node-flexibility.port-position)
     (is-flexible-ports nf))
    (t t)))

(defjstatic node-flexibility.get-node-flexibility (l-node)
  ;; getAllProperties().containsKey(P) == hasProperty(P): holders are keyed by id.
  (if (has-property l-node layered-options.node-placement-network-simplex-node-flexibility)
      (get-property l-node layered-options.node-placement-network-simplex-node-flexibility)
      (get-property (get-graph l-node)
                    layered-options.node-placement-network-simplex-node-flexibility-default)))

;;; ------------------------------------------------------------ Spacings

(defjctor spacings (graph)
  (setf (.graph this) graph)
  (let ((n (length (node-type.values))))
    (setf (.node-type-spacing-options-horizontal this) (make-array (list n n) :initial-element nil)
          (.node-type-spacing-options-vertical this) (make-array (list n n) :initial-element nil)))
  (%spacings-precalculate-node-type-spacings this))

(defun %spacings-precalculate-node-type-spacings (this)
  (flet ((nts (&rest args) (apply #'%spacings-node-type-spacing this args)))
    (nts node-type.normal layered-options.spacing-node-node layered-options.spacing-node-node-between-layers)
    (nts node-type.normal node-type.long-edge
         layered-options.spacing-edge-node layered-options.spacing-edge-node-between-layers)
    (nts node-type.normal node-type.north-south-port layered-options.spacing-edge-node)
    (nts node-type.normal node-type.external-port layered-options.spacing-edge-node) ; TODO
    (nts node-type.normal node-type.label
         layered-options.spacing-node-node layered-options.spacing-node-node-between-layers)
    (nts node-type.long-edge layered-options.spacing-edge-edge layered-options.spacing-edge-edge-between-layers)
    (nts node-type.long-edge node-type.north-south-port layered-options.spacing-edge-edge)
    (nts node-type.long-edge node-type.external-port layered-options.spacing-edge-edge) ; TODO
    (nts node-type.long-edge node-type.label
         layered-options.spacing-edge-node layered-options.spacing-edge-node-between-layers)
    (nts node-type.north-south-port layered-options.spacing-edge-edge)
    (nts node-type.north-south-port node-type.external-port layered-options.spacing-edge-edge) ; TODO
    (nts node-type.north-south-port node-type.label layered-options.spacing-label-node)
    (nts node-type.external-port layered-options.spacing-port-port)
    (nts node-type.external-port node-type.label
         layered-options.spacing-label-port-vertical layered-options.spacing-label-port-horizontal)
    (nts node-type.label layered-options.spacing-edge-edge layered-options.spacing-edge-edge)
    (nts node-type.breaking-point
         layered-options.spacing-edge-edge layered-options.spacing-edge-edge-between-layers)
    (nts node-type.breaking-point node-type.normal
         layered-options.spacing-edge-node layered-options.spacing-edge-node-between-layers)
    (nts node-type.breaking-point node-type.label
         layered-options.spacing-edge-node layered-options.spacing-edge-node-between-layers)
    (nts node-type.breaking-point node-type.long-edge
         layered-options.spacing-edge-node layered-options.spacing-edge-node-between-layers)))

(defun %spacings-node-type-spacing (this &rest args)
  "The four private nodeTypeSpacing overloads: (nt sp), (nt vert horz),
(n1 n2 sp), (n1 n2 vert horz)."
  (let ((v (.node-type-spacing-options-vertical this))
        (h (.node-type-spacing-options-horizontal this)))
    (if (typep (second args) 'node-type)
        (destructuring-bind (n1 n2 vert &optional (horz nil horz-p)) args
          (let ((a (ordinal n1)) (b (ordinal n2)))
            (setf (aref v a b) vert (aref v b a) vert)
            (when horz-p (setf (aref h a b) horz (aref h b a) horz))))
        (destructuring-bind (nt vert &optional (horz nil horz-p)) args
          (let ((a (ordinal nt)))
            (setf (aref v a a) vert)
            (when horz-p (setf (aref h a a) horz)))))))

(defjmethod get-horizontal-spacing ((this spacings) e1 e2)
  (cond ((and (typep e1 'node-type) (typep e2 'node-type))
         (%spacings-local-spacing-types this e1 e2 (.node-type-spacing-options-horizontal this)))
        ((and (typep e1 'l-node) (typep e2 'l-node))
         (%spacings-local-spacing-nodes this e1 e2 (.node-type-spacing-options-horizontal this)))
        (t (error 'unspecified-spacing-exception))))

(defjmethod get-vertical-spacing ((this spacings) n1 n2)
  (if (and (typep n1 'node-type) (typep n2 'node-type))
      (%spacings-local-spacing-types this n1 n2 (.node-type-spacing-options-vertical this))
      (%spacings-local-spacing-nodes this n1 n2 (.node-type-spacing-options-vertical this))))

(defun %spacings-local-spacing-nodes (this n1 n2 mapping)
  (declare (ignore this))
  (let* ((layout-option (aref mapping (ordinal (get-type n1)) (ordinal (get-type n2))))
         (s1 (spacings.get-individual-or-default n1 layout-option))
         (s2 (spacings.get-individual-or-default n2 layout-option)))
    (math.max s1 s2)))

(defun %spacings-local-spacing-types (this nt1 nt2 mapping)
  (get-property (.graph this) (aref mapping (ordinal nt1) (ordinal nt2))))

(defjstatic spacings.get-individual-or-default (node property)
  (let ((result nil))
    (when (has-property node layered-options.spacing-individual)
      (let ((individual-spacings (get-property node layered-options.spacing-individual)))
        (when (has-property individual-spacings property)
          (setf result (get-property individual-spacings property)))))
    (when (null result)
      (setf result (get-property (get-graph node) property)))
    result))

(defun new-unspecified-spacing-exception/0 () (make-condition 'unspecified-spacing-exception))
(defun new-unspecified-spacing-exception/1 (msg)
  (make-condition 'unspecified-spacing-exception :message msg))
(define-jcall new-unspecified-spacing-exception :static)

;;; ------------------------------------------------------------ LayeredSpacings

(defjstatic layered-spacings.with-base-value (base-spacing)
  (new-layered-spacings-builder base-spacing))

(defjconst layered-spacings-builder.base-spacing-option layered-options.spacing-node-node)

(defjconst layered-spacings-builder.dependent-spacing-options
  (jlist layered-options.spacing-component-component
         layered-options.spacing-edge-edge
         layered-options.spacing-edge-label
         layered-options.spacing-edge-node
         layered-options.spacing-label-label
         layered-options.spacing-label-node
         layered-options.spacing-label-port-horizontal
         layered-options.spacing-label-port-vertical
         layered-options.spacing-node-self-loop
         layered-options.spacing-port-port
         layered-options.spacing-edge-edge-between-layers
         layered-options.spacing-edge-node-between-layers
         layered-options.spacing-node-node-between-layers))

(defjctor layered-spacings-builder (d)
  (super-ctor abstract-spacings-builder this d))

(defjmethod this-t ((this layered-spacings-builder)) this)

(defjmethod get-base-spacing-option ((this layered-spacings-builder))
  layered-spacings-builder.base-spacing-option)

(defjmethod get-dependend-spacing-options ((this layered-spacings-builder))
  layered-spacings-builder.dependent-spacing-options)

(defun %layered-option-targets-contain (option-data target)
  "TARGET is a keyword (:nodes ...).  Works whether the targets are stored as
keywords (metadata.lisp) or as LayoutOptionData.Target constants."
  (let ((targets (get-targets option-data)))
    (flet ((match (x) (or (eq x target)
                          (and (typep x 'jenum) (string-equal (enum-name x) (symbol-name target))))))
      (if (listp targets)
          (some #'match targets)
          (let ((found nil)) (jdo (x targets) (when (match x) (setf found t))) found)))))

;; LayeredSpacings.OPTION_TARGET_FILTER: a BiFunction (holder, property) -> boolean.
(defjconst layered-spacings.option-target-filter
  (lambda (e property)
    (let ((option-data (get-option-data (layout-meta-data-service.get-instance) (get-id property))))
      (if option-data
          (typecase e
            (l-graph (or (%layered-option-targets-contain option-data :nodes)
                         (%layered-option-targets-contain option-data :parents)))
            (l-node (%layered-option-targets-contain option-data :nodes))
            (l-edge (%layered-option-targets-contain option-data :edges))
            (l-port (%layered-option-targets-contain option-data :ports))
            (l-label (%layered-option-targets-contain option-data :labels))
            (t t))
          t))))
