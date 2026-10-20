;;;; layer-constraints.lisp -- LayerConstraintPostprocessor, LayerConstraintPreprocessor
;;;; (with its private enum HiddenNodeConnections), LayerSizeAndGraphHeightCalculator.

(in-package #:elk)

;;; ------------------------------------------------ LayerConstraintPostprocessor

(defjmethod process ((this layer-constraint-postprocessor) layered-graph monitor)
  (begin monitor "Layer constraint postprocessing" 1)
  (let ((layers (get-layers layered-graph)))
    (unless (jl-is-empty layers)
      (let ((first-layer (jl-get layers 0))
            (last-layer (jl-last layers))
            (first-label-layer (new-layer layered-graph))
            (last-label-layer (new-layer layered-graph)))
        (move-first-and-last-nodes this layered-graph first-layer last-layer first-label-layer last-label-layer)
        (unless (jl-is-empty (get-nodes first-label-layer))
          (jl-add-at layers 0 first-label-layer))
        (unless (jl-is-empty (get-nodes last-label-layer))
          (jl-add layers last-label-layer))))
    ;; Restore hidden FIRST_SEPARATE and LAST_SEPARATE nodes, if any
    (when (has-property layered-graph layered-internal-properties.hidden-nodes)
      (let ((first-separate-layer (new-layer layered-graph))
            (last-separate-layer (new-layer layered-graph)))
        (restore-hidden-nodes this layered-graph first-separate-layer last-separate-layer)
        (unless (jl-is-empty (get-nodes first-separate-layer))
          (jl-add-at layers 0 first-separate-layer))
        (unless (jl-is-empty (get-nodes last-separate-layer))
          (jl-add layers last-separate-layer)))))
  (done monitor))

(defjmethod move-first-and-last-nodes ((this layer-constraint-postprocessor) layered-graph first-layer last-layer
                                       first-label-layer last-label-layer)
  (jdo (layer (jlist-from (get-layers layered-graph)))
    (jdo (node (l-graph-util.to-node-array (get-nodes layer)))
      (enum-case (get-property node layered-options.layering-layer-constraint)
        (layer-constraint.first
         (throw-up-unless-no-incoming-edges this node)
         (set-layer node first-layer)
         (move-labels-to-label-layer this node t first-label-layer))
        (layer-constraint.last
         (throw-up-unless-no-outgoing-edges this node)
         (set-layer node last-layer)
         (move-labels-to-label-layer this node nil last-label-layer)))))
  ;; remove the empty layers
  (jl-remove-if (get-layers layered-graph) (lambda (layer) (jl-is-empty (get-nodes layer)))))

(defjmethod move-labels-to-label-layer ((this layer-constraint-postprocessor) node incoming label-layer)
  (jdo (edge (jlist-from (if incoming (get-incoming-edges node) (get-outgoing-edges node))))
    (let ((possible-label-dummy (if incoming (get-node (get-source edge)) (get-node (get-target edge)))))
      (when (eq (get-type possible-label-dummy) node-type.label)
        (set-layer possible-label-dummy label-layer)))))

(defjmethod restore-hidden-nodes ((this layer-constraint-postprocessor) layered-graph first-separate-layer last-separate-layer)
  (jdo (hidden-node (get-property layered-graph layered-internal-properties.hidden-nodes))
    (enum-case (get-property hidden-node layered-options.layering-layer-constraint)
      (layer-constraint.first-separate (set-layer hidden-node first-separate-layer))
      (layer-constraint.last-separate (set-layer hidden-node last-separate-layer)))
    ;; Restore the node's edges
    (jdo (hidden-edge (jlist-from (get-connected-edges hidden-node)))
      ;; an edge connecting two hidden nodes may already be restored
      (unless (and (get-source hidden-edge) (get-target hidden-edge))
        (let ((is-outgoing (null (get-target hidden-edge)))
              (original-opposite-port (get-property hidden-edge layered-internal-properties.original-opposite-port)))
          (if is-outgoing
              (set-target hidden-edge original-opposite-port)
              (set-source hidden-edge original-opposite-port)))))))

(defjmethod throw-up-unless-no-incoming-edges ((this layer-constraint-postprocessor) node)
  (jdo (incoming (get-incoming-edges node))
    (unless (eq (get-type (get-node (get-source incoming))) node-type.label)
      (jthrow 'unsupported-configuration-exception
              "Node '~A' has its layer constraint set to FIRST, but has at least one incoming edge that  does not come from a FIRST_SEPARATE node. That must not happen."
              (get-designation node)))))

(defjmethod throw-up-unless-no-outgoing-edges ((this layer-constraint-postprocessor) node)
  (jdo (outgoing (get-outgoing-edges node))
    (unless (eq (get-type (get-node (get-target outgoing))) node-type.label)
      (jthrow 'unsupported-configuration-exception
              "Node '~A' has its layer constraint set to LAST, but has at least one outgoing edge that  does not go to a LAST_SEPARATE node. That must not happen."
              (get-designation node)))))

;;; ------------------------------------------------ LayerConstraintPreprocessor

(defjmethod process ((this layer-constraint-preprocessor) layered-graph monitor)
  (begin monitor "Layer constraint preprocessing" 1)
  (let ((hidden-nodes (new-jlist))
        (nodes (get-layerless-nodes layered-graph))
        (i 0))
    (loop while (< i (length nodes))
          do (let ((l-node (aref nodes i)))
               (if (is-relevant-node this l-node)
                   (progn
                     (hide this l-node)
                     (jl-add hidden-nodes l-node)
                     (jl-remove-at nodes i))
                   (incf i))))
    (unless (jl-is-empty hidden-nodes)
      (set-property layered-graph layered-internal-properties.hidden-nodes hidden-nodes)))
  (done monitor))

(defjmethod is-relevant-node ((this layer-constraint-preprocessor) l-node)
  (enum-case (get-property l-node layered-options.layering-layer-constraint)
    ((layer-constraint.first-separate layer-constraint.last-separate) t)
    (t nil)))

;;; Node Hiding -- hide(LNode) and hide(LNode, LEdge)

(defjmethod hide ((this layer-constraint-preprocessor) l-node)
  (ensure-no-inacceptable-edges this l-node)
  (jdo (l-edge (jlist-from (get-connected-edges l-node)))
    (hide this l-node l-edge)))

(defjmethod hide ((this layer-constraint-preprocessor) l-node l-edge)
  (let* ((is-outgoing (eq (get-node (get-source l-edge)) l-node))
         (opposite-port (if is-outgoing (get-target l-edge) (get-source l-edge))))
    (if is-outgoing
        (set-target l-edge nil)
        (set-source l-edge nil))
    (set-property l-edge layered-internal-properties.original-opposite-port opposite-port)
    (update-opposite-node-layer-constraints this l-node (get-node opposite-port))))

(defjmethod combine ((this hidden-node-connections) layer-constraint)
  (enum-case this
    (hidden-node-connections.none
     (if (eq layer-constraint layer-constraint.first-separate)
         hidden-node-connections.first-separate
         hidden-node-connections.last-separate))
    (hidden-node-connections.first-separate
     (if (eq layer-constraint layer-constraint.first-separate)
         hidden-node-connections.first-separate
         hidden-node-connections.both))
    (hidden-node-connections.last-separate
     (if (eq layer-constraint layer-constraint.first-separate)
         hidden-node-connections.both
         hidden-node-connections.last-separate))
    (t hidden-node-connections.both)))

(defproperty layer-constraint-preprocessor.hidden-node-connections "separateLayerConnections"
  hidden-node-connections.none)

(defjmethod update-opposite-node-layer-constraints ((this layer-constraint-preprocessor) hidden-node opposite-node)
  ;; If the node already has a layer constraint, don't bother
  (when (has-property opposite-node layered-options.layering-layer-constraint)
    (return-from update-opposite-node-layer-constraints/2 nil))
  (let ((connections (combine (get-property opposite-node layer-constraint-preprocessor.hidden-node-connections)
                              (get-property hidden-node layered-options.layering-layer-constraint))))
    (set-property opposite-node layer-constraint-preprocessor.hidden-node-connections connections)
    ;; If the node is still connected to things, don't bother
    (when (plusp (length (to-jvector (get-connected-edges opposite-node))))
      (return-from update-opposite-node-layer-constraints/2 nil))
    (enum-case connections
      (hidden-node-connections.first-separate
       (set-property opposite-node layered-options.layering-layer-constraint layer-constraint.first))
      (hidden-node-connections.last-separate
       (set-property opposite-node layered-options.layering-layer-constraint layer-constraint.last)))))

(defjmethod ensure-no-inacceptable-edges ((this layer-constraint-preprocessor) l-node)
  (let ((layer-constraint (get-property l-node layered-options.layering-layer-constraint)))
    (cond
      ((eq layer-constraint layer-constraint.first-separate)
       (jdo (in-edge (get-incoming-edges l-node))
         (unless (is-acceptable-incident-edge this in-edge)
           (jthrow 'unsupported-configuration-exception
                   "Node '~A' has its layer constraint set to FIRST_SEPARATE, but has at least one incoming edge. FIRST_SEPARATE nodes must not have incoming edges."
                   (get-designation l-node)))))
      ((eq layer-constraint layer-constraint.last-separate)
       (jdo (out-edge (get-outgoing-edges l-node))
         (unless (is-acceptable-incident-edge this out-edge)
           (jthrow 'unsupported-configuration-exception
                   "Node '~A' has its layer constraint set to LAST_SEPARATE, but has at least one outgoing edge. LAST_SEPARATE nodes must not have outgoing edges."
                   (get-designation l-node))))))))

(defjmethod is-acceptable-incident-edge ((this layer-constraint-preprocessor) edge)
  (and (eq (get-type (get-node (get-source edge))) node-type.external-port)
       (eq (get-type (get-node (get-target edge))) node-type.external-port)))

;;; ------------------------------------------------ LayerSizeAndGraphHeightCalculator

(defjmethod process ((this layer-size-and-graph-height-calculator) layered-graph monitor)
  (begin monitor "Layer size calculation" 1)
  (let ((min-y double.positive-infinity)
        (max-y double.negative-infinity)
        (found-nodes nil))
    (jdo (layer (get-layers layered-graph))
      (let ((layer-size (get-size layer)))
        (setf (.x layer-size) 0.0 (.y layer-size) 0.0)
        (unless (jl-is-empty (get-nodes layer))
          (setf found-nodes t)
          ;; width
          (jdo (node (get-nodes layer))
            (let ((node-size (get-size node)) (node-margin (get-margin node)))
              (setf (.x layer-size) (math.max (.x layer-size)
                                              (+ (.x node-size) (.left node-margin) (.right node-margin))))))
          ;; height
          (let* ((first-node (jl-get (get-nodes layer) 0))
                 (top (- (.y (get-position first-node)) (.top (get-margin first-node))))
                 (last-node (jl-last (get-nodes layer)))
                 (bottom (+ (.y (get-position last-node)) (.y (get-size last-node)) (.bottom (get-margin last-node)))))
            (when (eq (get-type first-node) node-type.external-port)
              (decf top (get-top (get-property layered-graph layered-options.spacing-ports-surrounding))))
            (when (eq (get-type last-node) node-type.external-port)
              (incf bottom (get-bottom (get-property layered-graph layered-options.spacing-ports-surrounding))))
            (setf (.y layer-size) (- bottom top))
            (setf min-y (math.min min-y top)
                  max-y (math.max max-y bottom))))))
    (unless found-nodes
      (setf min-y 0.0 max-y 0.0))
    (setf (.y (get-size layered-graph)) (- max-y min-y))
    (decf (.y (get-offset layered-graph)) min-y))
  (done monitor))
