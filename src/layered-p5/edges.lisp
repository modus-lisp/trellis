;;;; edges.lisp -- org.eclipse.elk.alg.layered.p5edges: EdgeRouterFactory,
;;;; OrthogonalEdgeRouter, PolylineEdgeRouter

(in-package #:elk)

;;; ======================================================================
;;; Processor configurations shared by the routers.  Java keeps them in
;;; static finals; they are built on demand here (addAll copies them anyway)
;;; so that loading this file does not need the configuration class.
;;; ======================================================================

(defun %p5-config (&rest specs)
  "SPECS: (:before PHASE PROC) / (:after PHASE PROC)."
  (let ((c (layout-processor-configuration.create)))
    (dolist (s specs c)
      (destructuring-bind (kind phase proc) s
        (ecase kind
          (:before (add-before c phase proc))
          (:after (add-after c phase proc)))))))

(defun %p5-self-loop-additions ()
  (%p5-config (list :before layered-phases.p1-cycle-breaking layered-intermediate-processor-strategy.self-loop-preprocessor)
              (list :after layered-phases.p5-edge-routing layered-intermediate-processor-strategy.self-loop-postprocessor)
              (list :before layered-phases.p4-node-placement layered-intermediate-processor-strategy.self-loop-port-restorer)
              (list :before layered-phases.p4-node-placement layered-intermediate-processor-strategy.self-loop-router)))

(defun %p5-center-label-additions ()
  (%p5-config (list :before layered-phases.p2-layering layered-intermediate-processor-strategy.label-dummy-inserter)
              (list :before layered-phases.p4-node-placement layered-intermediate-processor-strategy.label-dummy-switcher)
              (list :before layered-phases.p4-node-placement layered-intermediate-processor-strategy.label-side-selector)
              (list :after layered-phases.p5-edge-routing layered-intermediate-processor-strategy.label-dummy-remover)))

(defun %p5-end-label-additions ()
  (%p5-config (list :before layered-phases.p4-node-placement layered-intermediate-processor-strategy.label-side-selector)
              (list :before layered-phases.p4-node-placement layered-intermediate-processor-strategy.end-label-preprocessor)
              (list :after layered-phases.p5-edge-routing layered-intermediate-processor-strategy.end-label-postprocessor)))

(defun %p5-inverted-port-additions ()
  (%p5-config (list :before layered-phases.p3-node-ordering layered-intermediate-processor-strategy.inverted-port-processor)))

(defun %p5-north-south-additions ()
  (%p5-config (list :before layered-phases.p3-node-ordering layered-intermediate-processor-strategy.north-south-port-preprocessor)
              (list :after layered-phases.p5-edge-routing layered-intermediate-processor-strategy.north-south-port-postprocessor)))

;;; ======================================================================
;;; EdgeRouterFactory
;;; ======================================================================

(defvar *p5-factory-cache* (new-jmap))

(defjstatic edge-router-factory.factory-for (edge-routing-strategy)
  (unless (jm-contains-key *p5-factory-cache* edge-routing-strategy)
    (let ((factory (new-edge-router-factory)))
      (setf (.edge-routing-strategy factory) edge-routing-strategy)
      (jm-put *p5-factory-cache* edge-routing-strategy factory)))
  (jm-get *p5-factory-cache* edge-routing-strategy))

(defjmethod create ((this edge-router-factory))
  (enum-case (.edge-routing-strategy this)
    (edge-routing.polyline (new-polyline-edge-router))
    (edge-routing.splines (new-spline-edge-router))
    (t (new-orthogonal-edge-router))))

;;; ======================================================================
;;; PolylineEdgeRouter
;;; ======================================================================

(defjconst polyline-edge-router.pred-external-west-or-east-port
  (lambda (node)
    (let ((ext-port-side (get-property node layered-internal-properties.ext-port-side)))
      (and (eq (get-type node) node-type.external-port)
           (or (eq ext-port-side port-side.west) (eq ext-port-side port-side.east))))))

(defun %p5-all-external-p (nodes)
  "Iterables.all(nodes, PRED_EXTERNAL_WEST_OR_EAST_PORT)"
  (block nil
    (jdo (n nodes) (unless (funcall polyline-edge-router.pred-external-west-or-east-port n) (return nil)))
    t))

(defconstant +p5-min-vert-diff+ 1d0)
(defconstant +p5-layer-space-fac+ 0.4d0)

(defjmethod get-layout-processor-configuration ((this polyline-edge-router) graph)
  (let ((graph-properties (get-property graph layered-internal-properties.graph-properties))
        (configuration (layout-processor-configuration.create-from (%p5-inverted-port-additions))))
    (when (contains graph-properties layered-graph-properties.north-south-ports)
      (add-all configuration (%p5-north-south-additions)))
    (when (contains graph-properties layered-graph-properties.self-loops)
      (add-all configuration (%p5-self-loop-additions)))
    (when (contains graph-properties layered-graph-properties.center-labels)
      (add-all configuration (%p5-center-label-additions)))
    (when (contains graph-properties layered-graph-properties.end-labels)
      (add-all configuration (%p5-end-label-additions)))
    configuration))

(defjmethod process ((this polyline-edge-router) layered-graph monitor)
  (begin monitor "Polyline edge routing" 1)
  (let* ((sloped-edge-zone-width (get-property layered-graph layered-options.edge-routing-polyline-sloped-edge-zone-width))
         (node-spacing (get-property layered-graph layered-options.spacing-node-node-between-layers))
         (edge-spacing (get-property layered-graph layered-options.spacing-edge-edge-between-layers))
         (edge-space-fac (math.min 1d0 (/ edge-spacing node-spacing)))
         (xpos 0d0)
         (layer-spacing 0d0)
         (layers (get-layers layered-graph)))
    (unless (zerop (length layers))
      (let ((y-diff (calculate-west-in-layer-edge-y-diff this (jl-get layers 0))))
        (setf xpos (* +p5-layer-space-fac+ edge-space-fac y-diff))))
    (loop for li from 0 below (length layers)
          for layer = (aref layers li)
          do (let ((external-layer (%p5-all-external-p (get-nodes layer))))
               (when (and external-layer (> xpos 0))
                 (decf xpos node-spacing))
               (l-graph-util.place-nodes-horizontally layer xpos)
               (let ((max-vert-diff 0d0))
                 (jdo (node (get-nodes layer))
                   (let ((max-curr-output-y-diff 0d0))
                     (jdo (outgoing-edge (get-outgoing-edges node))
                       (let ((source-pos (.y (get-absolute-anchor (get-source outgoing-edge))))
                             (target-pos (.y (get-absolute-anchor (get-target outgoing-edge)))))
                         (when (and (eq layer (get-layer (get-node (get-target outgoing-edge))))
                                    (not (is-self-loop outgoing-edge)))
                           (process-in-layer-edge this outgoing-edge xpos
                                                  (* +p5-layer-space-fac+ edge-space-fac
                                                     (abs (- source-pos target-pos))))
                           (when (eq (get-side (get-source outgoing-edge)) port-side.west)
                             (setf source-pos 0d0 target-pos 0d0)))
                         (setf max-curr-output-y-diff
                               (math.max max-curr-output-y-diff (abs (- target-pos source-pos))))))
                     (enum-case (get-type node)
                       ((node-type.normal node-type.label node-type.long-edge
                         node-type.north-south-port node-type.breaking-point)
                        (process-node this node xpos sloped-edge-zone-width)))
                     (setf max-vert-diff (math.max max-vert-diff max-curr-output-y-diff))))
                 (when (< (1+ li) (length layers))
                   (let ((y-diff (calculate-west-in-layer-edge-y-diff this (aref layers (1+ li)))))
                     (setf max-vert-diff (math.max max-vert-diff y-diff))))
                 (setf layer-spacing (* +p5-layer-space-fac+ edge-space-fac max-vert-diff))
                 (when (and (not external-layer) (< (1+ li) (length layers)))
                   (incf layer-spacing node-spacing))
                 (incf xpos (+ (.x (get-size layer)) layer-spacing)))))
    (jl-clear (.created-junction-points this))
    (setf (.x (get-size layered-graph)) xpos)
    (done monitor)))

(defjmethod process-node ((this polyline-edge-router) node layer-left-x-pos max-acceptable-x-diff)
  (let ((layer-right-x-pos (+ layer-left-x-pos (.x (get-size (get-layer node))))))
    (jdo (port (get-ports node))
      (block continue
        (let ((absolute-port-anchor (get-absolute-anchor port)))
          (when (eq (get-type node) node-type.north-south-port)
            ;; see #515: use the x coordinate of the original north/south port
            (let ((corresponding-port (get-property port layered-internal-properties.origin)))
              (setf (.x absolute-port-anchor) (.x (get-absolute-anchor corresponding-port)))
              (setf (.x (get-position node)) (.x absolute-port-anchor))))
          (let ((bend-point (new-k-vector 0 (.y absolute-port-anchor))))
            (cond ((eq (get-side port) port-side.east) (setf (.x bend-point) layer-right-x-pos))
                  ((eq (get-side port) port-side.west) (setf (.x bend-point) layer-left-x-pos))
                  (t (return-from continue)))
            (let ((x-distance (abs (- (.x absolute-port-anchor) (.x bend-point)))))
              (when (and (<= x-distance max-acceptable-x-diff) (not (is-in-layer-dummy this node)))
                (return-from continue)))
            (let ((add-junction-point (> (+ (size (get-outgoing-edges port)) (size (get-incoming-edges port))) 1)))
              (jdo (e (get-connected-edges port))
                (let ((other-port (if (eq (get-source e) port) (get-target e) (get-source e))))
                  (when (> (abs (- (.y (get-absolute-anchor other-port)) (.y bend-point))) +p5-min-vert-diff+)
                    (add-bend-point this e bend-point add-junction-point port)))))))))))

(defjmethod process-in-layer-edge ((this polyline-edge-router) edge layer-x-pos edge-spacing)
  (let* ((source-port (get-source edge))
         (target-port (get-target edge))
         (source-anchor-y (.y (get-absolute-anchor source-port)))
         (mid-y (/ (+ source-anchor-y (.y (get-absolute-anchor target-port))) 2d0))
         (bend-point (if (eq (get-side source-port) port-side.east)
                         (new-k-vector (+ layer-x-pos (.x (get-size (get-layer (get-node source-port)))) edge-spacing)
                                       mid-y)
                         (new-k-vector (- layer-x-pos edge-spacing) mid-y))))
    (add (get-bend-points edge) 0 bend-point)))

(defjmethod calculate-west-in-layer-edge-y-diff ((this polyline-edge-router) layer)
  (let ((max-y-diff 0d0))
    (jdo (node (get-nodes layer))
      (jdo (outgoing-edge (get-outgoing-edges node))
        (when (and (eq layer (get-layer (get-node (get-target outgoing-edge))))
                   (eq (get-side (get-source outgoing-edge)) port-side.west))
          (let ((source-pos (.y (get-absolute-anchor (get-source outgoing-edge))))
                (target-pos (.y (get-absolute-anchor (get-target outgoing-edge)))))
            (setf max-y-diff (math.max max-y-diff (abs (- target-pos source-pos))))))))
    max-y-diff))

(defjmethod add-bend-point ((this polyline-edge-router) edge bend-point add-junction-point curr-port)
  (when (and (or (is-in-layer-edge edge)
                 (not (equals (get-absolute-anchor curr-port) bend-point)))
             (not (is-self-loop edge)))
    (if (eq (get-source edge) curr-port)
        (add (get-bend-points edge) 0 (new-k-vector bend-point))
        (add (get-bend-points edge) (new-k-vector bend-point)))
    (when (and add-junction-point (not (jl-contains (.created-junction-points this) bend-point)))
      (let ((junction-points (get-property edge layered-options.junction-points)))
        (when (null junction-points)
          (setf junction-points (new-k-vector-chain))
          (set-property edge layered-options.junction-points junction-points))
        (let ((jpoint (new-k-vector bend-point)))
          (add junction-points jpoint)
          (jl-add (.created-junction-points this) jpoint))))))

(defjmethod is-in-layer-dummy ((this polyline-edge-router) node)
  (when (eq (get-type node) node-type.long-edge)
    (jdo (e (get-connected-edges node))
      (when (is-in-layer-edge e)
        (return-from is-in-layer-dummy t))))
  nil)

;;; ======================================================================
;;; OrthogonalEdgeRouter
;;; ======================================================================

(defun %p5-hierarchical-port-additions ()
  (%p5-config (list :before layered-phases.p3-node-ordering layered-intermediate-processor-strategy.hierarchical-port-constraint-processor)
              (list :before layered-phases.p4-node-placement layered-intermediate-processor-strategy.hierarchical-port-dummy-size-processor)
              (list :after layered-phases.p5-edge-routing layered-intermediate-processor-strategy.hierarchical-port-orthogonal-edge-router)))

(defjmethod get-layout-processor-configuration ((this orthogonal-edge-router) graph)
  (let ((graph-properties (get-property graph layered-internal-properties.graph-properties))
        (configuration (layout-processor-configuration.create)))
    (when (contains graph-properties layered-graph-properties.hyperedges)
      (add-all configuration (%p5-config (list :before layered-phases.p4-node-placement
                                               layered-intermediate-processor-strategy.hyperedge-dummy-merger)))
      (add-all configuration (%p5-inverted-port-additions)))
    (when (or (contains graph-properties layered-graph-properties.non-free-ports)
              (get-property graph layered-options.feedback-edges))
      (add-all configuration (%p5-inverted-port-additions))
      (when (contains graph-properties layered-graph-properties.north-south-ports)
        (add-all configuration (%p5-north-south-additions))))
    (when (contains graph-properties layered-graph-properties.external-ports)
      (add-all configuration (%p5-hierarchical-port-additions)))
    (when (contains graph-properties layered-graph-properties.self-loops)
      (add-all configuration (%p5-self-loop-additions)))
    (when (contains graph-properties layered-graph-properties.hypernodes)
      (add-all configuration (%p5-config (list :after layered-phases.p5-edge-routing
                                               layered-intermediate-processor-strategy.hypernode-processor))))
    (when (contains graph-properties layered-graph-properties.center-labels)
      (add-all configuration (%p5-center-label-additions)))
    (when (contains graph-properties layered-graph-properties.end-labels)
      (add-all configuration (%p5-end-label-additions)))
    configuration))

(defjmethod process ((this orthogonal-edge-router) layered-graph monitor)
  (begin monitor "Orthogonal edge routing" 1)
  (let* ((node-node-spacing (jd (get-property layered-graph layered-options.spacing-node-node-between-layers)))
         (edge-edge-spacing (jd (get-property layered-graph layered-options.spacing-edge-edge-between-layers)))
         (edge-node-spacing (jd (get-property layered-graph layered-options.spacing-edge-node-between-layers)))
         (routing-generator (new-orthogonal-routing-generator routing-direction.west-to-east edge-edge-spacing "phase5"))
         ;; Java declares xpos as a float: every += narrows to single precision
         (xpos 0d0)
         (layers (get-layers layered-graph))
         (cursor 0)
         (left-layer nil) (right-layer nil)
         (left-layer-nodes nil) (right-layer-nodes nil)
         (left-layer-index -1) (right-layer-index -1))
    (loop
      (let ((slots-count 0))
        (if (< cursor (length layers))
            (progn (setf right-layer (aref layers cursor)) (incf cursor))
            (setf right-layer nil))
        (setf right-layer-nodes (and right-layer (get-nodes right-layer)))
        (setf right-layer-index (1- cursor))
        (when left-layer
          (l-graph-util.place-nodes-horizontally left-layer xpos)
          (setf xpos (%p5-float (+ xpos (.x (get-size left-layer))))))
        (let ((start-pos (if (null left-layer) xpos (+ xpos edge-node-spacing))))
          (setf slots-count (route-edges routing-generator monitor layered-graph left-layer-nodes
                                         left-layer-index right-layer-nodes start-pos)))
        (let ((is-left-layer-external (or (null left-layer) (%p5-all-external-p left-layer-nodes)))
              (is-right-layer-external (or (null right-layer) (%p5-all-external-p right-layer-nodes))))
          (cond ((> slots-count 0)
                 (let ((routing-width (* (- slots-count 1) edge-edge-spacing)))
                   (when left-layer (incf routing-width edge-node-spacing))
                   (when right-layer (incf routing-width edge-node-spacing))
                   (when (and (< routing-width node-node-spacing)
                              (not is-left-layer-external) (not is-right-layer-external))
                     (setf routing-width node-node-spacing))
                   (setf xpos (%p5-float (+ xpos routing-width)))))
                ((and (not is-left-layer-external) (not is-right-layer-external))
                 (setf xpos (%p5-float (+ xpos node-node-spacing))))))
        (setf left-layer right-layer
              left-layer-nodes right-layer-nodes
              left-layer-index right-layer-index)
        (when (null right-layer) (return))))
    (setf (.x (get-size layered-graph)) xpos)
    (done monitor)))
