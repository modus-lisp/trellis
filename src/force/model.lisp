;;;; org.eclipse.elk.alg.force.model

(in-package #:elk)

;;; ---------------------------------------------------------------- AbstractForceModel

(defjconst abstract-force-model.disp-bound-factor 16)

(defjmethod initialize ((this abstract-force-model) fgraph)
  (setf (.graph this) fgraph
        (.random this) (get-property fgraph force-internal-properties.random))
  ;; calculate the adjacency matrix for the graph
  (calc-adjacency fgraph)
  ;; calculate an upper bound for particle displacement
  (setf (.disp-bound this)
        (jd (math.max (+ (* (length (get-nodes fgraph)) abstract-force-model.disp-bound-factor)
                         (length (get-edges fgraph)))
                      (* abstract-force-model.disp-bound-factor abstract-force-model.disp-bound-factor))))
  ;; if interactive mode is off, randomize the layout
  (unless (get-property fgraph force-options.interactive)
    (let ((pos-scale (jd (length (get-nodes (.graph this)))))
          (random (.random this)))
      (jdo (node (get-nodes fgraph))
        (let ((pos (get-position node)))
          (setf (.x pos) (* (next-double random) pos-scale))
          (setf (.y pos) (* (next-double random) pos-scale))))))
  ;; create bend points for node repulsion
  (let ((bends (get-bendpoints fgraph)))
    (jdo (edge (get-edges fgraph))
      (let ((count (get-property edge force-options.repulsive-power)))
        (when (> count 0)
          (dotimes (i count)
            (declare (ignorable i))
            (jl-add bends (new-f-bendpoint edge)))
          (distribute-bendpoints edge))))))

(defjmethod get-graph ((this abstract-force-model)) (.graph this))
(defjmethod get-random ((this abstract-force-model)) (.random this))

(defjgeneric more-iterations 1)
(defjgeneric calc-displacement 2)

(defjmethod layout ((this abstract-force-model) fgraph monitor)
  (begin monitor "Component Layout" 1)
  (initialize this fgraph)
  (let ((iterations 0)
        (disp-bound (.disp-bound this)))
    (loop while (and (more-iterations this iterations) (not (is-canceled monitor)))
          do (iteration-done this)
             (setf disp-bound (.disp-bound this))
             ;; calculate attractive and repulsive forces
             (let ((particles (get-particles fgraph)))
               (jdo (v particles)
                 (jdo (u particles)
                   (unless (eq u v)
                     (let ((displacement (calc-displacement this u v)))
                       (when displacement
                         (add (get-displacement v) displacement))))))
               ;; apply calculated displacement
               (jdo (v particles)
                 (let ((d (get-displacement v)))
                   (bound d (- disp-bound) (- disp-bound) disp-bound disp-bound)
                   (add (get-position v) d)
                   (reset d))))
             (incf iterations)))
  (done monitor))

(defjmethod iteration-done ((this abstract-force-model))
  (jdo (edge (get-edges (.graph this)))
    ;; adjust label positions
    (jdo (label (get-labels edge))
      (refresh-position label))
    ;; adjust bend point positions
    (distribute-bendpoints edge)))

(defjstatic abstract-force-model.avoid-same-position (random u v)
  (let ((pu (get-position u))
        (pv (get-position v)))
    (loop while (and (= (- (.x pu) (.x pv)) 0) (= (- (.y pu) (.y pv)) 0))
          do (let ((tried-for-bend-points nil))
               (if (and (typep u 'f-bendpoint) (typep v 'f-bendpoint) (not tried-for-bend-points))
                   ;; Wiggle orthogonal to edge direction
                   (let* ((u-e (get-edge u))
                          (u-vector (sub (new-k-vector (get-target-point u-e)) (get-source-point u-e)))
                          (length 2.0)
                          (orthogonalu-v (new-k-vector (* (/ (.x u-vector) (j-length u-vector)) length)
                                                       (* (/ (- (.y u-vector)) (j-length u-vector)) length))))
                     (add pu orthogonalu-v)
                     (let* ((v-e (get-edge v))
                            (v-vector (sub (new-k-vector (get-target-point v-e)) (get-source-point v-e))))
                       (setf length (if (eq u-vector v-vector) -2.0 2.0))
                       (let ((orthogonalv-v (new-k-vector (* (/ (.x v-vector) (j-length v-vector)) length)
                                                          (* (- (/ (.y v-vector) (j-length v-vector))) length))))
                         (add pu orthogonalv-v)
                         (setf tried-for-bend-points t))))
                   (progn
                     (wiggle pu random 1.0)
                     (wiggle pv random 1.0)))))))

;;; ---------------------------------------------------------------- EadesModel

(defjconst eades-model.zero-factor 100.0)

(defjmethod initialize ((this eades-model) graph)
  (call-next-method)
  (setf (.max-iterations this) (get-property graph force-options.iterations)
        (.spring-length this) (jd (get-property graph force-options.spacing-node-node))
        (.repulsion-factor this) (jd (get-property graph force-options.repulsion))))

(defjmethod more-iterations ((this eades-model) count)
  (< count (.max-iterations this)))

(defjmethod calc-displacement ((this eades-model) forcer forcee)
  (abstract-force-model.avoid-same-position (get-random this) forcer forcee)
  ;; compute distance (z in the original algorithm)
  (let* ((displacement (sub (clone (get-position forcee)) (get-position forcer)))
         (length (j-length displacement))
         (d (math.max 0.0 (- length (get-radius forcer) (get-radius forcee))))
         (connection (get-connection (get-graph this) forcer forcee))
         (force (if (> connection 0)
                    (* (- (eades-model.attractive d (.spring-length this))) connection)
                    (* (eades-model.repulsive d (.repulsion-factor this))
                       (get-property forcer force-options.priority)))))
    ;; scale distance vector to the amount of repulsive forces
    (scale displacement (/ force length))
    displacement))

(defjstatic eades-model.repulsive (d r)
  (if (> d 0) (/ r (* d d)) (* r eades-model.zero-factor)))

(defjstatic eades-model.attractive (d s)
  (if (> d 0) (math.log (/ d s)) (- eades-model.zero-factor)))

;;; ---------------------------------------------------------------- FruchtermanReingoldModel

(defjconst fruchterman-reingold-model.spacing-factor 0.01)
(defjconst fruchterman-reingold-model.zero-factor 100.0)

(defjmethod initialize ((this fruchterman-reingold-model) graph)
  (call-next-method)
  (setf (.temperature this) (jd (get-property graph force-options.temperature)))
  (setf (.threshold this) (/ (.temperature this) (get-property graph force-options.iterations)))
  ;; calculate an appropriate value for K
  (let ((n (length (get-nodes graph)))
        (total-width 0.0)
        (total-height 0.0))
    (jdo (v (get-nodes graph))
      (incf total-width (.x (get-size v)))
      (incf total-height (.y (get-size v))))
    (let ((area (* total-width total-height))
          (c (* (get-property graph force-options.spacing-node-node)
                fruchterman-reingold-model.spacing-factor)))
      (setf (.k this) (* (math.sqrt (/ area (jd (* 2 n)))) c)))))

(defjmethod more-iterations ((this fruchterman-reingold-model) count)
  (declare (ignore count))
  (> (.temperature this) 0))

(defjmethod calc-displacement ((this fruchterman-reingold-model) forcer forcee)
  (abstract-force-model.avoid-same-position (get-random this) forcer forcee)
  (let* ((displacement (sub (clone (get-position forcee)) (get-position forcer)))
         (length (j-length displacement))
         (d (math.max 0.0 (- length (get-radius forcer) (get-radius forcee))))
         (k (.k this))
         ;; calculate repulsive force, independent of adjacency
         (force (* (fruchterman-reingold-model.repulsive d k)
                   (get-property forcer force-options.priority)))
         (connection (get-connection (get-graph this) forcer forcee)))
    ;; calculate attractive force, depending of adjacency
    (when (> connection 0)
      (decf force (* (fruchterman-reingold-model.attractive d k) connection)))
    ;; scale distance vector to the amount of repulsive forces
    (scale displacement (/ (* force (.temperature this)) length))
    displacement))

(defjmethod iteration-done ((this fruchterman-reingold-model))
  (call-next-method)
  (decf (.temperature this) (.threshold this)))

(defjstatic fruchterman-reingold-model.repulsive (d k)
  (if (> d 0) (/ (* k k) d) (* k k fruchterman-reingold-model.zero-factor)))

(defjstatic fruchterman-reingold-model.attractive (d k)
  (/ (* d d) k))

;;; ForceModelStrategy has no methods.
