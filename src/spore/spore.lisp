;;;; spore/spore.lisp -- org.eclipse.elk.alg.spore (+ graph, options, p1..p3)

(in-package #:elk)

;;; Java keys several HashMaps by KVector, whose equals() compares the
;;; coordinates.  We key an EQUAL table by the coordinate pair; adding 0.0
;;; folds -0.0 into 0.0 as Java's == does.
(defun %spore-vkey (v)
  (cons (+ (.x v) 0.0) (+ (.y v) 0.0)))

;;; Math.random() stand-in.
(defvar *spore-math-random* (new-random))

;;; ---------------------------------------------------------------- Graph

(defjctor spore-graph (cost-fun tree-strategy comp-strategy)
  (setf (.cost-function this) cost-fun
        (.tree-construction-strategy this) tree-strategy
        (.compaction-strategy this) comp-strategy))

;;; ---------------------------------------------------------------- IGraphImporter

(defjgeneric import-graph 1)
(defjgeneric apply-positions 1)
(defjgeneric update-graph 1)

;;; ---------------------------------------------------------------- ElkGraphImporter

(defun %spore-node-of (this v) (car (gethash (%spore-vkey v) (.node-map this))))

(defun %spore-cost-function (this id)
  (enum-case id
    (spanning-tree-cost-function.center-distance
     (lambda (e) (distance (.u e) (.v e))))
    (spanning-tree-cost-function.circle-underlap
     (lambda (e)
       (let ((n1 (%spore-node-of this (.u e)))
             (n2 (%spore-node-of this (.v e))))
         (- (distance (.u e) (.v e))
            (distance (.u e) (get-position (.rect n1)))
            (distance (.v e) (get-position (.rect n2)))))))
    (spanning-tree-cost-function.rectangle-underlap
     (lambda (e)
       (underlap (%spore-node-of this (.u e)) (%spore-node-of this (.v e)))))
    (spanning-tree-cost-function.inverted-overlap
     (lambda (e)
       (let* ((r1 (.rect (%spore-node-of this (.u e))))
              (r2 (.rect (%spore-node-of this (.v e))))
              (dist (elk-math.shortest-distance r1 r2)))
         (if (>= dist 0)
             dist
             (let ((s (j-length (sub (get-center r2) (get-center r1)))))
               (* (- (- (utils.overlap r1 r2) 1)) s))))))
    (spanning-tree-cost-function.minimum-root-distance
     (lambda (e)
       (let ((root (.vertex (.preferred-root (.graph this)))))
         (math.min (distance (.u e) root) (distance (.v e) root)))))
    (t (jthrow 'illegal-argument-exception "No implementation available for ~A" (name id)))))

(defjmethod import-graph ((this spore-elk-graph-importer) input-graph)
  (setf (.elk-graph this) input-graph
        (.node-map this) (new-jmap 'equal))
  (let ((elk-graph input-graph))
    (node-dimension-calculation.calculate-node-margins (elk-graph-adapters.adapt elk-graph))
    (let* ((preferred-root-id (get-property elk-graph spore-compaction-options.processing-order-preferred-root))
           (cost-function-id (get-property elk-graph spore-compaction-options.processing-order-spanning-tree-cost-function))
           (tree-strategy (get-property elk-graph spore-compaction-options.processing-order-tree-construction))
           (compaction-strategy (get-property elk-graph spore-compaction-options.compaction-compaction-strategy))
           (root-selection (get-property elk-graph spore-compaction-options.processing-order-root-selection))
           (cost-function nil)
           (graph nil))
      (setf (.spacing-node-node this) (get-property elk-graph spore-compaction-options.spacing-node-node))
      (setf cost-function (%spore-cost-function this cost-function-id))
      (setf graph (new-spore-graph cost-function tree-strategy compaction-strategy)
            (.graph this) graph)
      (set-property graph common-internal-properties.debug-svg
                    (get-property elk-graph spore-compaction-options.debug-mode))
      (setf (.orthogonal-compaction graph) (get-property elk-graph spore-compaction-options.compaction-orthogonal))
      (when (is-empty (get-children elk-graph))
        (return-from import-graph/1 graph))
      (let ((spacing (.spacing-node-node this)))
        (jdo (elk-node (get-children elk-graph))
          (let* ((half-width (/ (get-width elk-node) 2))
                 (half-height (/ (get-height elk-node) 2))
                 (vertex (new-k-vector (+ (get-x elk-node) half-width) (+ (get-y elk-node) half-height))))
            (loop while (jm-contains-key (.node-map this) (%spore-vkey vertex))
                  do (add vertex (* (- (next-double *spore-math-random*) 0.5) 0.001)
                                 (* (- (next-double *spore-math-random*) 0.5) 0.001)))
            (let* ((margin (get-property elk-node core-options.margins))
                   (node (new-node vertex
                                   (new-elk-rectangle
                                    (- (.x vertex) half-width (/ spacing 2) (.left margin))
                                    (- (.y vertex) half-height (/ spacing 2) (.top margin))
                                    (+ (get-width elk-node) spacing (get-horizontal margin))
                                    (+ (get-height elk-node) spacing (get-vertical margin))))))
              (jl-add (.vertices graph) node)
              (jm-put (.node-map this) (%spore-vkey vertex) (cons node elk-node))))))
      (enum-case root-selection
        (root-selection.fixed
         (if (null preferred-root-id)
             (setf (.preferred-root graph) (jl-get (.vertices graph) 0))
             (jdo (node (.vertices graph))
               (let ((id (get-identifier (cdr (gethash (%spore-vkey (.original-vertex node)) (.node-map this))))))
                 (when (and id (string= id preferred-root-id))
                   (setf (.preferred-root graph) node))))))
        (root-selection.center-node
         (let ((center (new-k-vector (get-width elk-graph) (get-height elk-graph)))
               (closest double.positive-infinity))
           (scale center 0.5)
           (add center (get-x elk-graph) (get-y elk-graph))
           (jdo (node (.vertices graph))
             (let ((d (distance (.original-vertex node) center)))
               (when (< d closest)
                 (setf closest d
                       (.preferred-root graph) node))))))
        (t (jthrow 'illegal-argument-exception "No implementation available for ~A" (name root-selection))))
      graph)))

(defjmethod update-graph ((this spore-elk-graph-importer) g)
  (let ((updated (new-jmap 'equal)))
    (setf (.t-edges g) nil
          (.tree g) nil)
    (jdo (n (.vertices g))
      (let ((original (gethash (%spore-vkey (.original-vertex n)) (.node-map this))))
        (setf (.original-vertex n) (get-center (.rect n)))
        (jm-put updated (%spore-vkey (.original-vertex n)) original)))
    (setf (.node-map this) updated)))

(defjmethod apply-positions ((this spore-elk-graph-importer) g)
  (let ((min-x double.positive-infinity) (min-y double.positive-infinity)
        (max-x double.negative-infinity) (max-y double.negative-infinity)
        (elk-graph (.elk-graph this)))
    (jdo (node (.vertices g))
      (let ((elk-node (cdr (gethash (%spore-vkey (.original-vertex node)) (.node-map this)))))
        (set-location elk-node (.x (.rect node)) (.y (.rect node)))
        (setf min-x (math.min min-x (get-x elk-node))
              min-y (math.min min-y (get-y elk-node))
              max-x (math.max max-x (+ (get-x elk-node) (get-width elk-node)))
              max-y (math.max max-y (+ (get-y elk-node) (get-height elk-node))))))
    (let ((padding (get-property elk-graph spore-compaction-options.padding)))
      (elk-util.resize-node elk-graph (+ (- max-x min-x) (get-horizontal padding))
                            (+ (- max-y min-y) (get-vertical padding)) t t)
      (elk-util.translate elk-graph (+ (- min-x) (.left padding)) (+ (- min-y) (.top padding))))
    (jdo (e (get-contained-edges elk-graph))
      (let* ((section (elk-graph-util.first-edge-section e t t))
             (source (elk-graph-util.get-source-node e))
             (target (elk-graph-util.get-target-node e))
             (start (new-k-vector (+ (get-x source) (/ (get-width source) 2))
                                  (+ (get-y source) (/ (get-height source) 2))))
             (end (new-k-vector (+ (get-x target) (/ (get-width target) 2))
                                (+ (get-y target) (/ (get-height target) 2))))
             (uv (sub (jclone end) start)))
        (elk-math.clip-vector uv (get-width source) (get-height source))
        (add start uv)
        (let ((vu (sub (jclone start) end)))
          (elk-math.clip-vector vu (get-width target) (get-height target))
          (add end vu))
        (set-start-location section (.x start) (.y start))
        (set-end-location section (.x end) (.y end))))))

;;; ---------------------------------------------------------------- ShrinkTree

(defjmethod shrink ((this shrink-tree) graph progress-monitor)
  (let ((asm (.algorithm-assembler this)))
    (reset asm)
    (set-phase asm sp-or-e-phases.p1-structure structure-extraction-strategy.delaunay-triangulation)
    (set-phase asm sp-or-e-phases.p2-processing-order (.tree-construction-strategy graph))
    (set-phase asm sp-or-e-phases.p3-execution (.compaction-strategy graph))
    (setf (.algorithm this) (build asm graph)))
  (begin progress-monitor "Compaction by shrinking a tree" (size (.algorithm this)))
  (when (> (length (.vertices graph)) 1)
    (jdo (processor (.algorithm this))
      (process processor graph (sub-task progress-monitor 1))))
  (done progress-monitor))

;;; ---------------------------------------------------------------- layout providers

(defun %spore-underlying-layout (layout-graph id-prop progress-monitor)
  (let* ((requested (get-property layout-graph id-prop))
         (lad (get-algorithm-data-by-suffix (layout-meta-data-service.get-instance) requested)))
    (when lad
      (layout (fetch (get-instance-pool lad)) layout-graph (sub-task progress-monitor 1)))))

(defjmethod layout ((this shrink-tree-layout-provider) layout-graph progress-monitor)
  (when (has-property layout-graph spore-compaction-options.underlying-layout-algorithm)
    (%spore-underlying-layout layout-graph spore-compaction-options.underlying-layout-algorithm
                              progress-monitor))
  (let* ((importer (new-spore-elk-graph-importer))
         (graph (import-graph importer layout-graph)))
    (shrink (.shrinktree this) graph (sub-task progress-monitor 1))
    (apply-positions importer graph)))

(defun %spore-add-t-edge (edges e)
  "HashSet<TEdge>.add -- TEdge equality is undirected (TEdge.equals)."
  (unless (find e edges :test #'jequals)
    (jl-add edges e)))

(defjmethod layout ((this overlap-removal-layout-provider) layout-graph progress-monitor)
  (when (has-property layout-graph spore-compaction-options.underlying-layout-algorithm)
    (%spore-underlying-layout layout-graph spore-overlap-removal-options.underlying-layout-algorithm
                              progress-monitor))
  (set-property layout-graph spore-compaction-options.processing-order-root-selection
                root-selection.center-node)
  (set-property layout-graph spore-compaction-options.processing-order-spanning-tree-cost-function
                spanning-tree-cost-function.inverted-overlap)
  (set-property layout-graph spore-compaction-options.processing-order-tree-construction
                tree-construction-strategy.minimum-spanning-tree)
  (let ((max-iterations (get-property layout-graph spore-overlap-removal-options.overlap-removal-max-iterations)))
    (begin progress-monitor "Overlap removal" 1)
    (let* ((svg (new-svg-image nil))     ; debug output is a no-op
           (overlap-edges (new-jlist))
           (overlap-handler (lambda (n1 n2)
                              (%spore-add-t-edge overlap-edges
                                                 (new-common-t-edge (.original-vertex n1)
                                                                    (.original-vertex n2)))))
           (importer (new-spore-elk-graph-importer))
           (graph (import-graph importer layout-graph))
           (overlaps-existed t)
           (iteration 0))
      (loop while (and (< iteration max-iterations) overlaps-existed)
            do (when (get-property layout-graph spore-overlap-removal-options.overlap-removal-run-scanline)
                 (jl-clear overlap-edges)
                 (sweep (new-scanline-overlap-check overlap-handler svg) (.vertices graph))
                 (when (jl-is-empty overlap-edges) (loop-finish))
                 (setf (.t-edges graph) overlap-edges))
               (let ((asm (.algorithm-assembler this)))
                 (reset asm)
                 (set-phase asm sp-or-e-phases.p1-structure structure-extraction-strategy.delaunay-triangulation)
                 (set-phase asm sp-or-e-phases.p2-processing-order (.tree-construction-strategy graph))
                 (set-phase asm sp-or-e-phases.p3-execution spore-overlap-removal-strategy.grow-tree)
                 (setf (.algorithm this) (build asm graph)))
               (jdo (processor (.algorithm this))
                 (process processor graph (sub-task progress-monitor 1)))
               (update-graph importer graph)
               (setf overlaps-existed (get-property graph common-internal-properties.overlaps-existed))
               (incf iteration))
      (apply-positions importer graph)
      (done progress-monitor))))

;;; ---------------------------------------------------------------- options (enum factories)

(defjmethod create ((this spore-overlap-removal-strategy))
  (new-grow-tree-phase))

(defjmethod create ((this tree-construction-strategy))
  (enum-case this
    (tree-construction-strategy.minimum-spanning-tree (new-min-st-phase))
    (tree-construction-strategy.maximum-spanning-tree (new-max-st-phase))
    (t (jthrow 'illegal-argument-exception "No implementation available for ~A" (name this)))))

(defjmethod create ((this structure-extraction-strategy))
  (enum-case this
    (structure-extraction-strategy.delaunay-triangulation (new-delaunay-triangulation-phase))
    (t (jthrow 'illegal-argument-exception "No implementation available for ~A" (name this)))))

(defjmethod create ((this spore-compaction-strategy))
  (enum-case this
    (spore-compaction-strategy.depth-first (new-shrink-tree-compaction-phase))
    (t (jthrow 'illegal-argument-exception "No implementation available for ~A" (name this)))))

;;; ---------------------------------------------------------------- phases

(defjmethod get-layout-processor-configuration ((this delaunay-triangulation-phase) graph)
  (declare (ignore graph))
  (layout-processor-configuration.create))
(defjmethod get-layout-processor-configuration ((this min-st-phase) graph)
  (declare (ignore graph))
  (layout-processor-configuration.create))
(defjmethod get-layout-processor-configuration ((this shrink-tree-compaction-phase) graph)
  (declare (ignore graph))
  (layout-processor-configuration.create))
(defjmethod get-layout-processor-configuration ((this grow-tree-phase) graph)
  (declare (ignore graph))
  (layout-processor-configuration.create))


;; p1structure.DelaunayTriangulationPhase
(defjmethod process ((this delaunay-triangulation-phase) graph progress-monitor)
  (begin progress-monitor "Delaunay triangulation" 1)
  (let ((vertices (new-jlist)))
    (jdo (v (.vertices graph)) (jl-add vertices (.original-vertex v)))
    (if (null (.t-edges graph))
        (setf (.t-edges graph) (bowyer-watson-triangulation.triangulate vertices nil))
        (add-all (.t-edges graph) (bowyer-watson-triangulation.triangulate vertices nil))))
  (done progress-monitor))

;; p2processingorder.MinSTPhase
(defjmethod process ((this min-st-phase) graph progress-monitor)
  (begin progress-monitor "Minimum spanning tree construction" 1)
  (let* ((root (if (.preferred-root graph)
                   (.original-vertex (.preferred-root graph))
                   (.original-vertex (jl-get (.vertices graph) 0))))
         (t-tree (naive-min-st.create-spanning-tree (.t-edges graph) root (.cost-function graph))))
    (convert this t-tree graph))
  (done progress-monitor))

(defjmethod convert ((this min-st-phase) t-tree graph)
  (clrhash (.node-map this))
  (jdo (n (.vertices graph)) (jm-put (.node-map this) (%spore-vkey (.original-vertex n)) n))
  (let ((root (new-tree (gethash (%spore-vkey (.node t-tree)) (.node-map this)))))
    (%spore-add-node this root t-tree)
    (setf (.tree graph) root)))

(defun %spore-add-node (this s tt)
  (jdo (t-tree (.children tt))
    (let ((child (new-tree (gethash (%spore-vkey (.node t-tree)) (.node-map this)))))
      (add (.children s) child)
      (%spore-add-node this child t-tree))))

;; p2processingorder.MaxSTPhase
(defjmethod process ((this max-st-phase) graph progress-monitor)
  (begin progress-monitor "Maximum spanning tree construction" 1)
  (let* ((cf (.cost-function graph))
         (inverted (lambda (e) (- (funcall cf e))))
         ;; NB: ELK uses .vertex here, not .originalVertex as MinSTPhase does
         (root (if (.preferred-root graph)
                   (.vertex (.preferred-root graph))
                   (.vertex (jl-get (.vertices graph) 0))))
         (tree (naive-min-st.create-spanning-tree (.t-edges graph) root inverted)))
    (convert this tree graph))
  (done progress-monitor))

;; p3execution.ShrinkTreeCompactionPhase
(defjmethod process ((this shrink-tree-compaction-phase) graph progress-monitor)
  (begin progress-monitor "Shrinking tree compaction" 1)
  (depth-first-compaction.compact (.tree graph) (.orthogonal-compaction graph))
  (done progress-monitor))

;; p3execution.GrowTreePhase
(defjmethod process ((this grow-tree-phase) graph progress-monitor)
  (begin progress-monitor "Grow Tree" 1)
  (setf (.root this) (.tree graph)
        (.svg this) (new-svg-image nil)
        (.overlaps-existed this) nil)
  (%spore-grow-at this (.tree graph))
  (set-property graph common-internal-properties.overlaps-existed (.overlaps-existed this))
  (done progress-monitor))

(defun %spore-grow-at (this r)
  (jdo (c (.children r))
    (translate (.node c) (sub (jclone (.vertex (.node r))) (.original-vertex (.node r))))
    (let ((tt (utils.overlap (.rect (.node r)) (.rect (.node c)))))
      (when (> tt 1.0)
        (setf (.overlaps-existed this) t))
      (set-center-position (.node c)
                           (add (jclone (.vertex (.node r)))
                                (scale (sub (jclone (.original-vertex (.node c))) (.original-vertex (.node r)))
                                       tt))))
    (%spore-grow-at this c)))
