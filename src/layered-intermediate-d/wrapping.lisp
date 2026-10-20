;;;; wrapping.lisp -- org.eclipse.elk.alg.layered.intermediate.wrapping

(in-package #:elk)

;;; ------------------------------------------------------------ ICutIndexCalculator.ManualCutIndexCalculator

(defjmethod get-cut-indexes ((this manual-cut-index-calculator) graph gs)
  (declare (ignore gs))
  (let ((cuts (get-property graph layered-options.wrapping-cutting-cuts)))
    (if cuts cuts (new-jlist))))

(defjmethod guarantee-valid ((this manual-cut-index-calculator)) nil)

;;; ------------------------------------------------------------ ARDCutIndexHeuristic

(defjmethod get-cut-indexes ((this ard-cut-index-heuristic) graph gs)
  (declare (ignore graph))
  (let* ((rows (ard-cut-index-heuristic.get-chunk-count gs))
         ;; the number of cuts is one less than the number of rows
         (cuts (new-jlist))
         (step (/ (jd (.longest-path gs)) rows)))
    (loop for idx from 1 below rows
          do (jl-add cuts (to-int (math.round (* idx step)))))
    cuts))

(defjstatic ard-cut-index-heuristic.get-chunk-count (gs)
  (let* ((rowsd (math.sqrt (/ (get-sum-width gs) (* (.dar gs) (get-max-height gs)))))
         (rows (to-int (math.round rowsd))))
    (min rows (.longest-path gs))))

(defjmethod guarantee-valid ((this ard-cut-index-heuristic)) nil)

;;; ------------------------------------------------------------ MSDCutIndexHeuristic

(defjmethod guarantee-valid ((this msd-cut-index-heuristic)) nil)

(defjmethod get-cut-indexes ((this msd-cut-index-heuristic) graph gs)
  ;; minimize the max of the sums of the widths
  (let* ((widths (get-widths gs))
         (heights (get-heights gs))
         ;; record the accumulated width at each index of the original layering
         (width-at-index (make-array (length widths) :initial-element 0.0))
         (total (aref widths 0)))
    (setf (aref width-at-index 0) (aref widths 0))
    (loop for i from 1 below (length widths)
          do (setf (aref width-at-index i) (+ (aref width-at-index (1- i)) (aref widths i)))
             (setf total (+ total (aref widths i))))
    ;; initial guess on a good number of cuts
    (let* ((cut-cnt (1- (ard-cut-index-heuristic.get-chunk-count gs)))
           (freedom (get-property graph layered-options.wrapping-cutting-msd-freedom))
           (best-max-scale double.negative-infinity)
           (best-cuts (new-jlist)))
      ;; now find the best set of cut indexes
      (loop for m from (max 0 (- cut-cnt freedom))
              to (min (1- (.longest-path gs)) (+ cut-cnt freedom))
            do (let* ((row-sum (/ total (+ m 1)))
                      (sum-so-far 0.0)
                      (index 1)
                      (cuts (new-jlist))
                      ;; maximum of any row width
                      (width double.negative-infinity)
                      (last-cut-width 0.0)
                      ;; sum of the row height maximums
                      (height 0.0)
                      (row-height-max (aref heights 0)))
                 (if (= m 0)
                     (setf width total
                           height (get-max-height gs))
                     (progn
                       (loop while (< index (.longest-path gs))
                             do (when (>= (- (aref width-at-index (1- index)) sum-so-far) row-sum)
                                  ;; cut _before_ index
                                  (jl-add cuts index)
                                  ;; update state
                                  (setf width (math.max width (- (aref width-at-index (1- index))
                                                                 last-cut-width)))
                                  (setf height (+ height row-height-max))
                                  (setf sum-so-far (+ sum-so-far (- (aref width-at-index (1- index))
                                                                    sum-so-far)))
                                  (setf last-cut-width (aref width-at-index (1- index)))
                                  (setf row-height-max (aref heights index)))
                                (setf row-height-max (math.max row-height-max (aref heights index)))
                                (incf index))
                       ;; add heights of last row
                       (setf height (+ height row-height-max))))
                 ;; are they better?
                 (let ((max-scale (math.min (/ 1.0 width) (/ (/ 1.0 (.dar gs)) height))))
                   (when (> max-scale best-max-scale)
                     (setf best-max-scale max-scale
                           best-cuts cuts)))))
      best-cuts)))

;;; ------------------------------------------------------------ CuttingUtils

(defjstatic cutting-utils.insert-dummies (layered-graph original-edge offset-first-in-layer-dummy)
  ;; to visually separate the backward wrapping edges, add some additional spacing
  (let* ((edge-node-spacing (get-property layered-graph layered-options.spacing-edge-node))
         (additional-spacing (get-property layered-graph layered-options.wrapping-additional-edge-spacing))
         (is (new-individual-spacings))
         (edge original-edge)
         (target-port (get-target edge))
         (src (get-node (get-source edge)))
         (tgt (get-node (get-target edge)))
         (src-index (get-index (get-layer src)))
         (tgt-index (get-index (get-layer tgt)))
         (created-edges (new-jlist)))
    (set-property is layered-options.spacing-edge-node (+ edge-node-spacing additional-spacing))
    (loop for i from src-index to tgt-index
          do (let ((dummy-node (new-l-node layered-graph)))
               ;; Create dummy node
               (set-type dummy-node node-type.long-edge)
               (set-property dummy-node layered-internal-properties.origin edge)
               (set-property dummy-node layered-options.port-constraints port-constraints.fixed-pos)
               (set-property dummy-node layered-options.spacing-individual is)
               (let ((next-layer (jl-get (get-layers layered-graph) i)))
                 (if (= i src-index)
                     (set-layer dummy-node (- (length (get-nodes next-layer)) offset-first-in-layer-dummy)
                                next-layer)
                     (set-layer dummy-node next-layer)))
               ;; Set thickness of the edge
               (let ((thickness (get-property edge layered-options.edge-thickness)))
                 (when (< thickness 0)
                   (setf thickness 0.0)
                   (set-property edge layered-options.edge-thickness thickness))
                 (setf (.y (get-size dummy-node)) thickness)
                 (let ((port-pos (math.floor (/ thickness 2)))
                       (dummy-input (new-l-port))
                       (dummy-output (new-l-port)))
                   ;; Create dummy input and output ports
                   (set-side dummy-input port-side.west)
                   (set-node dummy-input dummy-node)
                   (setf (.y (get-position dummy-input)) port-pos)
                   (set-side dummy-output port-side.east)
                   (set-node dummy-output dummy-node)
                   (setf (.y (get-position dummy-output)) port-pos)
                   (set-target edge dummy-input)
                   ;; Create a dummy edge
                   (let ((dummy-edge (new-l-edge)))
                     (copy-properties dummy-edge edge)
                     (set-property dummy-edge layered-options.junction-points nil)
                     (set-source dummy-edge dummy-output)
                     (set-target dummy-edge target-port)
                     (%cutting-utils-set-dummy-properties dummy-node edge dummy-edge)
                     (jl-add created-edges dummy-edge)
                     (setf edge dummy-edge))))))
    created-edges))

(defun %cutting-utils-set-dummy-properties (dummy in-edge out-edge)
  (let ((in-edge-source-node (get-node (get-source in-edge))))
    (if (eq (get-type in-edge-source-node) node-type.long-edge)
        (progn
          ;; The incoming edge originates from a long edge dummy node, so we can
          ;; just copy its properties
          (set-property dummy layered-internal-properties.long-edge-source
                        (get-property in-edge-source-node layered-internal-properties.long-edge-source))
          (set-property dummy layered-internal-properties.long-edge-target
                        (get-property in-edge-source-node layered-internal-properties.long-edge-target)))
        (progn
          ;; The source is the input edge's source port, the target is the output
          ;; edge's target port
          (set-property dummy layered-internal-properties.long-edge-source (get-source in-edge))
          (set-property dummy layered-internal-properties.long-edge-target (get-target out-edge))))))

;;; ------------------------------------------------------------ GraphStats

(defjctor graph-stats (graph)
  (setf (.graph this) graph)
  ;; since the graph direction may be horizontal (default) or vertical,
  ;;  the desired aspect ration must be adjusted correspondingly
  (let ((dir (get-property graph layered-options.direction))
        (aspect-ratio (get-property graph layered-options.aspect-ratio))
        (correction (get-property graph layered-options.wrapping-correction-factor)))
    (if (or (eq dir direction.left) (eq dir direction.right) (eq dir direction.undefined))
        (setf (.dar this) (* aspect-ratio correction))
        (setf (.dar this) (/ 1 (* aspect-ratio correction)))))
  (setf (.spacing this) (get-property graph layered-options.spacing-node-node-between-layers))
  (setf (.in-layer-spacing this) (get-property graph layered-options.spacing-node-node))
  (setf (.longest-path this) (length (get-layers graph))))

;; Approximations

(defjmethod get-approximate-layer-width ((this graph-stats) l)
  (%gs-determine-layer-width this l))

(defjmethod get-approximate-layering-width ((this graph-stats))
  (get-sum-width this))

(defjmethod get-approximate-chunk-based-layering-width ((this graph-stats) cuts)
  (if (is-empty cuts)
      (get-approximate-layering-width this)
      (let ((width 0.0) (low-idx 0))
        (jdo (high-idx cuts)
          (setf width (math.max width (%gs-determine-chunk-width this low-idx high-idx)))
          (setf low-idx high-idx))
        (math.max width (%gs-determine-chunk-width this low-idx (length (get-layers (.graph this))))))))

(defjmethod get-approximate-layer-height ((this graph-stats) l)
  (%gs-determine-layer-height this l))

(defjmethod get-approximate-layering-height ((this graph-stats))
  (get-max-height this))

(defjmethod get-approximate-chunk-based-layering-height ((this graph-stats) cuts)
  (if (is-empty cuts)
      0.0
      (let ((height 0.0) (low-idx 0))
        (jdo (high-idx cuts)
          (setf height (+ height (%gs-determine-chunk-height this low-idx high-idx)))
          (setf low-idx high-idx))
        (+ height (%gs-determine-chunk-height this low-idx (length (get-layers (.graph this))))))))

;; Widths

(defjmethod get-max-width ((this graph-stats))
  (or (.max-width this)
      (setf (.max-width this) (%gs-determine-width this #'math.max))))

(defjmethod get-sum-width ((this graph-stats))
  (or (.sum-width this)
      (setf (.sum-width this) (%gs-determine-width this #'+))))

(defjmethod get-widths ((this graph-stats))
  (unless (.widths this) (%gs-init-widths-and-heights this))
  (.widths this))

(defun %gs-init-widths-and-heights (this)
  (let ((n (.longest-path this)))
    (setf (.widths this) (make-array n :initial-element 0.0)
          (.heights this) (make-array n :initial-element 0.0))
    (dotimes (i n)
      (let ((l (jl-get (get-layers (.graph this)) i)))
        (setf (aref (.widths this) i) (%gs-determine-layer-width this l)
              (aref (.heights this) i) (%gs-determine-layer-height this l))))))

(defun %gs-reduce (fun values)
  "Stream.reduce(fun).get()"
  (if (zerop (length values))
      (jthrow 'no-such-element-exception "No value present")
      (reduce fun values)))

(defun %gs-determine-width (this fun)
  (%gs-reduce fun (map 'vector (lambda (l) (%gs-determine-layer-width this l))
                       (get-layers (.graph this)))))

(defun %gs-determine-layer-width (this l)
  (let ((max-w 0.0))
    (jdo (n (get-nodes l))
      (let ((n-w (+ (.x (get-size n)) (.right (get-margin n)) (.left (get-margin n)) (.spacing this))))
        (setf max-w (math.max max-w n-w))))
    max-w))

(defun %gs-determine-chunk-width (this low-idx high-idx)
  (let ((sum 0.0))
    (loop for i from low-idx below high-idx
          do (setf sum (+ sum (%gs-determine-layer-width this (jl-get (get-layers (.graph this)) i)))))
    sum))

;; Heights

(defjmethod get-max-height ((this graph-stats))
  (or (.max-height this)
      (setf (.max-height this) (%gs-determine-height this #'math.max))))

(defjmethod get-heights ((this graph-stats))
  (unless (.heights this) (%gs-init-widths-and-heights this))
  (.heights this))

(defun %gs-determine-height (this fun)
  (%gs-reduce fun (map 'vector (lambda (l) (%gs-determine-layer-height this l))
                       (get-layers (.graph this)))))

(defun %gs-determine-layer-height (this layer)
  (let ((l-h 0.0))
    (jdo (n (get-nodes layer))
      (setf l-h (+ l-h (.y (get-size n)) (.bottom (get-margin n)) (.top (get-margin n))
                   (.in-layer-spacing this)))
      (jdo (inc (get-incoming-edges n))
        (when (eq (get-type (get-node (get-source inc))) node-type.north-south-port)
          (let* ((src (get-node (get-source inc)))
                 (origin (get-property src layered-internal-properties.origin)))
            (setf l-h (+ l-h (.y (get-size origin)) (.bottom (get-margin origin))
                         (.top (get-margin origin))))))))
    l-h))

(defun %gs-determine-chunk-height (this low-idx high-idx)
  ;; DoubleStream.max().orElse(0)
  (let ((result nil))
    (loop for i from low-idx below high-idx
          do (let ((h (%gs-determine-layer-height this (jl-get (get-layers (.graph this)) i))))
               (setf result (if result (math.max result h) h))))
    (or result 0.0)))

;; Cutting allowed?

(defjmethod is-cut-allowed ((this graph-stats) layer-index-or-layer)
  (if (integerp layer-index-or-layer)
      (progn
        (unless (.cuts-allowed this) (%gs-init-cut-allowed this))
        (aref (.cuts-allowed this) layer-index-or-layer))
      (%gs-is-cut-allowed-layer layer-index-or-layer)))

(defun %gs-init-cut-allowed (this)
  (when (.cuts-allowed this)
    (return-from %gs-init-cut-allowed))
  (let* ((graph (.graph this))
         (layers (get-layers graph))
         (cuts-allowed (make-array (length layers) :initial-element nil)))
    (setf (.cuts-allowed this) cuts-allowed)
    (setf (aref cuts-allowed 0) nil)
    (if (has-property graph layered-options.wrapping-validify-forbidden-indices)
        ;; user-specified forbidden indices
        (jdo (f (get-property graph layered-options.wrapping-validify-forbidden-indices))
          (when (and (> f 0) (< f (length cuts-allowed)))
            (setf (aref cuts-allowed f) nil)))
        ;; 'default' behavior: skip the first layer, then check for valid cut indexes
        (loop for i from 1 below (length layers)
              do (setf (aref cuts-allowed i) (%gs-is-cut-allowed-layer (jl-get layers i)))))))

(defjmethod get-cuts-allowed ((this graph-stats))
  (unless (.cuts-allowed this) (%gs-init-cut-allowed this))
  (.cuts-allowed this))

(defun %gs-is-cut-allowed-layer (layer)
  ;; we only allow to cut between a pair of layers if there is only one pair
  ;; of nodes that is connected by 1 or more edges
  (let ((cut-allowed t) (n1 nil) (n2 nil))
    (block check
      (jdo (tgt (get-nodes layer))
        (jdo (e (get-incoming-edges tgt))
          ;; check for different source
          (when (and n1 (not (eq n1 tgt)))
            (setf cut-allowed nil)
            (return-from check))
          (setf n1 tgt)
          ;; check for different target
          (let ((src (get-node (get-source e))))
            (when (and n2 (not (eq n2 src)))
              (setf cut-allowed nil)
              (return-from check))
            (setf n2 src)))))
    cut-allowed))

;;; ------------------------------------------------------------ BreakingPointInserter

(defun %wrapping-cut-index-calculator (graph)
  (enum-case (get-property graph layered-options.wrapping-cutting-strategy)
    (cutting-strategy.manual (new-manual-cut-index-calculator))
    (cutting-strategy.ard (new-ard-cut-index-heuristic))
    (t (new-msd-cut-index-heuristic))))

(defjmethod process ((this breaking-point-inserter) graph progress-monitor)
  (begin progress-monitor "Breaking Point Insertion" 1)
  (let* ((gs (new-graph-stats graph))
         ;; #1 determine the cut points
         (icic (%wrapping-cut-index-calculator graph))
         (cuts (get-cut-indexes icic graph gs)))
    ;; #2 improve cuts
    (when (get-property graph layered-options.wrapping-multi-edge-improve-cuts)
      (setf cuts (%bpi-improve-cuts this graph cuts)))
    ;; #3 if they are not guaranteed to be valid, make them valid
    (when (and (not (guarantee-valid icic))
               (has-property graph layered-options.wrapping-validify-strategy))
      (enum-case (get-property graph layered-options.wrapping-validify-strategy)
        (validify-strategy.look-back
         (setf cuts (single-edge-graph-wrapper.validify-indexes-looking-back gs cuts)))
        (validify-strategy.greedy
         (setf cuts (single-edge-graph-wrapper.validify-indexes-greedily gs cuts)))
        (t nil)))
    (when (is-empty cuts)
      (done progress-monitor)
      (return-from process/2 nil))
    ;; #4 insert the breaking points
    (%bpi-apply-cuts this graph cuts)
    (done progress-monitor)))

(defun %bpi-apply-cuts (this graph cuts)
  (declare (ignore this))
  ;; the ListIterator over the layers is emulated by POS, the index of the next element
  (let* ((layers (get-layers graph))
         (pos 0)
         (cut-items (to-jvector cuts))
         (cut-pos 0)
         (idx 0)
         (cut (prog1 (aref cut-items cut-pos) (incf cut-pos)))
         (no-split-edges 0)
         (already-split (new-jset))
         ;; keep track of edges that have to be split upon cut insertion (LinkedHashSet)
         (open-edges (new-jlist)))
    (loop while (< pos (length layers))
          do (let ((layer (aref layers pos)))
               (incf pos)
               ;; book keeping of 'open' edges
               (jdo (n (get-nodes layer))
                 (jdo (e (get-outgoing-edges n))
                   (unless (find e open-edges) (vector-push-extend e open-edges)))
                 (jdo (e (get-incoming-edges n))
                   (let ((p (position e open-edges)))
                     (when p (jl-remove-at open-edges p)))))
               ;; at each cut index, insert two new layers plus breaking point dummies
               (when (= (+ idx 1) cut)
                 (let ((bp-layer1 (new-layer graph))
                       (bp-layer2 (new-layer graph)))
                   (jl-add-at layers pos bp-layer1) (incf pos)
                   (jl-add-at layers pos bp-layer2) (incf pos)
                   ;; splitting an edge 'e' looks like this:
                   ;;   node --nodeStartEdge--> startMarker --startEndEdge--> endMarker --e--> node
                   ;; note that the original edge is pushed "to the right"
                   (jdo (original-edge open-edges)
                     (unless (js-contains already-split original-edge)
                       (incf no-split-edges)
                       (js-add already-split original-edge))
                     ;; start dummy
                     (let ((bp-start-marker (new-l-node graph))
                           (in-port-bp1 (new-l-port))
                           (out-port-bp1 (new-l-port))
                           (bp-end-marker (new-l-node graph))
                           (in-port-bp2 (new-l-port))
                           (out-port-bp2 (new-l-port))
                           (node-start-edge (new-l-edge))
                           (start-end-edge (new-l-edge)))
                       (set-property bp-start-marker layered-options.port-constraints port-constraints.fixed-side)
                       (set-layer bp-start-marker bp-layer1)
                       (set-type bp-start-marker node-type.breaking-point)
                       (set-node in-port-bp1 bp-start-marker)
                       (set-side in-port-bp1 port-side.west)
                       (set-node out-port-bp1 bp-start-marker)
                       (set-side out-port-bp1 port-side.east)
                       ;; end dummy
                       (set-property bp-end-marker layered-options.port-constraints port-constraints.fixed-side)
                       (set-layer bp-end-marker bp-layer2)
                       (set-type bp-end-marker node-type.breaking-point)
                       (set-node in-port-bp2 bp-end-marker)
                       (set-side in-port-bp2 port-side.west)
                       (set-node out-port-bp2 bp-end-marker)
                       (set-side out-port-bp2 port-side.east)
                       ;; first dummy edge
                       (set-source node-start-edge (get-source original-edge))
                       (set-target node-start-edge in-port-bp1)
                       (set-property node-start-edge layered-internal-properties.model-order
                                     (get-property original-edge layered-internal-properties.model-order))
                       ;; second dummy edge
                       (set-source start-end-edge out-port-bp1)
                       (set-target start-end-edge in-port-bp2)
                       (set-property start-end-edge layered-internal-properties.model-order
                                     (get-property original-edge layered-internal-properties.model-order))
                       ;; reroute the original edge
                       (set-source original-edge out-port-bp2)
                       ;; attach information to the dummies
                       (let ((bpi (new-bp-info bp-start-marker bp-end-marker node-start-edge
                                               start-end-edge original-edge)))
                         (set-property bp-start-marker layered-internal-properties.breaking-point-info bpi)
                         (set-property bp-end-marker layered-internal-properties.breaking-point-info bpi)
                         ;; possibly link a chain of breaking point dummies
                         (let ((prev-node (get-node (get-source node-start-edge))))
                           (when (eq (get-type prev-node) node-type.breaking-point)
                             (let ((bpi-prev (get-property prev-node
                                                           layered-internal-properties.breaking-point-info)))
                               (setf (.next bpi-prev) bpi)
                               (setf (.prev bpi) bpi-prev)))))))
                   (if (< cut-pos (length cut-items))
                       (setf cut (prog1 (aref cut-items cut-pos) (incf cut-pos)))
                       ;; done
                       (loop-finish))))
               (incf idx)))
    no-split-edges))

(defun %bpi-improve-cuts (this graph cuts)
  (let ((improved-cuts (new-jlist))
        ;; convert to cut object
        (ccuts (new-jlist))
        (last-cut nil))
    (jdo (cut-idx cuts)
      (let ((cut (new-cut cut-idx)))
        (jl-add ccuts cut)
        (when last-cut
          (setf (.prev cut) last-cut)
          (setf (.suc last-cut) cut))
        (setf last-cut cut)))
    ;; compute edge spans
    (let ((spans (%bpi-compute-edge-spans this graph)))
      ;; iteratively find the best cuts
      (dotimes (i (length ccuts))
        (let ((l-cut nil)
              (r-cut (self-or-next (jl-get ccuts 0)))
              (best-cut nil)
              (best-score double.positive-infinity))
          (loop for idx from 1 below (length (get-layers graph))
                do (let* ((r-dist (if r-cut (abs (- (.index r-cut) idx)) (+ (abs (- idx (.index l-cut))) 1)))
                          (l-dist (if l-cut (abs (- idx (.index l-cut))) (+ r-dist 1)))
                          hit dist)
                     (if (< l-dist r-dist)
                         (setf hit l-cut dist l-dist)
                         (setf hit r-cut dist r-dist))
                     (let ((score (%bpi-compute-score this graph idx (aref spans idx) dist)))
                       (when (< score best-score)
                         (setf best-score score
                               best-cut hit)
                         (setf (.new-index best-cut) idx)))
                     (when (and r-cut (= idx (.index r-cut)))
                       (setf l-cut r-cut)
                       (setf r-cut (next r-cut)))))
          ;; record
          (when best-cut
            (jl-add improved-cuts (.new-index best-cut))
            (setf (.assigned best-cut) t)
            ;; propagate offsets as far as possible
            (offset best-cut)))))
    (jl-sort improved-cuts #'integer.compare)
    improved-cuts))

(defun %bpi-compute-score (this graph index spans dist)
  (declare (ignore this index))
  (let ((distance-penalty (get-property graph layered-options.wrapping-multi-edge-distance-penalty)))
    (+ spans (math.pow dist distance-penalty))))

(defun %bpi-compute-edge-spans (this graph)
  (declare (ignore this))
  (let ((spans (make-array (+ (length (get-layers graph)) 1) :initial-element 0))
        (open (new-jset))
        (open-count 0)
        (i 0))
    (jdo (l (get-layers graph))
      (setf (aref spans i) open-count)
      (incf i)
      (jdo (n (get-nodes l))
        (jdo (e (get-outgoing-edges n))
          (when (js-add open e) (incf open-count))))
      (jdo (n (get-nodes l))
        (jdo (e (get-incoming-edges n))
          (when (js-contains open e)
            (js-remove open e)
            (decf open-count)))))
    spans))

;; BPInfo

(defjctor bp-info (start-dummy end-dummy node-start-edge start-end-edge original-edge)
  (setf (.start this) start-dummy
        (.end this) end-dummy
        (.node-start-edge this) node-start-edge
        (.start-end-edge this) start-end-edge
        (.original-edge this) original-edge))

(defjstatic breaking-point-inserter.is-start (n)
  (let ((bpi (get-property n layered-internal-properties.breaking-point-info)))
    (if bpi (eq (.start bpi) n) nil)))

(defjstatic breaking-point-inserter.is-end (n)
  (let ((bpi (get-property n layered-internal-properties.breaking-point-info)))
    (if bpi (eq (.end bpi) n) nil)))

;; Aliases under the nested class's own name.
(defjstatic bp-info.is-start (n) (breaking-point-inserter.is-start n))
(defjstatic bp-info.is-end (n) (breaking-point-inserter.is-end n))

(defjmethod to-string ((this bp-info))
  (format nil "BPInfo[~%	start=~A~%	end=~A~%	nodeStartEdge=~A~%	startEndEdge=~A~%	originalEdge=~A~%	startInLayerDummy=~A~%	startInLayerEdge=~A~%	endInLayerDummy=~A~%	endInLayerEdge=~A"
          (.start this) (.end this) (.node-start-edge this) (.start-end-edge this)
          (.original-edge this) (.start-in-layer-dummy this) (.start-in-layer-edge this)
          (.end-in-layer-dummy this) (.end-in-layer-edge this)))

;; Cut

(defjctor cut (index)
  (setf (.index this) index))

(defjmethod self-or-next ((this cut))
  (cond ((not (.assigned this)) this)
        ((.suc this) (self-or-next (.suc this)))
        (t nil)))

;; Cut.next()
(define-jcall next :instance)
(defjmethod next ((this cut))
  (if (.suc this) (self-or-next (.suc this)) nil))

(defjmethod offset ((this cut))
  (unless (.assigned this)
    (jthrow 'illegal-state-exception "Cannot offset an unassigned cut."))
  (let ((offset (- (.new-index this) (.index this))))
    (%cut-offset this offset)
    (%cut-offset-prev this offset)
    (%cut-offset-suc this offset)))

(defun %cut-offset (this offset)
  (incf (.index this) offset))

(defun %cut-offset-prev (this offset)
  (let ((prev (.prev this)))
    (when (and prev (not (.assigned prev)))
      (%cut-offset prev offset)
      (%cut-offset-prev prev offset))))

(defun %cut-offset-suc (this offset)
  (let ((suc (.suc this)))
    (when (and suc (not (.assigned suc)))
      (%cut-offset suc offset)
      (%cut-offset-suc suc offset))))

;;; ------------------------------------------------------------ BreakingPointProcessor

(defjmethod process ((this breaking-point-processor) graph progress-monitor)
  (begin progress-monitor "Breaking Point Processor" 1)
  ;; #1 perform initial wrapping
  (%bpp-perform-wrapping this graph)
  ;; #2 if desired, improve edge lengths
  (when (get-property graph layered-options.wrapping-multi-edge-improve-wrapped-edges)
    ;; assign indexes to nodes, s.t. the node's index equals the nodes position within its layer
    (jdo (l (get-layers graph))
      (let ((index 0))
        (jdo (n (get-nodes l))
          (setf (.id n) index)
          (incf index))))
    (%bpp-improve-multi-cut-index-edges this graph)
    (%bpp-improve-unneccesarily-long-edges this graph t)
    (%bpp-improve-unneccesarily-long-edges this graph nil))
  (done progress-monitor))

(defun %bpp-perform-wrapping (this graph)
  (declare (ignore this))
  (let* ((layers (get-layers graph))
         (pos 0))
    ;; add initial empty layer to account for break point start dummies
    (jl-add-at layers pos (new-layer graph))
    (incf pos)
    ;; iterate the layers from left to right; as soon as a layer with a BREAKING POINT
    ;; start dummy is encountered, the nodes of the next layer are moved to the very
    ;; first layer, consequently 'wrapping' the graph
    (let ((reverse nil) (idx 1))
      (loop while (< pos (length layers))
            do (let* ((layer (aref layers pos))
                      (new-layer (progn (incf pos) (jl-get layers idx)))
                      (nodes-to-move (jlist-from (get-nodes layer)))
                      ;; remember an offset used for adding in-layer dummies later
                      (offset (length nodes-to-move)))
                 ;; move the nodes to their new layer
                 (jdo (n nodes-to-move)
                   (set-layer n new-layer))
                 (if reverse
                     (progn
                       ;; important to introduce the chains of long edge dummies in reversed order
                       (loop for k from (1- (length nodes-to-move)) downto 0
                             do (let ((n (aref nodes-to-move k)))
                                  (jdo (e (jlist-from (get-incoming-edges n)))
                                    ;; reverse the edge
                                    (j-reverse e graph t)
                                    (set-property graph layered-internal-properties.cyclic t)
                                    ;; insert proper dummy nodes for the newly created long edge
                                    (let* ((dummy-edges (cutting-utils.insert-dummies graph e offset))
                                           ;; ameliorate breaking point info
                                           (bpi (get-property n layered-internal-properties.breaking-point-info))
                                           (start-in-layer-edge (jl-get dummy-edges (1- (length dummy-edges)))))
                                      (setf (.start-in-layer-dummy bpi) (get-node (get-source start-in-layer-edge)))
                                      (setf (.start-in-layer-edge bpi) start-in-layer-edge)
                                      (setf (.end-in-layer-dummy bpi) (get-node (get-target e)))
                                      (setf (.end-in-layer-edge bpi) e)))))
                       (setf reverse nil))
                     (when (plusp (length nodes-to-move))
                       (let ((a-node (aref nodes-to-move 0)))
                         (when (eq (get-type a-node) node-type.breaking-point)
                           ;; next layer should be moved (it contains the breaking point end dummies)
                           (setf reverse t)
                           ;; start moving nodes to the very first layer
                           (setf idx -1)))))
                 (incf idx))))
    ;; remove old layers that are now empty
    (jl-remove-if layers (lambda (l) (zerop (length (get-nodes l)))))))

(defun %bpp-improve-multi-cut-index-edges (this graph)
  (declare (ignore this))
  (jdo (l (get-layers graph))
    (jdo (n (jlist-from (get-nodes l)))
      (when (breaking-point-inserter.is-start n)
        (let ((info (get-property n layered-internal-properties.breaking-point-info)))
          (when (and (null (.prev info)) (.next info))
            (let ((current info)
                  (next (.next info)))
              (loop while next
                    do ;; drop the dummy chain
                       (%bpp-drop-dummies (.start next) (.start-in-layer-dummy next) nil t)
                       ;; update the in-layer indexes of subsequent nodes
                       (%bpp-update-indexes-after (.end current))
                       (%bpp-update-indexes-after (.start next))
                       (%bpp-update-indexes-after (.start-in-layer-dummy next))
                       (%bpp-update-indexes-after (.end-in-layer-dummy next))
                       ;; reconnect the edge
                       (set-target (.end-in-layer-edge next) (get-target (.end-in-layer-edge current)))
                       (set-target (.end-in-layer-edge current) nil)
                       ;; throw out all the unnecessary stuff
                       (set-layer (.end current) nil)
                       (set-layer (.start next) nil)
                       (set-layer (.start-in-layer-dummy next) nil)
                       (set-layer (.end-in-layer-dummy next) nil)
                       ;; update BPInfo
                       (let ((new-info (new-bp-info (.start current) (.end next) (.node-start-edge current)
                                                    (.start-end-edge next) (.original-edge next))))
                         (setf (.start-in-layer-dummy new-info) (.start-in-layer-dummy current)
                               (.start-in-layer-edge new-info) (.start-in-layer-edge current)
                               (.end-in-layer-dummy new-info) (.end-in-layer-dummy current)
                               (.end-in-layer-edge new-info) (.end-in-layer-edge next)
                               (.prev new-info) (.prev current)
                               (.next new-info) (.next next))
                         (set-property (.start current) layered-internal-properties.breaking-point-info new-info)
                         (set-property (.end next) layered-internal-properties.breaking-point-info new-info)
                         (setf next (.next next))
                         ;; continue with the newly assembled one (_not_ 'next')
                         (setf current new-info))))))))))

(defun %bpp-improve-unneccesarily-long-edges (this graph forwards)
  (declare (ignore this))
  (let ((check (if forwards #'breaking-point-inserter.is-end/1 #'breaking-point-inserter.is-start/1))
        (didsome nil))
    (loop
      (setf didsome nil)
      (let ((layers (if forwards (reverse (jlist-from (get-layers graph))) (get-layers graph))))
        (jdo (layer layers)
          ;; NB: Java calls Lists.reverse(nodes) for !forwards and discards the result,
          ;; so the nodes are NOT reversed.
          (let ((nodes (jlist-from (get-nodes layer))))
            (jdo (n nodes)
              (when (funcall check n)
                (let* ((bp-node n)
                       (bp-info (get-property n layered-internal-properties.breaking-point-info))
                       (dummy (if forwards (.end-in-layer-dummy bp-info) (.start-in-layer-dummy bp-info))))
                  (setf didsome (%bpp-drop-dummies bp-node dummy forwards nil))))))))
      (unless didsome (return)))))

(defun %bpp-drop-dummies (bp-node in-layer-dummy forwards force)
  ;; get the adjacent long edge dummies (if any)
  (let ((pred-one (%bpp-next-long-edge-dummy bp-node forwards))
        (pred-two (%bpp-next-long-edge-dummy in-layer-dummy forwards))
        (didsome nil))
    ;; predOne and predTwo are guaranteed to be LONG_EDGE
    (loop while (and pred-one pred-two)
          do (if (or force (%bpp-is-adjacent-or-separated-by-breakingpoints pred-one pred-two forwards))
                 (let ((next-one (%bpp-next-long-edge-dummy pred-one forwards))
                       (next-two (%bpp-next-long-edge-dummy pred-two forwards)))
                   (%bpp-update-indexes-after in-layer-dummy)
                   (%bpp-update-indexes-after bp-node)
                   ;; the two dummies were in same layer
                   (let ((new-layer (get-layer pred-one)))
                     (long-edge-joiner.join-at pred-one nil)
                     (long-edge-joiner.join-at pred-two nil)
                     (if forwards
                         (progn
                           (set-layer in-layer-dummy (.id pred-two) new-layer)
                           (setf (.id in-layer-dummy) (.id pred-two))
                           (set-layer bp-node (+ (.id pred-one) 1) new-layer)
                           (setf (.id bp-node) (.id pred-one)))
                         (progn
                           (set-layer bp-node (.id pred-one) new-layer)
                           (setf (.id bp-node) (.id pred-one))
                           (set-layer in-layer-dummy (+ (.id pred-two) 1) new-layer)
                           (setf (.id in-layer-dummy) (.id pred-two)))))
                   (set-layer pred-one nil)
                   (set-layer pred-two nil)
                   (setf pred-one next-one
                         pred-two next-two
                         didsome t))
                 (loop-finish)))
    didsome))

(defun %bpp-is-adjacent-or-separated-by-breakingpoints (dummy1 dummy2 forwards)
  ;; d1 is the one in line with the bp dummy
  (let ((layer (get-layer dummy1))
        (start (if forwards dummy2 dummy1))
        (end (if forwards dummy1 dummy2)))
    (loop for i from (+ (.id start) 1) below (.id end)
          do (let ((node (jl-get (get-nodes layer) i)))
               (unless (or (eq (get-type node) node-type.breaking-point)
                           (%bpp-is-in-layer-dummy node))
                 (return-from %bpp-is-adjacent-or-separated-by-breakingpoints nil))))
    t))

(defun %bpp-next-long-edge-dummy (start forwards)
  ;; Usually long edge dummies are connected by exactly one edge. However, we allow the
  ;; 'start' node to be an in-layer dummy (two incoming edges, one the in-layer edge).
  (let ((edges (if forwards (get-outgoing-edges start) (get-incoming-edges start))))
    (jdo (e edges)
      (let ((other (get-other e start)))
        (when (and (eq (get-type other) node-type.long-edge)
                   (not (eq (get-layer other) (get-layer start))))
          (return-from %bpp-next-long-edge-dummy other))))
    nil))

(defun %bpp-is-in-layer-dummy (node)
  (when (eq (get-type node) node-type.long-edge)
    (jdo (e (get-connected-edges node))
      (when (and (not (is-self-loop e))
                 (eq (get-layer node) (get-layer (get-other e node))))
        (return-from %bpp-is-in-layer-dummy t))))
  nil)

(defun %bpp-update-indexes-after (node)
  (let ((nodes (get-nodes (get-layer node))))
    (loop for i from (+ (.id node) 1) below (length nodes)
          do (decf (.id (jl-get nodes i))))))

;;; ------------------------------------------------------------ BreakingPointRemover

(defjmethod process ((this breaking-point-remover) graph progress-monitor)
  (begin progress-monitor "Breaking Point Removing" 1)
  (setf (.edge-routing this) (get-property graph layered-options.edge-routing))
  (jdo (l (get-layers graph))
    (jdo (node (jlist-from (get-nodes l)))
      (when (breaking-point-inserter.is-end node)
        (let ((bpi (get-property node layered-internal-properties.breaking-point-info)))
          (when (null (.next bpi))
            (%bpr-remove this graph bpi))))))
  (done progress-monitor))

(defun %reversed-jlist (coll)
  "Lists.reverse(coll) as a fresh list."
  (let ((v (jlist-from coll))) (jl-reverse v) v))

(defun %bpr-remove (this graph bpi)
  ;; assemble the new edge route
  (let ((new-bends (new-k-vector-chain)))
    ;; depending on the edge routing style we have to act differently
    (enum-case (.edge-routing this)
      ;; for splines, the final bendpoints have not been computed yet; so far, only the
      ;; intermediate nubsplines have been attached via properties
      (edge-routing.splines
       (let* ((s1 (get-property (.node-start-edge bpi) layered-internal-properties.spline-route-start))
              (s2 (get-property (.start-end-edge bpi) layered-internal-properties.spline-route-start))
              (s3 (get-property (.original-edge bpi) layered-internal-properties.spline-route-start))
              (e1 (get-property (.node-start-edge bpi) layered-internal-properties.spline-edge-chain))
              (e2 (get-property (.start-end-edge bpi) layered-internal-properties.spline-edge-chain))
              (e3 (get-property (.original-edge bpi) layered-internal-properties.spline-edge-chain))
              (joined-segments (new-jlist))
              (joined-edges (new-jlist)))
         ;; join them (... and remember to reverse some of the segments)
         (jl-add-all joined-segments s1)
         (jdo (s s2) (setf (.inverse-order s) t))
         (jl-add-all joined-segments (%reversed-jlist s2))
         (jl-add-all joined-segments s3)
         (jl-add-all joined-edges e1)
         (jl-add-all joined-edges (%reversed-jlist e2))
         (jl-add-all joined-edges e3)
         ;; transfer the information to the original edge
         (set-property (.original-edge bpi) layered-internal-properties.spline-route-start joined-segments)
         (set-property (.original-edge bpi) layered-internal-properties.spline-edge-chain joined-edges)
         (set-property (.original-edge bpi) layered-internal-properties.spline-surviving-edge
                       (.original-edge bpi))
         ;; cleanup
         (set-property (.node-start-edge bpi) layered-internal-properties.spline-route-start nil)
         (set-property (.node-start-edge bpi) layered-internal-properties.spline-edge-chain nil)
         (set-property (.start-end-edge bpi) layered-internal-properties.spline-route-start nil)
         (set-property (.start-end-edge bpi) layered-internal-properties.spline-edge-chain nil)))
      ;; after the polyline router the edge segments are simply joined; the positions of
      ;; bpi.start and bpi.end must be added as bend points
      (edge-routing.polyline
       (add-all new-bends (get-bend-points (.node-start-edge bpi)))
       (add new-bends (get-position (.start bpi)))
       (add-all new-bends (%reversed-jlist (get-bend-points (.start-end-edge bpi))))
       (add new-bends (get-position (.end bpi)))
       (add-all new-bends (get-bend-points (.original-edge bpi))))
      ;; ORTHOGONAL: positions of bpi.start and bpi.end lie on a straight line, drop them
      (t
       (add-all new-bends (get-bend-points (.node-start-edge bpi)))
       (add-all new-bends (%reversed-jlist (get-bend-points (.start-end-edge bpi))))
       (add-all new-bends (get-bend-points (.original-edge bpi)))))
    ;; restore original edge
    (clear (get-bend-points (.original-edge bpi)))
    (add-all (get-bend-points (.original-edge bpi)) new-bends)
    (set-source (.original-edge bpi) (get-source (.node-start-edge bpi)))
    ;; collect any created junction points (order can be arbitrary)
    (let ((jp1 (get-property (.node-start-edge bpi) layered-options.junction-points))
          (jp2 (get-property (.start-end-edge bpi) layered-options.junction-points))
          (jp3 (get-property (.original-edge bpi) layered-options.junction-points)))
      (when (or jp1 jp2 jp3)
        (let ((new-junction-points (new-k-vector-chain)))
          (%bpr-add-null-safe new-junction-points jp3)
          (%bpr-add-null-safe new-junction-points jp2)
          (%bpr-add-null-safe new-junction-points jp1)
          (set-property (.original-edge bpi) layered-options.junction-points new-junction-points))))
    ;; remove all the dummy stuff
    (set-source (.start-end-edge bpi) nil)
    (set-target (.start-end-edge bpi) nil)
    (set-source (.node-start-edge bpi) nil)
    (set-target (.node-start-edge bpi) nil)
    (set-layer (.end bpi) nil)
    (set-layer (.start bpi) nil)
    (when (.prev bpi)
      (%bpr-remove this graph (.prev bpi)))))

(defun %bpr-add-null-safe (container to-add)
  (if (or (null container) (null to-add))
      nil
      (add-all container to-add)))

;;; ------------------------------------------------------------ SingleEdgeGraphWrapper

(defjmethod process ((this single-edge-graph-wrapper) graph progress-monitor)
  (begin progress-monitor "Path-Like Graph Wrapping" 1)
  (when (zerop (length (get-layers graph)))
    (done progress-monitor)
    (return-from process/2 nil))
  ;; initialization
  (let* ((gs (new-graph-stats graph))
         ;; stop early if there's nothing we can do
         (sum-width (* (get-max-width gs) (.longest-path gs)))
         (current-ar (/ sum-width (get-max-width gs))))
    (when (> (.dar gs) current-ar)
      ;; nothing to do for us
      (done progress-monitor)
      (return-from process/2 nil))
    ;; get indexes for cutting
    (let* ((icic (%wrapping-cut-index-calculator graph))
           (cuts (get-cut-indexes icic graph gs)))
      ;; if they are not guaranteed to be valid, make them valid
      (unless (guarantee-valid icic)
        (enum-case (get-property graph layered-options.wrapping-validify-strategy)
          (validify-strategy.look-back
           (setf cuts (single-edge-graph-wrapper.validify-indexes-looking-back gs cuts)))
          (validify-strategy.greedy
           (setf cuts (single-edge-graph-wrapper.validify-indexes-greedily gs cuts)))
          (t nil)))
      ;; re-build the layering
      (%segw-perform-cuts this graph gs cuts)
      (done progress-monitor))))

(defjstatic single-edge-graph-wrapper.validify-indexes-greedily (gs cuts)
  (let ((valid-cuts (new-jlist))
        (offset 0))
    (jdo (c cuts)
      (let ((cut (+ c offset)))
        (loop while (and (< cut (.longest-path gs)) (not (is-cut-allowed gs cut)))
              do (incf cut) (incf offset))
        (when (>= cut (.longest-path gs))
          ;; cut cannot be performed anymore, discard it
          (return))
        (jl-add valid-cuts cut)))
    valid-cuts))

(defjstatic single-edge-graph-wrapper.validify-indexes-looking-back (gs-or-desired desired-or-valid)
  (if (typep gs-or-desired 'graph-stats)
      (%segw-validify-looking-back-gs gs-or-desired desired-or-valid)
      (%segw-validify-looking-back-lists gs-or-desired desired-or-valid)))

(defun %segw-validify-looking-back-gs (gs desired-cuts)
  (when (is-empty desired-cuts)
    (return-from %segw-validify-looking-back-gs (new-jlist)))
  ;; initialize array with valid cuts
  (let ((valid-cuts (new-jlist)))
    (jl-add valid-cuts integer.min-value)
    (loop for i from 1 below (.longest-path gs)
          do (when (is-cut-allowed gs i) (jl-add valid-cuts i)))
    (when (= (length valid-cuts) 1)
      (return-from %segw-validify-looking-back-gs (new-jlist)))
    (jl-add valid-cuts integer.max-value)
    (%segw-validify-looking-back-lists desired-cuts valid-cuts)))

(defun %segw-validify-looking-back-lists (desired-cuts valid-cuts)
  (let ((final-cuts (new-jlist))
        (desired (to-jvector desired-cuts))
        (i-idx 0) (c-idx 0) (offset 0))
    (loop while (and (< i-idx (1- (length valid-cuts))) (< c-idx (length desired)))
          do (let ((current (+ (aref desired c-idx) offset)))
               (loop while (< (jl-get valid-cuts (+ i-idx 1)) current)
                     do (incf i-idx))
               (let* ((select 0)
                      (dist-lower (- current (jl-get valid-cuts i-idx)))
                      (dist-higher (- (jl-get valid-cuts (+ i-idx 1)) current)))
                 (when (> dist-lower dist-higher) (incf select))
                 (jl-add final-cuts (jl-get valid-cuts (+ i-idx select)))
                 (incf offset (- (jl-get valid-cuts (+ i-idx select)) current))
                 (incf c-idx)
                 (loop while (and (< c-idx (length desired))
                                  (<= (+ (aref desired c-idx) offset) (jl-get valid-cuts (+ i-idx select))))
                       do (incf c-idx))
                 (incf i-idx (+ 1 select)))))
    final-cuts))

(defun %segw-perform-cuts (this graph gs cuts)
  (declare (ignore this))
  (when (is-empty cuts)
    (return-from %segw-perform-cuts nil))
  (let* ((cut-items (to-jvector cuts))
         (cut-pos 1)
         ;; index in the "old" layering
         (index 0)
         ;; index in the "new" layering
         (new-index 0)
         (next-cut (aref cut-items 0)))
    ;; iterate all original layers
    (loop while (< index (.longest-path gs))
          do (when (= index next-cut)
               ;; start new "row"
               (setf new-index 0)
               (if (< cut-pos (length cut-items))
                   (setf next-cut (prog1 (aref cut-items cut-pos) (incf cut-pos)))
                   ;; no further cut to apply
                   (setf next-cut (+ (.longest-path gs) 1))))
             ;; once we started cutting, we have to move nodes
             (when (/= index new-index)
               (let* ((old-layer (jl-get (get-layers graph) index))
                      (new-layer (jl-get (get-layers graph) new-index))
                      (nodes-to-move (jlist-from (get-nodes old-layer))))
                 (jdo (n nodes-to-move)
                   ;; first move the original node to allow creating dummy nodes for the reversed edge
                   (set-layer n (length (get-nodes new-layer)) new-layer)
                   ;; at the cut points the edges have to be re-routed
                   (when (= new-index 0)
                     (jdo (e (jlist-from (get-incoming-edges n)))
                       (j-reverse e graph t)
                       (set-property graph layered-internal-properties.cyclic t)
                       ;; insert proper dummy nodes for the newly created long edge
                       (cutting-utils.insert-dummies graph e 1))))))
             ;; move to the next layer
             (incf new-index)
             (incf index))
    ;; remove old layers
    (jl-remove-if (get-layers graph) (lambda (l) (zerop (length (get-nodes l)))))))
