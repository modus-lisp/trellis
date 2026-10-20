;;;; topdownpacking/topdownpacking.lisp -- org.eclipse.elk.alg.topdownpacking

(in-package #:elk)

;;; ---------------------------------------------------------------- Grid

(defjgeneric put 3)
(defjgeneric get-row 1)
(defjgeneric get-column 1)
(defjgeneric get-columns 0)
(defjgeneric get-rows 0)
(defjgeneric set-grid-size 2)

;;; ---------------------------------------------------------------- GridElkNode

(defjctor grid-elk-node (elk-node)
  (setf (.node this) elk-node))

(defun %gen-oob (this col row)
  (jthrow 'index-out-of-bounds-exception
          "The grid has a size of (~A x ~A). The requested index of (~A x ~A) is out of bounds."
          (.cols this) (.rows this) col row))

(defjmethod put ((this grid-elk-node) col row item)
  (when (or (>= col (.cols this)) (>= row (.rows this)))
    (%gen-oob this col row))
  ;; Java: grid.get(row).add(col, item) -- an INSERT, kept as in ELK.
  (jl-add-at (jl-get (.grid this) row) col item))

(defjmethod j-get ((this grid-elk-node) col row)
  (when (or (>= col (.cols this)) (>= row (.rows this)))
    (%gen-oob this col row))
  (jl-get (jl-get (.grid this) row) col))

(defjmethod get-row ((this grid-elk-node) row)
  (when (>= row (.rows this))
    (jthrow 'index-out-of-bounds-exception
            "The grid has a size of (~A x ~A). The requested row ~A) is out of bounds."
            (.cols this) (.rows this) row))
  (jl-get (.grid this) row))

(defjmethod get-column ((this grid-elk-node) col)
  (when (>= col (.cols this))
    (jthrow 'index-out-of-bounds-exception
            "The grid has a size of (~A x ~A). The requested column ~A) is out of bounds."
            (.cols this) (.rows this) col))
  (let ((result (new-jlist)))
    (dotimes (i (.rows this))
      (jl-add result (jl-get (jl-get (.grid this) i) col)))
    result))

(defjmethod get-columns ((this grid-elk-node)) (.cols this))
(defjmethod get-rows ((this grid-elk-node)) (.rows this))

(defjmethod set-grid-size ((this grid-elk-node) cols rows)
  (setf (.cols this) cols
        (.rows this) rows
        (.grid this) (new-jlist))
  (dotimes (i rows)
    (let ((r (new-jlist)))
      (jl-add (.grid this) r)
      (dotimes (j cols) (jl-add r nil)))))

;; ElkNode delegation
(defjmethod get-outgoing-edges ((this grid-elk-node)) (get-outgoing-edges (.node this)))
(defjmethod get-incoming-edges ((this grid-elk-node)) (get-incoming-edges (.node this)))
(defjmethod get-height ((this grid-elk-node)) (get-height (.node this)))
(defjmethod set-height ((this grid-elk-node) v) (set-height (.node this) v))
(defjmethod get-width ((this grid-elk-node)) (get-width (.node this)))
(defjmethod set-width ((this grid-elk-node) v) (set-width (.node this) v))
(defjmethod get-x ((this grid-elk-node)) (get-x (.node this)))
(defjmethod set-x ((this grid-elk-node) v) (set-x (.node this) v))
(defjmethod get-y ((this grid-elk-node)) (get-y (.node this)))
(defjmethod set-y ((this grid-elk-node) v) (set-y (.node this) v))
(defjmethod set-dimensions ((this grid-elk-node) w h) (set-dimensions (.node this) w h))
(defjmethod set-location ((this grid-elk-node) x y) (set-location (.node this) x y))
(defjmethod get-labels ((this grid-elk-node)) (get-labels (.node this)))
(defjmethod get-identifier ((this grid-elk-node)) (get-identifier (.node this)))
(defjmethod set-identifier ((this grid-elk-node) v) (set-identifier (.node this) v))
(defjmethod j-get-properties ((this grid-elk-node)) (j-get-properties (.node this)))
(defjmethod set-property ((this grid-elk-node) p v) (set-property (.node this) p v))
(defjmethod get-property ((this grid-elk-node) p) (get-property (.node this) p))
(defjmethod has-property ((this grid-elk-node) p) (has-property (.node this) p))
(defjmethod copy-properties ((this grid-elk-node) h) (copy-properties (.node this) h))
(defjmethod get-all-properties ((this grid-elk-node)) (get-all-properties (.node this)))
(defjmethod get-ports ((this grid-elk-node)) (get-ports (.node this)))
(defjmethod get-children ((this grid-elk-node)) (get-children (.node this)))
(defjmethod get-parent ((this grid-elk-node)) (get-parent (.node this)))
(defjmethod set-parent ((this grid-elk-node) v) (set-parent (.node this) v))
(defjmethod get-contained-edges ((this grid-elk-node)) (get-contained-edges (.node this)))
(defjmethod is-hierarchical ((this grid-elk-node)) (is-hierarchical (.node this)))
(defjmethod e-container ((this grid-elk-node)) (e-container (.node this)))
(defmethod %property-table ((h grid-elk-node)) (%property-table (.node h)))

;;; ---------------------------------------------------------------- LeftRightTopDownNodePlacer

(defun %tdp-rows (n cols)
  (if (or (> n (- (* cols cols) cols)) (= cols 0)) cols (- cols 1)))

(defjmethod process ((this left-right-top-down-node-placer) layout-graph progress-monitor)
  (begin progress-monitor "Node placement" 1)
  (let* ((padding (get-property layout-graph topdownpacking-options.padding))
         (node-node-spacing (get-property layout-graph topdownpacking-options.spacing-node-node))
         (graph-size (get-predicted-size this layout-graph)))
    (set-dimensions layout-graph
                    (math.max (get-width layout-graph) (.x graph-size))
                    (math.max (get-height layout-graph) (.y graph-size)))
    (let* ((nodes (jlist-from (get-children layout-graph)))
           (cols (to-int (math.ceil (math.sqrt (length nodes)))))
           (rows (%tdp-rows (length nodes) cols)))
      (set-grid-size layout-graph cols rows)
      (done progress-monitor)
      (let ((curr-x (.left padding))
            (curr-y (.top padding))
            (current-col 0)
            (current-row 0)
            (desired-node-width (get-property layout-graph core-options.topdown-hierarchical-node-width))
            (aspect-ratio (get-property layout-graph core-options.topdown-hierarchical-node-aspect-ratio)))
        (jdo (node nodes)
          (set-dimensions node desired-node-width (/ desired-node-width aspect-ratio))
          (set-x node curr-x)
          (set-y node curr-y)
          (put layout-graph current-col current-row node)
          (incf curr-x (+ (get-width node) node-node-spacing))
          (incf current-col)
          (when (>= current-col cols)
            (setf curr-x (.left padding))
            (incf curr-y (+ (/ desired-node-width aspect-ratio) node-node-spacing))
            (setf current-col 0)
            (incf current-row))))))
  (done progress-monitor))

(defjmethod get-layout-processor-configuration ((this left-right-top-down-node-placer) graph)
  (declare (ignore graph))
  (layout-processor-configuration.create))

(defjmethod get-predicted-size ((this left-right-top-down-node-placer) graph)
  (let* ((number-of-children (size (get-children graph)))
         (padding (get-property graph topdownpacking-options.padding))
         (node-node-spacing (get-property graph topdownpacking-options.spacing-node-node))
         (hw (get-property graph core-options.topdown-hierarchical-node-width))
         (har (get-property graph core-options.topdown-hierarchical-node-aspect-ratio))
         (cols (to-int (math.ceil (math.sqrt number-of-children))))
         (required-width (+ (* cols hw) (.left padding) (.right padding) (* (- cols 1) node-node-spacing)))
         (rows (%tdp-rows number-of-children cols))
         (required-height (+ (/ (* rows hw) har) (.top padding) (.bottom padding)
                             (* (- rows 1) node-node-spacing))))
    (new-k-vector (jd required-width) (jd required-height))))

;;; ---------------------------------------------------------------- BottomRowEqualWhitespaceEliminator

(defjmethod process ((this bottom-row-equal-whitespace-eliminator) layout-graph progress-monitor)
  (begin progress-monitor "Whitespace elimination" 1)
  (when (or (= (get-width layout-graph) 0) (= (get-columns layout-graph) 0))
    (done progress-monitor)
    (return-from process/2 nil))
  (let ((padding (get-property layout-graph core-options.padding)))
    (dotimes (i (get-rows layout-graph))
      (let* ((row (get-row layout-graph i))
             (last nil)
             (last-index (length row)))
        (loop while (null last) do (setf last (jl-get row (decf last-index))))
        (let ((right-border (+ (get-x last) (get-width last))))
          (when (< (+ right-border (.right padding)) (get-width layout-graph))
            (let* ((extra-space (- (get-width layout-graph) (+ right-border (.right padding))))
                   (per-node (/ extra-space (+ last-index 1)))
                   (shift 0.0))
              (loop for j from 0 to last-index
                    for node = (jl-get row j)
                    do (set-x node (+ (get-x node) shift))
                       (set-width node (+ (get-width node) per-node))
                       (incf shift per-node)))))))
    (let* ((col (get-column layout-graph 0))
           (last (jl-get col (- (length col) 1)))
           (bottom-border (+ (get-y last) (get-height last)))
           (extra-space (- (get-height layout-graph) (+ bottom-border (.bottom padding))))
           (per-node (/ extra-space (+ (length col) 1)))
           (shift 0.0))
      (when (< (+ bottom-border (.bottom padding)) (get-height layout-graph))
        (dotimes (i (get-rows layout-graph))
          (block row
            (jdo (node (get-row layout-graph i))
              (when (null node) (return-from row))
              (set-y node (+ (get-y node) shift))
              (set-height node (+ (get-height node) per-node))
              (incf shift per-node)))))))
  (done progress-monitor))

(defjmethod get-layout-processor-configuration ((this bottom-row-equal-whitespace-eliminator) graph)
  (declare (ignore graph))
  (layout-processor-configuration.create))

;;; ---------------------------------------------------------------- strategies (enum methods)

(defjmethod create ((this node-arrangement-strategy))
  (enum-case this
    (node-arrangement-strategy.left-right-top-down-node-placer (new-left-right-top-down-node-placer))
    (t (jthrow 'illegal-argument-exception
               "No implementation is available for the node placer ~A" (name this)))))

(defjmethod create ((this whitespace-elimination-strategy))
  (enum-case this
    (whitespace-elimination-strategy.bottom-row-equal-whitespace-eliminator
     (new-bottom-row-equal-whitespace-eliminator))
    (t (jthrow 'illegal-argument-exception
               "No implementation is available for the whitespace eliminator~A" (name this)))))

;;; ---------------------------------------------------------------- TopdownpackingLayoutProvider

(defjmethod layout ((this topdownpacking-layout-provider) layout-graph progress-monitor)
  (let* ((wrapped (new-grid-elk-node layout-graph))
         (algorithm (assemble-algorithm this wrapped)))
    (begin progress-monitor "Topdown Packing" (size algorithm))
    (jdo (processor algorithm)
      (process processor wrapped (sub-task progress-monitor 1)))
    (done progress-monitor)))

(defjmethod assemble-algorithm ((this topdownpacking-layout-provider) graph)
  (let ((asm (.algorithm-assembler this)))
    (reset asm)
    (set-phase asm topdown-packing-phases.p1-node-arrangement
               (get-property graph topdownpacking-options.node-arrangement-strategy))
    (set-phase asm topdown-packing-phases.p2-whitespace-elimination
               (get-property graph topdownpacking-options.whitespace-elimination-strategy))
    (build asm graph)))

(defjmethod get-predicted-graph-size ((this topdownpacking-layout-provider) graph)
  (get-predicted-size (create (get-property graph topdownpacking-options.node-arrangement-strategy))
                      graph))
