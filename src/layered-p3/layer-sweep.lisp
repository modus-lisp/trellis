;;;; layer-sweep.lisp -- org.eclipse.elk.alg.layered.p3order:
;;;; LayerSweepCrossingMinimizer, InteractiveCrossingMinimizer, NoCrossingMinimizer.

(in-package #:elk)

;;; ---------------------------------------------------------------- helpers

(defmacro %lsc-or! (place expr)
  "Java's PLACE |= EXPR: EXPR is always evaluated (no short-circuit)."
  (let ((b (gensym "B")))
    `(let ((,b ,expr)) (setf ,place (or ,place ,b)))))

(defun %lsc-first-index (forward length) (if forward 0 (1- length)))
(defun %lsc-end-index (forward length) (if forward (1- length) 0))
(defun %lsc-first-free (forward length) (if forward 1 (- length 2)))
(defun %lsc-next (forward) (if forward 1 -1))
(defun %lsc-is-not-end (length index forward)
  (if forward (< index length) (>= index 0)))

(defun %lsc-intermediate-config ()
  "INTERMEDIATE_PROCESSING_CONFIGURATION of LayerSweepCrossingMinimizer (a fresh copy)."
  (add-after
   (add-before (add-before (layout-processor-configuration.create)
                           layered-phases.p3-node-ordering
                           layered-intermediate-processor-strategy.long-edge-splitter)
               layered-phases.p4-node-placement
               layered-intermediate-processor-strategy.in-layer-constraint-processor)
   layered-phases.p5-edge-routing
   layered-intermediate-processor-strategy.long-edge-joiner))

;;; Java private predicates of LayerSweepCrossingMinimizer (static or instance; no state used).
(defun %lsc-has-nested-graph (node) (not (null (get-nested-graph node))))
(defun %lsc-is-external-port-dummy (node) (eq (get-type node) node-type.external-port))
(defun %lsc-origin-port (node) (get-property node layered-internal-properties.origin))
(defun %lsc-is-hierarchical (port) (get-property port layered-internal-properties.inside-connections))
(defun %lsc-dummy-node-for (port) (get-property port layered-internal-properties.port-dummy))
(defun %lsc-is-on-end-of-sweep-side (port forward)
  (if forward (eq (get-side port) port-side.east) (eq (get-side port) port-side.west)))
(defun %lsc-side-opposed-sweep-direction (forward) (if forward port-side.west port-side.east))

;;; ---------------------------------------------------------------- LayerSweepCrossingMinimizer

(defjctor layer-sweep-crossing-minimizer (cross-min-type-arg)
  (setf (.cross-min-type this) cross-min-type-arg))

(defjmethod process ((this layer-sweep-crossing-minimizer) layered-graph progress-monitor)
  (begin progress-monitor (jstr "Minimize Crossings " (name (.cross-min-type this))) 1)
  ;; Short-circuit cases in which no crossings can be minimized. Note that in-layer edges may be subject to
  ;; crossing minimization if |layers| = 1 and that hierarchical crossing minimization may dive into a graph with
  ;; a single node. There can be graphs that consist only of empty layers, for example due to inside self-loops
  ;; with unconnected ports (see #298)
  (let* ((layers (get-layers layered-graph))
         (empty-graph (or (is-empty layers)
                          (every (lambda (layer) (is-empty (get-nodes layer))) layers)))
         (single-node (and (= (jsize layers) 1)
                           (= (jsize (get-nodes (j-get layers 0))) 1)))
         (hierarchical-layout
           (eq (get-property layered-graph layered-options.hierarchy-handling)
               hierarchy-handling.include-children)))
    (unless (or empty-graph (and single-node (not hierarchical-layout)))
      (let* ((graphs-to-sweep-on (initialize this layered-graph))
             (minimizing-method (choose-minimizing-method this graphs-to-sweep-on)))
        (minimize-crossings this graphs-to-sweep-on minimizing-method)
        (%lsc-transfer-node-and-port-orders-to-graph-all this))))
  (done progress-monitor))

(defjmethod choose-minimizing-method ((this layer-sweep-crossing-minimizer) graphs-to-sweep-on)
  (let ((parent (j-get graphs-to-sweep-on 0)))
    (cond ((not (cross-min-deterministic parent))
           (lambda (g) (compare-different-randomized-layouts this g)))
          ((cross-min-always-improves parent)
           (lambda (g) (minimize-crossings-no-counter this g)))
          (t
           (lambda (g) (minimize-crossings-with-counter this g))))))

(defjmethod minimize-crossings ((this layer-sweep-crossing-minimizer) graphs-to-sweep-on minimizing-method)
  (jdo (g-data graphs-to-sweep-on)
    (when (> (length (current-node-order g-data)) 0)
      (funcall minimizing-method g-data)
      (when (has-parent g-data)
        (set-port-order-on-parent-graph this g-data)))))

(defjmethod set-port-order-on-parent-graph ((this layer-sweep-crossing-minimizer) g-data)
  (when (and (has-external-ports g-data) (get-best-sweep g-data))
    (let ((best-sweep (get-best-sweep g-data)))
      ;; Sort ports on left and right side of the parent node
      (sort-ports-by-dummy-positions-in-last-layer this (nodes best-sweep) (parent g-data) t)
      (sort-ports-by-dummy-positions-in-last-layer this (nodes best-sweep) (parent g-data) nil)
      (set-property (parent g-data) layered-options.port-constraints port-constraints.fixed-order))))

(defjmethod minimize-crossings-no-counter ((this layer-sweep-crossing-minimizer) g-data)
  (let ((is-forward-sweep (next-boolean (.random this)))
        (improved t))
    (loop while improved do
      (setf improved nil)
      (setf improved (set-first-layer-order (cross-minimizer g-data) (current-node-order g-data) is-forward-sweep))
      (%lsc-or! improved (sweep-reducing-crossings this g-data is-forward-sweep nil))
      (setf is-forward-sweep (not is-forward-sweep)))
    (set-currently-best-node-orders this)))

(defjmethod compare-different-randomized-layouts ((this layer-sweep-crossing-minimizer) g-data)
  (let ((random (.random this))
        (l-graph (l-graph g-data)))
    ;; Reset the seed, otherwise copies of hierarchical graphs in different parent nodes are
    ;; layouted differently.
    (set-seed random (.random-seed this))
    ;; In order to only copy graphs whose node order has changed, save them in a set.
    (js-clear (.graphs-whose-node-order-changed this))
    (if (or (/= (get-property l-graph layered-options.consider-model-order-crossing-counter-node-influence) 0)
            (/= (get-property l-graph layered-options.consider-model-order-crossing-counter-node-influence) 0))
        (let ((best-crossings most-positive-double-float))
          (unless (eq (get-property l-graph layered-options.consider-model-order-strategy) ordering-strategy.none)
            ;; The first run should begin with a forward sweep.
            ;; If the order causes no additional crossings this preserves the current order.
            (set-property l-graph layered-internal-properties.first-try-with-initial-order t))
          (let ((thoroughness (get-property l-graph layered-options.thoroughness)))
            (dotimes (i thoroughness)
              (let ((crossings (minimize-crossings-node-port-order-with-counter this g-data)))
                (when (< crossings best-crossings)
                  (setf best-crossings crossings)
                  (save-all-node-orders-of-changed-graphs this)
                  (when (= best-crossings 0)
                    (return)))))))
        (let ((best-crossings integer.max-value))
          (unless (eq (get-property l-graph layered-options.consider-model-order-strategy) ordering-strategy.none)
            (set-property l-graph layered-internal-properties.first-try-with-initial-order t))
          (let ((thoroughness (get-property l-graph layered-options.thoroughness)))
            (dotimes (i thoroughness)
              (let ((crossings (minimize-crossings-with-counter this g-data)))
                (when (< crossings best-crossings)
                  (setf best-crossings crossings)
                  (save-all-node-orders-of-changed-graphs this)
                  (when (= best-crossings 0)
                    (return))))))))))

(defjmethod minimize-crossings-with-counter ((this layer-sweep-crossing-minimizer) g-data)
  (block minimize-crossings-with-counter-body
  (let* ((l-graph (l-graph g-data))
         (is-forward-sweep (next-boolean (.random this)))
         ;; If the first, initial ordering is already optimal, do not change anything.
         ;; If the initial order should be tried, model order should be active.
         ;; So if the initial ordering has no crossings, just do not change anything in the graph.
         ;; Normally a first sweep should change the order of not connected ports, which is bad.
         (initial-crossings (count-current-number-of-crossings this g-data)))
    (declare (ignorable initial-crossings))
    (when (and (= initial-crossings 0)
               (get-property l-graph layered-internal-properties.first-try-with-initial-order))
      ;; E.g. model order is already correct.
      (return-from minimize-crossings-with-counter-body 0))
    (if (or (and (not (get-property l-graph layered-internal-properties.first-try-with-initial-order))
                 (not (get-property l-graph layered-internal-properties.second-try-with-initial-order)))
            (eq (get-property l-graph layered-options.consider-model-order-strategy) ordering-strategy.none))
        (set-first-layer-order (cross-minimizer g-data) (current-node-order g-data) is-forward-sweep)
        (setf is-forward-sweep (get-property l-graph layered-internal-properties.first-try-with-initial-order)))
    (sweep-reducing-crossings this g-data is-forward-sweep t)
    (when (get-property l-graph layered-internal-properties.second-try-with-initial-order)
      (set-property l-graph layered-internal-properties.second-try-with-initial-order nil))
    (when (get-property l-graph layered-internal-properties.first-try-with-initial-order)
      (set-property l-graph layered-internal-properties.first-try-with-initial-order nil)
      (set-property l-graph layered-internal-properties.second-try-with-initial-order t))
    (let ((crossings-in-graph (count-current-number-of-crossings this g-data))
          (old-number-of-crossings 0))
      (loop
        (set-currently-best-node-orders this)
        (when (= crossings-in-graph 0)
          (return-from minimize-crossings-with-counter-body 0))
        (setf is-forward-sweep (not is-forward-sweep))
        (setf old-number-of-crossings crossings-in-graph)
        (sweep-reducing-crossings this g-data is-forward-sweep nil)
        (setf crossings-in-graph (count-current-number-of-crossings this g-data))
        (unless (> old-number-of-crossings crossings-in-graph)
          (return)))
      old-number-of-crossings))))

(defjmethod minimize-crossings-node-port-order-with-counter ((this layer-sweep-crossing-minimizer) g-data)
  (block minimize-crossings-node-port-order-with-counter-body
  (let* ((l-graph (l-graph g-data))
         (is-forward-sweep (next-boolean (.random this)))
         (initial-crossings (count-current-number-of-crossings-node-port-order this g-data)))
    (declare (ignorable initial-crossings))
    (when (and (= initial-crossings 0)
               (get-property l-graph layered-internal-properties.first-try-with-initial-order))
      (return-from minimize-crossings-node-port-order-with-counter-body 0d0))
    (if (or (and (not (get-property l-graph layered-internal-properties.first-try-with-initial-order))
                 (not (get-property l-graph layered-internal-properties.second-try-with-initial-order)))
            (eq (get-property l-graph layered-options.consider-model-order-strategy) ordering-strategy.none))
        (set-first-layer-order (cross-minimizer g-data) (current-node-order g-data) is-forward-sweep)
        (setf is-forward-sweep (get-property l-graph layered-internal-properties.first-try-with-initial-order)))
    (sweep-reducing-crossings this g-data is-forward-sweep t)
    (when (get-property l-graph layered-internal-properties.second-try-with-initial-order)
      (set-property l-graph layered-internal-properties.second-try-with-initial-order nil))
    (when (get-property l-graph layered-internal-properties.first-try-with-initial-order)
      (set-property l-graph layered-internal-properties.first-try-with-initial-order nil)
      (set-property l-graph layered-internal-properties.second-try-with-initial-order t))
    (let ((crossings-in-graph (count-current-number-of-crossings-node-port-order this g-data))
          (old-number-of-crossings 0d0))
      (loop
        (set-currently-best-node-orders this)
        (when (= crossings-in-graph 0)
          (return-from minimize-crossings-node-port-order-with-counter-body 0d0))
        (setf is-forward-sweep (not is-forward-sweep))
        (setf old-number-of-crossings crossings-in-graph)
        (sweep-reducing-crossings this g-data is-forward-sweep nil)
        (setf crossings-in-graph (count-current-number-of-crossings-node-port-order this g-data))
        (unless (> old-number-of-crossings crossings-in-graph)
          (return)))
      old-number-of-crossings))))

(defjmethod count-model-order-node-changes ((this layer-sweep-crossing-minimizer) graph layers strategy cm-group-order-strategy)
  (let ((previous-layer -1)
        (wrong-model-order 0))
    (loop for layer across layers do
      (let ((comp (new-model-order-node-comparator
                   graph
                   (if (= previous-layer -1) (aref layers 0) (aref layers previous-layer))
                   strategy long-edge-ordering-strategy.equal cm-group-order-strategy nil)))
        (loop for i from 0 below (length layer) do
          (loop for j from (1+ i) below (length layer) do
            (when (and (has-property (aref layer i) layered-internal-properties.model-order)
                       (has-property (aref layer j) layered-internal-properties.model-order)
                       (> (compare comp (aref layer i) (aref layer j)) 0))
              (incf wrong-model-order)))))
      (incf previous-layer))
    wrong-model-order))

(defjmethod count-model-order-port-changes ((this layer-sweep-crossing-minimizer) graph layers group-order-strategy)
  (declare (ignore group-order-strategy))
  (let ((previous-layer -1)
        (wrong-model-order 0))
    (loop for layer across layers do
      (loop for l-node across layer do
        (let ((comp (new-model-order-port-comparator
                     graph
                     (if (= previous-layer -1) (aref layers 0) (aref layers previous-layer))
                     (get-property (get-graph l-node) layered-options.consider-model-order-strategy)
                     (sort-by-input-model-processor.long-edge-target-node-preprocessing l-node)
                     (get-property (get-graph l-node) layered-options.consider-model-order-port-model-order))))
          (loop for i from 0 below (size (get-ports l-node)) do
            (loop for j from (1+ i) below (size (get-ports l-node)) do
              (when (> (compare comp (j-get (get-ports l-node) i) (j-get (get-ports l-node) j)) 0)
                (incf wrong-model-order))))))
      (incf previous-layer))
    wrong-model-order))

(defjmethod count-current-number-of-crossings ((this layer-sweep-crossing-minimizer) current-graph)
  (let ((total-crossings 0)
        (stack (list current-graph)))
    (loop while stack do
      (let ((gd (pop stack)))
        (incf total-crossings (count-all-crossings (cross-counter gd) (current-node-order gd)))
        (jdo (child-l-graph (child-graphs gd))
          (let ((child (j-get (.graph-info-holders this) (.id child-l-graph))))
            (unless (dont-sweep-into child)
              (incf total-crossings (count-current-number-of-crossings this child)))))))
    total-crossings))

(defjmethod count-current-number-of-crossings-node-port-order ((this layer-sweep-crossing-minimizer) current-graph)
  (let ((total-crossings 0d0)
        (stack (list current-graph)))
    (loop while stack do
      (let* ((gd (pop stack))
             (model-order-influence 0d0)
             ;; Additionally add changes to the model order to the number of crossings.
             ;; The influence of port and node order can be configured.
             (model-order-strategy
               (get-property (l-graph current-graph) layered-options.consider-model-order-strategy))
             (cm-group-order-strategy
               (get-property (l-graph current-graph)
                             layered-options.consider-model-order-group-model-order-cm-group-order-strategy))
             (crossing-counter-node-influence
               (get-property (l-graph current-graph)
                             layered-options.consider-model-order-crossing-counter-node-influence))
             (crossing-counter-port-influence
               (get-property (l-graph current-graph)
                             layered-options.consider-model-order-crossing-counter-port-influence)))
        (unless (eq model-order-strategy ordering-strategy.none)
          (incf model-order-influence
                (* crossing-counter-node-influence
                   (count-model-order-node-changes this (l-graph current-graph) (current-node-order gd)
                                                   model-order-strategy cm-group-order-strategy)))
          (incf model-order-influence
                (* crossing-counter-port-influence
                   (count-model-order-port-changes this (l-graph current-graph) (current-node-order gd)
                                                   cm-group-order-strategy))))
        (incf total-crossings (+ (count-all-crossings (cross-counter gd) (current-node-order gd))
                                 model-order-influence))
        (jdo (child-l-graph (child-graphs gd))
          (let ((child (j-get (.graph-info-holders this) (.id child-l-graph))))
            (unless (dont-sweep-into child)
              (incf total-crossings (count-current-number-of-crossings this child)))))))
    total-crossings))

(defjmethod sweep-reducing-crossings ((this layer-sweep-crossing-minimizer) graph forward first-sweep)
  (let* ((nodes (current-node-order graph))
         (length (length nodes))
         (improved (distribute-ports-while-sweeping (port-distributor graph)
                                                    nodes (%lsc-first-index forward length) forward)))
    (let ((first-layer (aref nodes (%lsc-first-index forward length))))
      (%lsc-or! improved (sweep-in-hierarchical-nodes this first-layer forward first-sweep)))
    (let ((i (%lsc-first-free forward length)))
      (loop while (%lsc-is-not-end length i forward) do
      (%lsc-or! improved
                (minimize-crossings (cross-minimizer graph) nodes i forward
                                    (and first-sweep
                                         (not (get-property (l-graph graph)
                                                            layered-internal-properties.first-try-with-initial-order))
                                         (not (get-property (l-graph graph)
                                                            layered-internal-properties.second-try-with-initial-order)))))
      (%lsc-or! improved (distribute-ports-while-sweeping (port-distributor graph) nodes i forward))
      (%lsc-or! improved (sweep-in-hierarchical-nodes this (aref nodes i) forward first-sweep))
      (incf i (%lsc-next forward))))
    (js-add (.graphs-whose-node-order-changed this) graph)
    improved))

(defjmethod sweep-in-hierarchical-nodes ((this layer-sweep-crossing-minimizer) layer is-forward-sweep is-first-sweep)
  (let ((improved nil))
    (loop for node across layer do
      (when (and (%lsc-has-nested-graph node)
                 (not (dont-sweep-into (j-get (.graph-info-holders this) (.id (get-nested-graph node))))))
        (%lsc-or! improved (sweep-in-hierarchical-node this is-forward-sweep node is-first-sweep))))
    improved))

(defjmethod sweep-in-hierarchical-node ((this layer-sweep-crossing-minimizer) is-forward-sweep node is-first-sweep)
  (let* ((nested-l-graph (get-nested-graph node))
         (nested-graph (j-get (.graph-info-holders this) (.id nested-l-graph)))
         (nested-graph-node-order (current-node-order nested-graph))
         (start-index (%lsc-first-index is-forward-sweep (length nested-graph-node-order)))
         (first-node (aref (aref nested-graph-node-order start-index) 0)))
    (if (%lsc-is-external-port-dummy first-node)
        (setf (aref nested-graph-node-order start-index)
              (sort-port-dummies-by-port-positions
               this node (aref nested-graph-node-order start-index)
               (%lsc-side-opposed-sweep-direction is-forward-sweep)))
        (set-first-layer-order (cross-minimizer nested-graph) nested-graph-node-order is-forward-sweep))
    (let ((improved (sweep-reducing-crossings this nested-graph is-forward-sweep is-first-sweep)))
      (sort-ports-by-dummy-positions-in-last-layer this (current-node-order nested-graph)
                                                   (parent nested-graph) is-forward-sweep)
      improved)))

(defjmethod sort-ports-by-dummy-positions-in-last-layer ((this layer-sweep-crossing-minimizer)
                                                         node-order parent on-right-most-layer)
  (let* ((end-index (%lsc-end-index on-right-most-layer (length node-order)))
         (last-layer (aref node-order end-index))
         ;; Check whether the node to check is an external port dummy.
         (j (%lsc-first-index on-right-most-layer (length last-layer))))
    (when (%lsc-is-external-port-dummy (aref last-layer j))
      (let ((ports (get-ports parent)))
        (loop for i from 0 below (size ports) do
          (let ((port (j-get ports i)))
            (when (and (%lsc-is-on-end-of-sweep-side port on-right-most-layer)
                       (%lsc-is-hierarchical port))
              ;; Only on external port dummy node has a port as its origin.
              (jl-set ports i (%lsc-origin-port (aref last-layer j)))
              (incf j (%lsc-next on-right-most-layer)))))))))

(defjmethod sort-port-dummies-by-port-positions ((this layer-sweep-crossing-minimizer)
                                                  parent-node layer-close-to-node-edge side)
  (let ((ports (cross-min-util.in-north-south-east-west-order parent-node side))
        (sorted-dummies (make-array (length layer-close-to-node-edge) :initial-element nil))
        (i 0))
    (jdo (port ports)
      (when (%lsc-is-hierarchical port)
        (setf (aref sorted-dummies i) (%lsc-dummy-node-for port))
        (incf i)))
    (when (< i (length layer-close-to-node-edge))
      (jthrow 'illegal-state-exception
              (jstr "Expected " (length layer-close-to-node-edge)
                    " hierarchical ports, but found only " i ".")))
    sorted-dummies))

(defjmethod save-all-node-orders-of-changed-graphs ((this layer-sweep-crossing-minimizer))
  (jdo (graph (.graphs-whose-node-order-changed this))
    (set-best-node-n-port-order graph (new-sweep-copy (currently-best-node-and-port-order graph)))))

(defjmethod set-currently-best-node-orders ((this layer-sweep-crossing-minimizer))
  (jdo (graph (.graphs-whose-node-order-changed this))
    (set-currently-best-node-and-port-order graph (new-sweep-copy (current-node-order graph)))))

(defjmethod initialize ((this layer-sweep-crossing-minimizer) root-graph)
  (setf (.graph-info-holders this) (new-jlist))
  (setf (.random this) (get-property root-graph layered-internal-properties.random))
  (setf (.random-seed this) (next-long (.random this)))
  (let ((graphs-to-sweep-on (new-jlist))
        (graphs (jlist root-graph))
        (i 0))
    (loop while (< i (size graphs)) do
      (let ((graph (j-get graphs i)))
        (setf (.id graph) i)
        (incf i)
        (let ((g-data (new-graph-info-holder graph (.cross-min-type this) (.graph-info-holders this))))
          (jl-add-all graphs (child-graphs g-data))
          (jl-add (.graph-info-holders this) g-data)
          (when (dont-sweep-into g-data)
            (jl-add-at graphs-to-sweep-on 0 g-data)))))
    (setf (.graphs-whose-node-order-changed this) (new-jset))
    graphs-to-sweep-on))

(defun %lsc-transfer-node-and-port-orders-to-graph-all (this)
  (jdo (gd (.graph-info-holders this))
    (let ((best-sweep (get-best-sweep gd)))
      (when best-sweep
        (transfer-node-and-port-orders-to-graph best-sweep (l-graph gd) t)))))

(defjmethod get-layout-processor-configuration ((this layer-sweep-crossing-minimizer) graph)
  (declare (ignore graph))
  (add-before (%lsc-intermediate-config)
              layered-phases.p3-node-ordering
              layered-intermediate-processor-strategy.port-list-sorter))

(defjmethod get-graph-data ((this layer-sweep-crossing-minimizer))
  (.graph-info-holders this))

;;; ---------------------------------------------------------------- InteractiveCrossingMinimizer

(defun %icm-intermediate-config ()
  (add-after
   (add-before (add-before (layout-processor-configuration.create)
                           layered-phases.p3-node-ordering
                           layered-intermediate-processor-strategy.long-edge-splitter)
               layered-phases.p4-node-placement
               layered-intermediate-processor-strategy.in-layer-constraint-processor)
   layered-phases.p5-edge-routing
   layered-intermediate-processor-strategy.long-edge-joiner))

(defjmethod get-layout-processor-configuration ((this interactive-crossing-minimizer) graph)
  (let ((configuration (layout-processor-configuration.create-from (%icm-intermediate-config))))
    (when (contains (get-property graph layered-internal-properties.graph-properties)
                    layered-graph-properties.non-free-ports)
      (add-before configuration layered-phases.p3-node-ordering
                  layered-intermediate-processor-strategy.port-list-sorter))
    configuration))

(defjmethod process ((this interactive-crossing-minimizer) layered-graph monitor)
  (begin monitor "Interactive crossing minimization" 1)
  ;; Set ID's for each layer since they will be used by the port distribution code to index into arrays
  (let ((layer-index 0))
    (jdo (layer (get-layers layered-graph))
      (setf (.id layer) layer-index)
      (incf layer-index)))
  (let* ((node-order (to-node-array layered-graph))
         (port-distributor (new-node-relative-port-distributor (length node-order)))
         (port-count 0)
         (layer-index 0))
    (i-initializable.init (jlist port-distributor) node-order)
    (jdo (layer (get-layers layered-graph))
      ;; determine a horizontal position for edge bend points comparison
      (let ((horiz-pos 0d0)
            (node-count 0))
        (jdo (node (get-nodes layer))
          (when (> (.x (get-position node)) 0)
            (incf horiz-pos (+ (.x (get-position node)) (/ (.x (get-size node)) 2)))
            (incf node-count))
          (jdo (port (get-ports node))
            (setf (.id port) port-count)
            (incf port-count)))
        (when (> node-count 0)
          (setf horiz-pos (/ horiz-pos node-count)))
        ;; create an array of vertical node positions
        (let ((pos (make-array (size (get-nodes layer)) :initial-element 0d0))
              (next-index 0))
          (jdo (node (get-nodes layer))
            (setf (.id node) next-index)
            (incf next-index)
            (setf (aref pos (.id node)) (%icm-get-pos node horiz-pos))
            ;; if we have a long edge dummy node, save the calculated position in a property
            ;; to be used by the interactive node placer (for dummy nodes other than long edge
            ;; dummies, we haven't calculated meaningful positions)
            (when (eq (get-type node) node-type.long-edge)
              (set-property node layered-internal-properties.original-dummy-node-position
                            (aref pos (.id node)))))
          ;; sort the nodes using the position array
          (jl-sort (get-nodes layer)
                   (lambda (node1 node2)
                     (let ((compare (double.compare (aref pos (.id node1)) (aref pos (.id node2)))))
                       (if (= compare 0)
                           ;; The two nodes have the same y coordinate. Check for node successor
                           ;; constraints
                           (let ((node1-successors
                                   (get-property node1 layered-internal-properties.in-layer-successor-constraints))
                                 (node2-successors
                                   (get-property node2 layered-internal-properties.in-layer-successor-constraints)))
                             (cond ((contains node1-successors node2) -1)
                                   ((contains node2-successors node1) 1)
                                   (t compare)))
                           compare)))))
        (distribute-ports-while-sweeping port-distributor node-order layer-index t)
        (incf layer-index))))
  (done monitor))

(defun %icm-get-pos (node horiz-pos)
  (block get-pos
    (enum-case (get-type node)
      (node-type.long-edge
       (let* ((edge (get-property node layered-internal-properties.origin))
              ;; reconstruct the original bend points from the node annotations
              (bendpoints (get-property edge layered-internal-properties.original-bendpoints)))
         (cond ((null bendpoints) (setf bendpoints (new-k-vector-chain)))
               ((get-property edge layered-internal-properties.reversed)
                (setf bendpoints (k-vector-chain.reverse bendpoints))))
         ;; Check if we can determine the position just by using the source point, if we can determine it
         (let ((source (get-property node layered-internal-properties.long-edge-source)))
           (when source
             (let ((source-point (get-absolute-anchor source)))
               (when (<= horiz-pos (.x source-point))
                 (return-from get-pos (.y source-point)))
               (add-first bendpoints source-point))))
         ;; Check if we can determine the position just by using the target point
         (let ((target (get-property node layered-internal-properties.long-edge-target)))
           (when target
             (let ((target-point (get-absolute-anchor target)))
               (when (<= (.x target-point) horiz-pos)
                 (return-from get-pos (.y target-point)))
               (add-last bendpoints target-point))))
         ;; Find the two points along the edge that the horizontal point lies between
         (when (>= (size bendpoints) 2)
           (let* ((points (new-jlist)))
             (jdo (p bendpoints) (jl-add points p))
             (let* ((idx 2)
                    (point1 (j-get points 0))
                    (point2 (j-get points 1)))
               (loop while (and (< (.x point2) horiz-pos) (< idx (size points))) do
                 (setf point1 point2)
                 (setf point2 (j-get points idx))
                 (incf idx))
               (return-from get-pos
                 (+ (.y point1)
                    (* (/ (- horiz-pos (.x point1)) (- (.x point2) (.x point1)))
                       (- (.y point2) (.y point1))))))))))
      (node-type.north-south-port
       ;; Get one of the ports the dummy node was created for, and its original node
       (let* ((origin-port (get-property (j-get (get-ports node) 0) layered-internal-properties.origin))
              (origin-node (get-node origin-port)))
         (enum-case (get-side origin-port)
           (port-side.north
            ;; Use the position of the node's northern side. This causes northern dummies to
            ;; have the same y coordinate as the node they were created from if TOP_LEFT is
            ;; used as the anchor point. We solve this when sorting the nodes by y coordinate
            ;; by respecting node successor constraints.
            (return-from get-pos (.y (get-position origin-node))))
           (port-side.south
            ;; Use the position of the node's southern side
            (return-from get-pos (+ (.y (get-position origin-node)) (.y (get-size origin-node)))))))))
    ;; the fallback solution is to take the previous position of the node's anchor point
    (.y (get-interactive-reference-point node))))

;;; ---------------------------------------------------------------- NoCrossingMinimizer

(defjmethod process ((this no-crossing-minimizer) graph progress-monitor)
  (declare (ignore graph))
  (begin progress-monitor "No crossing minimization" 1)
  (done progress-monitor))

(defjmethod get-layout-processor-configuration ((this no-crossing-minimizer) graph)
  (declare (ignore graph))
  (add-before (%lsc-intermediate-config)
              layered-phases.p3-node-ordering
              layered-intermediate-processor-strategy.port-list-sorter))
