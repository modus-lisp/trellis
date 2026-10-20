;;;; p1cycles.lisp -- org.eclipse.elk.alg.layered.p1cycles: the cycle breakers.

(in-package #:elk)

;;; ======================================================== GreedyCycleBreaker

(defjmethod get-layout-processor-configuration ((this greedy-cycle-breaker) graph)
  (declare (ignore graph))
  (%p1p2-cycle-config))

(defjmethod process ((this greedy-cycle-breaker) layered-graph monitor)
  (begin monitor "Greedy cycle removal" 1)
  (setf (.layered-graph this) layered-graph)
  (let* ((nodes (get-layerless-nodes layered-graph))
         ;; initialize values for the algorithm (sum of priorities of incoming edges and outgoing
         ;; edges per node, and the ordering calculated for each node)
         (unprocessed-node-count (length nodes))
         (indeg (%p1p2-int-array unprocessed-node-count))
         (outdeg (%p1p2-int-array unprocessed-node-count))
         (mark (%p1p2-int-array unprocessed-node-count))
         (index 0))
    (setf (.indeg this) indeg (.outdeg this) outdeg (.mark this) mark)
    (jdo (node nodes)
      ;; the node id is used as index for the indeg, outdeg, and mark arrays
      (setf (.id node) index)
      (jdo (port (get-ports node))
        ;; calculate the sum of edge priorities
        (jdo (edge (get-incoming-edges port))
          ;; ignore self-loops
          (unless (eq (get-node (get-source edge)) node)
            (let ((priority (get-property edge layered-options.priority-direction)))
              (incf (aref indeg index) (if (> priority 0) (+ priority 1) 1)))))
        (jdo (edge (get-outgoing-edges port))
          (unless (eq (get-node (get-target edge)) node)
            (let ((priority (get-property edge layered-options.priority-direction)))
              (incf (aref outdeg index) (if (> priority 0) (+ priority 1) 1))))))
      ;; collect sources and sinks
      (cond ((= (aref outdeg index) 0) (jl-add (.sinks this) node))
            ((= (aref indeg index) 0) (jl-add (.sources this) node)))
      (incf index))
    ;; next rank values used for sinks and sources (from right and from left)
    (let ((next-right -1) (next-left 1)
          (max-nodes (new-jlist)))
      (setf (.random this) (get-property layered-graph layered-internal-properties.random))
      (loop while (> unprocessed-node-count 0) do
        ;; sinks are put to the right --> assign negative rank, which is later shifted to positive
        (loop until (jl-is-empty (.sinks this)) do
          (let ((sink (jl-remove-first (.sinks this))))
            (setf (aref mark (.id sink)) next-right)
            (decf next-right)
            (update-neighbors this sink)
            (decf unprocessed-node-count)))
        ;; sources are put to the left --> assign positive rank
        (loop until (jl-is-empty (.sources this)) do
          (let ((source (jl-remove-first (.sources this))))
            (setf (aref mark (.id source)) next-left)
            (incf next-left)
            (update-neighbors this source)
            (decf unprocessed-node-count)))
        ;; while there are unprocessed nodes left that are neither sinks nor sources...
        (when (> unprocessed-node-count 0)
          (let ((max-outflow integer.min-value))
            ;; find the set of unprocessed node (=> mark == 0), with the largest out flow
            (jdo (node nodes)
              (when (= (aref mark (.id node)) 0)
                (let ((outflow (- (aref outdeg (.id node)) (aref indeg (.id node)))))
                  (when (>= outflow max-outflow)
                    (when (> outflow max-outflow)
                      (jl-clear max-nodes)
                      (setf max-outflow outflow))
                    (jl-add max-nodes node)))))
            ;; randomly select a node from the ones with maximal outflow and put it left
            (let ((max-node (choose-node-with-max-outflow this max-nodes)))
              (setf (aref mark (.id max-node)) next-left)
              (incf next-left)
              (update-neighbors this max-node)
              (decf unprocessed-node-count))))))
    ;; shift negative ranks to positive; this applies to sinks of the graph
    (let ((shift-base (+ (length nodes) 1)))
      (loop for i from 0 below (length nodes)
            when (< (aref mark i) 0) do (incf (aref mark i) shift-base)))
    ;; reverse edges that point left
    (jdo (node nodes)
      (jdo (port (l-graph-util.to-port-array (get-ports node)))
        (jdo (edge (l-graph-util.to-edge-array (get-outgoing-edges port)))
          (let ((target-ix (.id (get-node (get-target edge)))))
            (when (> (aref mark (.id node)) (aref mark target-ix))
              (j-reverse edge layered-graph t)
              (set-property layered-graph layered-internal-properties.cyclic t))))))
    (dispose this)
    (done monitor)))

(defjmethod choose-node-with-max-outflow ((this greedy-cycle-breaker) nodes)
  "Choose a node among all nodes with the same maximum outflow; randomly by default."
  (jl-get nodes (next-int (.random this) (length nodes))))

(defjmethod dispose ((this greedy-cycle-breaker))
  (setf (.indeg this) nil (.outdeg this) nil (.mark this) nil)
  (jl-clear (.sources this))
  (jl-clear (.sinks this)))

(defjmethod update-neighbors ((this greedy-cycle-breaker) node)
  "Updates indegree and outdegree values of the neighbors of NODE, simulating its removal."
  (let ((mark (.mark this)) (indeg (.indeg this)) (outdeg (.outdeg this)))
    (jdo (port (get-ports node))
      (jdo (edge (get-connected-edges port))
        (let* ((connected-port (if (eq (get-source edge) port) (get-target edge) (get-source edge)))
               (endpoint (get-node connected-port)))
          ;; exclude self-loops
          (unless (eq node endpoint)
            (let ((priority (get-property edge layered-options.priority-direction))
                  (index (.id endpoint)))
              (when (< priority 0) (setf priority 0))
              (when (= (aref mark index) 0)
                (if (eq (get-target edge) connected-port)
                    (progn
                      (decf (aref indeg index) (+ priority 1))
                      (when (and (<= (aref indeg index) 0) (> (aref outdeg index) 0))
                        (jl-add (.sources this) endpoint)))
                    (progn
                      (decf (aref outdeg index) (+ priority 1))
                      (when (and (<= (aref outdeg index) 0) (> (aref indeg index) 0))
                        (jl-add (.sinks this) endpoint))))))))))))

;;; ============================================== GreedyModelOrderCycleBreaker

(defjmethod choose-node-with-max-outflow ((this greedy-model-order-cycle-breaker) nodes)
  (let* ((return-node nil)
         (minimum-model-order integer.max-value)
         (lg (.layered-graph this))
         (offset (math.max (length (get-layerless-nodes lg))
                           (get-property lg layered-internal-properties.max-model-order-nodes)))
         (big-offset (* offset (get-property lg layered-internal-properties.cb-num-model-order-groups)))
         (mo-calculator (new-group-model-order-calculator))
         (enforce-group-model-order (%p1p2-enforce-group-model-order-p lg)))
    (jdo (node nodes)
      ;; In this step nodes without a model order are disregarded.
      (when (has-property node layered-internal-properties.model-order)
        (let ((model-order (if enforce-group-model-order
                               (compute-constraint-group-model-order mo-calculator node big-offset offset)
                               (compute-constraint-model-order mo-calculator node offset))))
          (when (> minimum-model-order model-order)
            (setf minimum-model-order model-order
                  return-node node)))))
    (if (null return-node)
        (call-next-method)
        return-node)))

;;; =================================================== DepthFirstCycleBreaker

(defjmethod get-layout-processor-configuration ((this depth-first-cycle-breaker) graph)
  (declare (ignore graph))
  (%p1p2-cycle-config))

(defjmethod process ((this depth-first-cycle-breaker) graph monitor)
  (begin monitor "Depth-first cycle removal" 1)
  (let* ((nodes (get-layerless-nodes graph))
         (node-count (length nodes))
         (index 0))
    (setf (.sources this) (new-jlist)
          (.visited this) (make-array node-count :initial-element nil)
          (.active this) (make-array node-count :initial-element nil)
          (.edges-to-be-reversed this) (new-jlist))
    (jdo (node nodes)
      ;; The node id is used as index into our arrays
      (setf (.id node) index)
      (when (is-empty (get-incoming-edges node))
        (jl-add (.sources this) node))
      (incf index))
    ;; From every source node start a DFS
    (jdo (source (.sources this))
      (dfs this source))
    ;; Start more DFS runs for all nodes that have not been visited yet. These must be part of a
    ;; cycle since they are not source nodes
    (loop for i from 0 below node-count
          unless (aref (.visited this) i)
            do (dfs this (jl-get nodes i)))
    ;; Reverse "back edges"
    (jdo (edge (.edges-to-be-reversed this))
      (j-reverse edge graph t)
      (set-property graph layered-internal-properties.cyclic t))
    ;; Cleanup
    (setf (.sources this) nil (.visited this) nil (.active this) nil (.edges-to-be-reversed this) nil)
    (done monitor)))

(defjmethod dfs ((this depth-first-cycle-breaker) n)
  (unless (aref (.visited this) (.id n))
    ;; We're now visiting the node, and it's active
    (setf (aref (.visited this) (.id n)) t
          (aref (.active this) (.id n)) t)
    (jdo (out (get-outgoing-edges n))
      ;; Ignore self loops
      (unless (is-self-loop out)
        (let ((target (get-node (get-target out))))
          ;; If the edge connects to an active node, we have found a path from said active node
          ;; back to itself: a backward edge that needs to be reversed
          (if (aref (.active this) (.id target))
              (jl-add (.edges-to-be-reversed this) out)
              (dfs this target)))))
    ;; We're leaving this node
    (setf (aref (.active this) (.id n)) nil)))

;;; ================================================== InteractiveCycleBreaker

(defjmethod get-layout-processor-configuration ((this interactive-cycle-breaker) graph)
  (declare (ignore graph))
  (add-after (add-before (layout-processor-configuration.create)
                         layered-phases.p1-cycle-breaking
                         layered-intermediate-processor-strategy.interactive-external-port-positioner)
             layered-phases.p5-edge-routing
             layered-intermediate-processor-strategy.reversed-edge-restorer))

(defjmethod process ((this interactive-cycle-breaker) layered-graph monitor)
  (begin monitor "Interactive cycle breaking" 1)
  ;; gather edges that point to the wrong direction
  (let ((rev-edges (new-jlist)))
    (jdo (source (get-layerless-nodes layered-graph))
      (setf (.id source) 1)
      (let ((sourcex (.x (get-interactive-reference-point source))))
        (jdo (port (get-ports source port-type.output))
          (jdo (edge (get-outgoing-edges port))
            (let ((target (get-node (get-target edge))))
              (unless (eq target source)
                (let ((targetx (.x (get-interactive-reference-point target))))
                  (when (< targetx sourcex)
                    (jl-add rev-edges edge)))))))))
    ;; reverse the gathered edges
    (jdo (edge rev-edges)
      (j-reverse edge layered-graph t))
    ;; perform an additional check for cycles - maybe we missed something
    ;; (could happen if some nodes have the same horizontal position)
    (jl-clear rev-edges)
    (jdo (node (get-layerless-nodes layered-graph))
      ;; unvisited nodes have id = 1
      (when (> (.id node) 0)
        (find-cycles this node rev-edges)))
    ;; again, reverse the edges that were marked
    (jdo (edge rev-edges)
      (j-reverse edge layered-graph t))
    (jl-clear rev-edges))
  (done monitor))

(defjmethod find-cycles ((this interactive-cycle-breaker) node1 rev-edges)
  ;; nodes with negative id are part of the currently inspected path
  (setf (.id node1) -1)
  (jdo (port (get-ports node1 port-type.output))
    (jdo (edge (get-outgoing-edges port))
      (let ((node2 (get-node (get-target edge))))
        (unless (eq node1 node2)
          (cond ((< (.id node2) 0)
                 ;; a node of the current path is found --> cycle
                 (jl-add rev-edges edge))
                ((> (.id node2) 0)
                 ;; the node has not been visited yet --> expand the current path
                 (find-cycles this node2 rev-edges)))))))
  ;; nodes with id = 0 have been already visited and are ignored if encountered again
  (setf (.id node1) 0))

;;; =================================================== ModelOrderCycleBreaker

(defjmethod get-layout-processor-configuration ((this model-order-cycle-breaker) graph)
  (declare (ignore graph))
  (%p1p2-cycle-config))

(defjmethod process ((this model-order-cycle-breaker) layered-graph monitor)
  (begin monitor "Model order cycle breaking" 1)
  ;; gather edges that point to the wrong direction
  (let* ((rev-edges (new-jlist))
         ;; One needs an offset to make sure that the model order of nodes with port constraints
         ;; is always lower/higher than that of other nodes.
         (offset (math.max (length (get-layerless-nodes layered-graph))
                           (get-property layered-graph layered-internal-properties.max-model-order-nodes)))
         (big-offset (* offset (get-property layered-graph layered-internal-properties.cb-num-model-order-groups)))
         (enforce-group-model-order (%p1p2-enforce-group-model-order-p layered-graph)))
    (jdo (source (get-layerless-nodes layered-graph))
      (let* ((calculator (new-group-model-order-calculator))
             (model-order-source (if enforce-group-model-order
                                     (compute-constraint-group-model-order calculator source big-offset offset)
                                     (compute-constraint-model-order calculator source offset))))
        (jdo (port (get-ports source port-type.output))
          (jdo (edge (get-outgoing-edges port))
            (let* ((target (get-node (get-target edge)))
                   (model-order-target
                     (if enforce-group-model-order
                         (compute-constraint-group-model-order calculator target big-offset offset)
                         (compute-constraint-model-order calculator target offset))))
              (when (< model-order-target model-order-source)
                (jl-add rev-edges edge)))))))
    ;; reverse the gathered edges
    (jdo (edge rev-edges)
      (j-reverse edge layered-graph t)
      (set-property layered-graph layered-internal-properties.cyclic t))
    (jl-clear rev-edges))
  (done monitor))

;;; ================================================= GroupModelOrderCalculator

(defun %p1p2-constraint-offset (calc node offset)
  (let ((model-order 0))
    (enum-case (get-property node layered-options.layering-layer-constraint)
      (layer-constraint.first-separate
       (setf model-order (+ (* 2 (- offset)) (.first-separate-nodes calc)))
       (incf (.first-separate-nodes calc)))
      (layer-constraint.first
       (setf model-order (- offset)))
      (layer-constraint.last
       (setf model-order offset))
      (layer-constraint.last-separate
       (setf model-order (+ (* 2 offset) (.last-separate-nodes calc)))
       (incf (.last-separate-nodes calc))))
    model-order))

(defjmethod compute-constraint-model-order ((this group-model-order-calculator) node offset)
  (let ((model-order (%p1p2-constraint-offset this node offset)))
    (when (has-property node layered-internal-properties.model-order)
      (incf model-order (get-property node layered-internal-properties.model-order)))
    model-order))

(defjmethod compute-constraint-group-model-order ((this group-model-order-calculator) node offset small-offset)
  (let ((model-order (%p1p2-constraint-offset this node offset)))
    (when (has-property node layered-internal-properties.model-order)
      (incf model-order
            (+ (* (get-property node layered-options.consider-model-order-group-model-order-cycle-breaking-id)
                  small-offset)
               (get-property node layered-internal-properties.model-order))))
    model-order))

(defjmethod reset-internal-counters ((this group-model-order-calculator))
  (setf (.first-separate-nodes this) 0
        (.last-separate-nodes this) 0))

;;; ================================== BFS/DFS node order cycle breakers (shared)

(defun %p1p2-model-order-edge-map (graph n)
  "The map model order -> set of outgoing edges of N that BFS/DFSNodeOrderCycleBreaker build.
Returns the hash table; its keys in ascending order are the TreeSet."
  (let ((model-order-map (new-jmap))
        (group-model-order (%p1p2-enforce-group-model-order-p graph)))
    (jdo (e (get-outgoing-edges n))
      (let ((target (get-node (get-target e))))
        (if (not (has-property target layered-internal-properties.model-order))
            ;; Handle edges that connect to nodes without model order.  They get a high unique
            ;; value such that the first such node is the last one and the second the second last.
            (jm-put model-order-map (- integer.max-value (hash-table-count model-order-map))
                    (jlist e))
            (let ((target-model-order
                    (if group-model-order
                        (+ (* (get-property graph layered-internal-properties.max-model-order-nodes)
                              (get-property target layered-options.consider-model-order-group-model-order-cycle-breaking-id))
                           (get-property target layered-internal-properties.model-order))
                        (get-property target layered-internal-properties.model-order))))
              (if (jm-contains-key model-order-map target-model-order)
                  (let ((set (gethash target-model-order model-order-map)))
                    (unless (find e set) (jl-add set e)))
                  (jm-put model-order-map target-model-order (jlist e)))))))
    model-order-map))

(defun %p1p2-sorted-keys (map)
  (sort (coerce (jm-keys map) 'list) #'<))

;;; ================================================= BFSNodeOrderCycleBreaker

(defjmethod get-layout-processor-configuration ((this bfs-node-order-cycle-breaker) graph)
  (declare (ignore graph))
  (%p1p2-cycle-config))

(defjmethod process ((this bfs-node-order-cycle-breaker) graph monitor)
  (begin monitor "Breadth-first cycle removal" 1)
  (setf (.graph this) graph)
  (let ((nodes (get-layerless-nodes graph))
        (index 0))
    ;; initialize values for the algorithm
    (setf (.bfs-queue this) (new-jlist)
          (.sources this) (new-jset)
          (.sinks this) (new-jset)
          (.visited this) (make-array (length nodes) :initial-element nil)
          (.edges-to-be-reversed this) (new-jlist))
    ;; Find all sources and sinks in the graph.
    (jdo (node nodes)
      (setf (.id node) index)
      (when (is-empty (get-incoming-edges node))
        (js-add (.sources this) node))
      (when (is-empty (get-outgoing-edges node))
        (js-add (.sinks this) node))
      (incf index))
    ;; Start BFS Search starting at each source sequentially.
    (jdo (source (.sources this))
      (jl-add (.bfs-queue this) source)
      (bfs-loop this))
    (bfs-loop this)
    ;; Start more BFS runs from the first node that has not been visited yet.
    (let ((changed t))
      (loop while changed do
        (setf changed nil)
        (loop for i from 0 below (length nodes)
              unless (aref (.visited this) i)
                do (jl-add (.bfs-queue this) (jl-get nodes i))
                   (setf changed t)
                   (return))
        (bfs-loop this)))
    ;; Reverse "back edges"
    (jdo (edge (.edges-to-be-reversed this))
      (j-reverse edge graph t)
      (set-property graph layered-internal-properties.cyclic t))
    ;; Cleanup
    (setf (.sources this) nil (.visited this) nil (.bfs-queue this) nil (.edges-to-be-reversed this) nil))
  (done monitor))

(defjmethod bfs-loop ((this bfs-node-order-cycle-breaker))
  (loop until (jl-is-empty (.bfs-queue this))
        do (bfs this (jl-poll-first (.bfs-queue this)))))

(defjmethod bfs ((this bfs-node-order-cycle-breaker) n)
  ;; Return if the node was already visited.
  (unless (aref (.visited this) (.id n))
    (setf (aref (.visited this) (.id n)) t)
    (let ((model-order-map (%p1p2-model-order-edge-map (.graph this) n)))
      ;; Since the model order determines the iteration order of e
      (dolist (key (%p1p2-sorted-keys model-order-map))
        (let ((out (jl-first (gethash key model-order-map))))
          ;; Do not visit self loops
          (unless (is-self-loop out)
            (let ((target (get-node (get-target out))))
              ;; If the target was already visited, reverse the edge to it.
              (if (and (aref (.visited this) (.id target))
                       (not (js-contains (.sources this) n))
                       (not (js-contains (.sinks this) target)))
                  (jl-add-all (.edges-to-be-reversed this) (gethash key model-order-map))
                  (jl-add (.bfs-queue this) target)))))))))

;;; ================================================= DFSNodeOrderCycleBreaker

(defjmethod get-layout-processor-configuration ((this dfs-node-order-cycle-breaker) graph)
  (declare (ignore graph))
  (%p1p2-cycle-config))

(defjmethod process ((this dfs-node-order-cycle-breaker) graph monitor)
  (begin monitor "Depth-first cycle removal" 1)
  (setf (.graph this) graph)
  (let* ((nodes (get-layerless-nodes graph))
         (node-count (length nodes))
         (index 0))
    (setf (.sources this) (new-jlist)
          (.visited this) (make-array node-count :initial-element nil)
          (.active this) (make-array node-count :initial-element nil)
          (.edges-to-be-reversed this) (new-jlist))
    (jdo (node nodes)
      (setf (.id node) index)
      (when (is-empty (get-incoming-edges node))
        (jl-add (.sources this) node))
      (incf index))
    ;; From every source node start a DFS
    (jdo (source (.sources this))
      (dfs this source))
    ;; Start more DFS runs for all nodes that have not been visited yet.
    (loop for i from 0 below node-count
          unless (aref (.visited this) i)
            do (dfs this (jl-get nodes i)))
    ;; Reverse "back edges"
    (jdo (edge (.edges-to-be-reversed this))
      (j-reverse edge graph t)
      (set-property graph layered-internal-properties.cyclic t))
    (setf (.sources this) nil (.visited this) nil (.active this) nil (.edges-to-be-reversed this) nil))
  (done monitor))

(defjmethod dfs ((this dfs-node-order-cycle-breaker) n)
  (unless (aref (.visited this) (.id n))
    ;; We're now visiting the node, and it's active
    (setf (aref (.visited this) (.id n)) t
          (aref (.active this) (.id n)) t)
    (let ((model-order-map (%p1p2-model-order-edge-map (.graph this) n)))
      ;; The model order serves as the key by which all elements are sorted.
      (dolist (key (%p1p2-sorted-keys model-order-map))
        (let ((out (jl-first (gethash key model-order-map))))
          ;; Ignore self loops
          (unless (is-self-loop out)
            (let ((target (get-node (get-target out))))
              ;; If the edge connects to an active node, it is a backward edge to be reversed
              (if (aref (.active this) (.id target))
                  (jl-add-all (.edges-to-be-reversed this) (gethash key model-order-map))
                  (dfs this target)))))))
    ;; We're leaving this node
    (setf (aref (.active this) (.id n)) nil)))

;;; =============================================== SCCModelOrderCycleBreaker

(defjmethod get-layout-processor-configuration ((this scc-model-order-cycle-breaker) graph)
  (declare (ignore graph))
  (%p1p2-cycle-config))

(defjmethod process ((this scc-model-order-cycle-breaker) layered-graph monitor)
  (begin monitor "Model order cycle breaking" 1)
  (setf (.graph this) layered-graph)
  ;; gather edges that point to the wrong direction
  (setf (.rev-edges this) (new-jlist))
  (let ((offset (math.max (length (get-layerless-nodes layered-graph))
                          (get-property layered-graph layered-internal-properties.max-model-order-nodes))))
    (loop
      (let ((tj (new-tarjan (.rev-edges this) (.strongly-connected-components this) (.node-to-sccid this))))
        (reset-tarjan tj layered-graph)
        (tarjan tj layered-graph))
      ;; If no Strongly connected components remain, the graph is acyclic.
      (when (= (size (.strongly-connected-components this)) 0)
        (return))
      ;; highest model order only incoming
      (find-nodes this offset
                  (* offset (get-property layered-graph layered-internal-properties.cb-num-model-order-groups)))
      ;; reverse the gathered edges
      (jdo (edge (.rev-edges this))
        (j-reverse edge layered-graph nil)
        (let ((src (get-node (get-source edge))))
          (set-property src layered-options.layering-layer-id
                        (+ (get-property src layered-options.layering-layer-id) 1)))
        (set-property layered-graph layered-internal-properties.cyclic t))
      (clear (.strongly-connected-components this))
      (clear (.node-to-sccid this))
      (clear (.rev-edges this))))
  (done monitor))

(defjmethod find-nodes ((this scc-model-order-cycle-breaker) offset big-offset)
  ;; All strongly connected components have one maximum element for which we can reverse all
  ;; outgoing edges.
  (let ((sccs (.strongly-connected-components this)))
    (dotimes (i (size sccs))
      (let ((max nil)
            (calculator (new-group-model-order-calculator))
            (max-model-order integer.min-value)
            (scc (j-get sccs i)))
        (jdo (n scc)
          (let ((enforce-group-model-order (%p1p2-enforce-group-model-order-p (.graph this))))
            (if (null max)
                (setf max n
                      max-model-order (if enforce-group-model-order
                                          (compute-constraint-group-model-order calculator n big-offset offset)
                                          (compute-constraint-model-order calculator n offset)))
                (let ((model-order-current
                        (if enforce-group-model-order
                            (compute-constraint-group-model-order calculator n big-offset offset)
                            (compute-constraint-model-order calculator n offset))))
                  (when (< max-model-order model-order-current)
                    (setf max n max-model-order model-order-current))))))
        (jdo (edge (get-outgoing-edges max))
          ;; Reverse all edges to the same strongly connected component.
          (when (contains scc (get-node (get-target edge)))
            (jl-add (.rev-edges this) edge)))))))

;;; ======================================= SCCNodeTypeCycleBreaker / SCConnectivity

(defun %p1p2-scc-min-max (this scc offset big-offset)
  "The min and max model order nodes of SCC (shared by SCCNodeTypeCycleBreaker and SCConnectivity)."
  (let ((min nil) (max nil)
        (model-order-min integer.max-value)
        (model-order-max integer.min-value)
        (enforce-group-model-order (%p1p2-enforce-group-model-order-p (.graph this)))
        (calculator (new-group-model-order-calculator)))
    (jdo (n scc)
      (if (or (null min) (null max))
          (setf min n
                model-order-min (if enforce-group-model-order
                                    (compute-constraint-group-model-order calculator n big-offset offset)
                                    (compute-constraint-model-order calculator n offset))
                max n
                model-order-max model-order-min)
          (let ((model-order-current
                  (if enforce-group-model-order
                      (compute-constraint-group-model-order calculator n big-offset offset)
                      (compute-constraint-model-order calculator n offset))))
            (when (> model-order-min model-order-current)
              (setf min n model-order-min model-order-current))
            (when (< model-order-max model-order-current)
              (setf max n model-order-max model-order-current)))))
    (values min max)))

(defun %p1p2-reverse-by-connectivity (this scc min max)
  (if (> (size (get-incoming-edges min)) (size (get-outgoing-edges max)))
      (jdo (edge (get-incoming-edges min))
        (when (contains scc (get-node (get-source edge)))
          (jl-add (.rev-edges this) edge)))
      (jdo (edge (get-outgoing-edges max))
        (when (contains scc (get-node (get-target edge)))
          (jl-add (.rev-edges this) edge)))))

(defjmethod find-nodes ((this scc-node-type-cycle-breaker) offset big-offset)
  (let ((sccs (.strongly-connected-components this))
        (graph (.graph this)))
    (dotimes (i (size sccs))
      (let ((scc (j-get sccs i)))
        ;; Nothing needs to be done if only one strongly connected component exists.
        (when (> (size scc) 1)
          (multiple-value-bind (min max) (%p1p2-scc-min-max this scc offset big-offset)
            (cond
              ((%p1p2-boxed-int-eq (get-property min layered-options.consider-model-order-group-model-order-cycle-breaking-id)
                        (get-property graph layered-options.consider-model-order-group-model-order-cb-preferred-source-id))
               ;; If the minimum model order node is a preferred source node, reverse all its
               ;; incoming edges.
               (jdo (edge (get-incoming-edges min))
                 (when (contains scc (get-node (get-source edge)))
                   (jl-add (.rev-edges this) edge))))
              ((%p1p2-boxed-int-eq (get-property max layered-options.consider-model-order-group-model-order-cycle-breaking-id)
                        (get-property graph layered-options.consider-model-order-group-model-order-cb-preferred-target-id))
               ;; If the maximum model order node is a preferred node reverse all its outgoing
               ;; edges.  (Java tests the SOURCE node here, which is always MAX itself.)
               (jdo (edge (get-outgoing-edges max))
                 (when (contains scc (get-node (get-source edge)))
                   (jl-add (.rev-edges this) edge))))
              (t
               ;; Use connectivity to decide.
               (%p1p2-reverse-by-connectivity this scc min max)))))))))

(defjmethod find-nodes ((this sc-connectivity) offset big-offset)
  (let ((sccs (.strongly-connected-components this)))
    (dotimes (i (size sccs))
      (let ((scc (j-get sccs i)))
        (when (> (size scc) 1)
          (multiple-value-bind (min max) (%p1p2-scc-min-max this scc offset big-offset)
            ;; If the minimum node has more incoming edges than the maximum node has outgoing
            ;; edges, reverse all edges to the minimum node, else all outgoing edges of the maximum.
            (%p1p2-reverse-by-connectivity this scc min max)))))))
