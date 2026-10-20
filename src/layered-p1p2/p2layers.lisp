;;;; p2layers.lisp -- org.eclipse.elk.alg.layered.p2layers: the layerers.

(in-package #:elk)

;;; ==================================================== NetworkSimplexLayerer

(defjconst network-simplex-layerer.iter-limit-factor 4)

(defjmethod get-layout-processor-configuration ((this network-simplex-layerer) graph)
  (declare (ignore graph))
  (%p1p2-layering-config))

(defjmethod connected-components ((this network-simplex-layerer) the-nodes)
  ;; initialize required attributes
  (if (or (null (.node-visited this)) (< (length (.node-visited this)) (length the-nodes)))
      (setf (.node-visited this) (make-array (length the-nodes) :initial-element nil))
      (fill (.node-visited this) nil))
  (setf (.component-nodes this) (new-jlist))
  ;; re-index nodes
  (let ((counter 0))
    (jdo (node the-nodes)
      (setf (.id node) counter)
      (incf counter)))
  ;; determine connected components
  (let ((components (new-jlist)))
    (jdo (node the-nodes)
      (unless (aref (.node-visited this) (.id node))
        (connected-components-dfs this node)
        ;; connected component with the most nodes should be layered first to guarantee
        ;; reusability of attribute instances
        (if (or (jl-is-empty components)
                (< (length (jl-first components)) (length (.component-nodes this))))
            (jl-add-first components (.component-nodes this))
            (jl-add components (.component-nodes this)))
        (setf (.component-nodes this) (new-jlist))))
    components))

(defjmethod connected-components-dfs ((this network-simplex-layerer) node)
  (setf (aref (.node-visited this) (.id node)) t)
  ;; node is part of the current connected component
  (jl-add (.component-nodes this) node)
  ;; continue with next nodes, if not already visited
  (jdo (port (get-ports node))
    (jdo (edge (get-connected-edges port))
      (let ((opposite (get-node (get-opposite this port edge))))
        (unless (aref (.node-visited this) (.id opposite))
          (connected-components-dfs this opposite))))))

(defjmethod initialize ((this network-simplex-layerer) the-nodes)
  (let ((node-map (new-jmap))
        (graph (new-n-graph)))
    ;; transform nodes
    (jdo (l-node the-nodes)
      (let ((n-node (create (origin (n-node.of) l-node) graph)))
        (jm-put node-map l-node n-node)))
    ;; transform edges
    (jdo (l-node the-nodes)
      (jdo (l-edge (get-outgoing-edges l-node))
        ;; ignore self-loops
        (unless (is-self-loop l-edge)
          (create
           (target
            (source
             (delta
              (weight (n-edge.of l-edge)
                      (jd (* 1 (math.max 1 (get-property l-edge layered-options.priority-shortness)))))
              1)
             (jm-get node-map (get-node (get-source l-edge))))
            (jm-get node-map (get-node (get-target l-edge))))))))
    graph))

(defjmethod dispose ((this network-simplex-layerer))
  (setf (.component-nodes this) nil
        (.layered-graph this) nil
        (.node-visited this) nil))

(defjmethod process ((this network-simplex-layerer) the-layered-graph monitor)
  (begin monitor "Network simplex layering" 1)
  (setf (.layered-graph this) the-layered-graph)
  (let ((thoroughness (* (get-property the-layered-graph layered-options.thoroughness)
                         network-simplex-layerer.iter-limit-factor))
        (the-nodes (get-layerless-nodes the-layered-graph)))
    (when (< (length the-nodes) 1)
      (done monitor)
      (return-from process/2 nil))
    ;; layer graph, each connected component separately
    (let ((connected-components (connected-components this the-nodes))
          (previous-layering-node-counts nil)
          (layered-graph (.layered-graph this)))
      (jdo (conn-comp connected-components)
        ;; determine a limit on the number of iterations
        (let ((iter-limit (* thoroughness (to-int (math.sqrt (length conn-comp)))))
              (graph (initialize this conn-comp)))
          ;; execute the network simplex algorithm on the (sub-)graph
          (execute (with-balancing
                    (with-previous-layering
                     (with-iteration-limit (network-simplex.for-graph graph) iter-limit)
                     previous-layering-node-counts)
                    t)
                   (sub-task monitor 1))
          ;; the layers are stored in the NNode's layer field.
          (let ((layers (get-layers layered-graph)))
            (jdo (n-node (.nodes graph))
              ;; add additional layers to match required number
              (loop while (<= (length layers) (.layer n-node))
                    do (jl-add-at layers (length layers) (new-layer layered-graph)))
              (set-layer (.origin n-node) (jl-get layers (.layer n-node)))))
          (when (> (length connected-components) 1)
            (let ((layers (get-layers layered-graph)))
              (setf previous-layering-node-counts (make-array (length layers) :initial-element 0))
              (loop for l across layers for layer-idx from 0
                    do (setf (aref previous-layering-node-counts layer-idx) (length (get-nodes l))))))))
      ;; empty the list of unlayered nodes
      (jl-clear the-nodes)
      ;; release the created resources
      (dispose this)
      (done monitor))))

(defjmethod get-opposite ((this network-simplex-layerer) port edge)
  (cond ((jequals (get-source edge) port) (get-target edge))
        ((jequals (get-target edge) port) (get-source edge))
        (t (jthrow 'illegal-argument-exception "Input edge is not connected to the input port."))))

;;; ====================================================== LongestPathLayerer

(defjmethod get-layout-processor-configuration ((this longest-path-layerer) graph)
  (declare (ignore graph))
  (%p1p2-layering-config))

(defjmethod process ((this longest-path-layerer) thelayered-graph monitor)
  (begin monitor "Longest path layering" 1)
  (setf (.layered-graph this) thelayered-graph)
  (let ((nodes (get-layerless-nodes thelayered-graph))
        (index 0))
    ;; initialize values required for the computation
    (setf (.node-heights this) (%p1p2-int-array (length nodes)))
    (jdo (node nodes)
      ;; the node id is used as index for the nodeHeights array
      (setf (.id node) index)
      (setf (aref (.node-heights this) index) -1)
      (incf index))
    ;; process all nodes
    (jdo (node nodes)
      (visit this node))
    ;; empty the list of unlayered nodes
    (jl-clear nodes)
    (setf (.layered-graph this) nil (.node-heights this) nil))
  (done monitor))

(defjmethod visit ((this longest-path-layerer) node)
  "Visit a node: if not already visited, find the longest path to a sink."
  (let ((height (aref (.node-heights this) (.id node))))
    (if (>= height 0)
        ;; the node was already visited (the case height == 0 should never occur)
        height
        (let ((max-height 1))
          (jdo (port (get-ports node))
            (jdo (edge (get-outgoing-edges port))
              (let ((target-node (get-node (get-target edge))))
                ;; ignore self-loops
                (unless (eq node target-node)
                  (let ((target-height (visit this target-node)))
                    (setf max-height (max max-height (+ target-height 1))))))))
          (put-node this node max-height)
          max-height))))

(defjmethod put-node ((this longest-path-layerer) node height)
  (let ((layers (get-layers (.layered-graph this))))
    ;; add layers so as to guarantee that number of layers >= height
    (loop for i from (length layers) below height
          do (jl-add-at layers 0 (new-layer (.layered-graph this))))
    ;; layer index = number of layers - height
    (set-layer node (jl-get layers (- (length layers) height)))
    (setf (aref (.node-heights this) (.id node)) height)))

;;; ================================================ LongestPathSourceLayerer

(defjmethod get-layout-processor-configuration ((this longest-path-source-layerer) graph)
  (declare (ignore graph))
  (%p1p2-layering-config))

(defjmethod process ((this longest-path-source-layerer) thelayered-graph monitor)
  (begin monitor "Longest path to source layering" 1)
  (setf (.layered-graph this) thelayered-graph)
  (let ((nodes (get-layerless-nodes thelayered-graph))
        (index 0))
    (setf (.node-heights this) (%p1p2-int-array (length nodes)))
    (jdo (node nodes)
      (setf (.id node) index)
      (setf (aref (.node-heights this) index) -1)
      (incf index))
    (jdo (node nodes)
      (visit this node))
    (jl-clear nodes)
    (setf (.layered-graph this) nil (.node-heights this) nil))
  (done monitor))

(defjmethod visit ((this longest-path-source-layerer) node)
  "Visit a node: if not already visited, find the longest path to a source."
  (let ((height (aref (.node-heights this) (.id node))))
    (if (>= height 0)
        height
        (let ((max-height 1))
          (jdo (port (get-ports node))
            (jdo (edge (get-incoming-edges port))
              (let ((source-node (get-node (get-source edge))))
                (unless (eq node source-node)
                  (let ((source-height (visit this source-node)))
                    (setf max-height (max max-height (+ source-height 1))))))))
          (put-node this node max-height)
          max-height))))

(defjmethod put-node ((this longest-path-source-layerer) node height)
  (let ((layers (get-layers (.layered-graph this))))
    (loop for i from (length layers) below height
          do (jl-add-at layers (length layers) (new-layer (.layered-graph this))))
    (set-layer node (jl-get layers (- height 1)))
    (setf (aref (.node-heights this) (.id node)) height)))

;;; ======================================================= InteractiveLayerer

(defjmethod get-layout-processor-configuration ((this interactive-layerer) graph)
  (declare (ignore graph))
  (add-before
   (add-before
    (add-before (layout-processor-configuration.create)
                layered-phases.p1-cycle-breaking
                layered-intermediate-processor-strategy.interactive-external-port-positioner)
    layered-phases.p2-layering
    layered-intermediate-processor-strategy.layer-constraint-preprocessor)
   layered-phases.p3-node-ordering
   layered-intermediate-processor-strategy.layer-constraint-postprocessor))

(defjmethod process ((this interactive-layerer) layered-graph monitor)
  (begin monitor "Interactive node layering" 1)
  ;; create layers with a start and an end position, merging when they overlap with others
  (let ((current-spans (new-jlist)))
    (jdo (node (get-layerless-nodes layered-graph))
      (let* ((minx (.x (get-position node)))
             (maxx (+ minx (.x (get-size node)))))
        ;; for the following code we have to guarantee that every node has a width,
        ;; which, for instance, might not be the case for external dummy nodes
        (setf maxx (math.max (+ minx 1) maxx))
        ;; look for a position in the sorted list where the node can be inserted
        ;; (a ListIterator: CURSOR is the index of the next element)
        (let ((cursor 0) (found-span nil))
          (loop while (< cursor (length current-spans)) do
            (let ((span (aref current-spans cursor)))
              (incf cursor)
              (cond ((>= (.start span) maxx)
                     ;; the next layer span is further right, so insert the node here
                     (decf cursor)
                     (return))
                    ((> (.end span) minx)
                     ;; the layer span has an intersection with the node
                     (if (null found-span)
                         (progn
                           ;; add the node to the current layer span
                           (jl-add (.nodes span) node)
                           (setf (.start span) (math.min (.start span) minx)
                                 (.end span) (math.max (.end span) maxx)
                                 found-span span))
                         (progn
                           ;; merge the previously found layer span with the current one
                           (jl-add-all (.nodes found-span) (.nodes span))
                           (setf (.end found-span) (math.max (.end found-span) (.end span)))
                           (decf cursor)
                           (jl-remove-at current-spans cursor)))))))
          (when (null found-span)
            ;; no intersecting span was found, so create a new one
            (setf found-span (new-layer-span))
            (setf (.start found-span) minx
                  (.end found-span) maxx)
            (jl-add-at current-spans cursor found-span)
            (jl-add (.nodes found-span) node)))))
    ;; create real layers from the layer spans
    (let ((layers (get-layers layered-graph))
          (next-index 0))
      (jdo (span current-spans)
        (let ((layer (new-layer layered-graph)))
          (setf (.id layer) next-index)
          (incf next-index)
          (jl-add layers layer)
          (jdo (node (.nodes span))
            (set-layer node layer)
            (setf (.id node) 0))))
      ;; correct the layering respecting the graph topology, so edges point from left to right
      (jdo (node (get-layerless-nodes layered-graph))
        (when (= (.id node) 0)
          (let ((shifted-nodes (check-node this node layered-graph)))
            ;; Since shiftedNodes might require other nodes to be shifted, do it again until all
            ;; shiftedNodes do no longer require another shift.
            (loop until (jl-is-empty shifted-nodes) do
              (let ((node-to-check (jl-remove-at shifted-nodes 0)))
                (jdo (n (check-node this node-to-check layered-graph))
                  (unless (find n shifted-nodes)
                    (jl-add shifted-nodes n))))))))
      ;; remove empty layers, which can happen when the layering has to be corrected
      (jl-remove-if layers (lambda (l) (jl-is-empty (get-nodes l))))))
  ;; clear the list of nodes that have no layer, since now they all have one
  (jl-clear (get-layerless-nodes layered-graph))
  (done monitor))

(defjmethod check-node ((this interactive-layerer) node1 graph)
  "Check the layering of NODE1 against its successors.  Returns the shifted nodes as an
insertion-ordered set (a jlist without duplicates; Java: LinkedHashSet)."
  (setf (.id node1) 1)
  (let ((layer1 (get-layer node1))
        (shift-nodes (new-jlist)))
    (jdo (port (get-ports node1 port-type.output))
      (jdo (edge (get-outgoing-edges port))
        (let ((node2 (get-node (get-target edge))))
          (unless (eq node1 node2)
            (let ((layer2 (get-layer node2)))
              (when (<= (.id layer2) (.id layer1))
                ;; a violation was detected - move the target node to the next layer
                (let ((new-index (+ (.id layer1) 1)))
                  (if (= new-index (length (get-layers graph)))
                      (let ((new-layer (new-layer graph)))
                        (setf (.id new-layer) new-index)
                        (jl-add (get-layers graph) new-layer)
                        (set-layer node2 new-layer))
                      (set-layer node2 (jl-get (get-layers graph) new-index))))
                (unless (find node2 shift-nodes)
                  (jl-add shift-nodes node2))))))))
    shift-nodes))

;;; ===================================================== CoffmanGrahamLayerer

(defjmethod get-layout-processor-configuration ((this coffman-graham-layerer) graph)
  (declare (ignore graph))
  (%p1p2-layering-config))

(defun %p1p2-in-topo (this node)
  "inTopo.get(node): the (possibly empty) list of a ListMultimap."
  (or (gethash node (.in-topo this)) (new-jlist 0)))

(defjmethod process ((this coffman-graham-layerer) layered-graph progress-monitor)
  (begin progress-monitor "Coffman-Graham Layering" 1)
  (when (jl-is-empty (get-layerless-nodes layered-graph))
    (done progress-monitor)
    (return-from process/2 nil))
  ;; the maximum number of allowed nodes per layer
  (let ((w (get-property layered-graph layered-options.layering-coffman-graham-layer-bound))
        (index 0) (edge-index 0))
    ;; initialization, assign indexes and initialize arrays
    (jdo (n (get-layerless-nodes layered-graph))
      (setf (.id n) index)
      (incf index)
      (jdo (e (get-outgoing-edges n))
        (setf (.id e) edge-index)
        (incf edge-index)))
    (setf (.node-mark this) (make-array index :initial-element nil)
          (.edge-mark this) (make-array edge-index :initial-element nil)
          (.in-deg this) (%p1p2-int-array index)
          (.out-deg this) (%p1p2-int-array index)
          (.topo-ord this) (%p1p2-int-array index))
    (clrhash (.in-topo this))
    ;; #1 Remove transitive edges
    (transitive-reduction this layered-graph)
    ;; #2 Compute topological ordering
    (let ((edge-mark (.edge-mark this)) (in-deg (.in-deg this))
          (out-deg (.out-deg this)) (topo-ord (.topo-ord this))
          (sources (new-jpq (lambda (u v) (compare-nodes-in-topo this u v)))))
      ;; for each node, determine its current in-degree and remember initial sources
      (jdo (v (get-layerless-nodes layered-graph))
        (jdo (e (get-incoming-edges v))
          (unless (aref edge-mark (.id e))
            (incf (aref in-deg (.id v)))))
        (when (= (aref in-deg (.id v)) 0)
          (add sources v)))
      ;; compute topological ordering
      (let ((i 0))
        (loop until (is-empty sources) do
          (let ((v (poll sources)))
            ;; assign number of topological order
            (setf (aref topo-ord (.id v)) i)
            (incf i)
            ;; update the rest of the graph
            (jdo (e (get-outgoing-edges v))
              (unless (aref edge-mark (.id e))
                (let ((tgt (get-node (get-target e))))
                  (decf (aref in-deg (.id tgt)))
                  (let ((l (gethash tgt (.in-topo this))))
                    (unless l (setf l (setf (gethash tgt (.in-topo this)) (new-jlist))))
                    (jl-add l (aref topo-ord (.id v))))
                  (when (= (aref in-deg (.id tgt)) 0)
                    (add sources tgt))))))))
      ;; #3 Actual layer assignment
      ;; note that this time we start with sinks and work our way to the original graph's sources
      (let ((sinks (new-jpq (lambda (n1 n2)
                              (- (integer.compare (aref topo-ord (.id n1)) (aref topo-ord (.id n2))))))))
        (jdo (v (get-layerless-nodes layered-graph))
          (jdo (e (get-outgoing-edges v))
            (unless (aref edge-mark (.id e))
              (incf (aref out-deg (.id v)))))
          (when (= (aref out-deg (.id v)) 0)
            (add sinks v)))
        ;; assign the layers
        (let* ((layers (new-jlist))
               (current-layer (create-layer this layered-graph layers)))
          (loop until (is-empty sinks) do
            ;; select a node for which all outgoing nodes have been placed,
            ;; and with maximum value in the topological sort
            (let ((u (poll sinks)))
              ;; start a new layer if the current one is already full,
              ;; or an in-layer edge would be introduced
              (when (or (is-layer-full this current-layer w) (not (can-add this u current-layer)))
                (setf current-layer (create-layer this layered-graph layers)))
              ;; place the node in the layer
              (set-layer u current-layer)
              ;; update out-degrees and collect the new sinks
              (jdo (e (get-incoming-edges u))
                (unless (aref edge-mark (.id e))
                  (let ((src (get-node (get-source e))))
                    (decf (aref out-deg (.id src)))
                    (when (= (aref out-deg (.id src)) 0)
                      (add sinks src)))))))
          ;; the layers were created in inverse order
          (loop for j from (1- (length layers)) downto 0
                do (jl-add (get-layers layered-graph) (jl-get layers j)))))))
  ;; clear layerless nodes
  (jl-clear (get-layerless-nodes layered-graph))
  (done progress-monitor))

(defjmethod is-layer-full ((this coffman-graham-layerer) layer w)
  (>= (length (get-nodes layer)) w))

(defjmethod can-add ((this coffman-graham-layerer) n l)
  (block nil
    (jdo (e (get-outgoing-edges n))
      (when (eq (get-layer (get-node (get-target e))) l)
        (return-from can-add/2 nil)))
    t))

(defjmethod create-layer ((this coffman-graham-layerer) graph layers)
  (let ((a-layer (new-layer graph)))
    (jl-add layers a-layer)
    a-layer))

(defjmethod compare-nodes-in-topo ((this coffman-graham-layerer) u v)
  (let* ((in-list-u (%p1p2-in-topo this u))
         (in-list-v (%p1p2-in-topo this v))
         ;; ListIterators positioned at the end
         (it-u (length in-list-u))
         (it-v (length in-list-v)))
    ;; find the node with the lower associated maximum value in 'inTopo'
    ;; break ties by ignoring all equal maxima
    (loop while (and (> it-u 0) (> it-v 0)) do
      (decf it-u) (decf it-v)
      (let ((iu (aref in-list-u it-u))
            (iv (aref in-list-v it-v)))
        ;; Java compares the boxed Integers by identity here
        (unless (%p1p2-boxed-int-eq iu iv)
          (return-from compare-nodes-in-topo/2 (integer.compare iu iv)))))
    (let ((u-has-next (< it-u (length in-list-u)))
          (v-has-next (< it-v (length in-list-v))))
      (cond ((and (not u-has-next) (not v-has-next))
             ;; If the two nodes are the same, a secondary criterion is needed.
             ;; Else use the node id as a last resort.
             (cond ((< (.id u) (.id v)) -1)
                   ((> (.id u) (.id v)) 1)
                   (t 0)))
            ((not u-has-next) -1)
            (t 1)))))

(defjmethod transitive-reduction ((this coffman-graham-layerer) graph)
  (jdo (start (get-layerless-nodes graph))
    (fill (.node-mark this) nil)
    (jdo (out (get-outgoing-edges start))
      (dfs this start (get-node (get-target out))))))

(defjmethod dfs ((this coffman-graham-layerer) start v)
  (unless (aref (.node-mark this) (.id v))
    (jdo (out (get-outgoing-edges v))
      (let ((w (get-node (get-target out))))
        (jdo (transitive (get-incoming-edges w))
          (when (eq (get-node (get-source transitive)) start)
            (setf (aref (.edge-mark this) (.id transitive)) t)))
        (dfs this start w)))
    (setf (aref (.node-mark this) (.id v)) t)))

;;; ============================================ BreadthFirstModelOrderLayerer

(defjmethod get-layout-processor-configuration ((this breadth-first-model-order-layerer) graph)
  (declare (ignore graph))
  (%p1p2-layering-config))

(defun %p1p2-sorted-real-nodes (graph message)
  (let ((real-nodes (new-jlist)))
    (jdo (node (get-layerless-nodes graph))
      (when (eq (get-type node) node-type.normal)
        (jl-add real-nodes node)))
    ;; Sort real nodes by model order.
    (jl-sort real-nodes
             (lambda (n1 n2)
               (if (and (has-property n1 layered-internal-properties.model-order)
                        (has-property n2 layered-internal-properties.model-order))
                   (integer.compare (get-property n1 layered-internal-properties.model-order)
                                    (get-property n2 layered-internal-properties.model-order))
                   (jthrow 'unsupported-graph-exception message))))
    real-nodes))

(defun %p1p2-finish-model-order-layering (graph)
  ;; Empty the list of unlayered nodes.
  (jl-clear (get-layerless-nodes graph))
  ;; Delete all empty dummy layers.
  (let ((to-delete (new-jlist)))
    (jdo (layer (get-layers graph))
      (when (jl-is-empty (get-nodes layer))
        (jl-add to-delete layer)))
    (jl-remove-if (get-layers graph) (lambda (l) (find l to-delete))))
  ;; Adjust layer ids
  (let ((layer-id 0))
    (jdo (layer (get-layers graph))
      (setf (.id layer) layer-id)
      (incf layer-id))))

(defjmethod process ((this breadth-first-model-order-layerer) thelayered-graph monitor)
  (begin monitor "Breadth first model order layering" 1)
  (setf (.layered-graph this) thelayered-graph)
  (let* ((lg thelayered-graph)
         (real-nodes (%p1p2-sorted-real-nodes
                      lg "The BF model order layer assigner requires all real nodes to have a model order."))
         (first-node t)
         (current-layer (new-layer lg))
         (current-dummy-layer nil))
    (jl-add (get-layers lg) current-layer)
    (jdo (node real-nodes)
      (if first-node
          (progn
            ;; Set the first node in the first layer.
            (set-layer node current-layer)
            (setf first-node nil))
          (progn
            ;; Check for each incoming edge whether it connects to the current layer.
            ;; If this is the case, add a new dummy and normal layer.
            (jdo (edge (get-incoming-edges node))
              (let ((src (get-node (get-source edge))))
                (when (or (and (eq (get-type src) node-type.normal) ; directly connected
                               (eq (get-layer src) current-layer))
                          (and (eq (get-type src) node-type.label) ; dummy label in-between
                               (eq (get-layer (get-node (get-source (%p1p2-first (get-incoming-edges src)))))
                                   current-layer)))
                  (setf current-dummy-layer (new-layer lg))
                  (jl-add (get-layers lg) current-dummy-layer)
                  (setf current-layer (new-layer lg))
                  (jl-add (get-layers lg) current-layer))))
            ;; Add all label dummies to the dummy layer between the last layer and the current one.
            (jdo (edge (get-incoming-edges node))
              (let ((src (get-node (get-source edge))))
                (when (and (eq (get-type src) node-type.label) (null (get-layer src)))
                  (set-layer src current-dummy-layer))))
            ;; Add the node to the (potentially new) current layer.
            (set-layer node current-layer))))
    (%p1p2-finish-model-order-layering lg))
  (done monitor))

;;; ============================================== DepthFirstModelOrderLayerer

(defjmethod get-layout-processor-configuration ((this depth-first-model-order-layerer) graph)
  (declare (ignore graph))
  (%p1p2-layering-config))

(defjmethod process ((this depth-first-model-order-layerer) thelayered-graph monitor)
  (begin monitor "Depth first model order layering" 1)
  (setf (.layered-graph this) thelayered-graph)
  (let* ((lg thelayered-graph)
         (real-nodes (%p1p2-sorted-real-nodes
                      lg "The DF model order layer assigner requires all real nodes to have a model order."))
         (first-node t))
    (setf (.current-layer this) (new-layer lg)
          (.current-dummy-layer this) nil)
    (jl-add (get-layers lg) (.current-layer this))
    (setf (.id (.current-layer this)) 0
          (.current-layer-id this) 0
          (.nodes-to-place this) (new-jlist))
    (jdo (node real-nodes)
      (cond
        (first-node
         ;; Set the first node in the first layer.
         (set-layer node (.current-layer this))
         (setf first-node nil))
        ((is-connected-to-current-layer this node)
         ;; Check whether there are incoming edges to layers higher than currentLayer.id + 2
         (let* ((max-layer (get-max-connected-layer this (.current-layer-id this) node))
                (desired-layer (+ max-layer 2))
                (layer-diff (- max-layer (.current-layer-id this))))
           ;; Move all nodes that where not already placed by the layer difference.
           (if (not (jl-is-empty (.nodes-to-place this)))
               (if (> layer-diff 0)
                   (progn
                     ;; Case some dependency to the existing graph was found add all nodes to
                     ;; their layers.
                     (jdo (to-place (.nodes-to-place this))
                       (incf (.id to-place) (- max-layer (.max-to-place this))))
                     (place-nodes-to-place this)
                     ;; Remove all nodes since they are now placed correctly.
                     (jl-clear (.nodes-to-place this))
                     (add-node-to-layer this desired-layer node))
                   (progn
                     ;; Case nodes cannot be placed without the need to move them later.
                     (jl-add (.nodes-to-place this) node)
                     (setf (.id node) desired-layer)
                     (setf (.max-to-place this) (math.max (.max-to-place this) desired-layer))
                     ;; Add dummy nodes and give them their desired layer id.
                     (jdo (edge (get-incoming-edges node))
                       (let ((src (get-node (get-source edge))))
                         (when (and (null (get-layer src)) (eq (get-type src) node-type.label))
                           (jl-add (.nodes-to-place this) src)
                           (setf (.id src) (- desired-layer 1)))))
                     ;; Update the current layer id.
                     (setf (.current-layer-id this) desired-layer)))
               (add-node-to-layer this desired-layer node))))
        (t
         ;; Case a new strip has to begin.
         (place-nodes-to-place this)
         (jl-clear (.nodes-to-place this))
         ;; Find the layer for the first element of the strip.
         (if (is-empty (get-incoming-edges node))
             (progn
               ;; Case no incoming connections, save the node to be placed.
               (jl-add (.nodes-to-place this) node)
               (setf (.id node) 0)      ; Save the layer the node will be placed in.
               (setf (.max-to-place this) (math.max (.max-to-place this) 0))
               (setf (.current-layer this) (jl-get (get-layers lg) 0)
                     (.current-layer-id this) 0))
             ;; Find the last layer the node is connected to and add the node to the desired layer.
             (let ((max-layer (get-max-connected-layer this 0 node)))
               (add-node-to-layer this (+ max-layer 2) node))))))
    ;; Finish the last
    (unless (jl-is-empty (.nodes-to-place this))
      (place-nodes-to-place this))
    (%p1p2-finish-model-order-layering lg))
  (done monitor))

(defjmethod is-connected-to-current-layer ((this depth-first-model-order-layerer) node)
  (let ((current-layer-id (.current-layer-id this)))
    (jdo (edge (get-incoming-edges node))
      (let ((src (get-node (get-source edge)))
            (directly-connected nil)
            (connected-via-label-dummy nil))
        (if (jl-is-empty (.nodes-to-place this))
            (progn
              ;; Case the node to check already has a layer.
              (setf directly-connected (and (eq (get-type src) node-type.normal)
                                            (get-layer src)
                                            (= (.id (get-layer src)) current-layer-id)))
              (if (not (is-empty (get-incoming-edges src)))
                  (let ((connected-layer-via-dummy
                          (get-layer (get-node (get-source (%p1p2-first (get-incoming-edges src)))))))
                    (setf connected-via-label-dummy
                          (and (eq (get-type src) node-type.label)
                               connected-layer-via-dummy
                               (= (.id connected-layer-via-dummy) current-layer-id))))
                  (setf connected-via-label-dummy nil)))
            (progn
              ;; Case the node to check might not be placed yet and the node id holds its desired layer.
              (setf directly-connected (and (eq (get-type src) node-type.normal)
                                            (= (.id src) current-layer-id)))
              (setf connected-via-label-dummy
                    (and (eq (get-type src) node-type.label)
                         (= (.id (get-node (get-source (%p1p2-first (get-incoming-edges src)))))
                            current-layer-id)))))
        (when (or directly-connected connected-via-label-dummy)
          (return-from is-connected-to-current-layer/1 t))))
    nil))

(defjmethod add-node-to-layer ((this depth-first-model-order-layerer) layer-id node)
  (let ((lg (.layered-graph this)))
    (if (< layer-id (length (get-layers lg)))
        ;; Case the layers already exist.
        (setf (.current-layer this) (jl-get (get-layers lg) layer-id)
              (.current-dummy-layer this) (jl-get (get-layers lg) (- layer-id 1))
              (.current-layer-id this) layer-id)
        ;; Case the layers need to be added.
        (progn
          (setf (.current-dummy-layer this) (new-layer lg))
          (setf (.id (.current-dummy-layer this)) (- layer-id 1))
          (jl-add (get-layers lg) (.current-dummy-layer this))
          (setf (.current-layer this) (new-layer lg))
          (setf (.id (.current-layer this)) layer-id)
          (jl-add (get-layers lg) (.current-layer this))
          (setf (.current-layer-id this) layer-id)))
    ;; Add node
    (set-layer node (.current-layer this))
    ;; Add dummy node
    (jdo (edge (get-incoming-edges node))
      (let ((src (get-node (get-source edge))))
        (when (and (null (get-layer src)) (eq (get-type src) node-type.label))
          (set-layer src (.current-dummy-layer this)))))))

(defjmethod get-max-connected-layer ((this depth-first-model-order-layerer) layer-id node)
  (let ((max-layer layer-id))
    (jdo (edge (get-incoming-edges node))
      (let ((src (get-node (get-source edge))))
        (when (get-layer src)
          (setf max-layer (math.max max-layer (.id (get-layer src)))))))
    max-layer))

(defjmethod place-nodes-to-place ((this depth-first-model-order-layerer))
  (let ((lg (.layered-graph this)))
    (setf (.max-to-place this) 0)
    (jdo (node-to-place (.nodes-to-place this))
      (when (>= (.id node-to-place) (length (get-layers lg)))
        ;; Add a normal and a dummy layer.
        (let ((dummy-layer (new-layer lg)))
          (setf (.id dummy-layer) (- (.id node-to-place) 1))
          (jl-add (get-layers lg) dummy-layer))
        (let ((new-layer (new-layer lg)))
          (setf (.id new-layer) (.id node-to-place))
          (jl-add (get-layers lg) new-layer)))
      (set-layer node-to-place (jl-get (get-layers lg) (.id node-to-place))))))

;;; ========================================================= MinWidthLayerer

(defjconst min-width-layerer.upperbound-on-width-range '(1 . 4))
(defjconst min-width-layerer.compensator-range '(1 . 2))

(defun %p1p2-self-loop-p (edge)
  "MinWidthLayerer.SelfLoopPredicate"
  (jequals (get-node (get-source edge)) (get-node (get-target edge))))

(defjmethod get-layout-processor-configuration ((this min-width-layerer) graph)
  (declare (ignore graph))
  (%p1p2-layering-config))

(defjmethod process ((this min-width-layerer) layered-graph progress-monitor)
  (begin progress-monitor "MinWidth layering" 1)
  (let* ((layers (get-layers layered-graph))
         (not-inserted (get-layerless-nodes layered-graph))
         (upper-bound-on-width (get-property layered-graph layered-options.layering-min-width-upper-bound-on-width))
         (compensator (get-property layered-graph layered-options.layering-min-width-upper-layer-estimation-scaling-factor)))
    ;; First step to consider the real size of nodes: Initialize the dummy size with the spacing
    (setf (.dummy-size this) (jd (get-property layered-graph layered-options.spacing-edge-edge)))
    ;; Compute the minimum nodes size (of the real nodes).
    (setf (.minimum-node-size this) double.positive-infinity)
    (jdo (node not-inserted)
      (when (eq (get-type node) node-type.normal)
        (setf (.minimum-node-size this) (math.min (.minimum-node-size this) (.y (get-size node))))))
    ;; The minimum nodes size might be zero. If This is the case, then simply don't normalize.
    (setf (.minimum-node-size this) (math.max 1 (.minimum-node-size this)))
    (let ((num-of-nodes (length not-inserted))
          (i 0))
      (setf (.in-degree this) (%p1p2-int-array num-of-nodes)
            (.out-degree this) (%p1p2-int-array num-of-nodes)
            (.norm-size this) (make-array num-of-nodes :initial-element 0d0)
            (.avg-size this) 0d0)
      (jdo (node not-inserted)
        ;; Warning: LNode.id is being redefined here!
        (setf (.id node) i)
        (incf i)
        (setf (aref (.in-degree this) (.id node)) (count-edges-except-self-loops this (get-incoming-edges node))
              (aref (.out-degree this) (.id node)) (count-edges-except-self-loops this (get-outgoing-edges node))
              (aref (.norm-size this) (.id node)) (/ (.y (get-size node)) (.minimum-node-size this)))
        ;; The average size of a node will also be based on the normalized size.
        (incf (.avg-size this) (aref (.norm-size this) (.id node))))
      ;; normalize dummy size, too:
      (setf (.dummy-size this) (/ (.dummy-size this) (.minimum-node-size this)))
      ;; Divide sum of normalized node sizes by the number of nodes to get an actual mean.
      (setf (.avg-size this) (with-java-floats (/ (.avg-size this) num-of-nodes))))
    ;; Precalculate the successors of all nodes (as a Set) and put them in a list.
    (let ((node-successors (precalc-successors this not-inserted)))
      ;; Order the nodes by descending maximum out-degree in advance (ConditionSelect).
      (let ((out-degree (.out-degree this)))
        (jl-sort not-inserted
                 (reversed-comparator
                  ;; MinOutgoingEdgesComparator
                  (lambda (o1 o2)
                    (let ((outs1 (aref out-degree (.id o1)))
                          (outs2 (aref out-degree (.id o2))))
                      (cond ((< outs1 outs2) -1)
                            ((= outs1 outs2) 0)
                            (t 1)))))))
      (let ((min-width double.positive-infinity)
            (min-num-of-layers integer.max-value)
            (candidate-layering nil)
            (ubw-start upper-bound-on-width) (ubw-end upper-bound-on-width)
            (c-start compensator) (c-end compensator))
        ;; check whether special (negative) values have been used; then use recommended ranges
        (when (< upper-bound-on-width 0)
          (setf ubw-start (car min-width-layerer.upperbound-on-width-range)
                ubw-end (cdr min-width-layerer.upperbound-on-width-range)))
        (when (< compensator 0)
          (setf c-start (car min-width-layerer.compensator-range)
                c-end (cdr min-width-layerer.compensator-range)))
        (loop for ubw from ubw-start to ubw-end do
          (loop for c from c-start to c-end do
            (multiple-value-bind (new-width layering)
                (compute-min-width-layering this ubw c not-inserted node-successors)
              ;; replace the current candidate layering with a newly computed one, if it is
              ;; narrower or has the same maximum width but less layers.
              (let ((new-num-of-layers (length layering)))
                (when (or (< new-width min-width)
                          (and (= new-width min-width) (< new-num-of-layers min-num-of-layers)))
                  (setf min-width new-width
                        min-num-of-layers new-num-of-layers
                        candidate-layering layering))))))
        ;; Finally, add the winning layering to the Klay layered data structures.
        (jdo (layer-list candidate-layering)
          (let ((current-layer (new-layer layered-graph)))
            (jdo (node layer-list)
              (set-layer node current-layer))
            (jl-add layers current-layer)))
        ;; The algorithm constructs the layering bottom up, but ElkLayered expects the list of
        ;; layers to be ordered top down.
        (jl-reverse layers)
        (jl-clear not-inserted))))
  (done progress-monitor))

(defjmethod precalc-successors ((this min-width-layerer) nodes)
  (let ((successors (new-jlist)))
    (jdo (node nodes)
      (let ((out-nodes (new-jset)))
        (jdo (edge (get-outgoing-edges node))
          (unless (%p1p2-self-loop-p edge)
            (js-add out-nodes (get-node (get-target edge)))))
        (jl-add successors out-nodes)))
    successors))

(defjmethod compute-min-width-layering ((this min-width-layerer) upper-bound-on-width compensator nodes node-successors)
  "Returns two values: the width (Pair.first) and the list of layers (Pair.second)."
  (let* ((layers (new-jlist))
         ;; Sets.newLinkedHashSet(nodes): only removals happen, so an ordered jlist will do
         (unplaced-nodes (jlist-from nodes))
         ;; our upper bound is taking node sizes into account
         (ubw-consider-size (* upper-bound-on-width (.avg-size this)))
         (in-deg 0) (out-deg 0)
         (already-placed-in-current-layer (new-jset))
         (already-placed-in-other-layers (new-jset))
         (current-layer (new-jlist))
         (width-current 0d0) (width-up 0d0)
         (max-width 0d0) (real-width 0d0)
         (current-spanning-edges 0d0)
         (going-out-from-this-layer 0d0)
         (dummy-size (.dummy-size this))
         (norm-size (.norm-size this)))
    (loop until (jl-is-empty unplaced-nodes) do
      ;; Find a node whose edges only point to nodes in alreadyPlacedInOtherLayers
      (let ((current-node (select-node this unplaced-nodes node-successors already-placed-in-other-layers)))
        (when current-node
          (jl-remove unplaced-nodes current-node)
          (jl-add current-layer current-node)
          (js-add already-placed-in-current-layer current-node)
          (setf out-deg (aref (.out-degree this) (.id current-node)))
          (incf width-current (- (aref norm-size (.id current-node)) (* out-deg dummy-size)))
          (setf in-deg (aref (.in-degree this) (.id current-node)))
          (incf width-up (* in-deg dummy-size))
          (incf going-out-from-this-layer (* out-deg dummy-size))
          (incf real-width (aref norm-size (.id current-node))))
        ;; Go to the next layer if no node was selected, none is left, or conditionGoUp holds
        (when (or (null current-node)
                  (jl-is-empty unplaced-nodes)
                  (and (>= width-current ubw-consider-size)
                       (> (aref norm-size (.id current-node)) (* out-deg dummy-size)))
                  (>= width-up (* compensator ubw-consider-size)))
          (jl-add layers current-layer)
          (setf current-layer (new-jlist))
          (js-add-all already-placed-in-other-layers already-placed-in-current-layer)
          (js-clear already-placed-in-current-layer)
          ;; Remove all edges from the dummy node count, which are starting at a node placed in
          ;; this layer ...
          (decf current-spanning-edges going-out-from-this-layer)
          ;; ... now we have the actual dummy node count for this layer
          (setf max-width (math.max max-width (+ (* current-spanning-edges dummy-size) real-width)))
          ;; In the next iteration we have to consider new dummy nodes from edges coming into
          ;; the layer we've just finished.
          (incf current-spanning-edges width-up)
          (setf width-current width-up
                width-up 0d0
                going-out-from-this-layer 0d0
                real-width 0d0))))
    (values max-width layers)))

(defjmethod select-node ((this min-width-layerer) nodes successors targets)
  (jdo (node nodes)
    (when (contains-all targets (jl-get successors (.id node)))
      (return-from select-node/3 node)))
  nil)

(defjmethod count-edges-except-self-loops ((this min-width-layerer) edges)
  (let ((i 0))
    (jdo (edge edges)
      (unless (%p1p2-self-loop-p edge)
        (incf i)))
    i))

;;; ===================================================== StretchWidthLayerer

(defjmethod get-layout-processor-configuration ((this stretch-width-layerer) graph)
  (declare (ignore graph))
  (%p1p2-layering-config))

(defjmethod process ((this stretch-width-layerer) layered-graph progress-monitor)
  (begin progress-monitor "StretchWidth layering" 1)
  ;; if no nodes need to be placed we can stop right here
  (when (jl-is-empty (get-layerless-nodes layered-graph))
    (done progress-monitor)
    (return-from process/2 nil))
  (setf (.current-graph this) layered-graph
        (.width-current this) 0d0
        (.width-up this) 0d0
        (.minimum-node-size this) double.positive-infinity
        (.maximum-node-size this) double.negative-infinity)
  ;; initialize the dummy size with the spacing properties
  (setf (.dummy-size this) (jd (get-property layered-graph layered-options.spacing-edge-edge)))
  (compute-sorted-nodes this)
  (compute-successors this)
  (compute-degrees this)
  (min-max-node-size this)
  (compute-normalized-size this)
  ;; make sure the values are reasonable
  (setf (.minimum-node-size this) (math.max 1 (.minimum-node-size this))
        (.maximum-node-size this) (math.max 1 (.maximum-node-size this)))
  ;; normalize dummy size
  (setf (.dummy-size this) (/ (.dummy-size this) (.minimum-node-size this))
        (.max-width this) (/ (.maximum-node-size this) (.minimum-node-size this)))
  (setf (.upper-layer-influence this) (get-average-out-degree this))
  (let ((current-layer (new-layer (.current-graph this))))
    (jl-add (get-layers (.current-graph this)) current-layer)
    ;; Copy the sorted layerless nodes so we don't overwrite it in the reset case
    (setf (.temp-layerless-nodes this) (jlist-from (.sorted-layerless-nodes this))
          (.remaining-out-going this) (copy-seq (.out-degree this)))
    (loop until (jl-is-empty (.temp-layerless-nodes this)) do
      ;; Select a node to be placed
      (setf (.selected-node this) (select-node this))
      (cond
        ((or (null (.selected-node this))
             (and (condition-go-up this) (not (is-empty (.already-placed-nodes this)))))
         ;; go to the next layer: update the remaining successors of the node
         (update-out-going this current-layer)
         (setf current-layer (new-layer (.current-graph this)))
         (jl-add (get-layers (.current-graph this)) current-layer)
         (js-add-all (.already-placed-in-other-layers this) (.already-placed-nodes this))
         (js-clear (.already-placed-nodes this))
         (setf (.width-current this) (.width-up this)
               (.width-up this) 0d0))
        ((condition-go-up this)
         ;; reset layering: clear the placed layers
         (jl-clear (get-layers (.current-graph this)))
         (setf current-layer (new-layer (.current-graph this)))
         (jl-add (get-layers (.current-graph this)) current-layer)
         (setf (.width-current this) 0d0
               (.width-up this) 0d0)
         (js-clear (.already-placed-nodes this))
         (js-clear (.already-placed-in-other-layers this))
         ;; increase maxWidth
         (incf (.max-width this))
         (setf (.temp-layerless-nodes this) (jlist-from (.sorted-layerless-nodes this))
               (.remaining-out-going this) (copy-seq (.out-degree this))))
        (t
         ;; add node to current layer
         (let ((sel (.selected-node this)))
           (set-layer sel current-layer)
           (jl-remove (.temp-layerless-nodes this) sel)
           (js-add (.already-placed-nodes this) sel)
           (setf (.width-current this)
                 (+ (- (.width-current this) (* (aref (.out-degree this) (.id sel)) (.dummy-size this)))
                    (aref (.norm-size this) (.id sel))))
           (incf (.width-up this) (* (aref (.in-degree this) (.id sel)) (.dummy-size this))))))))
  ;; Layering done, delete original layerlessNodes
  (jl-clear (get-layerless-nodes layered-graph))
  ;; Algorithm is Bottom-Up -> reverse Layers
  (jl-reverse (get-layers layered-graph))
  (done progress-monitor))

(defjmethod condition-go-up ((this stretch-width-layerer))
  (let* ((sel (.selected-node this))
         (a (> (+ (- (.width-current this) (* (aref (.out-degree this) (.id sel)) (.dummy-size this)))
                  (aref (.norm-size this) (.id sel)))
               (.max-width this)))
         (b (> (+ (.width-up this) (* (aref (.in-degree this) (.id sel)) (.dummy-size this)))
               (* (.max-width this) (.upper-layer-influence this) (.dummy-size this)))))
    (or a b)))

(defjmethod select-node ((this stretch-width-layerer))
  (jdo (node (.temp-layerless-nodes this))
    ;; If all successors of node are in alreadyPlacedInOtherLayers, choose this node, since the
    ;; list is sorted by rank
    (when (<= (aref (.remaining-out-going this) (.id node)) 0)
      (return-from select-node/0 node)))
  nil)

(defjmethod compute-sorted-nodes ((this stretch-width-layerer))
  (let ((unsorted-nodes (get-layerless-nodes (.current-graph this))))
    (setf (.sorted-layerless-nodes this) (jlist-from unsorted-nodes))
    ;; the id-field is reused later on, be careful
    (jdo (node unsorted-nodes)
      (setf (.id node) (get-rank this node)))
    ;; sorting the list by the node-id /rank, descending
    (jl-sort (.sorted-layerless-nodes this)
             (lambda (o1 o2)
               (cond ((< (.id o1) (.id o2)) 1)
                     ((> (.id o1) (.id o2)) -1)
                     (t 0))))))

(defjmethod get-rank ((this stretch-width-layerer) node)
  (let ((max (size (get-outgoing-edges node))))
    ;; compute max of predecessors out-degree and out-degree of the current node
    (jdo (pre-edge (get-incoming-edges node))
      (let ((pre (get-node (get-source pre-edge))))
        (setf max (math.max max (size (get-outgoing-edges pre))))))
    max))

(defjmethod compute-successors ((this stretch-width-layerer))
  (let ((i 0)
        (curr-succ (new-jset)))
    (setf (.successors this) (new-jlist))
    (jdo (node (.sorted-layerless-nodes this))
      (setf (.id node) i)
      (jdo (edge (get-outgoing-edges node))
        (js-add curr-succ (get-node (get-target edge))))
      ;; remove current node from accSucc to deal with self-loops
      (js-remove curr-succ node)
      (jl-add (.successors this) (jset-from curr-succ))
      (js-clear curr-succ)
      (incf i))))

(defjmethod compute-degrees ((this stretch-width-layerer))
  (let ((n (length (.sorted-layerless-nodes this))))
    (setf (.in-degree this) (%p1p2-int-array n)
          (.out-degree this) (%p1p2-int-array n))
    (jdo (node (.sorted-layerless-nodes this))
      (setf (aref (.in-degree this) (.id node)) (size (get-incoming-edges node))
            (aref (.out-degree this) (.id node)) (size (get-outgoing-edges node))))))

(defjmethod min-max-node-size ((this stretch-width-layerer))
  ;; a preprocessor transposes width and height, so we can take the y-size
  (jdo (node (.sorted-layerless-nodes this))
    (when (eq (get-type node) node-type.normal)
      (let ((size (.y (get-size node))))
        (setf (.minimum-node-size this) (math.min (.minimum-node-size this) size)
              (.maximum-node-size this) (math.max (.maximum-node-size this) size))))))

(defjmethod avereage-node-size ((this stretch-width-layerer))
  (let ((sum 0d0))
    (jdo (node (.sorted-layerless-nodes this))
      (setf sum (+ sum (aref (.norm-size this) (.id node)))))
    (with-java-floats (/ sum (length (.sorted-layerless-nodes this))))))

(defjmethod compute-normalized-size ((this stretch-width-layerer))
  (setf (.norm-size this) (make-array (length (.sorted-layerless-nodes this)) :initial-element 0d0))
  (jdo (node (.sorted-layerless-nodes this))
    (setf (aref (.norm-size this) (.id node))
          (with-java-floats (/ (.y (get-size node)) (.minimum-node-size this))))))

(defjmethod get-average-out-degree ((this stretch-width-layerer))
  "Java computes this in single-float (float) arithmetic; so do we."
  (let ((all-out 0f0))
    (jdo (node (get-layerless-nodes (.current-graph this)))
      (setf all-out (+ all-out (float (size (get-outgoing-edges node)) 1f0))))
    (float (/ all-out (float (length (get-layerless-nodes (.current-graph this))) 1f0)) 1d0)))

(defjmethod update-out-going ((this stretch-width-layerer) current-layer)
  (jdo (node (get-nodes current-layer))
    (jdo (edge (get-incoming-edges node))
      (let ((pos (.id (get-node (get-source edge)))))
        (setf (aref (.remaining-out-going this) pos)
              (- (aref (.remaining-out-going this) pos) 1))))))
