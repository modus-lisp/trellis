;;;; p3order.lisp -- org.eclipse.elk.alg.layered.p3order

(in-package #:elk)

(defun %p3-float-compare (a b) (double.compare (float a 1d0) (float b 1d0)))
(defun %p3-node-at (order l n) (aref (aref order l) n))

;;; ---------------------------------------------------------------- interfaces

(defjgeneric always-improves 0)
(defjgeneric set-first-layer-order 2)
(defjgeneric minimize-crossings 4)
(defjgeneric is-deterministic 0)
(defjgeneric distribute-ports-while-sweeping 3)

(defjstatic i-sweep-port-distributor.create (cmt r current-order)
  (cond ((eq cmt cross-min-type.two-sided-greedy-switch) (new-greedy-port-distributor))
        ;; Both lead to different results, but neither is clearly better: choose randomly.
        ((next-boolean r) (new-node-relative-port-distributor (length current-order)))
        (t (new-layer-total-port-distributor (length current-order)))))

;;; ---------------------------------------------------------------- GraphInfoHolder

(defjctor graph-info-holder (graph cross-min-type graphs)
  (setf (.l-graph this) graph
        (.current-node-order this) (to-node-array graph))
  (let ((parent (get-parent-node (.l-graph this))))
    (setf (.parent this) parent
          (.has-parent this) (not (null parent))
          (.parent-graph-data this) (if parent (j-get graphs (.id (get-graph parent))) nil)))
  (setf (.has-external-ports this)
        (contains (get-property graph layered-internal-properties.graph-properties)
                  layered-graph-properties.external-ports)
        (.child-graphs this) (new-jlist))
  (let* ((order (.current-node-order this))
         (random (get-property (.l-graph this) layered-internal-properties.random)))
    (setf (.crossings-counter this) (new-all-crossings-counter order)
          (.port-distributor this) (i-sweep-port-distributor.create cross-min-type random order)
          (.layer-sweep-type-decider this) (new-layer-sweep-type-decider this))
    (let ((initializables (jlist this (.crossings-counter this) (.layer-sweep-type-decider this)
                                 (.port-distributor this)))
          (force-mo (get-property graph layered-options.crossing-minimization-force-node-model-order)))
      (cond ((and (eq cross-min-type cross-min-type.barycenter) (not force-mo))
             (let ((resolver (new-forster-constraint-resolver order)))
               (jl-add initializables resolver)
               (setf (.cross-minimizer this)
                     (new-barycenter-heuristic resolver random (.port-distributor this) order))))
            ((and (eq cross-min-type cross-min-type.barycenter) force-mo)
             (let ((resolver (new-forster-constraint-resolver order)))
               (jl-add initializables resolver)
               (setf (.cross-minimizer this)
                     (new-model-order-barycenter-heuristic resolver random (.port-distributor this) order))))
            ((eq cross-min-type cross-min-type.median)
             (setf (.cross-minimizer this) (new-median-heuristic random)))
            (t (setf (.cross-minimizer this) (new-greedy-switch-heuristic cross-min-type this))))
      (jl-add initializables (.cross-minimizer this))
      (i-initializable.init initializables order)
      (setf (.use-bottom-up this) (use-bottom-up (.layer-sweep-type-decider this))))))

(defjmethod dont-sweep-into ((this graph-info-holder)) (.use-bottom-up this))
(defjmethod l-graph ((this graph-info-holder)) (.l-graph this))
(defjmethod current-node-order ((this graph-info-holder)) (.current-node-order this))
(defjmethod currently-best-node-and-port-order ((this graph-info-holder)) (.currently-best-node-and-port-order this))
(defjmethod set-currently-best-node-and-port-order ((this graph-info-holder) sc)
  (setf (.currently-best-node-and-port-order this) sc))
(defjmethod best-node-n-port-order ((this graph-info-holder)) (.best-node-and-port-order this))
(defjmethod set-best-node-n-port-order ((this graph-info-holder) sc) (setf (.best-node-and-port-order this) sc))
(defjmethod cross-counter ((this graph-info-holder)) (.crossings-counter this))
(defjmethod cross-minimizer ((this graph-info-holder)) (.cross-minimizer this))
(defjmethod port-distributor ((this graph-info-holder)) (.port-distributor this))
(defjmethod parent ((this graph-info-holder)) (.parent this))
(defjmethod has-parent ((this graph-info-holder)) (.has-parent this))
(defjmethod child-graphs ((this graph-info-holder)) (.child-graphs this))
(defjmethod has-external-ports ((this graph-info-holder)) (.has-external-ports this))
(defjmethod to-string ((this graph-info-holder)) (format nil "~A" (.current-node-order this)))
(defjmethod get-best-sweep ((this graph-info-holder))
  (if (is-deterministic (.cross-minimizer this))
      (currently-best-node-and-port-order this)
      (best-node-n-port-order this)))
(defjmethod parent-graph-data ((this graph-info-holder)) (.parent-graph-data this))
(defjmethod cross-min-deterministic ((this graph-info-holder)) (is-deterministic (.cross-minimizer this)))
(defjmethod cross-min-always-improves ((this graph-info-holder)) (always-improves (.cross-minimizer this)))
(defjmethod port-positions ((this graph-info-holder)) (.port-positions this))

(defjmethod init-at-node-level ((this graph-info-holder) l n node-order)
  (let ((nested (get-nested-graph (%p3-node-at node-order l n))))
    (when nested (jl-add (.child-graphs this) nested))))
(defjmethod init-at-port-level ((this graph-info-holder) l n p node-order)
  (declare (ignore l n p node-order))
  (incf (.n-ports this)))
(defjmethod init-after-traversal ((this graph-info-holder))
  (setf (.port-positions this) (%p3-int-array (.n-ports this))))

;;; ---------------------------------------------------------------- SweepCopy

(defun %sc-deep-copy (order)
  (when order
    (let ((result (make-array (length order))))
      (loop for i from 0 below (length order)
            ;; Java Arrays.copyOf: a fresh row. CL COERCE to SIMPLE-VECTOR returns the
            ;; SAME object for a simple-vector, which aliased the copy to the live order.
            do (setf (aref result i) (copy-seq (aref order i))))
      result)))

(defjctor sweep-copy (in)
  (if (typep in 'sweep-copy)
      (setf (.node-order this) (%sc-deep-copy (.node-order in))
            (.port-orders this) (jlist-from (.port-orders in)))
      (progn
        (setf (.node-order this) (%sc-deep-copy in)
              (.port-orders this) (new-jlist))
        (loop for l-nodes across in do
          (let ((layer (new-jlist)))
            (jl-add (.port-orders this) layer)
            (loop for node across l-nodes do (jl-add layer (jlist-from (get-ports node)))))))))

(defjmethod nodes ((this sweep-copy)) (.node-order this))

(defjmethod transfer-node-and-port-orders-to-graph ((this sweep-copy) l-graph set-port-constraints)
  ;; ALLOW_NON_FLOW_PORTS_TO_SWITCH_SIDES lets the crossing minimizer decide the
  ;; side a north/south dummy is placed on, so the configured port side may have
  ;; to be corrected.
  (let ((north-south-port-dummies (new-jlist))
        (update-port-order (new-jlist))
        (layers (get-layers l-graph)))
    (loop for i from 0 below (jsize layers) do
      (let ((nodes (get-nodes (j-get layers i))))
        (jl-clear north-south-port-dummies)
        (loop for j from 0 below (jsize nodes) do
          (let ((node (%p3-node-at (.node-order this) i j)))
            ;; use the id field to remember the order within the layer
            (setf (.id node) j)
            (when (eq (get-type node) node-type.north-south-port)
              (jl-add north-south-port-dummies node))
            (j-set (get-nodes (j-get (get-layers l-graph) i)) j node)
            (clear (get-ports node))
            (add-all (get-ports node) (jl-get (jl-get (.port-orders this) i) j))
            (when (and set-port-constraints
                       (not (is-order-fixed (get-property node layered-options.port-constraints))))
              (set-property node layered-options.port-constraints port-constraints.fixed-order))))
        (jdo (dummy north-south-port-dummies)
          (let ((origin (%sc-assert-correct-port-sides dummy)))
            (unless (find origin update-port-order) (jl-add update-port-order origin))
            (unless (find dummy update-port-order) (jl-add update-port-order dummy))))))
    ;; port sides may have changed: re-sort the port lists (see PortListSorter)
    ;; and re-cache the port list views.
    (jdo (node update-port-order)
      (jl-sort (get-ports node) port-list-sorter.cmp-combined)
      (cache-port-sides node))))

(defun %sc-assert-correct-port-sides (dummy)
  (let* ((origin (get-property dummy layered-internal-properties.in-layer-layout-unit))
         (dummy-port (j-get (get-ports dummy) 0)))
    (block found
      (jdo (port (get-ports origin))
        (when (jequals port (get-property dummy-port layered-internal-properties.origin))
          (cond ((and (eq (get-side port) port-side.north) (> (.id dummy) (.id origin)))
                 (set-side port port-side.south)
                 (when (is-explicitly-supplied-port-anchor port)
                   (let ((port-height (.y (get-size port))) (anchor-y (.y (get-anchor port))))
                     (setf (.y (get-anchor port)) (- port-height anchor-y)))))
                ((and (eq (get-side port) port-side.south) (> (.id origin) (.id dummy)))
                 (set-side port port-side.north)
                 (when (is-explicitly-supplied-port-anchor port)
                   (let ((port-height (.y (get-size port))) (anchor-y (.y (get-anchor port))))
                     (setf (.y (get-anchor port)) (- (- port-height anchor-y)))))))
          (return-from found))))
    origin))

;;; ---------------------------------------------------------------- AbstractBarycenterPortDistributor

(defjctor abstract-barycenter-port-distributor (num-layers)
  (setf (.in-layer-ports this) (new-jlist)
        (.node-positions this) (make-array num-layers :initial-element nil)))
(defjctor layer-total-port-distributor (num-layers)
  (super-ctor abstract-barycenter-port-distributor this num-layers))
(defjctor node-relative-port-distributor (num-layers)
  (super-ctor abstract-barycenter-port-distributor this num-layers))

(defjmethod get-port-ranks ((this abstract-barycenter-port-distributor)) (.port-ranks this))

(defun %abpd-position-of (this node)
  (aref (aref (.node-positions this) (.id (get-layer node))) (.id node)))

(defun %abpd-port-type-for (forward) (if forward port-type.output port-type.input))

(defjmethod distribute-ports-while-sweeping ((this abstract-barycenter-port-distributor) node-order current-index forward)
  (%abpd-update-node-positions this node-order current-index)
  (let ((free-layer (aref node-order current-index))
        (side (if forward port-side.west port-side.east)))
    (if (if forward (/= current-index 0) (/= current-index (1- (length node-order))))
        (let ((fixed-layer (aref node-order (if forward (1- current-index) (1+ current-index)))))
          (calculate-port-ranks this fixed-layer (%abpd-port-type-for forward))
          (loop for node across free-layer do (%abpd-distribute-ports-side this node side))
          (calculate-port-ranks this free-layer (%abpd-port-type-for (not forward)))
          (loop for node across fixed-layer
                unless (get-nested-graph node)
                  do (%abpd-distribute-ports-side this node (opposed side))))
        (loop for node across free-layer do (%abpd-distribute-ports-side this node side)))
    ;; cannot be used with always-improving heuristics which do not need to count
    nil))

;;; calculatePortRanks(LNode[], PortType) and the abstract calculatePortRanks(LNode, float,
;;; PortType) have different arities (2 and 3).
(defjmethod calculate-port-ranks ((this abstract-barycenter-port-distributor) layer port-type)
  (let ((consumed-rank 0f0))
    (loop for node across layer
          do (setf consumed-rank (+ consumed-rank (calculate-port-ranks this node consumed-rank port-type))))
    nil))
(defjgeneric calculate-port-ranks 3)

(defun %abpd-distribute-ports-side (this node side)
  (unless (is-order-fixed (get-property node layered-options.port-constraints))
    (%abpd-distribute-ports this node (get-ports node side))
    (%abpd-distribute-ports this node (get-ports node port-side.south))
    (%abpd-distribute-ports this node (get-ports node port-side.north))
    (%abpd-sort-ports this node)))

(defun %abpd-distribute-ports (this node ports)
  (jl-clear (.in-layer-ports this))
  (%abpd-iterate-ports-and-collect-in-layer-ports this node ports)
  (unless (jl-is-empty (.in-layer-ports this))
    (%abpd-calculate-in-layer-ports-barycenter-values this node)))

(defun %abpd-iterate-ports-and-collect-in-layer-ports (this node ports)
  (setf (.min-barycenter this) 0f0
        (.max-barycenter this) 0f0)
  (let ((absurdly-large-float (float (1+ (* 2 (jsize (get-nodes (get-layer node))))) 1f0))
        (ranks (.port-ranks this))
        (bary (.port-barycenter this)))
    (jdo (port ports)
      (block port-iteration
        (let ((north-south-port (or (eq (get-side port) port-side.north) (eq (get-side port) port-side.south)))
              (sum 0f0))
          (if north-south-port
              (let ((port-dummy (get-property port layered-internal-properties.port-dummy)))
                (when (null port-dummy) (return-from port-iteration))
                (setf sum (+ sum (%abpd-deal-with-north-south-ports this absurdly-large-float port port-dummy))))
              (progn
                (jdo (outgoing (get-outgoing-edges port))
                  (let ((connected (get-target outgoing)))
                    (if (eq (get-layer (get-node connected)) (get-layer node))
                        (progn (jl-add (.in-layer-ports this) port) (return-from port-iteration))
                        ;; outgoing edges go to the subsequent layer and are seen clockwise
                        (setf sum (+ sum (aref ranks (.id connected)))))))
                (jdo (incoming (get-incoming-edges port))
                  (let ((connected (get-source incoming)))
                    (if (eq (get-layer (get-node connected)) (get-layer node))
                        (progn (jl-add (.in-layer-ports this) port) (return-from port-iteration))
                        ;; incoming edges go to the preceding layer and are seen counter-clockwise
                        (setf sum (- sum (aref ranks (.id connected)))))))))
          (cond ((> (get-degree port) 0)
                 (setf (aref bary (.id port)) (/ sum (float (get-degree port) 1f0)))
                 (setf (.min-barycenter this) (math.min (.min-barycenter this) (aref bary (.id port)))
                       (.max-barycenter this) (math.max (.max-barycenter this) (aref bary (.id port)))))
                (north-south-port
                 ;; for northern and southern ports the sum is the barycenter value
                 (setf (aref bary (.id port)) sum))))))))

(defun %abpd-calculate-in-layer-ports-barycenter-values (this node)
  (let ((node-index-in-layer (1+ (%abpd-position-of this node)))
        (layer-size (1+ (jsize (get-nodes (get-layer node)))))
        (bary (.port-barycenter this)))
    (jdo (in-layer-port (.in-layer-ports this))
      (let ((sum 0) (in-layer-connections 0))
        (jdo (connected (get-connected-ports in-layer-port))
          (when (eq (get-layer (get-node connected)) (get-layer node))
            (incf sum (1+ (%abpd-position-of this (get-node connected))))
            (incf in-layer-connections)))
        ;; The mean index of connected nodes: lower than the node's index means most
        ;; in-layer edges point upwards, so the port goes to the top of the side.
        (let ((barycenter (/ (float sum 1f0) (float in-layer-connections 1f0)))
              (port-side (get-side in-layer-port)))
          (cond ((eq port-side port-side.east)
                 (setf (aref bary (.id in-layer-port))
                       (if (< barycenter node-index-in-layer)
                           (- (.min-barycenter this) barycenter)
                           (+ (.max-barycenter this) (- layer-size barycenter)))))
                ((eq port-side port-side.west)
                 (setf (aref bary (.id in-layer-port))
                       (if (< barycenter node-index-in-layer)
                           (+ (.max-barycenter this) barycenter)
                           (- (.min-barycenter this) (- layer-size barycenter)))))))))))

(defun %abpd-deal-with-north-south-ports (this absurdly-large-float port port-dummy)
  (let ((input nil) (output nil))
    (jdo (dp (get-ports port-dummy))
      (when (eq (get-property dp layered-internal-properties.origin) port)
        (cond ((not (is-empty (get-outgoing-edges dp))) (setf output t))
              ((not (is-empty (get-incoming-edges dp))) (setf input t)))))
    (let ((pos (float (%abpd-position-of this port-dummy) 1f0)))
      (cond ((and input (not output))
             ;; an input port: the index of its dummy node is its inverted sort key
             (if (eq (get-side port) port-side.north) (- pos) (- absurdly-large-float pos)))
            ((and output (not input))
             ;; an output port: the index of its dummy node is its sort key
             (+ pos 1f0))
            ((and input output)
             ;; both: between input and output ports
             (if (eq (get-side port) port-side.north) 0f0 (/ absurdly-large-float 2f0)))
            (t 0f0)))))

(defun %abpd-update-node-positions (this node-order current-index)
  (let ((layer (aref node-order current-index)))
    (loop for i from 0 below (length layer)
          do (let ((node (aref layer i)))
               (setf (aref (aref (.node-positions this) (.id (get-layer node))) (.id node)) i)))))

(defun %abpd-sort-ports (this node)
  (let ((bary (.port-barycenter this)))
    (jl-sort (get-ports node)
             (lambda (p1 p2)
               (let ((s1 (get-side p1)) (s2 (get-side p2)))
                 (if (not (eq s1 s2))
                     (- (ordinal s1) (ordinal s2))
                     (let ((b1 (aref bary (.id p1))) (b2 (aref bary (.id p2))))
                       (cond ((and (= b1 0) (= b2 0)) 0)
                             ((= b1 0) -1)
                             ((= b2 0) 1)
                             (t (%p3-float-compare b1 b2))))))))))

(defjmethod init-at-layer-level ((this abstract-barycenter-port-distributor) l node-order)
  (setf (aref (.node-positions this) l) (%p3-int-array (length (aref node-order l)))))
(defjmethod init-at-node-level ((this abstract-barycenter-port-distributor) l n node-order)
  (let ((node (%p3-node-at node-order l n)))
    (setf (.id node) n
          (aref (aref (.node-positions this) l) n) n)))
(defjmethod init-at-port-level ((this abstract-barycenter-port-distributor) l n p node-order)
  (setf (.id (j-get (get-ports (%p3-node-at node-order l n)) p)) (.n-ports this))
  (incf (.n-ports this)))
(defjmethod init-after-traversal ((this abstract-barycenter-port-distributor))
  (setf (.port-ranks this) (%p3-float-array (.n-ports this))
        (.port-barycenter this) (%p3-float-array (.n-ports this))))

;;; ---------------------------------------------------------------- LayerTotal / NodeRelative

(defun %p3-count-inputs (node)
  (let ((input-count 0) (north-input-count 0))
    (jdo (port (get-ports node))
      (unless (is-empty (get-incoming-edges port))
        (incf input-count)
        (when (eq (get-side port) port-side.north) (incf north-input-count))))
    (values input-count north-input-count)))

(defjmethod calculate-port-ranks ((this layer-total-port-distributor) node rank-sum type)
  (let ((port-ranks (get-port-ranks this)))
    (enum-case type
      (port-type.input
       (multiple-value-bind (input-count north-input-count) (%p3-count-inputs node)
         ;; north - west - south - east
         (let ((north-pos (+ rank-sum north-input-count))
               (rest-pos (+ rank-sum input-count)))
           (jdo (port (get-ports node port-type.input))
             (if (eq (get-side port) port-side.north)
                 (progn (setf (aref port-ranks (.id port)) north-pos) (setf north-pos (- north-pos 1)))
                 (progn (setf (aref port-ranks (.id port)) rest-pos) (setf rest-pos (- rest-pos 1)))))
           (float input-count 1f0))))
      (port-type.output
       ;; natural order: north - east - south - west
       (let ((pos 0))
         (jdo (port (get-ports node port-type.output))
           (incf pos)
           (setf (aref port-ranks (.id port)) (+ rank-sum pos)))
         (float pos 1f0)))
      (t (jthrow 'illegal-argument-exception)))))

(defjmethod calculate-port-ranks ((this node-relative-port-distributor) node rank-sum type)
  (let ((port-ranks (get-port-ranks this)))
    (enum-case type
      (port-type.input
       (multiple-value-bind (input-count north-input-count) (%p3-count-inputs node)
         (let* ((incr (/ 1f0 (float (1+ input-count) 1f0)))
                (north-pos (+ rank-sum (* north-input-count incr)))
                (rest-pos (- (+ rank-sum 1) incr)))
           (jdo (port (get-ports node port-type.input))
             (if (eq (get-side port) port-side.north)
                 (progn (setf (aref port-ranks (.id port)) north-pos) (setf north-pos (- north-pos incr)))
                 (progn (setf (aref port-ranks (.id port)) rest-pos) (setf rest-pos (- rest-pos incr))))))))
      (port-type.output
       (let ((output-count 0))
         (jdo (port (get-ports node))
           (unless (is-empty (get-outgoing-edges port)) (incf output-count)))
         (let* ((incr (/ 1f0 (float (1+ output-count) 1f0)))
                (pos (+ rank-sum incr)))
           (jdo (port (get-ports node port-type.output))
             (setf (aref port-ranks (.id port)) pos)
             (setf pos (+ pos incr))))))
      (t (jthrow 'illegal-argument-exception "Port type is undefined")))
    ;; the consumed rank is always 1
    1f0))

;;; ---------------------------------------------------------------- GreedyPortDistributor

(defjmethod distribute-ports-while-sweeping ((this greedy-port-distributor) node-order current-index forward)
  (%gpd-initialize this node-order current-index forward)
  (%gpd-distribute-ports-in-layer this node-order current-index forward))

(defun %gpd-distribute-ports-in-layer (this node-order current-index forward)
  (let ((side (if forward port-side.west port-side.east)) (improved nil))
    (loop for node across (aref node-order current-index) do
      (unless (is-order-fixed (get-property node layered-options.port-constraints))
        (let* ((nested (get-nested-graph node))
               (use-hier (and (not (is-empty (get-port-side-view node side))) nested)))
          (when use-hier
            (let ((inner (to-node-array nested)))
              (setf (.hierarchical-crossings-counter this)
                    (new-between-layer-edge-two-node-crossings-counter
                     inner (if forward 0 (1- (length inner)))))))
          (when (%gpd-distribute-ports-on-node this node side (and use-hier t))
            (setf improved t)))))
    improved))

(defun %gpd-distribute-ports-on-node (this node side use-hier)
  ;; PORTS is node.getPortSideView(side), reversed for SOUTH/WEST: a live view,
  ;; so the swaps in %GPD-SWITCH-PORTS write through to the node's port list.
  (let* ((rev (or (eq side port-side.south) (eq side port-side.west)))
         (view (get-port-side-view node side))
         (n (length view))
         (improved nil) (continue-switching t))
    (flet ((ref (i) (aref view (if rev (- n 1 i) i))))
      (loop while continue-switching do
        (setf continue-switching nil)
        (loop for i from 0 below (1- n) do
          (when (%gpd-switching-decreases-crossings this (ref i) (ref (1+ i)) node use-hier)
            (setf improved t)
            (%gpd-switch-ports this view rev i (1+ i))
            (setf continue-switching t)))))
    improved))

(defun %gpd-switching-decreases-crossings (this upper-port lower-port node use-hier)
  (declare (ignore node))
  (let* ((pr (count-crossings-between-ports-in-both-orders (.crossings-counter this) upper-port lower-port))
         (upper-lower (get-first pr))
         (lower-upper (get-second pr)))
    (when use-hier
      (let ((upper-node (get-property upper-port layered-internal-properties.port-dummy))
            (lower-node (get-property lower-port layered-internal-properties.port-dummy)))
        (when (and upper-node lower-node)
          (let ((h (.hierarchical-crossings-counter this)))
            (count-both-side-crossings h upper-node lower-node)
            (incf upper-lower (get-upper-lower-crossings h))
            (incf lower-upper (get-lower-upper-crossings h))))))
    (> upper-lower lower-upper)))

(defun %gpd-switch-ports (this view rev top bottom)
  ;; Java: ports.get/set on the (possibly reversed) live side view.
  (let* ((n (length view))
         (ti (if rev (- n 1 top) top)) (bi (if rev (- n 1 bottom) bottom))
         (tp (aref view ti)) (bp (aref view bi)))
    (switch-ports (.crossings-counter this) tp bp)
    (setf (aref view bi) tp (aref view ti) bp)))

(defun %gpd-initialize (this node-order current-index forward)
  (let ((cc (.crossings-counter this)))
    (cond ((and forward (> current-index 0))
           (init-for-counting-between cc (aref node-order (1- current-index)) (aref node-order current-index)))
          ((and (not forward) (< current-index (1- (length node-order))))
           (init-for-counting-between cc (aref node-order current-index) (aref node-order (1+ current-index))))
          (t (init-port-positions-for-in-layer-crossings
              cc (aref node-order current-index) (if forward port-side.west port-side.east))))))

(defjmethod init-at-node-level ((this greedy-port-distributor) l n node-order)
  (incf (.n-ports this) (jsize (get-ports (%p3-node-at node-order l n)))))
(defjmethod init-after-traversal ((this greedy-port-distributor))
  (setf (.port-pos this) (%p3-int-array (.n-ports this))
        (.crossings-counter this) (new-crossings-counter (.port-pos this))))

;;; ---------------------------------------------------------------- BarycenterHeuristic

(defjconst barycenter-heuristic.random-amount 0.07f0)

(defun %bh-state-comparator (this)
  (lambda (n1 n2)
    (let ((b1 (.barycenter (state-of this n1))) (b2 (.barycenter (state-of this n2))))
      (cond ((and b1 b2) (double.compare b1 b2))
            (b1 -1)
            (b2 1)
            (t 0)))))

(defjctor barycenter-heuristic (constraint-resolver random port-distributor graph)
  graph                                 ; the Java constructor ignores its LGraph argument
  (setf (.constraint-resolver this) constraint-resolver
        (.random this) random
        (.port-distributor this) port-distributor
        (.barycenter-state-comparator this) (%bh-state-comparator this)))

(defjctor barycenter-state (node) (setf (.node this) node))
(defjmethod to-string ((this barycenter-state))
  (jstr "BarycenterState [node=" (.node this) ", summedWeight=" (.summed-weight this)
        ", degree=" (.degree this) ", barycenter=" (.barycenter this) ", visited=" (.visited this) "]"))

(defjmethod state-of ((this barycenter-heuristic) node)
  (aref (aref (.barycenter-state this) (.id (get-layer node))) (.id node)))

;;; minimizeCrossings(List, boolean, boolean, boolean) and minimizeCrossings(LNode[][], int,
;;; boolean, boolean) share an arity; the second argument tells them apart.
(defmethod minimize-crossings/4 ((this barycenter-heuristic) a b c d)
  (if (integerp b)
      (%bh-minimize-crossings-order this a b c d)
      (%p3-minimize-crossings-layer this a b c d)))

(defgeneric %p3-minimize-crossings-layer (this layer pre-ordered randomize forward))

(defmethod %p3-minimize-crossings-layer ((this barycenter-heuristic) layer pre-ordered randomize forward)
  (if randomize
      ;; a permutation of the nodes in the layer
      (randomize-barycenters this layer)
      (progn (calculate-barycenters this layer forward)
             (fill-in-unknown-barycenters this layer pre-ordered)))
  (when (> (jsize layer) 1)
    (if (get-property (get-graph (j-get layer 0)) layered-options.crossing-minimization-force-node-model-order)
        (model-order-barycenter-heuristic.insertion-sort layer (.barycenter-state-comparator this) this)
        (jl-sort layer (.barycenter-state-comparator this)))
    (process-constraints (.constraint-resolver this) layer)))

(defjmethod randomize-barycenters ((this barycenter-heuristic) nodes)
  (jdo (node nodes)
    (let ((s (state-of this node)))
      (setf (.barycenter s) (next-double (.random this))
            (.summed-weight s) (.barycenter s)
            (.degree s) 1))))

(defjmethod fill-in-unknown-barycenters ((this barycenter-heuristic) nodes pre-ordered)
  (if pre-ordered
      (let ((last-value -1d0))
        (loop for idx from 0 below (jsize nodes) do
          (let* ((node (j-get nodes idx))
                 (value (.barycenter (state-of this node))))
            (when (null value)
              ;; take the center of the previous value and the next defined value
              (let ((next-value (+ last-value 1)))
                (loop for k from (1+ idx) below (jsize nodes)
                      do (let ((x (.barycenter (state-of this (j-get nodes k)))))
                           (when x (setf next-value x) (return))))
                (setf value (/ (+ last-value next-value) 2))
                (let ((s (state-of this node)))
                  (setf (.barycenter s) value (.summed-weight s) value (.degree s) 1))))
            (setf last-value value))))
      (let ((max-bary 0d0))
        (jdo (node nodes)
          (let ((b (.barycenter (state-of this node))))
            (when b (setf max-bary (math.max max-bary b)))))
        (setf max-bary (+ max-bary 2))
        (jdo (node nodes)
          (let ((s (state-of this node)))
            (when (null (.barycenter s))
              (let ((value (- (* (next-float (.random this)) max-bary) 1)))
                (setf (.barycenter s) value (.summed-weight s) value (.degree s) 1))))))))

(defjmethod calculate-barycenters ((this barycenter-heuristic) nodes forward)
  (jdo (node nodes) (setf (.visited (state-of this node)) nil))
  (jdo (node nodes) (%bh-calculate-barycenter this node forward)))

(defun %bh-calculate-barycenter (this node forward)
  (let ((s (state-of this node)))
    (if (.visited s)
        (return-from %bh-calculate-barycenter)
        (setf (.visited s) t))
    (setf (.degree s) 0 (.summed-weight s) 0d0 (.barycenter s) nil)
    (jdo (free-port (get-ports node))
      (jdo (fixed-port (if forward (get-predecessor-ports free-port) (get-successor-ports free-port)))
        (let ((fixed-node (get-node fixed-port)))
          (if (eq (get-layer fixed-node) (get-layer node))
              ;; in-layer edge: use the fixed node's barycenter; self-loops are ignored
              (unless (eq fixed-node node)
                (%bh-calculate-barycenter this fixed-node forward)
                (incf (.degree s) (.degree (state-of this fixed-node)))
                (incf (.summed-weight s) (.summed-weight (state-of this fixed-node))))
              (progn
                (incf (.summed-weight s) (float (aref (.port-ranks this) (.id fixed-port)) 1d0))
                (incf (.degree s)))))))
    (let ((associates (get-property node layered-internal-properties.barycenter-associates)))
      (when associates
        (jdo (associate associates)
          (when (eq (get-layer node) (get-layer associate))
            (%bh-calculate-barycenter this associate forward)
            (incf (.degree s) (.degree (state-of this associate)))
            (incf (.summed-weight s) (.summed-weight (state-of this associate)))))))
    (when (> (.degree s) 0)
      ;; a small random perturbation to increase the diversity of solutions (float arithmetic)
      (let ((amount barycenter-heuristic.random-amount))
        (incf (.summed-weight s)
              (float (- (* (float (next-float (.random this)) 1f0) amount) (/ amount 2f0)) 1d0)))
      (setf (.barycenter s) (/ (.summed-weight s) (.degree s))))))

(defun %bh-start-index (forward length) (if forward 0 (max 0 (1- length))))

(defun %bh-minimize-crossings-order (this order free-layer-index forward-sweep is-first-sweep)
  (unless (= free-layer-index (%bh-start-index forward-sweep (length order)))
    (let ((fixed-layer (aref order (- free-layer-index (if forward-sweep 1 -1)))))
      (calculate-port-ranks (.port-distributor this) fixed-layer
                            (if forward-sweep port-type.output port-type.input))))
  (let* ((first-node (%p3-node-at order free-layer-index 0))
         (pre-ordered (or (not is-first-sweep) (eq (get-type first-node) node-type.external-port)))
         (nodes (jlist-from (aref order free-layer-index))))
    (%p3-minimize-crossings-layer this nodes pre-ordered nil forward-sweep)
    (loop for n across nodes for index from 0
          do (setf (aref (aref order free-layer-index) index) n))
    nil))

(defjmethod set-first-layer-order ((this barycenter-heuristic) order forward)
  (let* ((start-index (%bh-start-index forward (length order)))
         (nodes (jlist-from (aref order start-index))))
    (%p3-minimize-crossings-layer this nodes nil t forward)
    (loop for n across nodes for index from 0
          do (setf (aref (aref order start-index) index) n))
    nil))

(defjmethod always-improves ((this barycenter-heuristic)) nil)
(defjmethod is-deterministic ((this barycenter-heuristic)) nil)
(defjmethod init-after-traversal ((this barycenter-heuristic))
  (setf (.barycenter-state this) (get-barycenter-states (.constraint-resolver this))
        (.port-ranks this) (get-port-ranks (.port-distributor this))))
(defjmethod init-at-layer-level ((this barycenter-heuristic) l node-order)
  (setf (.id (get-layer (%p3-node-at node-order l 0))) l))

;;; ---------------------------------------------------------------- ModelOrderBarycenterHeuristic

(defun %mobh-separate-p (n)
  (and (has-property n layered-options.layering-layer-constraint)
       (let ((c (get-property n layered-options.layering-layer-constraint)))
         (or (eq c layer-constraint.first-separate) (eq c layer-constraint.last-separate)))))

(defjctor model-order-barycenter-heuristic (constraint-resolver random port-distributor graph)
  (super-ctor barycenter-heuristic this constraint-resolver random port-distributor graph)
  (setf (.bigger-than this) (new-jmap) (.smaller-than this) (new-jmap))
  ;; The real nodes may need to be compared first such that the dummy nodes do
  ;; not overrule model order using the barycenter method.
  (setf (.barycenter-state-comparator this)
        (lambda (n1 n2)
          (block cmp
            (when (or (%mobh-separate-p n1) (%mobh-separate-p n2)) (return-from cmp 0))
            (let ((lgraph (get-graph n1))
                  (transitive (%mobh-compare-transitive this n1 n2)))
              (unless (zerop transitive) (return-from cmp transitive))
              (when (and (has-property n1 layered-internal-properties.model-order)
                         (has-property n2 layered-internal-properties.model-order))
                (let* ((max (get-property lgraph layered-internal-properties.max-model-order-nodes))
                       (value (integer.compare
                               (cm-group-model-order-calculator.calculate-model-order-or-group-model-order n1 n2 lgraph max)
                               (cm-group-model-order-calculator.calculate-model-order-or-group-model-order n2 n1 lgraph max))))
                  (when (and (eq (get-property lgraph layered-options.consider-model-order-group-model-order-cm-group-order-strategy)
                                 group-order-strategy.only-within-group)
                             (not (eql (get-property n1 layered-options.consider-model-order-group-model-order-crossing-minimization-id)
                                       (get-property n2 layered-options.consider-model-order-group-model-order-crossing-minimization-id))))
                    ;; separate model order groups: do not enforce, use the barycenter
                    (setf value 0))
                  (cond ((< value 0) (%mobh-update this n1 n2) (return-from cmp value))
                        ((> value 0) (%mobh-update this n2 n1) (return-from cmp value)))))
              (%mobh-compare-based-on-barycenter this n1 n2))))))

(defmethod %p3-minimize-crossings-layer ((this model-order-barycenter-heuristic) layer pre-ordered randomize forward)
  (if randomize
      (randomize-barycenters this layer)
      (progn (calculate-barycenters this layer forward)
             (fill-in-unknown-barycenters this layer pre-ordered)))
  (when (> (jsize layer) 1)
    (let ((force (get-property (get-graph (j-get layer 0)) layered-options.crossing-minimization-force-node-model-order)))
      (if force
          (model-order-barycenter-heuristic.insertion-sort layer (.barycenter-state-comparator this) this)
          (jl-sort layer (.barycenter-state-comparator this)))
      (unless (get-property (get-graph (j-get layer 0)) layered-options.crossing-minimization-force-node-model-order)
        (process-constraints (.constraint-resolver this) layer)))))

(defun %mobh-ensure (map key)
  (multiple-value-bind (v found) (gethash key map)
    (if found (values v t) (progn (setf (gethash key map) (new-jset)) (values nil nil)))))

(defun %mobh-compare-transitive (this n1 n2)
  (let ((bt (.bigger-than this)) (st (.smaller-than this)))
    (multiple-value-bind (s found) (%mobh-ensure bt n1)
      (when (and found (js-contains s n2)) (return-from %mobh-compare-transitive 1)))
    (multiple-value-bind (s found) (%mobh-ensure bt n2)
      (when (and found (js-contains s n1)) (return-from %mobh-compare-transitive -1)))
    (multiple-value-bind (s found) (%mobh-ensure st n1)
      (when (and found (js-contains s n2)) (return-from %mobh-compare-transitive -1)))
    (multiple-value-bind (s found) (%mobh-ensure st n2)
      (when (and found (js-contains s n1)) (return-from %mobh-compare-transitive 1)))
    0))

(defun %mobh-compare-based-on-barycenter (this n1 n2)
  (let ((b1 (.barycenter (state-of this n1))) (b2 (.barycenter (state-of this n2))))
    (cond ((and b1 b2)
           (let ((value (double.compare b1 b2)))
             (cond ((< value 0) (%mobh-update this n1 n2))
                   ((> value 0) (%mobh-update this n2 n1)))
             value))
          (b1 (%mobh-update this n1 n2) -1)
          (b2 (%mobh-update this n2 n1) 1)
          (t 0))))

(defun %mobh-update (this bigger smaller)
  (let* ((bt (.bigger-than this)) (st (.smaller-than this))
         (bigger-bigger (gethash bigger bt))
         (smaller-bigger (gethash smaller bt))
         (bigger-smaller (gethash bigger st))
         (smaller-smaller (gethash smaller st)))
    (js-add bigger-bigger smaller)
    (js-add smaller-smaller bigger)
    (jdo (very-small smaller-bigger)
      (js-add bigger-bigger very-small)
      (js-add (gethash very-small st) bigger)
      (js-add-all (gethash very-small st) bigger-smaller))
    (jdo (very-big bigger-smaller)
      (js-add smaller-smaller very-big)
      (js-add (gethash very-big bt) smaller)
      (js-add-all (gethash very-big bt) smaller-bigger))))

(defjstatic model-order-barycenter-heuristic.insertion-sort (layer comparator barycenter-heuristic)
  (loop for i from 1 below (jsize layer) do
    (let ((temp (j-get layer i)) (j i))
      (loop while (and (> j 0) (> (funcall comparator (j-get layer (1- j)) temp) 0))
            do (j-set layer j (j-get layer (1- j)))
               (decf j))
      (j-set layer j temp)))
  (clear-transitive-ordering barycenter-heuristic))

(defjmethod clear-transitive-ordering ((this model-order-barycenter-heuristic))
  (setf (.bigger-than this) (new-jmap) (.smaller-than this) (new-jmap)))

;;; ---------------------------------------------------------------- MedianHeuristic

(defun %mh-weight-comparator ()
  (lambda (n1 n2)
    (cond ((not (has-property n1 layered-internal-properties.weight)) 1)
          ((not (has-property n2 layered-internal-properties.weight)) -1)
          (t (let ((w1 (get-property n1 layered-internal-properties.weight))
                   (w2 (get-property n2 layered-internal-properties.weight)))
               (cond ((and w1 w2) (double.compare w1 w2))
                     (w1 -1)
                     (w2 1)
                     (t 0)))))))

(defjctor median-heuristic (random)
  (setf (.random this) random
        (.weight-comparator this) (%mh-weight-comparator))
  ;; Java calls Math.random() here; its value is unused.
  nil)

(defjmethod always-improves ((this median-heuristic)) nil)

(defjmethod set-first-layer-order ((this median-heuristic) order forward)
  (let* ((first-index (if forward 0 (max 0 (1- (length order)))))
         (first-layer (jlist-from (aref order first-index))))
    (jdo (node first-layer)
      (set-property node layered-internal-properties.weight (next-double (.random this))))
    (jl-sort first-layer (.weight-comparator this))
    (let ((index 0))
      (jdo (node first-layer)
        (setf (aref (aref order first-index) index) node)
        (incf index)
        (set-property node layered-internal-properties.weight (float index 1d0))))
    nil))

(defmethod minimize-crossings/4 ((this median-heuristic) order free-layer-index forward-sweep is-first-sweep)
  (declare (ignore is-first-sweep))
  (let ((free-layer (jlist-from (aref order free-layer-index))))
    (%mh-calculate-medians this free-layer (if forward-sweep (1- free-layer-index) (1+ free-layer-index)))
    (jl-sort free-layer (.weight-comparator this))
    (loop for node across free-layer for index from 0
          do (setf (aref (aref order free-layer-index) index) node))
    nil))
(define-jcall minimize-crossings :instance)

(defun %mh-calculate-medians (this nodes reference-layer)
  (let ((min-weight double.min-value) (max-weight double.max-value) (to-revisit (new-jlist)))
    (jdo (node nodes)
      (let ((connected (new-jlist)))
        (flet ((visit (edge)
                 (let ((target (get-node (get-target edge))) (source (get-node (get-source edge))))
                   (when (= (.id (get-layer target)) reference-layer) (jl-add connected target))
                   (when (= (.id (get-layer source)) reference-layer) (jl-add connected source)))))
          (jdo (edge (get-incoming-edges node)) (visit edge))
          (jdo (edge (get-outgoing-edges node)) (visit edge)))
        (if (jl-is-empty connected)
            (jl-add to-revisit node)
            (progn
              (jl-sort connected (.weight-comparator this))
              (let ((new-weight (get-property (aref connected (idiv (length connected) 2))
                                              layered-internal-properties.weight)))
                (set-property node layered-internal-properties.weight new-weight)
                (setf min-weight (math.min min-weight new-weight)
                      max-weight (math.max max-weight new-weight)))))))
    (let ((avg-weight (/ (+ max-weight min-weight) 2d0)))
      (jdo (n to-revisit) (set-property n layered-internal-properties.weight avg-weight)))))

(defjmethod is-deterministic ((this median-heuristic)) t)

;;; ---------------------------------------------------------------- ForsterConstraintResolver

(defjctor forster-constraint-resolver (current-node-order)
  (when (and (> (length current-node-order) 0) (> (length (aref current-node-order 0)) 0))
    (setf (.constraints-between-non-dummies this)
          (get-property (get-graph (%p3-node-at current-node-order 0 0))
                        layered-internal-properties.in-layer-successor-constraints-between-non-dummies)))
  (setf (.barycenter-states this) (make-array (length current-node-order) :initial-element nil)
        (.constraint-groups this) (make-array (length current-node-order) :initial-element nil)
        ;; LinkedHashMultimap: key -> jlist of distinct values in insertion order
        (.layout-units this) (new-jmap)))

(defun %fcr-layout-units-get (this key) (or (gethash key (.layout-units this)) (new-jlist)))

(defjmethod init-at-layer-level ((this forster-constraint-resolver) l node-order)
  (let ((n (length (aref node-order l))))
    (setf (aref (.barycenter-states this) l) (make-array n :initial-element nil)
          (aref (.constraint-groups this) l) (make-array n :initial-element nil))))

(defjmethod init-at-node-level ((this forster-constraint-resolver) l n node-order)
  (%fcr-init-node this (%p3-node-at node-order l n) t))

(defun %fcr-init-node (this node full-init)
  (let ((layer-index (.id (get-layer node))) (node-index (.id node)))
    (setf (aref (aref (.constraint-groups this) layer-index) node-index) (new-constraint-group this node))
    (when full-init
      (setf (aref (aref (.barycenter-states this) layer-index) node-index) (new-barycenter-state node))
      (let ((unit (get-property node layered-internal-properties.in-layer-layout-unit)))
        (when unit
          (let ((values (or (gethash unit (.layout-units this))
                            (setf (gethash unit (.layout-units this)) (new-jlist)))))
            (unless (find node values) (jl-add values node))))))))

(defjmethod get-barycenter-states ((this forster-constraint-resolver)) (.barycenter-states this))

(defun %fcr-group-of (this node)
  (aref (aref (.constraint-groups this) (.id (get-layer node))) (.id node)))
(defun %fcr-state-of (this node)
  (aref (aref (.barycenter-states this) (.id (get-layer node))) (.id node)))

(defjmethod process-constraints ((this forster-constraint-resolver) nodes)
  ;; With successor constraints between regular nodes, stage 1 resolves those;
  ;; stage 2 orders their north/south port dummies and keeps layout units from
  ;; overlapping, which needs the order of normal nodes fixed first.
  (when (.constraints-between-non-dummies this)
    (%fcr-process-constraints this nodes t)
    (jdo (node nodes) (%fcr-init-node this node nil)))
  (%fcr-process-constraints this nodes nil))

(defun %fcr-process-constraints (this nodes only-between-normal-nodes)
  (let ((groups (new-jlist)))
    (jdo (node nodes) (jl-add groups (%fcr-group-of this node)))
    (%fcr-build-constraints-graph this groups only-between-normal-nodes)
    (loop for violated = (%fcr-find-violated-constraint groups)
          while violated
          do (%fcr-handle-violated-constraint this (car violated) (cdr violated) groups))
    (clear nodes)
    (jdo (group groups)
      (loop for node across (get-nodes group)
            do (add nodes node)
               (setf (.barycenter (%fcr-state-of this node)) (get-barycenter group))))))

(defun %fcr-build-constraints-graph (this groups only-between-normal-nodes)
  (jdo (group groups)
    (reset-outgoing-constraints group)
    (setf (.incoming-constraints-count group) 0))
  (let ((last-non-dummy-node nil))
    (jdo (group groups)
      (block continue
        (let ((node (get-node group)))
          (when (and only-between-normal-nodes (not (eq (get-type node) node-type.normal)))
            (return-from continue))
          (jdo (successor (get-property node layered-internal-properties.in-layer-successor-constraints))
            (when (or (not only-between-normal-nodes) (eq (get-type successor) node-type.normal))
              (jl-add (get-outgoing-constraints group) (%fcr-group-of this successor))
              (incf (.incoming-constraints-count (%fcr-group-of this successor)))))
          ;; For a normal node, keep the previous normal node's layout unit and
          ;; this node's layout unit from mixing.
          (when (and (not only-between-normal-nodes) (eq (get-type node) node-type.normal))
            (when last-non-dummy-node
              (jdo (last-unit-node (%fcr-layout-units-get this last-non-dummy-node))
                (jdo (current-unit-node (%fcr-layout-units-get this node))
                  (jl-add (get-outgoing-constraints (%fcr-group-of this last-unit-node))
                          (%fcr-group-of this current-unit-node))
                  (incf (.incoming-constraints-count (%fcr-group-of this current-unit-node))))))
            (setf last-non-dummy-node node)))))))

(defun %fcr-find-violated-constraint (groups)
  "A cons (predecessor . group), or NIL."
  (let ((active-groups nil))
    (jdo (group groups)
      (reset-incoming-constraints group)
      (when (and (has-outgoing-constraints group) (zerop (.incoming-constraints-count group)))
        (unless active-groups (setf active-groups (new-jlist)))
        (jl-add active-groups group)))
    (when active-groups
      (loop while (plusp (length active-groups)) do
        (let ((group (jl-remove-at active-groups 0)))
          (when (has-incoming-constraints group)
            (jdo (predecessor (get-incoming-constraints group))
              (let ((pb (get-barycenter predecessor)) (gb (get-barycenter group)))
                (if (= (float pb 1f0) (float gb 1f0))
                    (when (> (position predecessor groups) (position group groups))
                      ;; equal barycenter, but higher index
                      (return-from %fcr-find-violated-constraint (cons predecessor group)))
                    (when (> pb gb)
                      (return-from %fcr-find-violated-constraint (cons predecessor group)))))))
          (jdo (successor (get-outgoing-constraints group))
            (let ((incoming (get-incoming-constraints successor)))
              (jl-add-at incoming 0 group)
              (when (= (.incoming-constraints-count successor) (length incoming))
                (jl-add active-groups successor)))))))
    nil))

(defjconst forster-constraint-resolver.barycenter-equality-delta 0.0001f0)

(defun %fcr-handle-violated-constraint (this first-group second-group groups)
  (let ((new-group (new-constraint-group this first-group second-group))
        (already-inserted nil)
        (i 0))
    ;; Remove the two groups, insert the new one keeping the list sorted by
    ;; barycenter, and redirect constraints on the way (a ListIterator walk).
    (loop while (< i (length groups)) do
      (let ((group (aref groups i)))
        (incf i)
        (cond ((or (eq group first-group) (eq group second-group))
               (decf i)
               (jl-remove-at groups i))
              ((and (not already-inserted) (> (get-barycenter group) (get-barycenter new-group)))
               ;; previous() + add(): the current group is looked at again next time
               (decf i)
               (jl-add-at groups i new-group)
               (incf i)
               (setf already-inserted t))
              ((has-outgoing-constraints group)
               (let ((c1 (jl-remove (get-outgoing-constraints group) first-group))
                     (c2 (jl-remove (get-outgoing-constraints group) second-group)))
                 (when (or c1 c2)
                   (jl-add (get-outgoing-constraints group) new-group)
                   (incf (.incoming-constraints-count new-group))))))))
    (unless already-inserted (jl-add groups new-group))))

;;; ConstraintGroup (inner class: OUTER is the resolver)

(defjctor constraint-group (outer node)
  (setf (.outer this) outer
        (.nodes this) (vector node)))

(defjctor constraint-group (outer g1 g2)
  (setf (.outer this) outer)
  (let ((nodes1 (.nodes g1)) (nodes2 (.nodes g2)))
    (setf (.nodes this) (concatenate 'simple-vector nodes1 nodes2)))
  ;; Add constraints, not to g1 or g2, decrementing the incoming count of those
  ;; that are successors of both
  (cond ((.outgoing-constraints g1)
         (setf (.outgoing-constraints this) (jlist-from (.outgoing-constraints g1)))
         (jl-remove (.outgoing-constraints this) g2)
         (when (.outgoing-constraints g2)
           (jdo (candidate (.outgoing-constraints g2))
             (cond ((eq candidate g1))
                   ((find candidate (.outgoing-constraints this))
                    (decf (.incoming-constraints-count candidate)))
                   (t (jl-add (.outgoing-constraints this) candidate))))))
        ((.outgoing-constraints g2)
         (setf (.outgoing-constraints this) (jlist-from (.outgoing-constraints g2)))
         (jl-remove (.outgoing-constraints this) g1)))
  (setf (.summed-weight this) (+ (.summed-weight g1) (.summed-weight g2))
        (.degree this) (+ (.degree g1) (.degree g2)))
  (let ((b1 (get-barycenter g1)) (b2 (get-barycenter g2)))
    (cond ((> (.degree this) 0) (set-barycenter this (/ (.summed-weight this) (.degree this))))
          ((and b1 b2) (set-barycenter this (/ (+ b1 b2) 2)))
          (b1 (set-barycenter this b1))
          (b2 (set-barycenter this b2)))))

(defjmethod set-barycenter ((this constraint-group) barycenter)
  (loop for node across (.nodes this)
        do (setf (.barycenter (%fcr-state-of (.outer this) node)) barycenter)))
(defjmethod get-barycenter ((this constraint-group))
  (.barycenter (%fcr-state-of (.outer this) (aref (.nodes this) 0))))
(defjmethod get-outgoing-constraints ((this constraint-group))
  (or (.outgoing-constraints this) (setf (.outgoing-constraints this) (new-jlist))))
(defjmethod reset-outgoing-constraints ((this constraint-group)) (setf (.outgoing-constraints this) nil))
(defjmethod has-outgoing-constraints ((this constraint-group))
  (and (.outgoing-constraints this) (plusp (length (.outgoing-constraints this)))))
(defjmethod get-incoming-constraints ((this constraint-group))
  (or (.incoming-constraints this) (setf (.incoming-constraints this) (new-jlist))))
(defjmethod reset-incoming-constraints ((this constraint-group)) (setf (.incoming-constraints this) nil))
(defjmethod has-incoming-constraints ((this constraint-group))
  (and (.incoming-constraints this) (plusp (length (.incoming-constraints this)))))
(defjmethod get-nodes ((this constraint-group)) (.nodes this))
(defjmethod get-node ((this constraint-group)) (aref (.nodes this) 0))
(defjmethod to-string ((this constraint-group))
  (with-output-to-string (s)
    (write-char #\[ s)
    (loop for i from 0 below (length (.nodes this)) do
      (write-string (jstr (aref (.nodes this) i)) s)
      (when (get-barycenter this) (write-string (jstr "<" (get-barycenter this) ">") s))
      (when (< i (1- (length (.nodes this)))) (write-string ", " s)))
    (write-char #\] s)))

;;; ---------------------------------------------------------------- LayerSweepTypeDecider

(defjctor layer-sweep-type-decider (graph-data)
  (setf (.graph-data this) graph-data
        (.node-info this) (make-array (length (current-node-order graph-data)) :initial-element nil)))

(defun %lstd-node-info-for (this n) (aref (aref (.node-info this) (.id (get-layer n))) (.id n)))

(defun %lstd-transfer-info-to (this current target)
  (let ((target-info (%lstd-node-info-for this target)))
    (transfer target-info current)
    (incf (.connected-edges target-info))))

(defun %lstd-no-connected-ports-p (ports)
  (or (is-empty ports)
      (notany (lambda (p) (not (is-empty (get-connected-edges p)))) (to-cl-list ports))))

(defjmethod use-bottom-up ((this layer-sweep-type-decider))
  (let* ((gd (.graph-data this))
         (boundary (get-property (l-graph gd) layered-options.crossing-minimization-hierarchical-sweepiness)))
    (when (or (< boundary -1)
              (not (has-parent gd))
              (is-order-fixed (get-property (parent gd) layered-options.port-constraints))
              (and (< (jsize (get-port-side-view (parent gd) port-side.east)) 2)
                   (< (jsize (get-port-side-view (parent gd) port-side.west)) 2)))
      (return-from use-bottom-up/0 t))
    (when (cross-min-deterministic gd)
      (return-from use-bottom-up/0 nil))
    (let ((paths-to-random 0) (paths-to-hierarchical 0) (ns-port-dummies (new-jlist)))
      (loop for layer across (current-node-order gd) do
        (loop for node across layer do
          (block continue
            ;; visit all sources of edges first: north/south dummies come later
            (when (eq (get-type node) node-type.north-south-port)
              (jl-add ns-port-dummies node)
              (return-from continue))
            (let ((current (%lstd-node-info-for this node)))
              (cond ((eq (get-type node) node-type.external-port)
                     (setf (.hierarchical-influence current) 1)
                     (when (eq (get-side (get-property node layered-internal-properties.origin)) port-side.east)
                       (incf paths-to-hierarchical (.connected-edges current))))
                    ((%lstd-no-connected-ports-p (get-port-side-view node port-side.west))
                     (setf (.random-influence current) 1))
                    ((%lstd-no-connected-ports-p (get-port-side-view node port-side.east))
                     (incf paths-to-random (.connected-edges current))))
              (jdo (edge (get-outgoing-edges node))
                (incf paths-to-random (.random-influence current))
                (incf paths-to-hierarchical (.hierarchical-influence current))
                (%lstd-transfer-info-to this current (get-node (get-target edge))))
              (dolist (port (append (to-cl-list (get-port-side-view node port-side.north))
                                    (to-cl-list (get-port-side-view node port-side.south))))
                (let ((ns-dummy (get-property port layered-internal-properties.port-dummy)))
                  (when ns-dummy
                    (incf paths-to-random (.random-influence current))
                    (incf paths-to-hierarchical (.hierarchical-influence current))
                    (%lstd-transfer-info-to this current ns-dummy)))))))
        (jdo (node ns-port-dummies)
          (let ((current (%lstd-node-info-for this node)))
            (jdo (edge (get-outgoing-edges node))
              (incf paths-to-random (.random-influence current))
              (incf paths-to-hierarchical (.hierarchical-influence current))
              (%lstd-transfer-info-to this current (get-node (get-target edge))))))
        (jl-clear ns-port-dummies))
      (let* ((all-paths (float (+ paths-to-random paths-to-hierarchical) 1d0))
             (normalized (if (zerop all-paths)
                             double.positive-infinity
                             (/ (- paths-to-random paths-to-hierarchical) all-paths))))
        (>= normalized boundary)))))

(defjmethod transfer ((this node-info) other)
  (incf (.hierarchical-influence this) (.hierarchical-influence other))
  (incf (.random-influence this) (.random-influence other))
  (incf (.connected-edges this) (.connected-edges other)))

(defjmethod to-string ((this node-info))
  (jstr "NodeInfo [connectedEdges=" (.connected-edges this) ", hierarchicalInfluence="
        (.hierarchical-influence this) ", randomInfluence=" (.random-influence this) "]"))

(defjmethod init-at-layer-level ((this layer-sweep-type-decider) l node-order)
  (setf (.id (get-layer (%p3-node-at node-order l 0))) l
        (aref (.node-info this) l) (make-array (length (aref node-order l)) :initial-element nil)))

(defjmethod init-at-node-level ((this layer-sweep-type-decider) l n node-order)
  (setf (.id (%p3-node-at node-order l n)) n
        (aref (aref (.node-info this) l) n) (new-node-info)))
