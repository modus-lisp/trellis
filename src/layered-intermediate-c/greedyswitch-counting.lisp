;;;; greedyswitch-counting.lisp -- org.eclipse.elk.alg.layered.intermediate.greedyswitch
;;;; crossing counters: BetweenLayerEdgeTwoNodeCrossingsCounter (with its inner
;;;; AdjacencyList and Adjacency classes), CrossingMatrixFiller and
;;;; NorthSouthEdgeNeighbouringNodeCrossingsCounter.

(in-package #:elk)

;;; ------------------------------------------------------------ BetweenLayerEdgeTwoNodeCrossingsCounter

(defjctor between-layer-edge-two-node-crossings-counter (current-node-order free-layer-index)
  (setf (.current-node-order this) current-node-order
        (.free-layer-index this) free-layer-index)
  (%blc-set-port-positions-for-neighbouring-layers this))

(defun %blc-set-port-positions-for-neighbouring-layers (this)
  (when (> (.free-layer-index this) 0)
    (%blc-set-port-positions-for-layer this (1- (.free-layer-index this)) port-side.east))
  (when (< (.free-layer-index this) (1- (length (.current-node-order this))))
    (%blc-set-port-positions-for-layer this (1+ (.free-layer-index this)) port-side.west)))

(defun %blc-set-port-positions-for-layer (this layer-index port-side)
  (let ((port-id 0))
    (loop for node across (aref (.current-node-order this) layer-index)
          do (jdo (port (cross-min-util.in-north-south-east-west-order node port-side))
               (jm-put (.port-positions this) port port-id)
               (incf port-id)))))

(defjmethod count-eastern-edge-crossings ((this between-layer-edge-two-node-crossings-counter) upper-node lower-node)
  (%blc-reset-crossing-count this)
  (unless (jequals upper-node lower-node)
    (%blc-add-eastern-crossings this upper-node lower-node)))

(defjmethod count-western-edge-crossings ((this between-layer-edge-two-node-crossings-counter) upper-node lower-node)
  (%blc-reset-crossing-count this)
  (unless (jequals upper-node lower-node)
    (%blc-add-western-crossings this upper-node lower-node)))

(defjmethod count-both-side-crossings ((this between-layer-edge-two-node-crossings-counter) upper-node lower-node)
  (%blc-reset-crossing-count this)
  (unless (jequals upper-node lower-node)
    (%blc-add-western-crossings this upper-node lower-node)
    (%blc-add-eastern-crossings this upper-node lower-node)))

(defun %blc-reset-crossing-count (this)
  (setf (.upper-lower-crossings this) 0
        (.lower-upper-crossings this) 0))

(defun %blc-add-eastern-crossings (this upper-node lower-node)
  (setf (.upper-adjacencies this) (%blc-get-adjacency-for this upper-node port-side.east (.eastern-adjacencies this))
        (.lower-adjacencies this) (%blc-get-adjacency-for this lower-node port-side.east (.eastern-adjacencies this)))
  (unless (or (zerop (size (.upper-adjacencies this))) (zerop (size (.lower-adjacencies this))))
    (%blc-count-crossings-by-merging-adjacency-lists this)))

(defun %blc-add-western-crossings (this upper-node lower-node)
  (setf (.upper-adjacencies this) (%blc-get-adjacency-for this upper-node port-side.west (.western-adjacencies this))
        (.lower-adjacencies this) (%blc-get-adjacency-for this lower-node port-side.west (.western-adjacencies this)))
  (unless (or (zerop (size (.upper-adjacencies this))) (zerop (size (.lower-adjacencies this))))
    (%blc-count-crossings-by-merging-adjacency-lists this)))

(defun %blc-get-adjacency-for (this node side adjacencies)
  (when (zerop (hash-table-count adjacencies))
    (loop for n across (aref (.current-node-order this) (.free-layer-index this))
          do (jm-put adjacencies n (new-adjacency-list this n side))))
  (let ((al (jm-get adjacencies node)))
    (reset al)
    al))

(defun %blc-count-crossings-by-merging-adjacency-lists (this)
  (loop while (and (not (is-empty (.upper-adjacencies this)))
                   (not (is-empty (.lower-adjacencies this))))
        do (let ((ua (.upper-adjacencies this))
                 (la (.lower-adjacencies this)))
             (cond ((%blc-is-below (j-first ua) (j-first la))
                    (incf (.upper-lower-crossings this) (size ua))
                    (remove-first la))
                   ((%blc-is-below (j-first la) (j-first ua))
                    (incf (.lower-upper-crossings this) (size la))
                    (remove-first ua))
                   (t
                    (incf (.upper-lower-crossings this) (count-adjacencies-below-node-of-first-port ua))
                    (incf (.lower-upper-crossings this) (count-adjacencies-below-node-of-first-port la))
                    (remove-first ua)
                    (remove-first la))))))

(defun %blc-is-below (first-port second-port)
  (> first-port second-port))

(defjmethod get-upper-lower-crossings ((this between-layer-edge-two-node-crossings-counter))
  (.upper-lower-crossings this))
(defjmethod get-lower-upper-crossings ((this between-layer-edge-two-node-crossings-counter))
  (.lower-upper-crossings this))

;;; ------------------------------------------------------------ AdjacencyList (inner class)

(defjctor adjacency-list (outer node side)
  (setf (.outer this) outer
        (.node this) node
        (.side this) side)
  (%al-get-adjacencies-sorted-by-position this))

(defun %al-get-adjacencies-sorted-by-position (this)
  (%al-iterate-trough-edges-collecting-adjacencies this)
  (jl-sort (.adjacency-list this)
           (lambda (a b) (integer.compare (.position a) (.position b)))))

(defun %al-iterate-trough-edges-collecting-adjacencies (this)
  (jdo (port (cross-min-util.in-north-south-east-west-order (.node this) (.side this)))
    (jdo (edge (%al-get-edges-connected-to this port))
      (when (and (not (is-self-loop edge)) (%al-is-not-in-layer edge))
        (%al-add-adjacency-of this edge)
        (incf (.size this))
        (incf (.current-size this))))))

(defun %al-get-edges-connected-to (this port)
  (if (eq (.side this) port-side.west)
      (get-incoming-edges port)
      (get-outgoing-edges port)))

(defun %al-is-not-in-layer (edge)
  (not (eq (get-layer (get-node (get-source edge)))
           (get-layer (get-node (get-target edge))))))

(defun %al-add-adjacency-of (this edge)
  (let* ((adjacent-port (%al-adjacent-port-of edge (.side this)))
         (adjacent-port-position (jm-get (.port-positions (.outer this)) adjacent-port))
         (list (.adjacency-list this))
         (last-index (1- (length list))))
    (if (and (not (zerop (length list)))
             (= (.position (jl-get list last-index)) adjacent-port-position))
        (progn
          (incf (.cardinality (jl-get list last-index)))
          (incf (.current-cardinality (jl-get list last-index))))
        (jl-add list (new-adjacency adjacent-port-position adjacent-port)))))

(defun %al-adjacent-port-of (edge side)
  (if (eq side port-side.west) (get-source edge) (get-target edge)))

(defjmethod reset ((this adjacency-list))
  (setf (.current-index this) 0
        (.current-size this) (.size this))
  (unless (is-empty this)
    (reset (%al-current-adjacency this))))

(defjmethod count-adjacencies-below-node-of-first-port ((this adjacency-list))
  (- (.current-size this) (.current-cardinality (%al-current-adjacency this))))

(defjmethod remove-first ((this adjacency-list))
  (unless (is-empty this)
    (let ((current-entry (%al-current-adjacency this)))
      (if (= (.current-cardinality current-entry) 1)
          (%al-increment-current-index this)
          (decf (.current-cardinality current-entry))))
    (decf (.current-size this))))

(defun %al-increment-current-index (this)
  (incf (.current-index this))
  (when (< (.current-index this) (length (.adjacency-list this)))
    (reset (%al-current-adjacency this))))

(defjmethod is-empty ((this adjacency-list))
  (zerop (.current-size this)))

(defjmethod j-first ((this adjacency-list))
  (.position (%al-current-adjacency this)))

(defjmethod size ((this adjacency-list))
  (.current-size this))

(defun %al-current-adjacency (this)
  (jl-get (.adjacency-list this) (.current-index this)))

;;; ------------------------------------------------------------ Adjacency (inner class of AdjacencyList)

(defjctor adjacency (adjacent-port-position port)
  (setf (.position this) adjacent-port-position
        (.cardinality this) 1
        (.current-cardinality this) 1))

(defjmethod reset ((this adjacency))
  (setf (.current-cardinality this) (.cardinality this)))

;;; ------------------------------------------------------------ CrossingMatrixFiller

(defjctor crossing-matrix-filler (greedy-switch-type graph free-layer-index direction)
  (setf (.direction this) direction
        (.one-sided this) (eq greedy-switch-type cross-min-type.one-sided-greedy-switch))
  (let* ((free-layer (aref graph free-layer-index))
         (n (length free-layer)))
    (setf (.is-crossing-matrix-filled this) (make-array (list n n) :initial-element nil)
          (.crossing-matrix this) (make-array (list n n) :initial-element 0)
          (.in-between-layer-crossing-counter this)
          (new-between-layer-edge-two-node-crossings-counter graph free-layer-index))))

(defjmethod get-crossing-matrix-entry ((this crossing-matrix-filler) upper-node lower-node)
  (let ((ui (.id upper-node))
        (li (.id lower-node)))
    (unless (aref (.is-crossing-matrix-filled this) ui li)
      (%cmf-fill-crossing-matrix this upper-node lower-node)
      (setf (aref (.is-crossing-matrix-filled this) ui li) t
            (aref (.is-crossing-matrix-filled this) li ui) t))
    (aref (.crossing-matrix this) ui li)))

(defun %cmf-fill-crossing-matrix (this upper-node lower-node)
  (let ((counter (.in-between-layer-crossing-counter this)))
    (if (.one-sided this)
        (let ((direction (.direction this)))
          (cond ((eq direction crossing-count-side.east)
                 (count-eastern-edge-crossings counter upper-node lower-node))
                ((eq direction crossing-count-side.west)
                 (count-western-edge-crossings counter upper-node lower-node))))
        (count-both-side-crossings counter upper-node lower-node))
    (setf (aref (.crossing-matrix this) (.id upper-node) (.id lower-node))
          (get-upper-lower-crossings counter))
    (setf (aref (.crossing-matrix this) (.id lower-node) (.id upper-node))
          (get-lower-upper-crossings counter))))

;;; ------------------------------------------------------------ NorthSouthEdgeNeighbouringNodeCrossingsCounter

(defjctor north-south-edge-neighbouring-node-crossings-counter (nodes)
  (setf (.layer this) nodes)
  (%nsc-initialize-port-positions this))

(defun %nsc-initialize-port-positions (this)
  (loop for node across (.layer this)
        do (%nsc-set-port-ids-on this node port-side.south)
           (%nsc-set-port-ids-on this node port-side.north)))

(defun %nsc-set-port-ids-on (this node side)
  (let ((port-id 0))
    (jdo (port (cross-min-util.in-north-south-east-west-order node side))
      (jm-put (.port-positions this) port port-id)
      (incf port-id))))

(defjmethod count-crossings ((this north-south-edge-neighbouring-node-crossings-counter) upper-node lower-node)
  (setf (.upper-lower-crossings this) 0
        (.lower-upper-crossings this) 0)
  (%nsc-process-if-two-north-south-nodes this upper-node lower-node)
  (%nsc-process-if-north-south-long-edge-dummy-crossing this upper-node lower-node)
  (%nsc-process-if-normal-node-with-ns-ports-and-long-edge-dummy this upper-node lower-node))

(defun %nsc-process-if-two-north-south-nodes (this upper-node lower-node)
  (when (and (%nsc-is-north-south upper-node)
             (%nsc-is-north-south lower-node)
             (not (%nsc-have-different-origins upper-node lower-node)))
    (if (%nsc-is-north-of-normal-node upper-node)
        (%nsc-count-crossings-of-two-north-south-dummies this upper-node lower-node)
        (%nsc-count-crossings-of-two-north-south-dummies this lower-node upper-node))))

(defun %nsc-count-crossings-of-two-north-south-dummies (this further-from-normal-node closer-to-normal-node)
  (flet ((first-degree (ports)
           (if (is-empty ports) 0 (get-degree (jl-get ports 0)))))
    (if (> (%nsc-origin-port-position-of this further-from-normal-node)
           (%nsc-origin-port-position-of this closer-to-normal-node))
        (progn
          (setf (.upper-lower-crossings this)
                (first-degree (get-port-side-view closer-to-normal-node port-side.east)))
          (setf (.lower-upper-crossings this)
                (first-degree (get-port-side-view further-from-normal-node port-side.west))))
        (progn
          (setf (.upper-lower-crossings this)
                (first-degree (get-port-side-view closer-to-normal-node port-side.west)))
          (setf (.lower-upper-crossings this)
                (first-degree (get-port-side-view further-from-normal-node port-side.east)))))))

(defun %nsc-process-if-north-south-long-edge-dummy-crossing (this upper-node lower-node)
  (cond ((and (%nsc-is-north-south upper-node) (%nsc-is-long-edge-dummy lower-node))
         (if (%nsc-is-north-of-normal-node upper-node)
             (setf (.upper-lower-crossings this) 1)
             (setf (.lower-upper-crossings this) 1)))
        ((and (%nsc-is-north-south lower-node) (%nsc-is-long-edge-dummy upper-node))
         (if (%nsc-is-north-of-normal-node lower-node)
             (setf (.lower-upper-crossings this) 1)
             (setf (.upper-lower-crossings this) 1)))))

(defun %nsc-process-if-normal-node-with-ns-ports-and-long-edge-dummy (this upper-node lower-node)
  (when (and (%nsc-is-normal upper-node) (%nsc-is-long-edge-dummy lower-node))
    (setf (.upper-lower-crossings this) (%nsc-number-of-north-south-edges upper-node port-side.south))
    (setf (.lower-upper-crossings this) (%nsc-number-of-north-south-edges upper-node port-side.north)))
  (when (and (%nsc-is-normal lower-node) (%nsc-is-long-edge-dummy upper-node))
    (setf (.upper-lower-crossings this) (%nsc-number-of-north-south-edges lower-node port-side.north))
    (setf (.lower-upper-crossings this) (%nsc-number-of-north-south-edges lower-node port-side.south))))

(defun %nsc-number-of-north-south-edges (node side)
  (let ((number-of-edges 0))
    (jdo (port (get-port-side-view node side))
      (incf number-of-edges (if (%nsc-has-connected-north-south-edge port) 1 0)))
    number-of-edges))

(defun %nsc-has-connected-north-south-edge (port)
  (not (null (get-property port layered-internal-properties.port-dummy))))

(defun %nsc-have-different-origins (upper-node lower-node)
  (not (eq (%nsc-origin-of upper-node) (%nsc-origin-of lower-node))))

(defun %nsc-origin-port-position-of (this node)
  (jm-get (.port-positions this) (%nsc-origin-port-of node)))

(defun %nsc-origin-port-of (node)
  (get-property (jl-get (get-ports node) 0) layered-internal-properties.origin))

(defun %nsc-is-north-of-normal-node (upper-node)
  (eq (get-side (%nsc-origin-port-of upper-node)) port-side.north))

(defun %nsc-origin-of (node)
  (get-property node layered-internal-properties.origin))

(defun %nsc-is-long-edge-dummy (node)
  (eq (get-type node) node-type.long-edge))

(defun %nsc-is-north-south (node)
  (eq (get-type node) node-type.north-south-port))

(defun %nsc-is-normal (node)
  (eq (get-type node) node-type.normal))

(defjmethod get-upper-lower-crossings ((this north-south-edge-neighbouring-node-crossings-counter))
  (.upper-lower-crossings this))
(defjmethod get-lower-upper-crossings ((this north-south-edge-neighbouring-node-crossings-counter))
  (.lower-upper-crossings this))
