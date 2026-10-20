;;;; org.eclipse.elk.alg.force.stress

(in-package #:elk)

;;; ---------------------------------------------------------------- StressMajorization

(defjmethod initialize ((this stress-majorization) fgraph)
  (when (<= (length (get-nodes fgraph)) 1)
    (return-from initialize/1 nil))
  (setf (.graph this) fgraph
        (.dim this) (get-property fgraph stress-options.dimension)
        (.iteration-limit this) (get-property fgraph stress-options.iteration-limit)
        (.epsilon this) (jd (get-property fgraph stress-options.epsilon))
        (.desired-edge-length this) (jd (get-property fgraph stress-options.desired-edge-length)))
  (let ((connected (.connected-edges this)))
    (clrhash connected)
    (flet ((put (k v) (jl-add (or (gethash k connected) (setf (gethash k connected) (new-jlist))) v)))
      (jdo (edge (get-edges fgraph))
        (put (get-source edge) edge)
        (put (get-target edge) edge))))
  ;; all pairs shortest path
  (let* ((n (length (get-nodes fgraph)))
         (apsp (make-array n)))
    (dotimes (i n) (setf (aref apsp i) (make-array n :element-type 'double-float :initial-element 0d0)))
    (setf (.apsp this) apsp)
    (jdo (source (get-nodes fgraph))
      (%stress-dijkstra this source (aref apsp (.id source))))
    ;; init weight matrix
    (let ((w (make-array n)))
      (dotimes (i n)
        (setf (aref w i) (make-array n :element-type 'double-float :initial-element 0d0))
        (dotimes (j n)
          (let* ((dij (aref (aref apsp i) j))
                 (wij (/ 1.0 (* dij dij))))
            (setf (aref (aref w i) j) wij))))
      (setf (.w this) w))))

(defjmethod execute ((this stress-majorization))
  (when (<= (length (get-nodes (.graph this))) 1)
    (return-from execute/0 nil))
  (let ((count 0)
        (prev-stress (%stress-compute-stress this))
        (cur-stress double.positive-infinity))
    (loop
      (when (> count 0)
        (setf prev-stress cur-stress))
      (jdo (u (get-nodes (.graph this)))
        ;; note that we do not use 'NO_LAYOUT' here, since that option results in the node
        ;; already being excluded by the layout engine
        (unless (get-property u stress-options.fixed)
          (let ((new-pos (%stress-compute-new-position this u)))
            (add (reset (get-position u)) new-pos))))
      (setf cur-stress (%stress-compute-stress this))
      (when (%stress-done this (prog1 count (incf count)) prev-stress cur-stress)
        (return)))))

(defun %stress-dijkstra (this source dist)
  (let* ((graph (.graph this))
         (nodes (new-jpq (lambda (n1 n2) (double.compare (aref dist (.id n1)) (aref dist (.id n2))))))
         (mark (make-array (length (get-nodes graph)) :initial-element nil)))
    ;; init
    (setf (aref dist (.id source)) 0d0)
    (jdo (n (get-nodes graph))
      (when (/= (.id n) (.id source))
        (setf (aref dist (.id n)) (jd integer.max-value)))
      (add nodes n))
    ;; find shortest paths
    (loop until (is-empty nodes)
          do (let ((u (poll nodes)))
               (setf (aref mark (.id u)) t)
               (jdo (e (gethash u (.connected-edges this)))
                 (let ((v (%stress-get-other e u)))
                   (unless (aref mark (.id v))
                     ;; get e's desired length
                     (let* ((el (if (has-property e stress-options.desired-edge-length)
                                    (jd (get-property e stress-options.desired-edge-length))
                                    (.desired-edge-length this)))
                            (d (+ (aref dist (.id u)) el)))
                       (when (< d (aref dist (.id v)))
                         (setf (aref dist (.id v)) d)
                         (j-remove nodes v)
                         (add nodes v))))))))))

(defun %stress-done (this count prev-stress cur-stress)
  (or (= prev-stress 0)
      (< (/ (- prev-stress cur-stress) prev-stress) (.epsilon this))
      (>= count (.iteration-limit this))))

(defun %stress-compute-stress (this)
  (let ((stress 0.0)
        (nodes (get-nodes (.graph this)))
        (apsp (.apsp this))
        (w (.w this)))
    (loop for i from 0 below (length nodes)
          do (let ((u (aref nodes i)))
               (loop for j from (1+ i) below (length nodes)
                     do (let* ((v (aref nodes j))
                               (euc-dist (distance (get-position u) (get-position v)))
                               (euc-displacement (- euc-dist (aref (aref apsp (.id u)) (.id v)))))
                          (incf stress (* (aref (aref w (.id u)) (.id v)) euc-displacement euc-displacement))))))
    stress))

(defun %stress-compute-new-position (this u)
  (let ((weight-sum 0.0) (x-disp 0.0) (y-disp 0.0)
        (dim (.dim this))
        (apsp (.apsp this))
        (w (.w this)))
    (jdo (v (get-nodes (.graph this)))
      (unless (eq u v)
        (let* ((wij (aref (aref w (.id u)) (.id v)))
               (euc-dist (progn (incf weight-sum wij)
                                (distance (get-position u) (get-position v))))
               (a (aref (aref apsp (.id u)) (.id v)))
               (pu (get-position u))
               (pv (get-position v)))
          (when (and (> euc-dist 0) (not (eq dim dimension.y)))
            (incf x-disp (* wij (+ (.x pv) (/ (* a (- (.x pu) (.x pv))) euc-dist)))))
          (when (and (> euc-dist 0) (not (eq dim dimension.x)))
            (incf y-disp (* wij (+ (.y pv) (/ (* a (- (.y pu) (.y pv))) euc-dist))))))))
    (enum-case dim
      (dimension.x (new-k-vector (/ x-disp weight-sum) (.y (get-position u))))
      (dimension.y (new-k-vector (.x (get-position u)) (/ y-disp weight-sum)))
      (t (new-k-vector (/ x-disp weight-sum) (/ y-disp weight-sum))))))

(defun %stress-get-other (edge one)
  (cond ((eq (get-source edge) one) (get-target edge))
        ((eq (get-target edge) one) (get-source edge))
        (t (jthrow 'illegal-argument-exception
                   "Node 'one' must be either source or target of edge 'edge'."))))

;;; ---------------------------------------------------------------- StressLayoutProvider

(defjmethod layout ((this stress-layout-provider) layout-graph progress-monitor)
  (begin progress-monitor "ELK Stress" 1)
  ;; calculate initial coordinates
  (if (not (get-property layout-graph stress-options.interactive))
      (layout (new-force-layout-provider) layout-graph (sub-task progress-monitor 1.0))
      ;; If requested, compute nodes's dimensions, place node labels, ports, port labels, etc.
      ;; Note that for the non-interactive case (above) this will be taken care of by the force
      ;; layout provider
      (unless (get-property layout-graph stress-options.omit-node-micro-layout)
        (execute (node-micro-layout.for-graph layout-graph))))
  ;; transform the input graph
  (let* ((graph-importer (new-force-elk-graph-importer))
         (fgraph (import-graph graph-importer layout-graph))
         ;; split the input graph into components
         (components (split (.components-processor this) fgraph)))
    ;; perform the actual layout
    (jdo (sub-graph components)
      (when (> (length (get-nodes sub-graph)) 1)
        (initialize (.stress-majorization this) sub-graph)
        (execute (.stress-majorization this))
        ;; Note that contrary to force itself, labels are not considered during stress layout.
        ;; Hence, all we can do here is to place the labels at reasonable positions after
        ;; layout has finished.
        (jdo (label (get-labels sub-graph))
          (refresh-position label))))
    ;; pack the components back into one graph
    (setf fgraph (recombine (.components-processor this) components))
    ;; apply the layout results to the original graph
    (apply-layout graph-importer fgraph))
  (done progress-monitor))

;;; StressMajorization.Dimension is the generated enum DIMENSION; it has no methods.
