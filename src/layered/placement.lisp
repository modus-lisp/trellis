;;;; placement.lisp -- phase 4: positions along the layers.
;;;;
;;;; Brandes and Köpf (2001), with the corrections of Brandes, Walter and Zink
;;;; (2020): four alignments (aligning with upper or lower neighbours, packing
;;;; toward one end of the layers or the other), each made of vertical blocks
;;;; and compacted, then balanced.  Nodes have sizes and ports, so a block is
;;;; aligned on the ports of the edges that join it, not on node corners: each
;;;; member carries an inner shift relative to its block's root, and spacing
;;;; between neighbours in a layer is the sum of their extents and the spacing
;;;; for their kinds (node-node, edge-node, edge-edge).
;;;;
;;;; Here "x" is the coordinate BK assigns, which is our y (along the layer).

(in-package #:trellis)

;;; ---- ports on nodes ------------------------------------------------------------

(defun place-lports (lg)
  "Give each port not fixed in place a position on its side, spread in order."
  (dolist (n (lgraph-nodes lg))
    (let ((w (lnode-width n)) (h (lnode-height n)))
      (dolist (side '(:north :east :south :west))
        (let* ((ps (remove side (lnode-ports n) :key #'lport-side :test-not #'eq))
               (ordered (if (member side '(:south :west)) (reverse ps) ps))
               (k (length ordered)))
          (loop for p in ordered for i from 1
                unless (lport-fixed p)
                  do (let ((pos (* (if (member side '(:north :south)) w h) (/ i (d (1+ k)))))
                           (pw (lport-width p)) (ph (lport-height p)))
                       (ecase side
                         (:north (setf (lport-x p) (- pos (/ pw 2)) (lport-y p) (- ph)))
                         (:south (setf (lport-x p) (- pos (/ pw 2)) (lport-y p) h))
                         (:west (setf (lport-x p) (- pw) (lport-y p) (- pos (/ ph 2))))
                         (:east (setf (lport-x p) w (lport-y p) (- pos (/ ph 2))))))))))))

;;; ---- spacing -------------------------------------------------------------------

(defun lspacing (lg key) (getf (lgraph-spacing lg) key))

(defun in-layer-gap (lg u v)
  "Free space required between U and the node V following it in a layer."
  (let ((du (dummy-p u)) (dv (dummy-p v)))
    (+ (pad-bottom (lnode-margin u))
       (cond ((and du dv) (lspacing lg :edge-edge))
             ((or du dv) (lspacing lg :edge-node))
             (t (lspacing lg :node-node)))
       (pad-top (lnode-margin v)))))

;;; ---- Brandes-Köpf ------------------------------------------------------------------

(defun bk-run (lg down left)
  "One BK alignment: DOWN aligns with the previous layer, LEFT packs toward
   low y.  Returns a vector of node tops (by lnode-index)."
  (let* ((size (lgraph-next-index lg))
         (layers (let ((ls (coerce (lgraph-layers lg) 'list))) (if down ls (reverse ls))))
         (layers (mapcar (lambda (l) (let ((v (copy-seq l))) (if left v (reverse v)))) layers))
         (pos (make-array size :initial-element 0))
         (pred (make-array size :initial-element nil))
         (root (make-array size :initial-element nil))
         (align (make-array size :initial-element nil))
         (ishift (make-array size :initial-element 0d0))
         (sink (make-array size :initial-element nil))
         (xr (make-array size :initial-element nil))
         (shift (make-array size :initial-element nil))
         (marked (make-hash-table :test 'eq))
         (by-index (make-array size :initial-element nil)))
    (labels ((ix (v) (lnode-index v))
             (h (v) (lnode-height v))
             (off (p) (if left (port-offset p) (- (lnode-height (lport-node p)) (port-offset p))))
             (gap (u v) ;; U precedes V in run order
               (if left (in-layer-gap lg u v) (in-layer-gap lg v u)))
             (delta (u v) (+ (h u) (gap u v)))
             (upper-edges (v)
               ;; edges to the layer processed before V's, sorted by neighbour position
               (let ((es (cross-layer (if down (node-incoming v) (node-outgoing v)))))
                 (sort (copy-list es) #'key<
                       :key (lambda (e)
                              (let ((u (if down (src-node e) (tgt-node e)))
                                    (pu (if down (ledge-source e) (ledge-target e))))
                                (cons (aref pos (ix u)) (off pu)))))))
             (other (e) (if down (src-node e) (tgt-node e)))
             (other-port (e) (if down (ledge-source e) (ledge-target e)))
             (own-port (e) (if down (ledge-target e) (ledge-source e)))
             (inner-p (e) (and (eq (lnode-kind (src-node e)) :long-edge)
                               (eq (lnode-kind (tgt-node e)) :long-edge))))
      (dolist (l layers)
        (loop for v across l for i from 0
              do (setf (aref pos (ix v)) i
                       (aref pred (ix v)) (if (> i 0) (aref l (1- i)) nil)
                       (aref root (ix v)) v (aref align (ix v)) v (aref sink (ix v)) v
                       (aref by-index (ix v)) v)))
      ;; type 1 conflicts: non-inner segments crossing inner ones
      (loop for (upper lower) on layers while lower
            do (let ((k0 0) (l 0) (n (length lower)))
                 (loop for l1 from 0 below n
                       for v = (aref lower l1)
                       for inner = (find-if #'inner-p (upper-edges v))
                       do (when (or (= l1 (1- n)) inner)
                            (let ((k1 (if inner (aref pos (ix (other inner))) (1- (length upper)))))
                              (loop while (<= l l1)
                                    do (dolist (e (upper-edges (aref lower l)))
                                         (let ((k (aref pos (ix (other e)))))
                                           (when (and (or (< k k0) (> k k1)) (not (inner-p e)))
                                             (setf (gethash e marked) t))))
                                       (incf l))
                              (setf k0 k1))))))
      ;; vertical alignment
      (dolist (l layers)
        (let ((r -1))
          (loop for v across l
                do (let* ((es (coerce (upper-edges v) 'vector)) (d (length es)))
                     (when (plusp d)
                       (dolist (m (remove-duplicates (list (floor (1- d) 2) (ceiling (1- d) 2))))
                         (when (eq (aref align (ix v)) v)
                           (let* ((e (aref es m)) (u (other e)))
                             (when (and (not (gethash e marked)) (< r (aref pos (ix u))))
                               (setf (aref align (ix u)) v
                                     (aref root (ix v)) (aref root (ix u))
                                     (aref align (ix v)) (aref root (ix v))
                                     (aref ishift (ix v)) (+ (aref ishift (ix u))
                                                             (off (other-port e)) (- (off (own-port e))))
                                     r (aref pos (ix u))))))))))))
      ;; horizontal compaction: blocks relative to their class's sink
      (labels ((place-block (v)
                 (unless (aref xr (ix v))
                   (setf (aref xr (ix v)) 0d0)
                   (let ((w v) (first t))
                     (loop
                       (let ((p (aref pred (ix w))))
                         (when p
                           (let ((u (aref root (ix p))))
                             (place-block u)
                             (when (eq (aref sink (ix v)) v) (setf (aref sink (ix v)) (aref sink (ix u))))
                             (when (eq (aref sink (ix v)) (aref sink (ix u)))
                               (let ((need (- (+ (aref xr (ix u)) (aref ishift (ix p)) (delta p w))
                                              (aref ishift (ix w)))))
                                 (setf (aref xr (ix v)) (if first need (max (aref xr (ix v)) need))
                                       first nil))))))
                       (setf w (aref align (ix w)))
                       (when (eq w v) (return)))))))
        (dolist (l layers)
          (loop for v across l when (eq (aref root (ix v)) v) do (place-block v))))
      ;; class shifts, highest class first (Brandes, Walter, Zink, Alg. 3b)
      (let ((neighborings '()))
        (loop for l in layers for li from 0
              do (loop for i from 1 below (length l)
                       for p = (aref l (1- i)) for w = (aref l i)
                       for sp = (aref sink (ix (aref root (ix p))))
                       for sw = (aref sink (ix (aref root (ix w))))
                       unless (eq sp sw) do (push (list sw sp p w) neighborings)))
        (setf neighborings (nreverse neighborings))
        (let ((sinks (remove-duplicates
                      (loop for l in layers for li from 0
                            nconc (loop for v across l
                                        when (and (eq (aref root (ix v)) v) (eq (aref sink (ix v)) v))
                                          collect (cons li v)))
                      :key #'cdr)))
          (setf sinks (stable-sort sinks #'< :key #'car))
          (let ((by-sink (make-hash-table :test 'eq)))
            (dolist (nb neighborings) (push nb (gethash (first nb) by-sink)))
            (dolist (s sinks)
              (let ((sk (cdr s)))
                (unless (aref shift (ix sk)) (setf (aref shift (ix sk)) 0d0))
                (dolist (nb (gethash sk by-sink))
                  (destructuring-bind (sw sp p w) nb
                    (declare (ignore sw))
                    (let ((cand (- (+ (aref shift (ix sk)) (aref xr (ix (aref root (ix w)))) (aref ishift (ix w)))
                                   (+ (aref xr (ix (aref root (ix p)))) (aref ishift (ix p)) (delta p w)))))
                      (setf (aref shift (ix sp))
                            (if (aref shift (ix sp)) (min (aref shift (ix sp)) cand) cand))))))))))
      ;; absolute
      (let ((result (make-array size :initial-element nil)))
        (dolist (l layers)
          (loop for v across l
                do (let* ((r (aref root (ix v)))
                          (y (+ (aref xr (ix r)) (aref ishift (ix v))
                                (or (aref shift (ix (aref sink (ix r)))) 0d0))))
                     (setf (aref result (ix v)) (if left y (- (+ y (h v))))))))
        result))))

(defun layout-extent (lg ys)
  (let ((lo most-positive-double-float) (hi most-negative-double-float))
    (dolist (n (lgraph-nodes lg))
      (let ((y (aref ys (lnode-index n))))
        (when y
          (setf lo (min lo (- y (pad-top (lnode-margin n))))
                hi (max hi (+ y (lnode-height n) (pad-bottom (lnode-margin n))))))))
    (if (> lo hi) (values 0d0 0d0) (values lo hi))))

(defun layout-valid-p (lg ys)
  "Do the tops YS keep every pair of neighbours in a layer apart?"
  (loop for l across (lgraph-layers lg)
        always (loop for i from 1 below (length l)
                     for u = (aref l (1- i)) for v = (aref l i)
                     always (>= (+ (aref ys (lnode-index v)) 1d-6)
                                (+ (aref ys (lnode-index u)) (lnode-height u) (in-layer-gap lg u v))))))

(defun place-nodes (lg)
  (let* ((opts (lgraph-options-node lg))
         (strategy (opt-enum opts :node-placement-strategy :brandes-koepf))
         (fixed (opt-enum opts :node-placement-bk-fixed-alignment
                          (opt-enum opts :fixed-alignment :none) '(:none :balanced :leftup :leftdown :rightup :rightdown)))
         (ys (if (eq strategy :simple)
                 (simple-placement lg)
                 (bk-placement lg fixed))))
    (multiple-value-bind (lo hi) (layout-extent lg ys)
      (declare (ignore hi))
      (dolist (n (lgraph-nodes lg))
        (setf (lnode-y n) (- (aref ys (lnode-index n)) lo))))))

(defun simple-placement (lg)
  "Each layer stacked and centred on the others."
  (let ((ys (make-array (lgraph-next-index lg) :initial-element 0d0)) (heights '()))
    (loop for l across (lgraph-layers lg)
          do (let ((y 0d0))
               (loop for i from 0 below (length l)
                     for v = (aref l i)
                     do (when (> i 0) (incf y (in-layer-gap lg (aref l (1- i)) v)))
                        (setf (aref ys (lnode-index v)) y)
                        (incf y (lnode-height v)))
               (push (cons l y) heights)))
    (let ((tallest (reduce #'max heights :key #'cdr :initial-value 0d0)))
      (dolist (lh heights)
        (loop for v across (car lh) do (incf (aref ys (lnode-index v)) (/ (- tallest (cdr lh)) 2)))))
    ys))

(defun bk-placement (lg fixed)
  (let* ((runs (list (bk-run lg t t) (bk-run lg t nil) (bk-run lg nil t) (bk-run lg nil nil)))
         (names '(:leftdown :rightdown :leftup :rightup))
         (extents (mapcar (lambda (ys) (multiple-value-list (layout-extent lg ys))) runs))
         (widths (mapcar (lambda (e) (- (second e) (first e))) extents))
         (best (position (reduce #'min widths) widths)))
    (case fixed
      ((:leftdown :rightdown :leftup :rightup)
       (nth (position fixed names) runs))
      ((:none nil)
       ;; the most compact single alignment keeps its blocks straight
       (nth best runs))
      (t
       ;; align all four to the narrowest, then take the mean of the medians
       (let* ((ref (nth best extents))
              (aligned (loop for ys in runs for e in extents for i from 0
                             collect (let ((dlt (if (evenp i) (- (first ref) (first e))
                                                    (- (second ref) (second e)))))
                                       (map 'vector (lambda (y) (and y (+ y dlt))) ys))))
              (balanced (make-array (lgraph-next-index lg) :initial-element nil)))
         (dolist (n (lgraph-nodes lg))
           (let ((vals (sort (mapcar (lambda (ys) (aref ys (lnode-index n))) aligned) #'<)))
             (setf (aref balanced (lnode-index n)) (/ (+ (second vals) (third vals)) 2))))
         (if (layout-valid-p lg balanced) balanced (nth best runs)))))))
