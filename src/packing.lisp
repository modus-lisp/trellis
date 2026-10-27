;;;; packing.lisp -- shared tools for the non-layered algorithms, and the
;;;; packing algorithms (rectpacking, topdownpacking).
;;;;
;;;; Overlap removal first pushes overlapping pairs apart along the axis where
;;;; they overlap least, a few rounds, which keeps the picture's shape; what is
;;;; left is settled by a left-to-right scan that places every node right of
;;;; anything it would still overlap -- slower to look at, but certain.
;;;; Compaction is the same scan run from zero, once along x and once along y.

(in-package #:trellis)

;;; ---- boxes ---------------------------------------------------------------------

(defun node-box (n spacing)
  "N's extent, grown by half of SPACING on every side, in its parent's frame."
  (let ((e (node-extent n)) (h (/ spacing 2)))
    (rect (- (+ (node-x n) (rect-x e)) h) (- (+ (node-y n) (rect-y e)) h)
          (+ (rect-w e) spacing) (+ (rect-h e) spacing))))

(defun boxes-overlap-p (a b)
  (and (< (rect-x a) (- (rect-right b) 1d-6)) (< (rect-x b) (- (rect-right a) 1d-6))
       (< (rect-y a) (- (rect-bottom b) 1d-6)) (< (rect-y b) (- (rect-bottom a) 1d-6))))

(defun remove-overlaps (nodes spacing &key (rounds 30))
  "Move NODES apart until none overlap (keeping SPACING between them)."
  (let ((v (coerce nodes 'vector)))
    (when (> (length v) 1)
      ;; gentle rounds
      (loop repeat rounds
            for moved = nil
            do (let ((boxes (map 'vector (lambda (n) (node-box n spacing)) v))
                     (idx (sort (loop for i below (length v) collect i) #'<
                                :key (lambda (i) (rect-x (node-box (aref v i) spacing))))))
                 (let ((order (coerce idx 'vector)))
                   (loop for a from 0 below (length order)
                         for i = (aref order a)
                         do (loop for b from (1+ a) below (length order)
                                  for j = (aref order b)
                                  while (< (rect-x (aref boxes j)) (rect-right (aref boxes i)))
                                  do (let ((bi (aref boxes i)) (bj (aref boxes j)))
                                       (when (boxes-overlap-p bi bj)
                                         (setf moved t)
                                         (let* ((ox (- (min (rect-right bi) (rect-right bj)) (max (rect-x bi) (rect-x bj))))
                                                (oy (- (min (rect-bottom bi) (rect-bottom bj)) (max (rect-y bi) (rect-y bj))))
                                                (ci (+ (rect-x bi) (/ (rect-w bi) 2))) (cj (+ (rect-x bj) (/ (rect-w bj) 2)))
                                                (di (+ (rect-y bi) (/ (rect-h bi) 2))) (dj (+ (rect-y bj) (/ (rect-h bj) 2))))
                                           (if (<= ox oy)
                                               (let ((s (if (< ci cj) 1d0 (if (> ci cj) -1d0 (if (< i j) 1d0 -1d0)))))
                                                 (decf (node-x (aref v i)) (* s (/ ox 2)))
                                                 (incf (node-x (aref v j)) (* s (/ ox 2)))
                                                 (decf (rect-x bi) (* s (/ ox 2))) (incf (rect-x bj) (* s (/ ox 2))))
                                               (let ((s (if (< di dj) 1d0 (if (> di dj) -1d0 (if (< i j) 1d0 -1d0)))))
                                                 (decf (node-y (aref v i)) (* s (/ oy 2)))
                                                 (incf (node-y (aref v j)) (* s (/ oy 2)))
                                                 (decf (rect-y bi) (* s (/ oy 2))) (incf (rect-y bj) (* s (/ oy 2))))))))))))
            while moved)
      ;; certain: scan left to right
      (when (some-overlap-p v spacing)
        (scan-place v spacing :axis :x :from-current t))))
  nodes)

(defun some-overlap-p (v spacing)
  (let ((boxes (sort (map 'vector (lambda (n) (node-box n spacing)) v) #'< :key #'rect-x)))
    (loop for i below (length boxes)
          thereis (loop for j from (1+ i) below (length boxes)
                        while (< (rect-x (aref boxes j)) (rect-right (aref boxes i)))
                        thereis (boxes-overlap-p (aref boxes i) (aref boxes j))))))

(defun scan-place (v spacing &key (axis :x) from-current)
  "Place the nodes of V one by one in order of position along AXIS, each just
   past every earlier node it overlaps across the other axis.  FROM-CURRENT
   keeps a node where it is when nothing is in its way (overlap removal);
   otherwise nodes move as far back as they can (compaction)."
  (let* ((x-axis (eq axis :x))
         (order (sort (copy-seq v) #'<
                      :key (lambda (n) (let ((b (node-box n spacing))) (if x-axis (rect-x b) (rect-y b))))))
         (placed '())
         (start (if (plusp (length order))
                    (reduce #'min order :key (lambda (n) (let ((b (node-box n spacing)))
                                                           (if x-axis (rect-x b) (rect-y b)))))
                    0d0)))
    (loop for n across order
          do (let* ((b (node-box n spacing))
                    (lo (if x-axis (rect-y b) (rect-x b)))
                    (hi (if x-axis (rect-bottom b) (rect-right b)))
                    (pos (if from-current (if x-axis (rect-x b) (rect-y b)) start)))
               (dolist (q placed)
                 (let ((qlo (if x-axis (rect-y q) (rect-x q))) (qhi (if x-axis (rect-bottom q) (rect-right q))))
                   (when (and (< lo (- qhi 1d-6)) (< qlo (- hi 1d-6)))
                     (setf pos (max pos (if x-axis (rect-right q) (rect-bottom q)))))))
               (let ((delta (- pos (if x-axis (rect-x b) (rect-y b)))))
                 (if x-axis
                     (progn (incf (node-x n) delta) (incf (rect-x b) delta))
                     (progn (incf (node-y n) delta) (incf (rect-y b) delta))))
               (push b placed)))))

(defun compact-nodes (nodes spacing)
  "Squeeze overlap-free NODES toward the top-left, keeping their order."
  (let ((v (coerce nodes 'vector)))
    (scan-place v spacing :axis :x)
    (scan-place v spacing :axis :y)))

;;; ---- components and packing ------------------------------------------------------

(defun sibling-edges (node)
  "NODE's frame edges whose two ends are children of NODE (or their ports)."
  (remove-if-not (lambda (e)
                   (let ((a (endpoint-node (first (edge-sources e))))
                         (b (endpoint-node (first (edge-targets e)))))
                     (and (eq (node-parent a) node) (eq (node-parent b) node)
                          (not (excluded-p a)) (not (excluded-p b)))))
                 (frame-edges node)))

(defun neighbours-table (nodes edges)
  (let ((h (make-hash-table :test 'eq)))
    (dolist (n nodes) (setf (gethash n h) '()))
    (dolist (e edges)
      (let ((a (endpoint-node (first (edge-sources e)))) (b (endpoint-node (first (edge-targets e)))))
        (unless (eq a b)
          (push b (gethash a h)) (push a (gethash b h)))))
    h))

(defun node-components (nodes edges)
  (let ((nb (neighbours-table nodes edges)))
    (undirected-components nodes (lambda (n) (gethash n nb)))))

(defun group-rect (nodes)
  (let ((r nil))
    (dolist (n nodes r)
      (let ((e (node-extent n)))
        (setf r (rect-union r (rect (+ (node-x n) (rect-x e)) (+ (node-y n) (rect-y e)) (rect-w e) (rect-h e))))))))

(defun pack-groups (groups spacing aspect)
  "Put each group of positioned nodes in rows, largest first."
  (let* ((items (mapcar (lambda (g) (cons g (or (group-rect g) (rect 0d0 0d0 0d0 0d0)))) groups))
         (items (sort items #'> :key (lambda (it) (* (rect-w (cdr it)) (rect-h (cdr it))))))
         (area (reduce #'+ items :key (lambda (it) (* (+ (rect-w (cdr it)) spacing) (+ (rect-h (cdr it)) spacing)))))
         (widest (reduce #'max items :key (lambda (it) (rect-w (cdr it))) :initial-value 0d0))
         (row-width (max widest (sqrt (* area aspect))))
         (x 0d0) (y 0d0) (row-h 0d0))
    (dolist (it items)
      (let ((r (cdr it)))
        (when (and (> x 0) (> (+ x (rect-w r)) row-width))
          (setf x 0d0 y (+ y row-h spacing) row-h 0d0))
        (let ((dx (- x (rect-x r))) (dy (- y (rect-y r))))
          (dolist (n (car it)) (incf (node-x n) dx) (incf (node-y n) dy)))
        (incf x (+ (rect-w r) spacing))
        (setf row-h (max row-h (rect-h r)))))))

(defun layout-by-components (node spacing aspect fn)
  "Lay out each connected component of NODE's children with FN (called with
   the component's nodes and edges), then pack the components."
  (let* ((kids (layout-children node))
         (edges (sibling-edges node))
         (comps (node-components kids edges))
         (edge-of (make-hash-table :test 'eq)))
    (dolist (e edges) (push e (gethash (endpoint-node (first (edge-sources e))) edge-of)))
    (dolist (c comps)
      (funcall fn c (loop for n in c append (gethash n edge-of))))
    (when (cdr comps) (pack-groups comps spacing aspect))))

;;; ---- rectpacking -----------------------------------------------------------------

(defalgorithm :rectpacking (node)
  (prepare-children node)
  (let* ((spacing (opt-num node :spacing-node-node 15d0 :min 0))
         (aspect (let ((a (opt-num node :aspect-ratio 1.3d0))) (if (> a 0) a 1.3d0)))
         (kids (layout-children node))
         (order (if (opt-bool node :order-by-size nil)
                    (stable-sort (copy-list kids) #'> :key (lambda (c) (let ((e (node-extent c))) (* (rect-w e) (rect-h e)))))
                    kids))
         (target (opt-num node :width-approximation-target-width -1d0)))
    (if (> target 0)
        (row-pack order spacing target)
        (shelf-pack order spacing aspect))
    (finish-simple node :default-padding (pad 15d0 15d0 15d0 15d0))))

(defun row-pack (nodes spacing width)
  (let ((x 0d0) (y 0d0) (row-h 0d0))
    (dolist (c nodes)
      (let* ((e (node-extent c)) (w (rect-w e)) (h (rect-h e)))
        (when (and (> x 0) (> (+ x w) width))
          (setf x 0d0 y (+ y row-h spacing) row-h 0d0))
        (setf (node-x c) (- x (rect-x e)) (node-y c) (- y (rect-y e)))
        (incf x (+ w spacing))
        (setf row-h (max row-h h))))))

;;; ---- topdownpacking: a grid of equal cells ------------------------------------------

(defalgorithm :topdownpacking (node)
  (prepare-children node)
  (let* ((spacing (opt-num node :spacing-node-node 20d0 :min 0))
         (kids (layout-children node))
         (n (length kids))
         (cols (max 1 (ceiling (sqrt n))))
         (cw (+ spacing (reduce #'max kids :key (lambda (c) (rect-w (node-extent c))) :initial-value 0d0)))
         (ch (+ spacing (reduce #'max kids :key (lambda (c) (rect-h (node-extent c))) :initial-value 0d0))))
    (loop for c in kids for i from 0
          for e = (node-extent c)
          do (setf (node-x c) (- (* (mod i cols) cw) (rect-x e))
                   (node-y c) (- (* (floor i cols) ch) (rect-y e))))
    (finish-simple node :default-padding (pad 12d0 12d0 12d0 12d0))))
