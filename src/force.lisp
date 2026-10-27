;;;; force.lisp -- force-directed and stress layouts, and the SPOrE / DisCo
;;;; post-processors.
;;;;
;;;; force: the spring embedders of Eades (1984) and of Fruchterman and
;;;; Reingold (1991), repulsion limited to nearby nodes with a grid, the step
;;;; cooling linearly.  stress: stress majorization (Gansner, Koren and North
;;;; 2004), ideal distances from shortest paths.  Both lay out connected
;;;; components separately and pack them, and finish by removing overlaps.

(in-package #:trellis)

(defun node-center (n)
  (let ((e (node-extent n)))
    (pt (+ (node-x n) (rect-x e) (/ (rect-w e) 2)) (+ (node-y n) (rect-y e) (/ (rect-h e) 2)))))

(defun set-node-center (n c)
  (let ((e (node-extent n)))
    (setf (node-x n) (- (px c) (rect-x e) (/ (rect-w e) 2))
          (node-y n) (- (py c) (rect-y e) (/ (rect-h e) 2)))))

(defun initial-positions (nodes rng scale interactive)
  "Start positions: given ones when INTERACTIVE and present, else random."
  (let* ((n (length nodes)) (side (* scale (sqrt (max 1 n))))
         (xs (make-array n :element-type 'double-float))
         (ys (make-array n :element-type 'double-float)))
    (loop for v in nodes for i from 0
          do (if (and interactive (shape-positioned v))
                 (let ((c (node-center v))) (setf (aref xs i) (px c) (aref ys i) (py c)))
                 (setf (aref xs i) (* side (rng-float rng)) (aref ys i) (* side (rng-float rng)))))
    (values xs ys)))

(defun index-edges (nodes edges)
  (let ((idx (make-hash-table :test 'eq)))
    (loop for v in nodes for i from 0 do (setf (gethash v idx) i))
    (loop for e in edges
          for a = (gethash (endpoint-node (first (edge-sources e))) idx)
          for b = (gethash (endpoint-node (first (edge-targets e))) idx)
          when (and a b (/= a b)) collect (list a b e))))

;;; ---- force -------------------------------------------------------------------------

(defvar *force-spacing* 80d0 "The ideal edge length force uses when not told.")

(defun force-component (nodes edges opts rng)
  (let* ((n (length nodes))
         (pairs (index-edges nodes edges))
         (sizes (map 'vector #'node-diagonal nodes))
         (mean-size (if (plusp n) (/ (reduce #'+ sizes) n) 0d0))
         (k (+ (opt-num opts :spacing-node-node *force-spacing* :min 1) (* 0.5d0 mean-size)))
         (model (opt-enum opts :model :fruchterman-reingold '(:eades :fruchterman-reingold)))
         (iterations (min 300 (opt-int opts :iterations 300 :min 1)))
         (interactive (opt-bool opts :interactive nil)))
    (multiple-value-bind (xs ys) (initial-positions nodes rng k interactive)
      (when (> n 1)
        (let ((dx (make-array n :element-type 'double-float))
              (dy (make-array n :element-type 'double-float))
              (temp (* k (sqrt n) 0.2d0))
              (cell (* 2 k)))
          (dotimes (it iterations)
            (fill dx 0d0) (fill dy 0d0)
            ;; repulsion between nodes in neighbouring grid cells
            (let ((grid (make-hash-table :test 'equal)))
              (dotimes (i n)
                (push i (gethash (cons (floor (aref xs i) cell) (floor (aref ys i) cell)) grid)))
              (dotimes (i n)
                (let ((gx (floor (aref xs i) cell)) (gy (floor (aref ys i) cell)))
                  (loop for ox from -1 to 1
                        do (loop for oy from -1 to 1
                                 do (dolist (j (gethash (cons (+ gx ox) (+ gy oy)) grid))
                                      (when (< i j)
                                        (let* ((ddx (- (aref xs i) (aref xs j))) (ddy (- (aref ys i) (aref ys j)))
                                               (dist (max 0.01d0 (sqrt (+ (* ddx ddx) (* ddy ddy))))))
                                          (when (< dist cell)
                                            (when (< dist 0.02d0)
                                              (setf ddx (- (rng-float rng) 0.5d0) ddy (- (rng-float rng) 0.5d0)))
                                            (let ((f (if (eq model :eades)
                                                         (/ (* k k k) (* dist dist dist))
                                                         (/ (* k k) (* dist dist)))))
                                              (incf (aref dx i) (* ddx f)) (incf (aref dy i) (* ddy f))
                                              (decf (aref dx j) (* ddx f)) (decf (aref dy j) (* ddy f))))))))))))
            ;; attraction along edges
            (dolist (p pairs)
              (destructuring-bind (i j e) p
                (declare (ignore e))
                (let* ((ddx (- (aref xs i) (aref xs j))) (ddy (- (aref ys i) (aref ys j)))
                       (dist (max 0.01d0 (sqrt (+ (* ddx ddx) (* ddy ddy)))))
                       (f (if (eq model :eades)
                              (/ (log (/ dist k)) dist)
                              (/ dist k))))
                  (decf (aref dx i) (* ddx f)) (decf (aref dy i) (* ddy f))
                  (incf (aref dx j) (* ddx f)) (incf (aref dy j) (* ddy f)))))
            (let ((tt (* temp (- 1 (/ it (d iterations))))))
              (dotimes (i n)
                (let ((len (sqrt (+ (expt (aref dx i) 2) (expt (aref dy i) 2)))))
                  (when (> len 0)
                    (let ((s (/ (min len tt) len)))
                      (incf (aref xs i) (* s (aref dx i))) (incf (aref ys i) (* s (aref dy i)))))))))))
      (loop for v in nodes for i from 0 do (set-node-center v (pt (aref xs i) (aref ys i)))))))

(defun force-like (node fn default-spacing)
  (prepare-children node)
  (let* ((spacing (opt-num node :spacing-node-node default-spacing :min 0))
         (rng (make-rng (opt-int node :random-seed 1))))
    (if (opt-bool node :separate-connected-components t)
        (layout-by-components node (opt-num node :spacing-component-component spacing :min 0)
                              (let ((a (opt-num node :aspect-ratio 1.6d0))) (if (> a 0) a 1.6d0))
                              (lambda (nodes edges) (funcall fn nodes edges node rng)))
        (funcall fn (layout-children node) (sibling-edges node) node rng))
    (remove-overlaps (layout-children node) (min spacing 20d0))
    (finish-simple node :default-padding (pad 50d0 50d0 50d0 50d0))))

(defalgorithm :force (node)
  (force-like node #'force-component 80d0))

;;; ---- stress -------------------------------------------------------------------------

(defun shortest-paths (n pairs)
  "All-pairs distances over PAIRS (i j length), as a 2D array; unreachable
   pairs are NIL."
  (let ((adj (make-array n :initial-element '()))
        (dist (make-array (list n n) :initial-element nil)))
    (dolist (p pairs)
      (destructuring-bind (i j len) p
        (push (cons j len) (aref adj i)) (push (cons i len) (aref adj j))))
    (dotimes (s n)
      (let ((h (make-heap)))
        (setf (aref dist s s) 0d0)
        (heap-push h 0d0 s)
        (loop until (heap-empty-p h)
              do (multiple-value-bind (v dv) (heap-pop h)
                   (when (<= dv (aref dist s v))
                     (dolist (w (aref adj v))
                       (let ((nd (+ dv (cdr w))))
                         (when (or (null (aref dist s (car w))) (< nd (aref dist s (car w))))
                           (setf (aref dist s (car w)) nd)
                           (heap-push h nd (car w))))))))))
    dist))

(defun stress-component (nodes edges opts rng)
  (let* ((n (length nodes))
         (default-len (opt-num opts :desired-edge-length 100d0 :min 1))
         (sizes (map 'vector #'node-diagonal nodes))
         (pairs (mapcar (lambda (p)
                          (destructuring-bind (i j e) p
                            (list i j (+ (opt-num e :desired-edge-length default-len :min 1)
                                         (* 0.25d0 (+ (aref sizes i) (aref sizes j)))))))
                        (index-edges nodes edges)))
         (dimension (opt-enum opts :dimension :xy '(:xy :x :y)))
         (epsilon (max 1d-6 (opt-num opts :epsilon 1d-4)))
         (limit (max 5 (min (opt-int opts :iteration-limit 300 :min 1)
                            (floor 4e7 (max 1 (* n n)))))))
    (multiple-value-bind (xs ys) (initial-positions nodes rng default-len (opt-bool opts :interactive nil))
      (when (> n 1)
        (let ((dist (shortest-paths n pairs)) (prev nil))
          (dotimes (it limit)
            (let ((stress 0d0))
              (dotimes (i n)
                (let ((sx 0d0) (sy 0d0) (sw 0d0))
                  (dotimes (j n)
                    (let ((dij (aref dist i j)))
                      (when (and dij (/= i j) (> dij 0))
                        (let* ((w (/ 1d0 (* dij dij)))
                               (ddx (- (aref xs i) (aref xs j))) (ddy (- (aref ys i) (aref ys j)))
                               (len (max 1d-6 (sqrt (+ (* ddx ddx) (* ddy ddy))))))
                          (incf stress (* w (expt (- len dij) 2)))
                          (incf sw w)
                          (incf sx (* w (+ (aref xs j) (* dij (/ ddx len)))))
                          (incf sy (* w (+ (aref ys j) (* dij (/ ddy len)))))))))
                  (when (> sw 0)
                    (unless (eq dimension :y) (setf (aref xs i) (/ sx sw)))
                    (unless (eq dimension :x) (setf (aref ys i) (/ sy sw))))))
              (when (and prev (< (abs (- prev stress)) (* epsilon (max prev 1d-9)))) (return))
              (setf prev stress)))))
      (loop for v in nodes for i from 0 do (set-node-center v (pt (aref xs i) (aref ys i)))))))

(defalgorithm :stress (node)
  (force-like node #'stress-component 20d0))

;;; ---- DisCo, SPOrE ----------------------------------------------------------------------

(defalgorithm :disco (node)
  (let ((*force-spacing* 20d0))
    (force-like node #'force-component 20d0)))

(defalgorithm :spore-overlap (node)
  (prepare-children node)
  (let ((spacing (opt-num node :spacing-node-node 8d0 :min 0)))
    (remove-overlaps (layout-children node) spacing)
    (finish-simple node :default-padding (pad 8d0 8d0 8d0 8d0))))

(defalgorithm :spore-compaction (node)
  (prepare-children node)
  (let ((spacing (opt-num node :spacing-node-node 8d0 :min 0)) (kids (layout-children node)))
    (remove-overlaps kids spacing)
    (compact-nodes kids spacing)
    (finish-simple node :default-padding (pad 8d0 8d0 8d0 8d0))))
