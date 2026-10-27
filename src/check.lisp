;;;; check.lisp -- what a finished layout must satisfy, and how good it is.
;;;;
;;;; CHECK-LAYOUT applies the checks of the ELK corpus harness, with the same
;;;; tolerances: finite coordinates, no overlapping siblings (by more than 1 on
;;;; both axes), every edge routed, every route starting and ending within 3 of
;;;; its first source and first target, and children inside compound parents
;;;; (to within 1).  LAYOUT-METRICS measures quality, for comparing layouts of
;;;; the same graph.

(in-package #:trellis)

(defun non-finite-p (&rest xs)
  (some (lambda (x) (not (and (realp x) (finite-p x)))) xs))

(defun touches-p (shape x y)
  (let* ((o (if (port-p shape) (absolute-position shape) (absolute-position shape)))
         (sx (px o)) (sy (py o)) (tol 3d0))
    (and (>= x (- sx tol)) (<= x (+ sx (shape-width shape) tol))
         (>= y (- sy tol)) (<= y (+ sy (shape-height shape) tol)))))

(defun check-layout (graph)
  "Problems with GRAPH's layout, as a plist of counts:
   (:non-finite n :overlaps n :detached n :unrouted n :outside n).  All zero
   means the layout passes."
  (let ((non-finite 0) (overlaps 0) (detached 0) (unrouted 0) (outside 0))
    (labels ((walk (p)
               (let ((ch (coerce (node-children p) 'vector)))
                 (loop for n across ch
                       do (when (non-finite-p (node-x n) (node-y n) (node-width n) (node-height n))
                            (incf non-finite))
                          (dolist (pt (node-ports n))
                            (when (non-finite-p (port-x pt) (port-y pt)) (incf non-finite)))
                          (when (and (node-parent p)
                                     (or (< (node-x n) -1) (< (node-y n) -1)
                                         (> (+ (node-x n) (node-width n)) (+ (node-width p) 1))
                                         (> (+ (node-y n) (node-height n)) (+ (node-height p) 1))))
                            (incf outside)))
                 (setf overlaps (+ overlaps (count-overlaps ch)))
                 (dolist (e (node-edges p))
                   (if (null (edge-sections e))
                       (incf unrouted)
                       (let* ((off (content-origin p))
                              (first (first (edge-sections e)))
                              (last (car (last (edge-sections e))))
                              (s (section-start first)) (tt (section-end last)))
                         (cond ((non-finite-p (px s) (py s) (px tt) (py tt)) (incf non-finite))
                               ((or (null (edge-sources e)) (null (edge-targets e))
                                    (not (touches-p (first (edge-sources e))
                                                    (+ (px off) (px s)) (+ (py off) (py s))))
                                    (not (touches-p (first (edge-targets e))
                                                    (+ (px off) (px tt)) (+ (py off) (py tt)))))
                                (incf detached))))))
                 (loop for n across ch do (walk n)))))
      (walk graph))
    (when (non-finite-p (node-width graph) (node-height graph)) (incf non-finite))
    (list :non-finite non-finite :overlaps overlaps :detached detached
          :unrouted unrouted :outside outside)))

(defun count-overlaps (nodes)
  "Pairs of NODES (a vector) overlapping by more than 1 on both axes.  Sorted
   by x so only pairs whose x ranges meet are compared."
  (let ((v (sort (copy-seq nodes) #'< :key #'node-x)) (count 0))
    (loop for i from 0 below (length v)
          for a = (aref v i)
          do (loop for j from (1+ i) below (length v)
                   for b = (aref v j)
                   while (< (node-x b) (+ (node-x a) (max 0d0 (node-width a)) 1))
                   do (let ((ox (- (min (+ (node-x a) (node-width a)) (+ (node-x b) (node-width b)))
                                   (max (node-x a) (node-x b))))
                            (oy (- (min (+ (node-y a) (node-height a)) (+ (node-y b) (node-height b)))
                                   (max (node-y a) (node-y b)))))
                        (when (and (> ox 1) (> oy 1)) (incf count)))))
    ;; nodes with negative width can overlap from the left; count those too
    (when (some (lambda (n) (minusp (node-width n))) nodes)
      (setf count 0)
      (loop for i from 0 below (length nodes)
            do (loop for j from (1+ i) below (length nodes)
                     for a = (aref nodes i) for b = (aref nodes j)
                     do (let ((ox (- (min (+ (node-x a) (node-width a)) (+ (node-x b) (node-width b)))
                                     (max (node-x a) (node-x b))))
                              (oy (- (min (+ (node-y a) (node-height a)) (+ (node-y b) (node-height b)))
                                     (max (node-y a) (node-y b)))))
                          (when (and (> ox 1) (> oy 1)) (incf count))))))
    count))

;;; ---- metrics ---------------------------------------------------------------

(defun edge-polyline (e)
  "E's route as absolute points, all sections joined."
  (let ((off (content-origin (edge-container e))))
    (loop for s in (edge-sections e)
          append (mapcar (lambda (p) (pt+ off p)) (section-path s)))))

(defun segments-cross-p (a b c d)
  "Do segments AB and CD cross properly (not just touch)?"
  (flet ((orient (p q r)
           (let ((v (- (* (- (px q) (px p)) (- (py r) (py p)))
                       (* (- (py q) (py p)) (- (px r) (px p))))))
             (cond ((> v 1d-7) 1) ((< v -1d-7) -1) (t 0)))))
    (let ((o1 (orient a b c)) (o2 (orient a b d)) (o3 (orient c d a)) (o4 (orient c d b)))
      (and (/= o1 0) (/= o2 0) (/= o3 0) (/= o4 0) (/= o1 o2) (/= o3 o4)))))

(defun layout-metrics (graph &key (edges (all-edges graph)))
  "Quality measures of GRAPH's layout, as a plist: :width :height :area,
   :crossings (proper crossings between different edges), :bends,
   :edge-length, and :node-hits (edge segments passing through a node that is
   not an endpoint of the edge).  EDGES limits the edges measured."
  (let ((segs '()) (bends 0) (len 0d0))
    (dolist (e edges)
      (let ((pts (edge-polyline e)))
        (dolist (s (edge-sections e)) (incf bends (length (section-bends s))))
        (loop for (a b) on pts while b
              do (incf len (sqrt (+ (expt (- (px b) (px a)) 2) (expt (- (py b) (py a)) 2))))
                 (push (list (min (px a) (px b)) (max (px a) (px b)) a b e) segs))))
    (let ((v (sort (coerce segs 'vector) #'< :key #'first)) (crossings 0))
      (loop for i from 0 below (length v)
            for (x0 x1 a b e) = (aref v i)
            do (loop for j from (1+ i) below (length v)
                     for (y0 nil c dd f) = (aref v j)
                     while (<= y0 x1)
                     do (when (and (not (eq e f)) (segments-cross-p a b c dd))
                          (incf crossings)))
               (progn x0))
      (list :width (node-width graph) :height (node-height graph)
            :area (* (max 0d0 (node-width graph)) (max 0d0 (node-height graph)))
            :crossings crossings :bends bends :edge-length len
            :node-hits (count-node-hits graph v)))))

(defun count-node-hits (graph segments)
  "Edge segments that pass through the interior of a node other than the
   edge's own endpoints (or their ancestors)."
  (let ((boxes '()))
    (map-nodes (lambda (n)
                 (let ((o (absolute-position n)))
                   (when (and (> (node-width n) 2) (> (node-height n) 2))
                     (push (list (+ (px o) 1) (+ (py o) 1)
                                 (+ (px o) (node-width n) -1) (+ (py o) (node-height n) -1) n)
                           boxes))))
               graph)
    (let ((hits 0) (bx (sort (coerce boxes 'vector) #'< :key #'first)))
      (loop for (sx0 sx1 a b e) across segments
            do (loop for (x0 y0 x1 y1 n) across bx
                     while (<= x0 sx1)
                     do (when (and (>= x1 sx0)
                                   (segment-hits-box-p a b x0 y0 x1 y1)
                                   (not (edge-touches-node-p e n)))
                          (incf hits))))
      hits)))

(defun edge-touches-node-p (e n)
  (flet ((related (ep) (let ((m (endpoint-node ep))) (or (ancestor-p n m) (ancestor-p m n)))))
    (or (some #'related (edge-sources e)) (some #'related (edge-targets e)))))

(defun segment-hits-box-p (a b x0 y0 x1 y1)
  "Liang-Barsky: does segment AB meet the open box?"
  (let ((t0 0d0) (t1 1d0) (dx (- (px b) (px a))) (dy (- (py b) (py a))))
    (flet ((clip (p q)
             (cond ((zerop p) (>= q 0))
                   (t (let ((r (/ q p)))
                        (if (< p 0)
                            (if (> r t1) nil (progn (setf t0 (max t0 r)) t))
                            (if (< r t0) nil (progn (setf t1 (min t1 r)) t))))))))
      (and (clip (- dx) (- (px a) x0)) (clip dx (- x1 (px a)))
           (clip (- dy) (- (py a) y0)) (clip dy (- y1 (py a)))
           (< t0 t1)))))
