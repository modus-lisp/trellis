;;;; export.lisp -- from an lgraph back to the graph.
;;;;
;;;; Positions come out relative to the component's own top-left corner; the
;;;; caller packs components and adds padding (SHIFT-PLACED).

(in-package #:trellis)

(defstruct (placed (:constructor make-placed ()))
  (width 0d0) (height 0d0)
  (nodes '()) (edges '()) (externals '()) (ties '()))

(defun internal-bounds (lg routes)
  "min-x min-y max-x max-y over node extents and route points."
  (let ((x0 most-positive-double-float) (y0 most-positive-double-float)
        (x1 most-negative-double-float) (y1 most-negative-double-float))
    (dolist (n (lgraph-nodes lg))
      (let ((m (lnode-margin n)))
        (setf x0 (min x0 (- (lnode-x n) (pad-left m)))
              y0 (min y0 (- (lnode-y n) (pad-top m)))
              x1 (max x1 (+ (lnode-x n) (lnode-width n) (pad-right m)))
              y1 (max y1 (+ (lnode-y n) (lnode-height n) (pad-bottom m))))))
    (dolist (r routes)
      (dolist (p (second r))
        (setf x0 (min x0 (px p)) y0 (min y0 (py p)) x1 (max x1 (px p)) y1 (max y1 (py p)))))
    (if (> x0 x1) (values 0d0 0d0 0d0 0d0) (values x0 y0 x1 y1))))

(defun chain-route (lg pieces)
  "Points of a chain of ledges, in the graph edge's own direction."
  (let ((pts (loop for e in pieces append (ledge-route lg e))))
    (if (ledge-reversed (first pieces)) (reverse pts) pts)))

(defun export-component (lg parent)
  (let* ((dir (lgraph-direction lg))
         (splines (eq (getf (lgraph-spacing lg) :routing) :splines))
         (routes '())                   ; (edge points curve)
         (bands '())
         (placed (make-placed)))
    ;; routes in internal coordinates
    (let ((seen (make-hash-table :test 'eq)) (origins '()))
      (dolist (n (lgraph-nodes lg))
        (dolist (e (node-outgoing n))
          (let ((origin (ledge-origin e)))
            (unless (gethash origin seen)
              (setf (gethash origin seen) t)
              (when (chain lg origin) (push origin origins))))))
      (setf origins (nreverse origins))
      (if splines
          (multiple-value-bind (ties tie-bands)
              (if (opt-bool parent :wire-ties nil)
                  (plan-ties lg origins)
                  (values (make-hash-table :test 'eq) '()))
            (setf bands tie-bands)
            (dolist (o origins)
              (push (list o (curve-through (chain-knots lg (chain lg o) ties)) t) routes)))
          (dolist (o origins)
            (push (list o (chain-route lg (chain lg o)) nil) routes))))
    (dolist (sl (lgraph-self-loops lg))
      (push (list (first sl) (self-loop-route lg sl) nil) routes))
    (multiple-value-bind (x0 y0 x1 y1) (internal-bounds lg routes)
      (let ((fw (- x1 x0)) (fh (- y1 y0)))
        (flet ((out-box (x y w h) (box-out dir (- x x0) (- y y0) w h fw fh))
               (out-pt (p) (point-out dir (pt (- (px p) x0) (- (py p) y0)) fw fh)))
          (multiple-value-bind (ow oh) (size-in dir fw fh)
            (setf (placed-width placed) ow (placed-height placed) oh))
          ;; nodes and ports
          (dolist (n (lgraph-nodes lg))
            (case (lnode-kind n)
              (:normal
               (let ((g (lnode-origin n)))
                 (multiple-value-bind (x y) (out-box (lnode-x n) (lnode-y n) (lnode-width n) (lnode-height n))
                   (setf (node-x g) x (node-y g) y))
                 (dolist (p (lnode-ports n))
                   (let ((gp (lport-origin p)))
                     (when (port-p gp)
                       (multiple-value-bind (x y)
                           (out-box (+ (lnode-x n) (lport-x p)) (+ (lnode-y n) (lport-y p))
                                    (lport-width p) (lport-height p))
                         (setf (port-x gp) (- x (node-x g)) (port-y gp) (- y (node-y g)))))))
                 (place-port-labels g :spacing (opt-num parent :spacing-label-port-horizontal 1d0 :min 0))
                 (push g (placed-nodes placed))))
              (:external
               ;; the dummy's port faces inward; the parent's port is on the
               ;; border the dummy's layer stands for
               (let ((a (port-abs (first (lnode-ports n)))))
                 (push (list (lnode-origin n) (out-pt a)
                             (side-out dir (if (eq (lnode-constraint n) :first-separate) :west :east)))
                       (placed-externals placed))))
              (:label
               (let* ((labels (lnode-data n :labels))
                      (size (lnode-data n :label-size)))
                 (multiple-value-bind (x y w h)
                     (out-box (lnode-x n) (lnode-y n) (first size) (second size))
                   (declare (ignore h))
                   (let ((yy y) (ll (getf (lgraph-spacing lg) :label-label 0d0)))
                     (dolist (l labels)
                       (setf (label-x l) (+ x (/ (- w (label-width l)) 2)) (label-y l) yy)
                       (incf yy (+ (label-height l) ll)))))))))
          ;; edges
          (setf (placed-ties placed)
                (mapcar (lambda (b) (cons (out-pt (car b)) (out-pt (cdr b)))) bands))
          (dolist (r routes)
            (destructuring-bind (e points curve) r
             (let ((pts (mapcar #'out-pt points)))
              (if curve (set-curve-route e pts parent) (set-route e pts parent))
              (place-end-labels e)
              (when (member e (lgraph-self-loops lg) :key #'first)
                (place-loop-labels e))
              (push e (placed-edges placed))))))))
    placed))

(defun place-end-labels (edge)
  "Head and tail labels next to the ends of EDGE's route."
  (let ((pts (section-points (first (edge-sections edge)))))
    (dolist (l (edge-labels edge))
      (let ((where (edge-label-placement l)))
        (when (member where '(:head :tail))
          (multiple-value-bind (end prev)
              (if (eq where :head)
                  (values (car (last pts)) (car (last pts 2)))
                  (values (first pts) (second pts)))
            (let* ((dx (- (px end) (px prev))) (dy (- (py end) (py prev)))
                   (lw (label-width l)) (lh (label-height l)) (g 2d0))
              (if (>= (abs dx) (abs dy))
                  (setf (label-x l) (if (> dx 0) (- (px end) lw g) (+ (px end) g))
                        (label-y l) (- (py end) lh g))
                  (setf (label-x l) (+ (px end) g)
                        (label-y l) (if (> dy 0) (- (py end) lh g) (+ (py end) g)))))))))))

(defun shift-placed (placed dx dy)
  (dolist (g (placed-nodes placed)) (incf (node-x g) dx) (incf (node-y g) dy))
  (dolist (e (placed-edges placed)) (move-route e (pt dx dy)))
  (setf (placed-ties placed)
        (mapcar (lambda (b) (cons (pt+ (car b) (pt dx dy)) (pt+ (cdr b) (pt dx dy)))) (placed-ties placed)))
  (setf (placed-externals placed)
        (mapcar (lambda (x) (list (first x) (pt+ (second x) (pt dx dy)) (third x)))
                (placed-externals placed))))

;;; ---- self loops ------------------------------------------------------------------

(defun place-loop-labels (edge)
  "Put a self loop's labels outside the loop, beyond its farthest segment."
  (let* ((pts (section-points (first (edge-sections edge))))
         (cx (/ (reduce #'+ pts :key #'px) (length pts)))
         (cy (/ (reduce #'+ pts :key #'py) (length pts)))
         ;; the loop's outer corner: farthest from where it starts
         (a (first pts))
         (far (reduce (lambda (p q) (if (> (+ (abs (- (px q) (px a))) (abs (- (py q) (py a))))
                                           (+ (abs (- (px p) (px a))) (abs (- (py p) (py a)))))
                                        q p))
                      pts))
         (y (py far)))
    (declare (ignore cx cy))
    (dolist (l (edge-labels edge))
      (setf (label-x l) (+ (px far) 2d0) (label-y l) y)
      (incf y (label-height l)))))

(defun self-loop-route (lg loop)
  "A route for a self loop around its node, at the distance stored for it."
  (destructuring-bind (e sp tp) loop
    (declare (ignore e))
    (let* ((n (lport-node sp))
           (d (or (cdr (assoc sp (lnode-data n :loops))) 10d0))
           (w (lnode-width n)) (h (lnode-height n))
           (a (port-abs sp)) (b (port-abs tp))
           (x0 (lnode-x n)) (y0 (lnode-y n)))
      (flet ((out (p side)
               (ecase side
                 (:north (pt (px p) (- y0 d))) (:south (pt (px p) (+ y0 h d)))
                 (:west (pt (- x0 d) (py p))) (:east (pt (+ x0 w d) (py p)))))
             (corner (from)
               ;; the corner reached walking clockwise from side FROM
               (ecase from
                 (:north (pt (+ x0 w d) (- y0 d))) (:east (pt (+ x0 w d) (+ y0 h d)))
                 (:south (pt (- x0 d) (+ y0 h d))) (:west (pt (- x0 d) (- y0 d)))))
             (next (side) (ecase side (:north :east) (:east :south) (:south :west) (:west :north))))
        (let ((sa (lport-side sp)) (sb (lport-side tp)) (corners '()))
          (loop with s = sa
                until (eq s sb)
                do (push (corner s) corners) (setf s (next s)))
          (append (list a (out a sa)) (nreverse corners) (list (out b sb) b)))))))
