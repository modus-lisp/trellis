;;;; simple.lisp -- fixed, box and random layouts.

(in-package #:trellis)

(defun prepare-children (node)
  (dolist (c (node-children node)) (micro-layout c node)))

(defun finish-simple (node &key (default-padding (pad 12d0 12d0 12d0 12d0)) (shift t))
  "Fit NODE around its children and route its edges straight."
  (let ((padding (graph-padding node default-padding)))
    (if shift
        (fit-node-to-children node padding)
        (let ((r nil))
          (dolist (c (node-children node))
            (let ((e (node-extent c)))
              (setf r (rect-union r (rect (+ (node-x c) (rect-x e)) (+ (node-y c) (rect-y e))
                                          (rect-w e) (rect-h e))))))
          (resize-graph node (if r (+ (rect-right r) (pad-right padding)) (+ (pad-left padding) (pad-right padding)))
                        (if r (+ (rect-bottom r) (pad-bottom padding)) (+ (pad-top padding) (pad-bottom padding))))))
    (route-frame-edges-straight node)))

;;; Fixed: every child stays where it is (or at its position option).
(defalgorithm :fixed (node)
  (prepare-children node)
  (dolist (c (node-children node))
    (let ((p (option c :position)))
      (when (consp p) (setf (node-x c) (sane (px p)) (node-y c) (sane (py p))))))
  (finish-simple node :default-padding (pad 15d0 15d0 15d0 15d0) :shift nil))

;;; Box: children in rows, left to right, each row as wide as the aspect ratio
;;; allows; ordered by priority, then by size when packing asks for it.
(defalgorithm :box (node)
  (prepare-children node)
  (let* ((spacing (opt-num node :spacing-node-node 15d0 :min 0))
         (aspect (let ((a (opt-num node :aspect-ratio 1.3d0))) (if (> a 0) a 1.3d0)))
         (kids (stable-sort (copy-list (layout-children node)) #'>
                            :key (lambda (c) (opt-num c :priority 0d0)))))
    (shelf-pack kids spacing aspect)
    (finish-simple node :default-padding (pad 15d0 15d0 15d0 15d0))))

(defun shelf-pack (nodes spacing aspect)
  "Rows of NODES, wrapped at a width that makes the whole roughly ASPECT wide."
  (let* ((area (reduce #'+ nodes :key (lambda (c) (let ((e (node-extent c)))
                                                    (* (+ (rect-w e) spacing) (+ (rect-h e) spacing))))))
         (widest (reduce #'max nodes :key (lambda (c) (rect-w (node-extent c))) :initial-value 0d0))
         (row-width (max widest (sqrt (* (max area 1d0) aspect))))
         (x 0d0) (y 0d0) (row-h 0d0))
    (dolist (c nodes)
      (let* ((e (node-extent c)) (w (rect-w e)) (h (rect-h e)))
        (when (and (> x 0) (> (+ x w) row-width))
          (setf x 0d0 y (+ y row-h spacing) row-h 0d0))
        (setf (node-x c) (- x (rect-x e)) (node-y c) (- y (rect-y e)))
        (incf x (+ w spacing))
        (setf row-h (max row-h h))))))

;;; Random: children in random cells of a grid, so none overlap.
(defalgorithm :random (node)
  (prepare-children node)
  (let* ((kids (coerce (layout-children node) 'vector))
         (rng (make-rng (opt-int node :random-seed 0)))
         (spacing (opt-num node :spacing-node-node 15d0 :min 0))
         (aspect (let ((a (opt-num node :aspect-ratio 1.6d0))) (if (> a 0) a 1.6d0)))
         (cw (+ spacing (reduce #'max kids :key (lambda (c) (rect-w (node-extent c))) :initial-value 0d0)))
         (ch (+ spacing (reduce #'max kids :key (lambda (c) (rect-h (node-extent c))) :initial-value 0d0)))
         (n (length kids))
         (cells (max 1 (* 2 n)))
         (cols (max 1 (round (sqrt (/ (* cells aspect ch) (max cw 1d0))))))
         (rows (ceiling cells cols))
         (order (rng-shuffle rng (coerce (loop for i below (* rows cols) collect i) 'vector))))
    (loop for c across kids for i from 0
          for cell = (aref order i)
          for e = (node-extent c)
          do (setf (node-x c) (- (+ (* (mod cell cols) cw) (* (rng-float rng) (- cw (rect-w e) spacing)))
                                 (rect-x e))
                   (node-y c) (- (+ (* (floor cell cols) ch) (* (rng-float rng) (- ch (rect-h e) spacing)))
                                 (rect-y e))))
    (finish-simple node :default-padding (pad 15d0 15d0 15d0 15d0))))
