;;;; radial/compaction.lisp -- org.eclipse.elk.alg.radial.intermediate.compaction and
;;;; intermediate.overlaps.RadiusExtensionOverlapRemoval

(in-package #:elk)

;;; ---------------------------------------------------------------- AbstractRadiusExtensionCompaction

(defjmethod set-compaction-step ((this abstract-radius-extension-compaction) step)
  (setf (.compaction-step this) step))

(defjmethod get-compaction-step ((this abstract-radius-extension-compaction))
  (.compaction-step this))

(defjmethod set-spacing ((this abstract-radius-extension-compaction) spacing)
  (setf (.spacing this) spacing))

(defjmethod get-spacing ((this abstract-radius-extension-compaction))
  (.spacing this))

(defjmethod set-root ((this abstract-radius-extension-compaction) root)
  (setf (.root this) root))

(defjmethod get-root ((this abstract-radius-extension-compaction))
  (.root this))

(defjmethod contract-layer ((this abstract-radius-extension-compaction) layer-nodes is-contracting)
  (jdo (node layer-nodes)
    ;; node center
    (let* ((x-pos (+ (get-x node) (/ (get-width node) 2)))
           (y-pos (+ (get-y node) (/ (get-height node) 2)))
           (tree-parent (.root this))
           (parent-x (+ (get-x tree-parent) (/ (get-width tree-parent) 2)))
           (parent-y (+ (get-y tree-parent) (/ (get-height tree-parent) 2)))
           ;; vector of edge
           (x (- x-pos parent-x))
           (y (- y-pos parent-y))
           ;; vector length
           (len (math.sqrt (+ (* x x) (* y y)))))
      ;; multiply with normalized vector
      (setf x (* x (/ (.compaction-step this) len))
            y (* y (/ (.compaction-step this) len)))
      (if is-contracting
          (setf x-pos (- x-pos x)
                y-pos (- y-pos y))
          (setf x-pos (+ x-pos x)
                y-pos (+ y-pos y)))
      (set-x node (- x-pos (/ (get-width node) 2)))
      (set-y node (- y-pos (/ (get-height node) 2)))))
  nil)

(defjmethod move-node ((this abstract-radius-extension-compaction) node distance)
  ;; Move child distance into the direction of the connection to the root node
  ;; We have to measure from center to center since the different nodes might have different sizes.
  (let* ((root (.root this))
         (root-x (+ (get-x root) (/ (get-width root) 2.0d0)))
         (root-y (+ (get-y root) (/ (get-height root) 2.0d0)))
         (node-x (+ (get-x node) (/ (get-width node) 2.0d0)))
         (node-y (+ (get-y node) (/ (get-height node) 2.0d0)))
         (difference-x (- node-x root-x))
         (difference-y (- node-y root-y))
         ;; Calculate unit vector
         (len (math.sqrt (+ (* difference-x difference-x) (* difference-y difference-y))))
         (unit-x (/ difference-x len))
         (unit-y (/ difference-y len)))
    ;; Move node by distance in direction of uni vector.
    (set-x node (+ (get-x node) (* unit-x distance)))
    (set-y node (+ (get-y node) (* unit-y distance)))))

(defjmethod overlap ((this abstract-radius-extension-compaction) node1 node2)
  (let* ((spacing (.spacing this))
         (x1 (- (get-x node1) (/ spacing 2)))
         (x2 (- (get-x node2) (/ spacing 2)))
         (y1 (- (get-y node1) (/ spacing 2)))
         (y2 (- (get-y node2) (/ spacing 2)))
         (width1 (+ (get-width node1) spacing))
         (width2 (+ (get-width node2) spacing))
         (height1 (+ (get-height node1) spacing))
         (height2 (+ (get-height node2) spacing)))
    (cond ((and (< x1 (+ x2 width2)) (< x2 x1) (< y1 (+ y2 height2)) (< y2 y1))
           ;; left upper and right lower corner overlap
           t)
          ((and (< x2 (+ x1 width1)) (< x1 x2) (< y2 (+ y1 height1)) (< y1 y2))
           ;; right lower and left upper corner overlap
           t)
          ((and (< x1 (+ x2 width2)) (< x2 x1) (< y1 y2) (< y2 (+ y1 height1)))
           ;; left lower and right upper corner overlap
           t)
          ((and (< x2 (+ x1 width1)) (< x1 x2) (< y1 (+ y2 height2)) (< y2 y1))
           ;; right upper and left lower corner overlap
           t)
          (t nil))))

(defjmethod overlap-layer ((this abstract-radius-extension-compaction) nodes)
  (let ((overlapping nil))
    (when (< (size nodes) 2)
      (return-from %overlap-layer-block nil))
    nil)
  (let ((overlapping nil)
        (n (size nodes)))
    (when (< n 2)
      (return-from overlap-layer-body nil))
    nil))
