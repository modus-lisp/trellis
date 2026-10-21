;;;; overlaps.lisp -- org.eclipse.elk.alg.common.overlaps methods
;;;; RectangleStripOverlapRemover, its RectangleNode and the greedy strategy.
;;;; The scanline's TreeSet (sorted by right border, comparator-equal elements
;;;; rejected on add) is a sorted adjustable vector here: same iteration order,
;;;; same duplicate rule.

(in-package #:elk)

;;; ------------------------------------------------------------ RectangleNode

(defjctor rectangle-node (original-rectangle rectangle)
  (setf (.original-rectangle this) original-rectangle
        (.rectangle this) rectangle))

(defjmethod get-rectangle ((this rectangle-node)) (.rectangle this))
(defjmethod get-overlapping-nodes ((this rectangle-node)) (.overlapping-nodes this))

;;; ------------------------------------------------------------ strategy

(defjgeneric remove-overlaps 1)

(defjmethod remove-overlaps ((this greedy-rectangle-strip-overlap-remover) overlap-remover)
  (let ((vertical-gap (get-vertical-gap overlap-remover))
        (already-placed-nodes (new-jset))
        (strip-size 0d0))
    (jdo (curr-node (get-rectangle-nodes overlap-remover))
      (let ((y-pos 0d0))
        (jl-sort (get-overlapping-nodes curr-node)
                 (lambda (a b) (greedy-rectangle-strip-overlap-remover.compare-by-y-coordinate a b)))
        (jdo (overlap-node (get-overlapping-nodes curr-node))
          (when (js-contains already-placed-nodes overlap-node)
            (let ((curr-rect (get-rectangle curr-node))
                  (overlap-rect (get-rectangle overlap-node)))
              (when (and (< y-pos (+ (.y overlap-rect) (.height overlap-rect) vertical-gap))
                         (> (+ y-pos (.height curr-rect) vertical-gap) (.y overlap-rect)))
                (setf y-pos (+ (.y overlap-rect) (.height overlap-rect) vertical-gap))))))
        (setf (.y (get-rectangle curr-node)) y-pos)
        (js-add already-placed-nodes curr-node)
        (setf strip-size (math.max strip-size
                                   (+ (.y (get-rectangle curr-node))
                                      (.height (get-rectangle curr-node)))))))
    strip-size))

(defjstatic greedy-rectangle-strip-overlap-remover.compare-by-y-coordinate (node1 node2)
  (double.compare (.y (get-rectangle node1)) (.y (get-rectangle node2))))

;;; ------------------------------------------------------------ remover

(defjstatic rectangle-strip-overlap-remover.create-for-direction (direction)
  (let ((remover (new-rectangle-strip-overlap-remover)))
    (setf (.overlap-removal-direction remover) direction)
    remover))

(defjmethod with-gap ((this rectangle-strip-overlap-remover) horizontal-gap vertical-gap)
  (setf (.gap-horizontal this) horizontal-gap
        (.gap-vertical this) vertical-gap)
  this)

(defjmethod with-start-coordinate ((this rectangle-strip-overlap-remover) coordinate)
  (setf (.start-coordinate this) coordinate)
  this)

(defjmethod with-overlap-removal-strategy ((this rectangle-strip-overlap-remover) strategy)
  (setf (.overlap-removal-strategy this) strategy)
  this)

(defjmethod add-rectangle ((this rectangle-strip-overlap-remover) rectangle)
  (jl-add (.rectangle-nodes this)
          (new-rectangle-node rectangle (%rsor-import-rectangle this rectangle)))
  this)

(defjmethod get-horizontal-gap ((this rectangle-strip-overlap-remover)) (.gap-horizontal this))
(defjmethod get-vertical-gap ((this rectangle-strip-overlap-remover)) (.gap-vertical this))
(defjmethod get-rectangle-nodes ((this rectangle-strip-overlap-remover)) (.rectangle-nodes this))

(defun %rsor-import-rectangle (this rectangle)
  (let ((dir (.overlap-removal-direction this)))
    (cond ((or (eq dir overlap-removal-direction.up) (eq dir overlap-removal-direction.down))
           rectangle)
          ((or (eq dir overlap-removal-direction.left) (eq dir overlap-removal-direction.right))
           (new-elk-rectangle (.y rectangle) 0d0 (.height rectangle) (.width rectangle)))
          (t (error "assert false")))))

(defun %rsor-export-rectangle (this rectangle-node strip-size)
  (declare (ignore strip-size))
  (let ((rectangle (.rectangle rectangle-node))
        (original (.original-rectangle rectangle-node))
        (dir (.overlap-removal-direction this)))
    (cond ((eq dir overlap-removal-direction.up)
           (setf (.y original) (- (.start-coordinate this) (.height rectangle) (.y rectangle))))
          ((eq dir overlap-removal-direction.down)
           (incf (.y original) (.start-coordinate this)))
          ((eq dir overlap-removal-direction.left)
           (setf (.x original) (- (.start-coordinate this) (.height rectangle) (.y rectangle))))
          ((eq dir overlap-removal-direction.right)
           (setf (.x original) (+ (.start-coordinate this) (.y rectangle)))))))

(defjmethod remove-overlaps ((this rectangle-strip-overlap-remover))
  (unless (.overlap-removal-strategy this)
    (setf (.overlap-removal-strategy this) (new-greedy-rectangle-strip-overlap-remover)))
  (setf (.rectangle-nodes this)
        (jl-sort (.rectangle-nodes this)
                 (lambda (a b) (rectangle-strip-overlap-remover.compare-left-rectangle-borders a b))))
  (%rsor-compute-overlaps this)
  (let ((strip-size (remove-overlaps (.overlap-removal-strategy this) this)))
    (jdo (node (.rectangle-nodes this))
      (%rsor-export-rectangle this node strip-size))
    strip-size))

(defun %rsor-compute-overlaps (this)
  (let ((intersecting (new-jlist)))
    (jdo (curr-node (.rectangle-nodes this))
      (let ((scanline-pos (.x (.rectangle curr-node))))
        (loop while (not (jl-is-empty intersecting))
              do (let ((ir (jl-first intersecting)))
                   (if (< (+ (.x (.rectangle ir)) (.width (.rectangle ir))) scanline-pos)
                       (%rsor-tree-remove intersecting ir)
                       (return))))
        (jdo (intersecting-node intersecting)
          (jl-add (.overlapping-nodes intersecting-node) curr-node)
          (jl-add (.overlapping-nodes curr-node) intersecting-node))
        (%rsor-tree-add intersecting curr-node)))))

(defun %rsor-tree-compare (a b)
  (double.compare (+ (.x (.rectangle a)) (.width (.rectangle a)))
                  (+ (.x (.rectangle b)) (.width (.rectangle b)))))

(defun %rsor-tree-add (set node)
  "TreeSet.add: a comparator-equal element is already present, so nothing is added."
  (let ((lo 0) (hi (length set)))
    (loop while (< lo hi)
          do (let* ((mid (floor (+ lo hi) 2))
                    (c (%rsor-tree-compare (aref set mid) node)))
               (cond ((zerop c) (return-from %rsor-tree-add nil))
                     ((< c 0) (setf lo (1+ mid)))
                     (t (setf hi mid)))))
    (jl-add-at set lo node)
    t))

(defun %rsor-tree-remove (set node)
  "TreeSet.remove: removes the element comparator-equal to NODE."
  (let ((lo 0) (hi (length set)))
    (loop while (< lo hi)
          do (let* ((mid (floor (+ lo hi) 2))
                    (c (%rsor-tree-compare (aref set mid) node)))
               (cond ((zerop c) (jl-remove-at set mid) (return-from %rsor-tree-remove t))
                     ((< c 0) (setf lo (1+ mid)))
                     (t (setf hi mid)))))
    nil))

(defjstatic rectangle-strip-overlap-remover.compare-left-rectangle-borders (rn1 rn2)
  (double.compare (.x (.rectangle rn1)) (.x (.rectangle rn2))))

(defjstatic rectangle-strip-overlap-remover.compare-right-rectangle-borders (rn1 rn2)
  (%rsor-tree-compare rn1 rn2))
