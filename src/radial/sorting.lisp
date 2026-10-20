;;;; radial/sorting.lisp -- org.eclipse.elk.alg.radial.sorting

(in-package #:elk)

;;; ---------------------------------------------------------------- IDSorter

(defjmethod j-sort ((this id-sorter) nodes)
  (jl-sort nodes
           (lambda (node1 node2)
             (let ((order-id1 (get-property node1 radial-options.order-id))
                   (order-id2 (get-property node2 radial-options.order-id)))
               (integer.compare order-id1 order-id2)))))

(defjmethod initialize ((this id-sorter) root)
  ;; nothing to do here
  nil)

;;; ---------------------------------------------------------------- PolarCoordinateSorter

(defjmethod initialize ((this polar-coordinate-sorter) root)
  (setf (.id-sorter this) (new-id-sorter))
  (let ((successors (radial-util.get-successors root)))
    ;; sort the first layer, the sorting starts at degree 0 the first polar coordinate position, which is on the
    ;; right of the circle.
    (jl-sort successors (.comp-right this))
    (%polar-set-id-for-nodes this successors 0)))

(defun %polar-set-id-for-nodes (this nodes id-offset)
  (let ((id id-offset)
        (next-layer-id 0))
    (jdo (node nodes)
      (set-property node radial-options.order-id id)
      (incf id)
      (let* ((node-successors (radial-util.get-successors node))
             (arc (math.atan2 (+ (get-y node) (/ (get-height node) 2))
                              (+ (get-x node) (/ (get-width node) 2)))))
        (when (< arc 0)
          (incf arc (* 2 pi)))
        ;; node is right of parent node
        (cond ((or (< arc polar-coordinate-sorter.degree-45) (> arc polar-coordinate-sorter.degree-315))
               (jl-sort node-successors (.comp-left this)))
              ;; node is below parent node
              ((and (<= arc polar-coordinate-sorter.degree-315) (> arc polar-coordinate-sorter.degree-225))
               (jl-sort node-successors (.comp-top this)))
              ;; node is left
              ((and (<= arc polar-coordinate-sorter.degree-225) (> arc polar-coordinate-sorter.degree-135))
               (jl-sort node-successors (.comp-right this)))
              ;; node is top
              ((<= arc polar-coordinate-sorter.degree-135)
               (jl-sort node-successors (.comp-bottom this))))
        (setf next-layer-id (%polar-set-id-for-nodes this node-successors next-layer-id))))
    id))

(defjmethod j-sort ((this polar-coordinate-sorter) nodes)
  (unless (jl-is-empty nodes)
    (when (null (.id-sorter this))
      (let ((root (radial-util.find-root-of-node (j-get nodes 0))))
        (initialize this root)))
    (j-sort (.id-sorter this) nodes)))

;;; ---------------------------------------------------------------- p1position.wedge

(defjmethod calculate-wedge-space ((this annulus-wedge-by-leafs) node)
  (radial-util.get-number-of-leaves node))

(defjmethod calculate-wedge-space ((this annulus-wedge-by-node-space) node)
  (let* ((successors (radial-util.get-successors node))
         (height (get-height node))
         (width (get-width node))
         (node-size (math.sqrt (+ (* height height) (* width width))))
         (child-space 0))
    (jdo (child successors)
      (incf child-space (calculate-wedge-space this child)))
    (math.max child-space node-size)))
