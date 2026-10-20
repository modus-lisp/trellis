;;;; radial/util.lisp -- org.eclipse.elk.alg.radial.RadialUtil

(in-package #:elk)

;;; Guava DoubleMath.fuzzyEquals / fuzzyCompare (local copy: the core-math
;;; helpers are private to that module).
(defun %rad-fuzzy-equals (a b tolerance)
  (or (<= (abs (- a b)) tolerance) (= a b) (and (nan-p a) (nan-p b))))

(defun %rad-fuzzy-compare (a b tolerance)
  (cond ((%rad-fuzzy-equals a b tolerance) 0)
        ((< a b) -1)
        ((> a b) 1)
        (t (boolean.compare (nan-p a) (nan-p b)))))

(defjconst radial-util.two-pi (* 2 pi))
(defjconst radial-util.epsilon 1d-10)

(defjstatic radial-util.get-successors (node)
  (let ((successors (new-jlist))
        (children (jset-from (get-children node))))
    (jdo (outgoing-edge (elk-graph-util.all-outgoing-edges node))
      (unless (typep (j-get (get-sources outgoing-edge) 0) 'elk-port)
        (let ((target (elk-graph-util.connectable-shape-to-node
                       (j-get (get-targets outgoing-edge) 0))))
          (unless (js-contains children target)
            (jl-add successors target)))))
    successors))

(defjstatic radial-util.get-successor-set (node)
  (let ((successors (new-jset))
        (children (jset-from (get-children node))))
    (jdo (outgoing-edge (elk-graph-util.all-outgoing-edges node))
      (unless (typep (j-get (get-sources outgoing-edge) 0) 'elk-port)
        (let ((target (elk-graph-util.connectable-shape-to-node
                       (j-get (get-targets outgoing-edge) 0))))
          (unless (js-contains children target)
            (js-add successors target)))))
    successors))

(defjstatic radial-util.find-root (graph)
  (block find-root
    (jdo (child (get-children graph))
      (let ((incoming-edges (elk-graph-util.all-incoming-edges child)))
        (when (jl-is-empty incoming-edges)
          (return-from find-root child))))
    nil))

(defjstatic radial-util.find-root-of-node (elk-node)
  (let ((parent (radial-util.get-tree-parent elk-node)))
    (if (null parent)
        elk-node
        (radial-util.find-root-of-node parent))))

(defjstatic radial-util.get-number-of-leaves (node)
  (let ((leafs 0)
        (successors (radial-util.get-successors node)))
    (if (jl-is-empty successors)
        1
        (progn
          (jdo (child successors)
            (incf leafs (radial-util.get-number-of-leaves child)))
          leafs))))

(defjstatic radial-util.create-polar-comparator (radial-offset node-offset-y)
  (lambda (node1 node2)
    (let* ((position1 (get-property node1 core-options.position))
           (x-pos1 (.x position1))
           (y-pos1 (+ (.y position1) node-offset-y))
           (arc1 (math.atan2 y-pos1 x-pos1)))
      (when (< arc1 0)
        (setf arc1 (+ arc1 radial-util.two-pi)))
      (setf arc1 (+ arc1 radial-offset))
      (when (> arc1 radial-util.two-pi)
        (setf arc1 (- arc1 radial-util.two-pi)))
      (let* ((position2 (get-property node2 core-options.position))
             (x-pos2 (.x position2))
             (y-pos2 (+ (.y position2) node-offset-y))
             (arc2 (math.atan2 y-pos2 x-pos2)))
        (when (< arc2 0)
          (setf arc2 (+ arc2 radial-util.two-pi)))
        (setf arc2 (+ arc2 radial-offset))
        (when (> arc2 radial-util.two-pi)
          (setf arc2 (- arc2 radial-util.two-pi)))
        (%rad-fuzzy-compare arc1 arc2 radial-util.epsilon)))))

(defjstatic radial-util.find-largest-node-in-graph (graph)
  (let ((largest-child-size 0d0))
    (jdo (child (get-children graph))
      (let* ((width (get-width child))
             (height (get-height child))
             (diameter (math.sqrt (+ (* width width) (* height height)))))
        (setf largest-child-size (math.max diameter largest-child-size))
        (let ((largest-child (radial-util.find-largest-node-in-graph child)))
          (setf largest-child-size (math.max largest-child largest-child-size)))))
    largest-child-size))

(defjstatic radial-util.get-next-level-nodes (nodes)
  (let ((successors (new-jlist)))
    (jdo (node nodes)
      (jl-add-all successors (radial-util.get-successors node)))
    successors))

(defjstatic radial-util.get-next-level-node-set (nodes)
  (let ((successors (new-jset)))
    (jdo (node nodes)
      (jdo (s (radial-util.get-successor-set node))
        (js-add successors s)))
    successors))

(defjstatic radial-util.get-last-level-nodes (nodes)
  (let ((parents (new-jlist)))
    (jdo (node nodes)
      (jl-add parents (radial-util.get-tree-parent node)))
    parents))

(defjstatic radial-util.center-nodes-on-radi (node x-pos y-pos)
  (let ((x-position (- x-pos (/ (get-width node) 2)))
        (y-position (- y-pos (/ (get-height node) 2))))
    (set-x node x-position)
    (set-y node y-position)))

(defjstatic radial-util.shift-closest-edge-to-radi (node x-pos y-pos)
  ;; center root
  (if (and (%rad-fuzzy-equals x-pos 0 radial-util.epsilon)
           (%rad-fuzzy-equals y-pos 0 radial-util.epsilon))
      (progn
        (set-x node (- x-pos (/ (get-width node) 2)))
        (set-y node (- y-pos (/ (get-height node) 2))))
      (if (< x-pos 0)
          (if (< y-pos 0)
              ;; lower left rectangle
              (progn
                (set-x node (- x-pos (get-width node)))
                (set-y node y-pos))
              ;; upper left rectangle
              (progn
                (set-x node (- x-pos (get-width node)))
                (set-y node (+ y-pos (get-height node)))))
          (if (< y-pos 0)
              ;; lower right rectangle
              (progn
                (set-x node x-pos)
                (set-y node y-pos))
              ;; upper right rectangle
              (progn
                (set-x node x-pos)
                (set-y node (+ y-pos (get-height node))))))))
  nil)

(defjstatic radial-util.get-tree-parent (node)
  (let ((incoming (elk-graph-util.all-incoming-edges node)))
    (if (not (jl-is-empty incoming))
        (let ((edge-from-parent (jl-get incoming 0)))
          (elk-graph-util.connectable-shape-to-node
           (j-get (get-sources edge-from-parent) 0)))
        nil)))
