;;;; tree.lisp -- trees: mrtree (tidy drawing), radial, vertiflex.
;;;;
;;;; mrtree is a tidy tree drawing in the manner of Reingold and Tilford:
;;;; subtrees are drawn bottom-up, each placed next to its left siblings as
;;;; close as their contours allow, and a parent centred over its children.
;;;; Graphs that are not trees are drawn by a spanning forest (breadth-first,
;;;; along edge direction first), the other edges straight.
;;;;
;;;; radial follows Eades' radial drawing: the root at the centre, each level
;;;; on a circle, each subtree in a wedge as wide as its share of leaves.  The
;;;; radii are chosen so no two nodes can overlap.

(in-package #:trellis)

;;; ---- spanning forests ---------------------------------------------------------

(defun spanning-forest (nodes edges &key root)
  "Roots, and a table node -> children, of a breadth-first spanning forest of
   NODES.  Edges are followed forward first; nodes they do not reach start
   further trees or are reached backwards."
  (let ((out (make-hash-table :test 'eq)) (in (make-hash-table :test 'eq))
        (children (make-hash-table :test 'eq)) (seen (make-hash-table :test 'eq))
        (roots '()))
    (dolist (e edges)
      (let ((a (endpoint-node (first (edge-sources e)))) (b (endpoint-node (first (edge-targets e)))))
        (unless (eq a b)
          (push b (gethash a out)) (push a (gethash b in)))))
    (dolist (n nodes)
      (setf (gethash n out) (reverse (gethash n out)) (gethash n in) (reverse (gethash n in))))
    (labels ((grow (r)
               (push r roots)
               (setf (gethash r seen) t)
               (let ((queue (list r)))
                 (loop while queue
                       do (let ((v (pop queue)) (kids '()))
                            (dolist (w (append (gethash v out) (gethash v in)))
                              (unless (gethash w seen)
                                (setf (gethash w seen) t)
                                (push w kids)
                                (setf queue (nconc queue (list w)))))
                            (setf (gethash v children) (nreverse kids)))))))
      (when root (grow root))
      (dolist (n nodes) (unless (or (gethash n seen) (gethash n in)) (grow n)))
      (dolist (n nodes) (unless (gethash n seen) (grow n))))
    (values (nreverse roots) children)))

;;; ---- tidy trees ----------------------------------------------------------------------

(defun tree-direction (node default)
  (opt-enum node :direction default '(:down :up :right :left)))

(defun breadth-depth (n dir)
  "N's extent across the tree (breadth) and along it (depth), with its offset."
  (let ((e (node-extent n)))
    (if (member dir '(:down :up))
        (values (rect-w e) (rect-h e) (rect-x e) (rect-y e))
        (values (rect-h e) (rect-w e) (rect-y e) (rect-x e)))))

(defun tidy-tree (roots children dir spacing level-spacing)
  "Place the trees under ROOTS; returns nothing, sets node positions."
  (let ((offset (make-hash-table :test 'eq)) (depth-of (make-hash-table :test 'eq))
        (level-size (make-hash-table)))
    (labels ((depths (v d)
               (setf (gethash v depth-of) d)
               (setf (gethash d level-size)
                     (max (gethash d level-size 0d0) (nth-value 1 (breadth-depth v dir))))
               (dolist (c (gethash v children)) (depths c (1+ d))))
             (contour (v)
               ;; returns a list of (left . right) per depth below V, relative to V's centre
               (let* ((bw (breadth-depth v dir))
                      (kids (gethash v children))
                      (acc '()) (offsets '()))
                 (dolist (c kids)
                   (let* ((cc (contour c))
                          (o (if (null acc) 0d0
                                 (loop for (al . ar) in acc for (cl . nil) in cc
                                       maximize (+ (- ar cl) spacing)))))
                     (push o offsets)
                     ;; merge
                     (let ((shifted (mapcar (lambda (lr) (cons (+ (car lr) o) (+ (cdr lr) o))) cc)))
                       (setf acc (merge-contours acc shifted)))))
                 (setf offsets (nreverse offsets))
                 (let ((mid (if offsets (/ (+ (first offsets) (car (last offsets))) 2) 0d0)))
                   (loop for c in kids for o in offsets do (setf (gethash c offset) (- o mid)))
                   (cons (cons (- (/ bw 2)) (/ bw 2))
                         (mapcar (lambda (lr) (cons (- (car lr) mid) (- (cdr lr) mid))) acc))))))
      (dolist (r roots) (depths r 0))
      (let ((level-pos (make-hash-table)) (max-depth (reduce #'max (hash-keys level-size) :initial-value 0)))
        (loop with y = 0d0 for d from 0 to max-depth
              do (setf (gethash d level-pos) y)
                 (incf y (+ (gethash d level-size 0d0) level-spacing)))
        (let ((x 0d0))
          (dolist (r roots)
            (let* ((c (contour r))
                   (left (reduce #'min c :key #'car)) (right (reduce #'max c :key #'cdr)))
              (setf (gethash r offset) (- x left))
              (incf x (+ (- right left) spacing)))))
        (labels ((place (v center)
                   (multiple-value-bind (bw bd bo dofs) (breadth-depth v dir)
                     (declare (ignore bd))
                     (let ((b (- center (/ bw 2) bo))
                           (dd (- (gethash (gethash v depth-of) level-pos) dofs)))
                       (ecase dir
                         (:down (setf (node-x v) b (node-y v) dd))
                         (:up (setf (node-x v) b (node-y v) (- dd)))
                         (:right (setf (node-y v) b (node-x v) dd))
                         (:left (setf (node-y v) b (node-x v) (- dd))))))
                   (dolist (c (gethash v children))
                     (place c (+ center (gethash c offset))))))
          (dolist (r roots) (place r (gethash r offset)))
          ;; mirrored directions grow toward negative coordinates: flip each node box
          (when (member dir '(:up :left))
            (dolist (r roots)
              (labels ((flip (v)
                         (multiple-value-bind (bw bd) (breadth-depth v dir)
                           (declare (ignore bw))
                           (if (eq dir :up) (decf (node-y v) bd) (decf (node-x v) bd)))
                         (mapc #'flip (gethash v children))))
                (flip r)))))))))

(defun merge-contours (a b)
  "Per depth, the outer extent of contours A and B."
  (loop for i from 0 below (max (length a) (length b))
        for x = (nth i a) for y = (nth i b)
        collect (cond ((null x) y) ((null y) x)
                      (t (cons (min (car x) (car y)) (max (cdr x) (cdr y)))))))

(defun tree-root-option (nodes)
  (find-if (lambda (n) (opt-bool n :radial-root nil)) nodes))

(defalgorithm :mrtree (node)
  (prepare-children node)
  (let* ((kids (layout-children node))
         (edges (sibling-edges node))
         (dir (tree-direction node :down))
         (spacing (opt-num node :spacing-node-node 20d0 :min 0)))
    (multiple-value-bind (roots children) (spanning-forest kids edges)
      (tidy-tree roots children dir spacing spacing))
    (remove-overlaps kids spacing :rounds 0)
    (finish-simple node :default-padding (pad 20d0 20d0 20d0 20d0))))

(defalgorithm :vertiflex (node)
  (prepare-children node)
  (let* ((kids (layout-children node))
         (edges (sibling-edges node))
         (spacing (opt-num node :spacing-node-node 20d0 :min 0))
         (layer-spacing (opt-num node :layer-distance 50d0 :min 0)))
    (multiple-value-bind (roots children) (spanning-forest kids edges)
      (tidy-tree roots children :down spacing layer-spacing)
      ;; vertical constraints: a node's top where asked, never above its parent
      (labels ((settle (v parent-bottom)
                 (let ((c (option v :vertical-constraint)))
                   (let ((y (max (if (and (realp c) (finite-p c)) (d c) (node-y v))
                                 (if parent-bottom (+ parent-bottom spacing) most-negative-double-float))))
                     (setf (node-y v) y)
                     (dolist (k (gethash v children))
                       (settle k (+ y (max 0d0 (node-height v)))))))))
        (dolist (r roots) (settle r nil))))
    (remove-overlaps kids spacing :rounds 0)
    (finish-simple node :default-padding (pad 20d0 20d0 20d0 20d0))))

;;; ---- radial -------------------------------------------------------------------------

(defun radial-root (nodes edges)
  "The node with no incoming edges reaching most, else the best connected."
  (let ((in (make-hash-table :test 'eq)) (deg (make-hash-table :test 'eq)))
    (dolist (e edges)
      (let ((a (endpoint-node (first (edge-sources e)))) (b (endpoint-node (first (edge-targets e)))))
        (unless (eq a b)
          (setf (gethash b in) t)
          (incf (gethash a deg 0)) (incf (gethash b deg 0)))))
    (or (tree-root-option nodes)
        (let ((sources (remove-if (lambda (n) (gethash n in)) nodes)))
          (if sources
              (first (sort (copy-list sources) #'> :key (lambda (n) (gethash n deg 0))))
              (first (sort (copy-list nodes) #'> :key (lambda (n) (gethash n deg 0)))))))))

(defun node-diagonal (n)
  (let ((e (node-extent n))) (sqrt (+ (expt (rect-w e) 2) (expt (rect-h e) 2)))))

(defun radial-component (nodes edges spacing)
  (let ((root (radial-root nodes edges)))
    (multiple-value-bind (roots children) (spanning-forest nodes edges :root root)
      (declare (ignore roots))
      (let ((leaves (make-hash-table :test 'eq)) (depth (make-hash-table :test 'eq))
            (wedge (make-hash-table :test 'eq)) (angle (make-hash-table :test 'eq))
            (levels (make-hash-table)))
        (labels ((count-leaves (v d)
                   (setf (gethash v depth) d)
                   (push v (gethash d levels))
                   (setf (gethash v leaves)
                         (let ((ks (gethash v children)))
                           (if ks (reduce #'+ (mapcar (lambda (k) (count-leaves k (1+ d))) ks)) 1))))
                 (assign (v start width)
                   (setf (gethash v wedge) width (gethash v angle) (+ start (/ width 2)))
                   (let ((s start) (total (gethash v leaves)))
                     (dolist (k (gethash v children))
                       (let ((w (* width (/ (gethash k leaves) total))))
                         (assign k s w)
                         (incf s w))))))
          (count-leaves root 0)
          (assign root 0d0 (* 2 pi))
          (let ((radius (make-hash-table)) (max-d (reduce #'max (hash-keys levels))))
            (setf (gethash 0 radius) 0d0)
            (loop for d from 1 to max-d
                  for prev-max = (reduce #'max (gethash (1- d) levels) :key #'node-diagonal)
                  for this = (gethash d levels)
                  for this-max = (reduce #'max this :key #'node-diagonal)
                  do (setf (gethash d radius)
                           (max (+ (gethash (1- d) radius) (/ (+ prev-max this-max) 2) spacing)
                                (reduce #'max this
                                        :key (lambda (n)
                                               (/ (+ (node-diagonal n) spacing)
                                                  (* 2 (sin (/ (min pi (max 1d-9 (gethash n wedge))) 2)))))))))
            (dolist (v nodes)
              (let* ((r (gethash (gethash v depth) radius)) (a (gethash v angle))
                     (e (node-extent v))
                     (cx (* r (cos a))) (cy (* r (sin a))))
                (setf (node-x v) (- cx (/ (rect-w e) 2) (rect-x e))
                      (node-y v) (- cy (/ (rect-h e) 2) (rect-y e)))))))))))

(defalgorithm :radial (node)
  (prepare-children node)
  (let ((spacing (opt-num node :spacing-node-node 20d0 :min 0)))
    (layout-by-components node spacing 1.6d0
                          (lambda (nodes edges) (radial-component nodes edges spacing)))
    (remove-overlaps (layout-children node) spacing :rounds 0)
    (finish-simple node :default-padding (pad 12d0 12d0 12d0 12d0))))
