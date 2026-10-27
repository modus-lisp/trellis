;;;; layered.lisp -- the layered (Sugiyama) algorithm, put together.
;;;;
;;;;   1 cycle breaking      Eades, Lin, Smyth                      cycles.lisp
;;;;   2 layering            network simplex, Gansner et al.        layering.lisp
;;;;     dummy nodes         labels and long edges                  dummies.lisp
;;;;   3 crossing reduction  layer sweep, ports after Schulze et al.,
;;;;                         counting after Barth, Jünger, Mutzel   crossings.lisp
;;;;   4 node placement      Brandes-Köpf with the 2020 erratum     placement.lisp
;;;;   5 edge routing        orthogonal after Sander / Weber        routing.lisp
;;;;
;;;; Connected components are laid out one by one and packed in rows.

(in-package #:trellis)

(defun layered-spacing (node)
  "The spacings the layered algorithm uses, from NODE's options.  A base value
   scales the defaults; negative spacings count as zero."
  (let* ((base (option node :spacing-base-value))
         (s (if (and (realp base) (finite-p base) (> base 0)) (/ (d base) 20d0) 1d0)))
    (flet ((sp (key default) (max 0d0 (opt-num node key (* s default)))))
      (list :node-node (sp :spacing-node-node 20d0)
            :edge-node (sp :spacing-edge-node 10d0)
            :edge-edge (sp :spacing-edge-edge 10d0)
            :node-node-between (sp :spacing-node-node-between-layers 20d0)
            :edge-node-between (sp :spacing-edge-node-between-layers 10d0)
            :edge-edge-between (sp :spacing-edge-edge-between-layers 10d0)
            :component (sp :spacing-component-component 20d0)
            :edge-label (min 200d0 (sp :spacing-edge-label 2d0))
            :label-label (sp :spacing-label-label 0d0)
            :self-loop (max 1d0 (sp :spacing-node-self-loop 10d0))
            :routing (opt-enum node :edge-routing :orthogonal '(:orthogonal :polyline :splines))))))

(defalgorithm :layered (node)
  (let ((n (max (opt-int node :candidates 1 :min 1 :max 64)
                (opt-int node :trellis-candidates 1 :min 1 :max 64))))
    (if (> n 1)
        (best-of-candidates node n)
        (layered-layout node))))

;;; ---- candidates ----------------------------------------------------------------
;;;
;;; The candidates option runs the layout with that many consecutive seeds and
;;; keeps the lowest-energy result: the fewest crossings, then the shortest
;;; total edge length, then the smallest area, compared in that order.

(defun frame-score (node)
  (let ((m (layout-metrics node :edges (frame-edges node))))
    (list (getf m :crossings) (getf m :edge-length) (* (node-width node) (node-height node)))))

(defun score< (a b)
  (loop for x in a for y in b
        do (cond ((< x (- y 1d-6)) (return t))
                 ((> x (+ y 1d-6)) (return nil)))
        finally (return nil)))

(defun snapshot-frame (node)
  (let ((shapes '()))
    (flet ((keep (s) (push (list s (shape-x s) (shape-y s) (shape-width s) (shape-height s)) shapes)))
      (keep node)
      (dolist (p (node-ports node)) (keep p))
      (dolist (c (node-children node))
        (keep c)
        (dolist (l (node-labels c)) (keep l))
        (dolist (p (node-ports c)) (keep p) (dolist (l (port-labels p)) (keep l))))
      (dolist (e (frame-edges node)) (dolist (l (edge-labels e)) (keep l))))
    (list shapes
          (mapcar (lambda (e) (list e (edge-sections e) (gethash e *edge-frames*))) (frame-edges node))
          (copy-list (gethash node *ports-placed*))
          (node-ties node))))

(defun restore-frame (node snap)
  (destructuring-bind (shapes edges placed ties) snap
    (setf (node-ties node) ties)
    (dolist (s shapes)
      (destructuring-bind (sh x y w h) s
        (setf (shape-x sh) x (shape-y sh) y (shape-width sh) w (shape-height sh) h)))
    (dolist (e edges)
      (setf (edge-sections (first e)) (second e) (gethash (first e) *edge-frames*) (third e)))
    (setf (gethash node *ports-placed*) placed)))

(defun best-of-candidates (node n)
  (let* ((seed (opt-int node :random-seed 1))
         (start (snapshot-frame node))
         (best nil) (best-score nil))
    (unwind-protect
         (dotimes (i n)
           (restore-frame node start)
           (set-option node :random-seed (+ seed i))
           ;; each candidate starts from its own seed, not the input order
           (let ((*sweep-start* :random-first)) (layered-layout node))
           (let ((score (frame-score node)))
             (when (or (null best-score) (score< score best-score))
               (setf best-score score best (snapshot-frame node)))))
      (set-option node :random-seed seed))
    (restore-frame node best)))

(defun layered-layout (parent)
  (let* ((lg (import-layered parent))
         (padding (graph-padding parent (pad 12d0 12d0 12d0 12d0))))
    (setf (lgraph-spacing lg) (layered-spacing parent)
          (lgraph-rng lg) (make-rng (opt-int parent :random-seed 1)))
    (init-chains lg)
    (let* ((comps (split-components lg parent))
           (placed (mapcar (lambda (c) (run-layered-phases c) (export-component c parent)) comps)))
      (pack-placed placed (getf (lgraph-spacing lg) :component)
                   (let ((a (opt-num parent :aspect-ratio 1.6d0))) (if (> a 0) a 1.6d0)))
      (let ((w 0d0) (h 0d0))
        (dolist (p placed)
          (setf w (max w (placed-width p)) h (max h (placed-height p))))
        (dolist (p placed) (shift-placed p (pad-left padding) (pad-top padding)))
        (resize-graph parent (+ w (pad-left padding) (pad-right padding))
                      (+ h (pad-top padding) (pad-bottom padding)))
        (dolist (p placed) (attach-externals parent p))
        (setf (node-ties parent) (loop for p in placed append (placed-ties p)))))))

(defun split-components (lg parent)
  "LG's connected components as lgraphs, or LG itself when they are not to be
   separated."
  (if (or (not (opt-bool parent :separate-connected-components t))
          (some (lambda (n) (eq (lnode-kind n) :external)) (lgraph-nodes lg))
          (null (cdr (lgraph-nodes lg))))
      (list lg)
      (let* ((loops-of (make-hash-table :test 'eq))
             (comps (undirected-components
                     (lgraph-nodes lg)
                     (lambda (v) (append (mapcar #'tgt-node (node-outgoing v))
                                         (mapcar #'src-node (node-incoming v)))))))
        (dolist (sl (lgraph-self-loops lg)) (push sl (gethash (lport-node (second sl)) loops-of)))
        (if (null (cdr comps))
            (list lg)
            (loop for nodes in comps
                  collect (let ((c (make-lgraph)))
                            (setf (lgraph-options-node c) (lgraph-options-node lg)
                                  (lgraph-direction c) (lgraph-direction lg)
                                  (lgraph-spacing c) (lgraph-spacing lg)
                                  (lgraph-rng c) (lgraph-rng lg)
                                  (lgraph-chains c) (lgraph-chains lg)
                                  (lgraph-nodes c) nodes
                                  (lgraph-edges c) (loop for n in nodes append (node-outgoing n))
                                  (lgraph-self-loops c) (loop for n in nodes append (gethash n loops-of)))
                            (loop for n in nodes for i from 0 do (setf (lnode-index n) i))
                            (setf (lgraph-next-index c) (length nodes))
                            c))))))

(defun run-layered-phases (lg)
  (break-cycles lg)
  (assign-free-port-sides lg)
  (insert-label-dummies lg)
  (assign-layers lg)
  (insert-long-edge-dummies lg)
  (init-port-orders lg)
  (insert-inverted-port-dummies lg)
  (insert-north-south-dummies lg)
  (minimize-crossings lg)
  (order-in-layer-ports lg)
  (place-lports lg)
  (reserve-self-loops lg)
  (place-nodes lg)
  (route-layers lg)
  lg)

(defun reserve-self-loops (lg)
  "Give each self loop a distance from its node and widen the node's margins
   on the sides the loop runs along."
  (let ((count (make-hash-table :test 'eq)) (s (getf (lgraph-spacing lg) :self-loop)))
    (dolist (sl (lgraph-self-loops lg))
      (destructuring-bind (e sp tp) sl
        (declare (ignore e))
        (let* ((n (lport-node sp)) (k (incf (gethash n count 0))) (d (* k s)))
          (push (cons sp d) (lnode-data n :loops))
          (let ((m (lnode-margin n)) (sides '()))
            (loop with side = (lport-side sp)
                  do (push side sides)
                     (when (eq side (lport-side tp)) (return))
                     (setf side (ecase side (:north :east) (:east :south) (:south :west) (:west :north))))
            (dolist (side sides)
              (ecase side
                (:north (setf (pad-top m) (max (pad-top m) (+ d 2))))
                (:south (setf (pad-bottom m) (max (pad-bottom m) (+ d 2))))
                (:east (setf (pad-right m) (max (pad-right m) (+ d 2))))
                (:west (setf (pad-left m) (max (pad-left m) (+ d 2))))))))))))

(defun pack-placed (placed spacing aspect)
  "Arrange components in rows, largest first."
  (when (cdr placed)
    (let* ((sorted (sort (copy-list placed) #'> :key (lambda (p) (* (placed-width p) (placed-height p)))))
           (area (reduce #'+ sorted :key (lambda (p) (* (+ (placed-width p) spacing) (+ (placed-height p) spacing)))))
           (widest (reduce #'max sorted :key #'placed-width))
           (row-width (max widest (sqrt (* area aspect))))
           (x 0d0) (y 0d0) (row-h 0d0) (total-w 0d0) (total-h 0d0))
      (dolist (p sorted)
        (when (and (> x 0) (> (+ x (placed-width p)) row-width))
          (setf x 0d0 y (+ y row-h spacing) row-h 0d0))
        (shift-placed p x y)
        (setf total-w (max total-w (+ x (placed-width p))))
        (incf x (+ (placed-width p) spacing))
        (setf row-h (max row-h (placed-height p))
              total-h (max total-h (+ y (placed-height p)))))
      ;; every component now reports the whole packing's size
      (dolist (p placed) (setf (placed-width p) total-w (placed-height p) total-h)))))

(defun attach-externals (parent placed)
  "Put PARENT's ports used by its children's edges on its border, level with
   where the layout put them, and run those edges out to the border."
  (dolist (x (placed-externals placed))
    (destructuring-bind (endpoint at side) x
      (let ((w (node-width parent)) (h (node-height parent)))
        (when (port-p endpoint)
          (put-port-on-side endpoint side (if (member side '(:north :south)) (px at) (py at)))
          (when *ports-placed* (setf (gethash parent *ports-placed*) t)))
        (let ((border (ecase side
                        (:west (pt 0 (py at))) (:east (pt w (py at)))
                        (:north (pt (px at) 0)) (:south (pt (px at) h)))))
          (dolist (e (placed-edges placed))
            (let ((s (first (edge-sources e))) (tt (first (edge-targets e))))
              (when (or (eq s endpoint) (eq tt endpoint))
                (let* ((sec (first (edge-sections e))) (curve (section-curve sec)))
                  ;; a curve takes the lead to the border as a straight Bezier piece
                  (cond ((and (eq s endpoint) (approx= (px (section-start sec)) (px at) 1d-3)
                              (approx= (py (section-start sec)) (py at) 1d-3))
                         (setf (section-bends sec)
                               (append (if curve (list border (section-start sec)) '())
                                       (list (section-start sec)) (section-bends sec)))
                         (setf (section-start sec) border))
                        ((and (eq tt endpoint) (approx= (px (section-end sec)) (px at) 1d-3)
                              (approx= (py (section-end sec)) (py at) 1d-3))
                         (setf (section-bends sec)
                               (append (section-bends sec) (list (section-end sec))
                                       (if curve (list (section-end sec) border) '())))
                         (setf (section-end sec) border)))
                  (unless curve
                    (let ((pts (remove-collinear (section-points sec))))
                      (setf (section-start sec) (first pts) (section-end sec) (car (last pts))
                            (section-bends sec) (butlast (rest pts))))))))))))))
