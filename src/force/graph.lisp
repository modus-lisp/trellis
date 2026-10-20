;;;; org.eclipse.elk.alg.force.graph

(in-package #:elk)

;;; ---------------------------------------------------------------- FParticle

(defjmethod get-position ((this f-particle)) (.position this))
(defjmethod get-size ((this f-particle)) (.size this))
(defjmethod get-radius ((this f-particle)) (/ (j-length (.size this)) 2))
(defjmethod get-displacement ((this f-particle)) (.displacement this))

;;; ---------------------------------------------------------------- FNode

(defjctor f-node (label) (setf (.label this) label))
(defjctor f-node (label the-parent)
  (setf (.label this) label
        (.parent this) the-parent))

(defjmethod to-string ((this f-node))
  (if (or (null (.label this)) (zerop (length (.label this))))
      (jstr "n_" (.id this))
      (jstr "n_" (.label this))))

(defjmethod is-compound ((this f-node))
  (and (.children this) (> (length (.children this)) 0)))
(defjmethod get-label ((this f-node)) (.label this))
(defjmethod get-parent ((this f-node)) (.parent this))
(defjmethod get-children ((this f-node))
  (or (.children this) (setf (.children this) (new-jlist))))
(defjmethod get-depth ((this f-node))
  (let ((depth 0) (node (.parent this)))
    (loop while node do (setf node (get-parent node)) (incf depth))
    depth))

;;; ---------------------------------------------------------------- FEdge

(defjmethod to-string ((this f-edge))
  (if (and (.source this) (.target this))
      (jstr (to-string (.source this)) "->" (to-string (.target this)))
      (jstr "e_" (sxhash this))))

(defjmethod get-source ((this f-edge)) (.source this))
(defjmethod get-target ((this f-edge)) (.target this))
(defjmethod get-bendpoints ((this f-edge)) (.bendpoints this))
(defjmethod get-labels ((this f-edge)) (.labels this))

(defjmethod get-source-point ((this f-edge))
  (let* ((source (.source this))
         (v (sub (clone (get-position (.target this))) (get-position source))))
    (elk-math.clip-vector v (.x (get-size source)) (.y (get-size source)))
    (add v (get-position source))))

(defjmethod get-target-point ((this f-edge))
  (let* ((target (.target this))
         (v (sub (clone (get-position (.source this))) (get-position target))))
    (elk-math.clip-vector v (.x (get-size target)) (.y (get-size target)))
    (add v (get-position target))))

(defjmethod set-source ((this f-edge) the-source) (setf (.source this) the-source))
(defjmethod set-target ((this f-edge) the-target) (setf (.target this) the-target))

(defjmethod to-vector-chain ((this f-edge))
  (let ((vector-chain (new-k-vector-chain)))
    (add vector-chain (get-source-point this))
    (jdo (bend-point (.bendpoints this))
      (add vector-chain (get-position bend-point)))
    (add vector-chain (get-target-point this))
    vector-chain))

(defjmethod distribute-bendpoints ((this f-edge))
  (let ((count (length (.bendpoints this))))
    (when (> count 0)
      (let* ((source-pos (get-position (.source this)))
             (target-pos (get-position (.target this)))
             (incr (scale (sub (clone target-pos) source-pos) (/ 1 (jd (+ count 1)))))
             (pos (clone source-pos)))
        (jdo (bend-point (.bendpoints this))
          (setf (.x (get-position bend-point)) (+ (.x pos) (.x incr))
                (.y (get-position bend-point)) (+ (.y pos) (.y incr)))
          (add pos incr))))))

;;; ---------------------------------------------------------------- FBendpoint

(defjctor f-bendpoint (edge)
  (setf (.edge this) edge)
  (jl-add (get-bendpoints edge) this))

(defjmethod to-string ((this f-bendpoint))
  (let ((edge (.edge this)))
    (if edge
        (let ((index (or (position this (get-bendpoints edge)) -1)))
          (if (>= index 0)
              (jstr "b" index "[" (to-string edge) "]")
              (jstr "b[" (to-string edge) "]")))
        (jstr "b_" (sxhash this)))))

(defjmethod get-edge ((this f-bendpoint)) (.edge this))

;;; ---------------------------------------------------------------- FLabel

(defjctor f-label (fedge text)
  (setf (.edge this) fedge
        (.text this) text)
  (jl-add (get-labels fedge) this))

(defjmethod to-string ((this f-label))
  (if (or (null (.text this)) (zerop (length (.text this))))
      (jstr "l[" (to-string (.edge this)) "]")
      (jstr "l_" (.text this))))

(defjmethod get-text ((this f-label)) (.text this))
(defjmethod get-edge ((this f-label)) (.edge this))

(defjmethod refresh-position ((this f-label))
  (let* ((place-inline (get-property this force-options.edge-labels-inline))
         (edge (.edge this))
         (src (get-position (get-source edge)))
         (tgt (get-position (get-target edge))))
    (if place-inline
        (let* ((src-to-tgt (scale (sub (clone tgt) src) 0.5))
               (to-label-center (scale (clone (get-size this)) 0.5))
               (new-label-position (sub (add (clone src) src-to-tgt) to-label-center)))
          (j-set (get-position this) new-label-position))
        ;; TODO (ELK) The spacing has to be retrieved from the surrounding node/graph
        (let ((spacing (jd (get-property edge force-options.spacing-edge-label)))
              (pos (get-position this)))
          (if (>= (.x src) (.x tgt))
              (if (>= (.y src) (.y tgt))
                  ;; CASE1, src top left, tgt bottom right
                  (setf (.x pos) (+ (.x tgt) (/ (- (.x src) (.x tgt)) 2) spacing)
                        (.y pos) (- (+ (.y tgt) (/ (- (.y src) (.y tgt)) 2)) spacing (.y (get-size this))))
                  ;; CASE2, src bottom left, tgt top right
                  (setf (.x pos) (+ (.x tgt) (/ (- (.x src) (.x tgt)) 2) spacing)
                        (.y pos) (+ (.y src) (/ (- (.y tgt) (.y src)) 2) spacing)))
              (if (>= (.y src) (.y tgt))
                  ;; CASE2, src top right, tgt bottom left
                  (setf (.x pos) (+ (.x src) (/ (- (.x tgt) (.x src)) 2) spacing)
                        (.y pos) (+ (.y tgt) (/ (- (.y src) (.y tgt)) 2) spacing))
                  ;; CASE1, src bottom right, tgt top left
                  (setf (.x pos) (+ (.x src) (/ (- (.x tgt) (.x src)) 2) spacing)
                        (.y pos) (- (+ (.y src) (/ (- (.y tgt) (.y src)) 2)) spacing (.y (get-size this))))))))))

;;; ---------------------------------------------------------------- FGraph

(defjmethod get-edges ((this f-graph)) (.edges this))
(defjmethod get-nodes ((this f-graph)) (.nodes this))
(defjmethod get-labels ((this f-graph)) (.labels this))
(defjmethod get-bendpoints ((this f-graph)) (.bend-points this))

(defjmethod get-particles ((this f-graph))
  "Iterables.concat(nodes, labels, bendPoints), as a fresh list."
  (let ((all (new-jlist)))
    (jl-add-all all (.nodes this))
    (jl-add-all all (.labels this))
    (jl-add-all all (.bend-points this))
    all))

(defjmethod get-connection ((this f-graph) particle1 particle2)
  (cond ((and (typep particle1 'f-node) (typep particle2 'f-node))
         (let ((adj (.adjacency this))
               (i (.id particle1)) (j (.id particle2)))
           (+ (aref adj i j) (aref adj j i))))
        ((and (typep particle1 'f-bendpoint) (typep particle2 'f-bendpoint)
              (eq (get-edge particle1) (get-edge particle2)))
         (get-property (get-edge particle2) force-options.priority))
        (t 0)))

(defjmethod calc-adjacency ((this f-graph))
  (let* ((n (length (.nodes this)))
         (adj (make-array (list n n) :initial-element 0)))
    (setf (.adjacency this) adj)
    (jdo (edge (.edges this))
      (incf (aref adj (.id (get-source edge)) (.id (get-target edge)))
            (get-property edge force-options.priority)))))
