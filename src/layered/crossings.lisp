;;;; crossings.lisp -- phase 3: order the nodes in each layer.
;;;;
;;;; The layer sweep with the barycenter heuristic, made aware of ports as in
;;;; Schulze, Spönemann and von Hanxleden (2014), Section 3: a node's
;;;; barycenter averages the ranks of the ports its edges attach to in the
;;;; fixed layer, and a port's rank is its node's index plus its place among
;;;; that node's ports (node-relative ranking).  Ports whose order is still free
;;;; share a rank per side.  Crossings are counted per pair of layers with the
;;;; accumulator tree of Barth, Jünger and Mutzel (2004), over port positions.
;;;;
;;;; Port order convention: an lnode's ports are kept clockwise -- north left
;;;; to right, east top to bottom, south right to left, west bottom to top.
;;;; That is the order outgoing edges leave in; incoming edges arrive in the
;;;; counter-clockwise order starting at the right end of the north side.

(in-package #:trellis)

(defun side-index (side) (ecase side (:north 0) (:east 1) (:south 2) (:west 3)))

(defun clockwise-key (p)
  "Sort key putting ports in clockwise order."
  (let ((x (+ (lport-x p) (/ (lport-width p) 2))) (y (+ (lport-y p) (/ (lport-height p) 2))))
    (ecase (lport-side p)
      (:north (cons 0 x)) (:east (cons 1 y)) (:south (cons 2 (- x))) (:west (cons 3 (- y))))))

(defun key< (a b) (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b)))))

(defun fixed-port-order-p (n) (member (lnode-port-constraints n) '(:fixed-order :fixed-pos)))

(defun init-port-orders (lg)
  (dolist (n (lgraph-nodes lg))
    (setf (lnode-ports n)
          (if (fixed-port-order-p n)
              (stable-sort (copy-list (lnode-ports n)) #'key< :key #'clockwise-key)
              (stable-sort (copy-list (lnode-ports n)) #'< :key (lambda (p) (side-index (lport-side p))))))))

(defun ports-out-order (n) (lnode-ports n))

(defun ports-in-order (n)
  "N's ports counter-clockwise from the right end of the north side."
  (let ((north (remove :north (lnode-ports n) :key #'lport-side :test-not #'eq))
        (rest (remove :north (lnode-ports n) :key #'lport-side)))
    (append (reverse north) (reverse rest))))

(defun set-ports-from-in-order (n in-order)
  (let ((by-side (lambda (s) (reverse (remove s in-order :key #'lport-side :test-not #'eq)))))
    (setf (lnode-ports n) (append (funcall by-side :north) (funcall by-side :east)
                                  (funcall by-side :south) (funcall by-side :west)))))

;;; ---- ranks -------------------------------------------------------------------

(defun compute-port-ranks (layer forward &key exact)
  "Rank every port of LAYER's nodes that has edges toward the next layer in the
   sweep (outgoing when FORWARD, incoming otherwise).  EXACT ranks every port by
   its current place, free or not."
  (loop for n across layer for i from 0
        do (let* ((seq (if forward (ports-out-order n) (ports-in-order n)))
                  (rel (remove-if-not (lambda (p) (cross-layer (if forward (lport-outgoing p) (lport-incoming p)))) seq)))
             (when rel
               (if (or exact (fixed-port-order-p n))
                   (let ((range (length rel)))
                     (loop for p in rel for o from 1
                           do (setf (lport-rank p) (+ i (/ (d o) (1+ range))))))
                   (let* ((sides (remove-duplicates (mapcar #'lport-side rel) :from-end t))
                          (range (length sides)))
                     (dolist (p rel)
                       (setf (lport-rank p)
                             (+ i (/ (d (1+ (position (lport-side p) sides))) (1+ range)))))))))))

(defun node-barycenter (n forward)
  "Mean rank of the ports N's edges reach in the fixed layer, or NIL."
  (let ((sum 0d0) (count 0))
    (dolist (p (lnode-ports n))
      (dolist (e (cross-layer (if forward (lport-incoming p) (lport-outgoing p))))
        (incf sum (lport-rank (if forward (ledge-source e) (ledge-target e))))
        (incf count)))
    (values (and (plusp count) (/ sum count)) sum count)))

(defun in-layer-partners (n)
  (loop for e in (append (node-incoming n) (node-outgoing n))
        when (in-layer-p e) collect (if (eq (src-node e) n) (tgt-node e) (src-node e))))

(defun order-layer (layer forward rng)
  "Sort LAYER (a vector) by barycenters toward the fixed layer.  Nodes joined
   by in-layer edges count each other's barycenters (Schulze et al., eq. 6),
   and layout units move as one."
  (let* ((n (length layer))
         (bary (make-array n))
         (sums (make-array n)) (counts (make-array n)))
    (loop for v across layer for i from 0
          do (multiple-value-bind (b sum count) (node-barycenter v forward)
               (setf (aref bary i) b (aref sums i) sum (aref counts i) count)))
    ;; eq. 6: a node's in-layer partners stand in for edges it lacks
    (loop for v across layer for i from 0
          do (dolist (w (in-layer-partners v))
               (let ((b (aref bary (lnode-order w))))
                 (when b (incf (aref sums i) b) (incf (aref counts i))))))
    (loop for i below n
          when (plusp (aref counts i)) do (setf (aref bary i) (/ (aref sums i) (aref counts i))))
    ;; a layout unit takes its members' combined barycenter
    (loop for v across layer
          for unit = (unit-members v)
          when unit
            do (let ((s 0d0) (c 0))
                 (dolist (m unit) (incf s (aref sums (lnode-order m))) (incf c (aref counts (lnode-order m))))
                 (dolist (m unit) (setf (aref bary (lnode-order m)) (and (plusp c) (/ s c))))))
    ;; nodes without neighbours there take the mean of the known ones around them
    (loop for i below n
          unless (aref bary i)
            do (let ((prev (loop for j from (1- i) downto 0 when (aref bary j) return (aref bary j)))
                     (next (loop for j from (1+ i) below n when (aref bary j) return (aref bary j))))
                 (setf (aref bary i)
                       (cond ((and prev next) (/ (+ prev next) 2))
                             (prev prev) (next next)
                             (t (d i))))))
    (let ((pairs (loop for v across layer for i from 0
                       collect (cons (+ (aref bary i) (if rng (* 1d-7 (rng-float rng)) 0d0)) v))))
      (setf pairs (stable-sort pairs #'< :key #'car))
      (loop for (nil . v) in pairs for i from 0
            do (setf (aref layer i) v (lnode-order v) i))
      (regroup-units layer))
    (loop for v across layer
          unless (fixed-port-order-p v)
            do (sort-free-ports v forward))))

(defun sort-free-ports (n forward)
  "Order N's ports facing the fixed layer by the ranks they connect to,
   keeping each side's ports together."
  (let* ((in-seq (if forward (ports-in-order n) (ports-out-order n)))
         (sides (remove-duplicates (mapcar #'lport-side in-seq) :from-end t))
         (decorated
           (mapcar (lambda (p)
                     (let ((es (cross-layer (if forward (lport-incoming p) (lport-outgoing p)))))
                       (list (position (lport-side p) sides)
                             (if es
                                 (/ (reduce #'+ es :key (lambda (e) (lport-rank (if forward (ledge-source e) (ledge-target e)))))
                                    (length es))
                                 most-positive-double-float)
                             p)))
                   in-seq))
         (sorted (mapcar #'third
                         (stable-sort decorated
                                      (lambda (a b) (or (< (first a) (first b))
                                                        (and (= (first a) (first b)) (< (second a) (second b)))))))))
    (if forward
        (set-ports-from-in-order n sorted)
        (setf (lnode-ports n) sorted))))

;;; ---- counting ------------------------------------------------------------------

(defun count-crossings-between (left right)
  "Crossings between the edges from layer LEFT to layer RIGHT, from the order
   of their ports (Barth, Jünger, Mutzel)."
  (let ((rpos (make-hash-table :test 'eq)) (q 0) (pairs '()) (lpos 0))
    (loop for n across right
          do (dolist (p (ports-in-order n))
               (when (lport-incoming p) (setf (gethash p rpos) q) (incf q))))
    (loop for n across left
          do (dolist (p (ports-out-order n))
               (when (lport-outgoing p)
                 (dolist (e (lport-outgoing p))
                   (let ((r (gethash (ledge-target e) rpos)))
                     (when r (push (cons lpos r) pairs))))
                 (incf lpos))))
    (if (or (< q 2) (null pairs))
        0
        (let* ((sorted (sort pairs (lambda (a b) (or (< (car a) (car b))
                                                       (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))
               (first-index (1- (ash 1 (integer-length (1- q)))))
               (tree (make-array (+ (* 2 first-index) 2) :element-type 'fixnum :initial-element 0))
               (count 0))
          (dolist (pr sorted)
            (let ((index (+ (cdr pr) first-index)))
              (incf (aref tree index))
              (loop while (> index 0)
                    do (when (oddp index) (incf count (aref tree (1+ index))))
                       (setf index (floor (1- index) 2))
                       (incf (aref tree index)))))
          count))))

(defun count-all-crossings (lg)
  (let ((layers (lgraph-layers lg)))
    (loop for i from 0 below (1- (length layers))
          sum (count-crossings-between (aref layers i) (aref layers (1+ i))))))

;;; ---- the sweep -------------------------------------------------------------------

(defun snapshot-order (lg)
  (list (map 'vector #'copy-seq (lgraph-layers lg))
        (mapcar (lambda (n) (cons n (copy-list (lnode-ports n)))) (lgraph-nodes lg))))

(defun restore-order (lg snap)
  (destructuring-bind (layers ports) snap
    (loop for l across layers for i from 0
          do (let ((target (aref (lgraph-layers lg) i)))
               (loop for n across l for j from 0
                     do (setf (aref target j) n (lnode-order n) j))))
    (dolist (np ports) (setf (lnode-ports (car np)) (cdr np)))))

(defun sweep (lg forward rng)
  (let* ((layers (lgraph-layers lg)) (k (length layers)))
    (if forward
        (loop for i from 1 below k
              do (compute-port-ranks (aref layers (1- i)) t)
                 (order-layer (aref layers i) t rng))
        (loop for i from (- k 2) downto 0
              do (compute-port-ranks (aref layers (1+ i)) nil)
                 (order-layer (aref layers i) nil rng)))))

(defun minimize-crossings (lg)
  (let* ((opts (lgraph-options-node lg))
         (strategy (opt-enum opts :crossing-minimization-strategy :layer-sweep))
         (layers (lgraph-layers lg))
         (size (loop for l across layers sum (length l)))
         (runs (max 1 (min (opt-int opts :thoroughness 7 :min 1 :max 50)
                           (if (> size 4000) 2 (if (> size 1500) 4 100)))))
         (rng (lgraph-rng lg)))
    (when (and (> (length layers) 1) (not (eq strategy :none)))
      (let ((best (count-all-crossings lg)) (best-snap (snapshot-order lg))
            (start (snapshot-order lg)))
        (dotimes (run runs)
          (restore-order lg start)
          (when (plusp run)
            (let ((first (aref layers 0)))
              (rng-shuffle rng first)
              (loop for n across first for i from 0 do (setf (lnode-order n) i))
              (regroup-units first)))
          (let ((forward t) (current most-positive-fixnum) (stall 0))
            (loop for iter from 0 below 40
                  do (sweep lg forward (and (plusp run) rng))
                     (let ((c (count-all-crossings lg)))
                       (if (< c current)
                           (setf current c stall 0)
                           (incf stall))
                       (when (< c best) (setf best c best-snap (snapshot-order lg)))
                       (when (or (zerop c) (>= stall 2)) (return)))
                     (setf forward (not forward)))))
        (restore-order lg best-snap)
        (unless (eq (opt-enum opts :greedy-switch-type :two-sided '(:off :one-sided :two-sided)) :off)
          (greedy-switch lg))))
    (final-port-order lg)))

;;; ---- greedy switch -------------------------------------------------------------------
;;;
;;; After the sweeps: swap neighbours in a layer whenever that removes
;;; crossings with the layers on both sides, until no swap helps.

(defun port-positions (layer incoming)
  "Table port -> place along LAYER, in the order edges arrive (INCOMING) or
   leave."
  (let ((h (make-hash-table :test 'eq)) (i 0))
    (loop for n across layer
          do (dolist (p (if incoming (ports-in-order n) (ports-out-order n)))
               (setf (gethash p h) i) (incf i)))
    h))

(defun pair-crossings (u v left-pos right-pos)
  "Crossings between U's and V's edges with U above V."
  (flet ((ends (n incoming pos)
           (let ((out '()))
             (dolist (p (if incoming (ports-in-order n) (ports-out-order n)) out)
               (dolist (e (if incoming (lport-incoming p) (lport-outgoing p)))
                 (let ((q (gethash (if incoming (ledge-source e) (ledge-target e)) pos)))
                   (when q (push q out))))))))
    (+ (if left-pos
           (let ((a (ends u t left-pos)) (b (ends v t left-pos)))
             (loop for x in a sum (count-if (lambda (y) (< y x)) b)))
           0)
       (if right-pos
           (let ((a (ends u nil right-pos)) (b (ends v nil right-pos)))
             (loop for x in a sum (count-if (lambda (y) (< y x)) b)))
           0))))

(defun greedy-switch (lg)
  (let* ((layers (lgraph-layers lg)) (k (length layers)))
    (loop repeat 20
          for improved = nil
          do (dotimes (i k)
               (let* ((layer (aref layers i))
                      (left (and (> i 0) (port-positions (aref layers (1- i)) nil)))
                      (right (and (< i (1- k)) (port-positions (aref layers (1+ i)) t))))
                 (loop for j from 0 below (1- (length layer))
                       do (let ((u (aref layer j)) (v (aref layer (1+ j))))
                            (when (and (not (unit-member-p u)) (not (unit-member-p v))
                                       (< (pair-crossings v u left right) (pair-crossings u v left right)))
                              (setf (aref layer j) v (aref layer (1+ j)) u
                                    (lnode-order v) j (lnode-order u) (1+ j)
                                    improved t))))))
          while improved)))

(defun final-port-order (lg)
  "Settle the order of ports still free (Schulze et al., Algorithm 1), layer
   by layer from the left: outgoing ports by the nodes they reach, then the
   next layer's incoming ports by the exact order of those, so edges between
   the same two nodes run side by side."
  (let ((layers (lgraph-layers lg)))
    (loop for i from 0 below (length layers)
          do (when (> i 0)
               (compute-port-ranks (aref layers (1- i)) t :exact t)
               (loop for n across (aref layers i)
                     unless (fixed-port-order-p n) do (sort-free-ports n t)))
             (when (< i (1- (length layers)))
               (compute-port-ranks (aref layers (1+ i)) nil)
               (loop for n across (aref layers i)
                     unless (fixed-port-order-p n) do (sort-free-ports n nil))))))
