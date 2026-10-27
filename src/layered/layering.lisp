;;;; layering.lisp -- phase 2: assign nodes to layers.
;;;;
;;;; Network simplex (Gansner, Koutsofios, North, Vo 1993, Section 2): ranks
;;;; minimizing the total weighted edge length subject to every edge pointing
;;;; at least one layer forward.  A feasible spanning tree of tight edges is
;;;; grown, cut values are computed with the low/lim postorder numbering, and
;;;; tree edges with negative cut value are exchanged for the non-tree edge
;;;; of least slack crossing the same cut, updating cut values along the tree
;;;; path only.  Longest-path layering is the alternative and the fallback.
;;;;
;;;; Then the constraints: partitions are layered one by one and stacked;
;;;; first/last nodes go to the first/last layer, the *-separate ones to a
;;;; layer of their own before or after everything else.

(in-package #:trellis)

;;; ---- network simplex on a plain graph ----------------------------------------

(defstruct (ns (:constructor %make-ns))
  (n 0 :type fixnum)
  tail head minlen weight cut intree     ; per edge
  rank low lim par out in tree-out tree-in ; per node
  (search-start 0))

(defun ns-slack (ns e)
  (- (aref (ns-rank ns) (aref (ns-head ns) e))
     (aref (ns-rank ns) (aref (ns-tail ns) e))
     (aref (ns-minlen ns) e)))

(defun make-ns (n edges)
  "EDGES: list of (tail head minlen weight)."
  (let* ((m (length edges))
         (ns (%make-ns :n n
                       :tail (make-array m) :head (make-array m) :minlen (make-array m)
                       :weight (make-array m) :cut (make-array m :initial-element 0)
                       :intree (make-array m :initial-element nil)
                       :rank (make-array n :initial-element 0)
                       :low (make-array n :initial-element 0) :lim (make-array n :initial-element 0)
                       :par (make-array n :initial-element -1)
                       :out (make-array n :initial-element '()) :in (make-array n :initial-element '())
                       :tree-out (make-array n :initial-element '())
                       :tree-in (make-array n :initial-element '()))))
    (loop for (tl hd ml w) in edges for e from 0
          do (setf (aref (ns-tail ns) e) tl (aref (ns-head ns) e) hd
                   (aref (ns-minlen ns) e) ml (aref (ns-weight ns) e) w)
             (push e (aref (ns-out ns) tl))
             (push e (aref (ns-in ns) hd)))
    ns))

(defun ns-init-rank (ns)
  "Longest path from the sources, in topological order."
  (let* ((n (ns-n ns)) (indeg (make-array n :initial-element 0)) (queue '()))
    (loop for e below (length (ns-tail ns)) do (incf (aref indeg (aref (ns-head ns) e))))
    (loop for v below n when (zerop (aref indeg v)) do (push v queue))
    (setf queue (nreverse queue))
    (loop while queue
          do (let ((v (pop queue)))
               (dolist (e (aref (ns-out ns) v))
                 (let ((w (aref (ns-head ns) e)))
                   (setf (aref (ns-rank ns) w)
                         (max (aref (ns-rank ns) w) (+ (aref (ns-rank ns) v) (aref (ns-minlen ns) e))))
                   (when (zerop (decf (aref indeg w))) (setf queue (nconc queue (list w))))))))))

(defun ns-add-tree-edge (ns e)
  (setf (aref (ns-intree ns) e) t)
  (push e (aref (ns-tree-out ns) (aref (ns-tail ns) e)))
  (push e (aref (ns-tree-in ns) (aref (ns-head ns) e))))

(defun ns-remove-tree-edge (ns e)
  (setf (aref (ns-intree ns) e) nil)
  (let ((tl (aref (ns-tail ns) e)) (hd (aref (ns-head ns) e)))
    (setf (aref (ns-tree-out ns) tl) (delete e (aref (ns-tree-out ns) tl))
          (aref (ns-tree-in ns) hd) (delete e (aref (ns-tree-in ns) hd)))))

(defun ns-feasible-tree (ns)
  "Grow a spanning tree of tight edges, shifting the tree as needed."
  (let* ((n (ns-n ns)) (in-tree (make-array n :initial-element nil)) (size 0))
    (labels ((grow (v)
               ;; add every node reachable from V over tight edges
               (let ((stack (list v)))
                 (loop while stack
                       do (let ((u (pop stack)))
                            (dolist (e (aref (ns-out ns) u))
                              (let ((w (aref (ns-head ns) e)))
                                (when (and (not (aref in-tree w)) (zerop (ns-slack ns e)))
                                  (setf (aref in-tree w) t) (incf size)
                                  (ns-add-tree-edge ns e) (push w stack))))
                            (dolist (e (aref (ns-in ns) u))
                              (let ((w (aref (ns-tail ns) e)))
                                (when (and (not (aref in-tree w)) (zerop (ns-slack ns e)))
                                  (setf (aref in-tree w) t) (incf size)
                                  (ns-add-tree-edge ns e) (push w stack)))))))))
      (when (plusp n)
        (setf (aref in-tree 0) t size 1)
        (grow 0)
        (loop while (< size n)
              do (let ((best nil) (best-slack nil))
                   (loop for e below (length (ns-tail ns))
                         for a = (aref in-tree (aref (ns-tail ns) e))
                         for b = (aref in-tree (aref (ns-head ns) e))
                         when (and (not (eq a b)))
                           do (let ((s (ns-slack ns e)))
                                (when (or (null best) (< s best-slack))
                                  (setf best e best-slack s))))
                   (unless best (return))  ; disconnected: caller splits components
                   (let ((delta (if (aref in-tree (aref (ns-head ns) best)) (- best-slack) best-slack)))
                     (loop for v below n when (aref in-tree v) do (incf (aref (ns-rank ns) v) delta)))
                   ;; BEST is tight now: grow on from its end in the tree
                   (grow (if (aref in-tree (aref (ns-tail ns) best))
                             (aref (ns-tail ns) best)
                             (aref (ns-head ns) best)))))))
    size))

(defun ns-dfs-range (ns v par low)
  (setf (aref (ns-par ns) v) par (aref (ns-low ns) v) low)
  (let ((lim low))
    (dolist (e (aref (ns-tree-out ns) v))
      (unless (eql e par) (setf lim (ns-dfs-range ns (aref (ns-head ns) e) e lim))))
    (dolist (e (aref (ns-tree-in ns) v))
      (unless (eql e par) (setf lim (ns-dfs-range ns (aref (ns-tail ns) e) e lim))))
    (setf (aref (ns-lim ns) v) lim)
    (1+ lim)))

(defun ns-seq (a b c) (and (<= a b) (<= b c)))

(defun ns-x-val (ns e v dir)
  (let* ((tl (aref (ns-tail ns) e)) (hd (aref (ns-head ns) e))
         (other (if (= tl v) hd tl))
         (f nil) (rv 0) (d 0))
    (if (not (ns-seq (aref (ns-low ns) v) (aref (ns-lim ns) other) (aref (ns-lim ns) v)))
        (setf f t rv (aref (ns-weight ns) e))
        (setf rv (- (if (aref (ns-intree ns) e) (aref (ns-cut ns) e) 0) (aref (ns-weight ns) e))))
    (setf d (if (> dir 0) (if (= hd v) 1 -1) (if (= tl v) 1 -1)))
    (when f (setf d (- d)))
    (if (< d 0) (- rv) rv)))

(defun ns-x-cutval (ns f)
  (multiple-value-bind (v dir)
      (if (eql (aref (ns-par ns) (aref (ns-tail ns) f)) f)
          (values (aref (ns-tail ns) f) 1)
          (values (aref (ns-head ns) f) -1))
    (let ((sum 0))
      (dolist (e (aref (ns-out ns) v)) (incf sum (ns-x-val ns e v dir)))
      (dolist (e (aref (ns-in ns) v)) (incf sum (ns-x-val ns e v dir)))
      (setf (aref (ns-cut ns) f) sum))))

(defun ns-dfs-cutval (ns v par)
  (dolist (e (aref (ns-tree-out ns) v))
    (unless (eql e par) (ns-dfs-cutval ns (aref (ns-head ns) e) e)))
  (dolist (e (aref (ns-tree-in ns) v))
    (unless (eql e par) (ns-dfs-cutval ns (aref (ns-tail ns) e) e)))
  (when par (ns-x-cutval ns par)))

(defun ns-leave-edge (ns)
  "A tree edge with negative cut value: the most negative of the first few
   found, searching on from where the last search stopped."
  (let* ((m (length (ns-tail ns))) (best nil) (count 0))
    (loop for k below m
          for e = (mod (+ (ns-search-start ns) k) m)
          when (and (aref (ns-intree ns) e) (< (aref (ns-cut ns) e) 0))
            do (when (or (null best) (< (aref (ns-cut ns) e) (aref (ns-cut ns) best)))
                 (setf best e))
               (when (>= (incf count) 30)
                 (setf (ns-search-start ns) e)
                 (return)))
    best))

(defun ns-enter-edge (ns e)
  (let* ((tl (aref (ns-tail ns) e)) (hd (aref (ns-head ns) e))
         (outsearch (not (< (aref (ns-lim ns) tl) (aref (ns-lim ns) hd))))
         (v (if outsearch hd tl))
         (low (aref (ns-low ns) v)) (lim (aref (ns-lim ns) v))
         (enter nil) (slack most-positive-fixnum))
    (labels ((out-search (v)
               (dolist (e (aref (ns-out ns) v))
                 (if (not (aref (ns-intree ns) e))
                     (unless (ns-seq low (aref (ns-lim ns) (aref (ns-head ns) e)) lim)
                       (let ((s (ns-slack ns e)))
                         (when (or (< s slack) (null enter)) (setf enter e slack s))))
                     (when (< (aref (ns-lim ns) (aref (ns-head ns) e)) (aref (ns-lim ns) v))
                       (out-search (aref (ns-head ns) e)))))
               (dolist (e (aref (ns-tree-in ns) v))
                 (when (<= slack 0) (return))
                 (when (< (aref (ns-lim ns) (aref (ns-tail ns) e)) (aref (ns-lim ns) v))
                   (out-search (aref (ns-tail ns) e)))))
             (in-search (v)
               (dolist (e (aref (ns-in ns) v))
                 (if (not (aref (ns-intree ns) e))
                     (unless (ns-seq low (aref (ns-lim ns) (aref (ns-tail ns) e)) lim)
                       (let ((s (ns-slack ns e)))
                         (when (or (< s slack) (null enter)) (setf enter e slack s))))
                     (when (< (aref (ns-lim ns) (aref (ns-tail ns) e)) (aref (ns-lim ns) v))
                       (in-search (aref (ns-tail ns) e)))))
               (dolist (e (aref (ns-tree-out ns) v))
                 (when (<= slack 0) (return))
                 (when (< (aref (ns-lim ns) (aref (ns-head ns) e)) (aref (ns-lim ns) v))
                   (in-search (aref (ns-head ns) e))))))
      (if outsearch (out-search v) (in-search v)))
    enter))

(defun ns-rerank (ns v delta)
  (decf (aref (ns-rank ns) v) delta)
  (dolist (e (aref (ns-tree-out ns) v))
    (unless (eql e (aref (ns-par ns) v)) (ns-rerank ns (aref (ns-head ns) e) delta)))
  (dolist (e (aref (ns-tree-in ns) v))
    (unless (eql e (aref (ns-par ns) v)) (ns-rerank ns (aref (ns-tail ns) e) delta))))

(defun ns-tree-update (ns v w cutvalue dir)
  (loop until (ns-seq (aref (ns-low ns) v) (aref (ns-lim ns) w) (aref (ns-lim ns) v))
        do (let* ((e (aref (ns-par ns) v))
                  (d (if (= v (aref (ns-tail ns) e)) dir (not dir))))
             (if d (incf (aref (ns-cut ns) e) cutvalue) (decf (aref (ns-cut ns) e) cutvalue))
             (setf v (if (> (aref (ns-lim ns) (aref (ns-tail ns) e)) (aref (ns-lim ns) (aref (ns-head ns) e)))
                         (aref (ns-tail ns) e) (aref (ns-head ns) e)))))
  v)

(defun ns-update (ns e f)
  (let ((delta (ns-slack ns f))
        (tl (aref (ns-tail ns) e)) (hd (aref (ns-head ns) e)))
    (when (> delta 0)
      (let ((s (+ (length (aref (ns-tree-in ns) tl)) (length (aref (ns-tree-out ns) tl)))))
        (if (= s 1)
            (ns-rerank ns tl delta)
            (let ((s (+ (length (aref (ns-tree-in ns) hd)) (length (aref (ns-tree-out ns) hd)))))
              (if (= s 1)
                  (ns-rerank ns hd (- delta))
                  (if (< (aref (ns-lim ns) tl) (aref (ns-lim ns) hd))
                      (ns-rerank ns tl delta)
                      (ns-rerank ns hd (- delta))))))))
    (let* ((cutvalue (aref (ns-cut ns) e))
           (lca (ns-tree-update ns (aref (ns-tail ns) f) (aref (ns-head ns) f) cutvalue t)))
      (ns-tree-update ns (aref (ns-head ns) f) (aref (ns-tail ns) f) cutvalue nil)
      (setf (aref (ns-cut ns) f) (- cutvalue) (aref (ns-cut ns) e) 0)
      (ns-remove-tree-edge ns e)
      (ns-add-tree-edge ns f)
      (ns-dfs-range ns lca (aref (ns-par ns) lca) (aref (ns-low ns) lca)))))

(defun network-simplex (n edges &key (max-iterations nil))
  "Ranks for nodes 0..N-1 of a connected DAG with EDGES (tail head minlen
   weight), minimizing the sum of weight * length.  Returns a vector."
  (let ((ns (make-ns n edges)))
    (ns-init-rank ns)
    (when (> n 1)
      (when (< (ns-feasible-tree ns) n)
        (return-from network-simplex (normalize-ranks (ns-rank ns))))
      (ns-dfs-range ns 0 nil 1)
      (ns-dfs-cutval ns 0 nil)
      (loop with limit = (or max-iterations (max 1000 (* 20 n)))
            for iter from 0 below limit
            for e = (ns-leave-edge ns)
            while e
            do (let ((f (ns-enter-edge ns e)))
                 (unless f (return))
                 (ns-update ns e f))))
    (normalize-ranks (ns-rank ns))))

(defun normalize-ranks (ranks)
  (let ((m (if (plusp (length ranks)) (reduce #'min ranks) 0)))
    (map 'vector (lambda (r) (- r m)) ranks)))

;;; ---- layering the lgraph -----------------------------------------------------

(defun undirected-components (nodes edges-of)
  "Lists of NODES connected through EDGES-OF (node -> neighbour nodes)."
  (let ((seen (make-hash-table :test 'eq)) (out '()))
    (dolist (n nodes)
      (unless (gethash n seen)
        (let ((comp '()) (stack (list n)))
          (setf (gethash n seen) t)
          (loop while stack
                do (let ((v (pop stack)))
                     (push v comp)
                     (dolist (w (funcall edges-of v))
                       (unless (gethash w seen) (setf (gethash w seen) t) (push w stack)))))
          (push (nreverse comp) out))))
    (nreverse out)))

(defun layer-subgraph (nodes strategy rank)
  "Fill RANK (hash lnode -> layer) for NODES using edges among them."
  (let ((members (make-hash-table :test 'eq)))
    (dolist (n nodes) (setf (gethash n members) t))
    (dolist (comp (undirected-components
                   nodes
                   (lambda (v) (remove-if-not (lambda (w) (gethash w members))
                                              (append (mapcar #'tgt-node (node-outgoing v))
                                                      (mapcar #'src-node (node-incoming v)))))))
      (let ((idx (make-hash-table :test 'eq)) (edges '()))
        (loop for v in comp for i from 0 do (setf (gethash v idx) i))
        (dolist (v comp)
          (dolist (e (node-outgoing v))
            (let ((j (gethash (tgt-node e) idx)))
              (when j (push (list (gethash v idx) j 1 (max 1 (round (* 1 (ledge-weight e))))) edges)))))
        (let ((ranks (if (and (member strategy '(:network-simplex :interactive :stretch-width :min-width
                                                  :coffman-graham :bf-model-order :df-model-order))
                              (<= (length comp) 3000))
                         (network-simplex (length comp) (nreverse edges))
                         (longest-path-ranks (length comp) (nreverse edges)))))
          (loop for v in comp for i from 0 do (setf (gethash v rank) (aref ranks i))))))))

(defun longest-path-ranks (n edges)
  (let ((ns (make-ns n edges))) (ns-init-rank ns) (normalize-ranks (ns-rank ns))))

(defun assign-layers (lg)
  (let* ((nodes (lgraph-nodes lg))
         (strategy (opt-enum (lgraph-options-node lg) :layering-strategy :network-simplex))
         (rank (make-hash-table :test 'eq))
         (inner (remove-if (lambda (n) (member (lnode-constraint n) '(:first-separate :last-separate)))
                           nodes)))
    ;; partitions: layer each, then stack them
    (let ((parts (sort (group-by (lambda (n) (or (lnode-partition n) 0)) inner) #'< :key #'car))
          (offset 0))
      (dolist (part parts)
        (let ((sub (make-hash-table :test 'eq)))
          (layer-subgraph (cdr part) strategy sub)
          (let ((top 0))
            (dolist (n (cdr part))
              (setf (gethash n rank) (+ offset (gethash n sub)))
              (setf top (max top (gethash n sub))))
            (setf offset (+ offset top 1))))))
    ;; first / last
    (let ((maxr 0))
      (dolist (n inner) (when (eq (lnode-constraint n) :first) (setf (gethash n rank) 0)))
      (enforce-edge-lengths inner rank)
      (dolist (n inner)
        (unless (eq (lnode-constraint n) :last) (setf maxr (max maxr (gethash n rank)))))
      (dolist (n inner)
        (when (eq (lnode-constraint n) :last)
          (setf (gethash n rank)
                (max maxr (reduce #'max (mapcar (lambda (e) (1+ (gethash (src-node e) rank 0)))
                                                (node-incoming n))
                                  :initial-value 0)))))
      (enforce-edge-lengths inner rank))
    ;; separate first / last layers
    (let ((has-first (some (lambda (n) (eq (lnode-constraint n) :first-separate)) nodes))
          (maxr (reduce #'max inner :key (lambda (n) (gethash n rank)) :initial-value -1)))
      (dolist (n inner) (when has-first (incf (gethash n rank))))
      (dolist (n nodes)
        (case (lnode-constraint n)
          (:first-separate (setf (gethash n rank) 0))
          (:last-separate (setf (gethash n rank) (+ maxr 1 (if has-first 1 0)))))))
    (build-layers lg rank)))

(defun enforce-edge-lengths (nodes rank)
  "Push nodes forward until every edge among NODES spans at least one layer."
  (let ((members (make-hash-table :test 'eq)) (indeg (make-hash-table :test 'eq)) (queue '()))
    (dolist (n nodes) (setf (gethash n members) t (gethash n indeg) 0))
    (dolist (n nodes)
      (dolist (e (node-outgoing n))
        (when (gethash (tgt-node e) members) (incf (gethash (tgt-node e) indeg)))))
    (dolist (n nodes) (when (zerop (gethash n indeg)) (push n queue)))
    (loop while queue
          do (let ((v (pop queue)))
               (dolist (e (node-outgoing v))
                 (let ((w (tgt-node e)))
                   (when (gethash w members)
                     (setf (gethash w rank) (max (gethash w rank) (1+ (gethash v rank))))
                     (when (zerop (decf (gethash w indeg))) (push w queue)))))))))

(defun build-layers (lg rank)
  "Turn RANK into the lgraph's layer vectors, dropping empty layers; nodes in
   a layer start in input order."
  (let* ((used (sort (remove-duplicates (mapcar (lambda (n) (gethash n rank)) (lgraph-nodes lg))) #'<))
         (index (make-hash-table)))
    (loop for r in used for i from 0 do (setf (gethash r index) i))
    (let ((layers (make-array (length used))))
      (dotimes (i (length used)) (setf (aref layers i) (make-array 0 :adjustable t :fill-pointer t)))
      (dolist (n (lgraph-nodes lg))
        (let ((l (gethash (gethash n rank) index)))
          (setf (lnode-layer n) l)
          (vector-push-extend n (aref layers l))))
      (loop for layer across layers
            do (loop for n across layer for i from 0 do (setf (lnode-order n) i)))
      (setf (lgraph-layers lg) layers))))
