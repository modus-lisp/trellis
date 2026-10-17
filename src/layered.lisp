;;;; layered.lisp -- ELK Layered: the Sugiyama pipeline.
;;;;
;;;; All work is done in an abstract frame: U runs along the flow (layer
;;;; after layer), V across it (order within a layer).  The direction option
;;;; maps that frame onto x/y only at the end, so every phase is written once.
;;;;
;;;;   1. cycle breaking     DFS from sources; back edges are reversed
;;;;   2. layering           longest path, then sources pulled toward successors
;;;;   3. long edges         one dummy node per layer crossed
;;;;   4. crossing min.      layer sweeps by barycenter + adjacent transposition
;;;;   5. port sides         outgoing ports east, incoming west (in the U frame)
;;;;   6. node placement     weighted mean of neighbours, made feasible by
;;;;                         isotonic regression (pool adjacent violators)
;;;;   7. attachment         ports / edge ends spread along each side in order
;;;;   8. edge routing       orthogonal: vertical segments assigned to slots in
;;;;                         the channel between layers; or polyline
;;;;   9. U coordinates      layer widths + channel widths

(in-package #:diagram)

(defstruct (vnode (:conc-name vn-))
  node                       ; the NODE, or NIL for a dummy
  (len 0d0) (breadth 0d0)    ; size along / across the flow
  (west 0d0) (east 0d0)      ; port protrusion before / after the node along the flow
  (layer 0) (pos 0)
  (u 0d0) (c 0d0)            ; U of the node's box (ports excluded); V of its centre
  in out                     ; vedges
  (order 0))                 ; initial ordering key

(defstruct (vedge (:conc-name ve-))
  from to chain (weight 1d0) (slot nil) (gap 0))

(defstruct (chain (:conc-name ch-))
  edge src tgt sport tport reversed
  vnodes vedges
  (sv 0d0) (tv 0d0)          ; V of the attachment point at each end
  points)

(defparameter *eps* 0.01d0)

(defun real-p (vn) (not (null (vn-node vn))))

;;; ---------------------------------------------------------------- phase 1

(defun break-cycles (vnodes chains)
  (let ((outs (make-hash-table :test #'eq))
        (has-in (make-hash-table :test #'eq))
        (state (make-hash-table :test #'eq)))
    (dolist (ch chains)
      (push ch (gethash (ch-src ch) outs))
      (setf (gethash (ch-tgt ch) has-in) t))
    (maphash (lambda (k v) (setf (gethash k outs) (reverse v))) outs)
    (labels ((visit (v)
               (setf (gethash v state) 1)
               (dolist (ch (gethash v outs))
                 (let ((w (ch-tgt ch)))
                   (case (gethash w state 0)
                     (1 (setf (ch-reversed ch) t))
                     (0 (visit w)))))
               (setf (gethash v state) 2)))
      (dolist (v vnodes) (unless (gethash v has-in) (when (zerop (gethash v state 0)) (visit v))))
      (dolist (v vnodes) (when (zerop (gethash v state 0)) (visit v))))
    (dolist (ch chains)
      (when (ch-reversed ch)
        (rotatef (ch-src ch) (ch-tgt ch))
        (rotatef (ch-sport ch) (ch-tport ch))))))

;;; ---------------------------------------------------------------- phase 2

(defun assign-layers (vnodes chains)
  (let ((indeg (make-hash-table :test #'eq))
        (outs (make-hash-table :test #'eq))
        (queue '()) (layer (make-hash-table :test #'eq)))
    (dolist (ch chains)
      (incf (gethash (ch-tgt ch) indeg 0))
      (push ch (gethash (ch-src ch) outs)))
    (dolist (v vnodes) (when (zerop (gethash v indeg 0)) (push v queue)))
    (setf queue (nreverse queue))
    (dolist (v vnodes) (setf (gethash v layer) 0))
    (loop while queue
          do (let ((v (pop queue)))
               (dolist (ch (gethash v outs))
                 (let ((w (ch-tgt ch)))
                   (setf (gethash w layer) (max (gethash w layer) (1+ (gethash v layer))))
                   (when (zerop (decf (gethash w indeg)))
                     (setf queue (append queue (list w))))))))
    ;; A source sits immediately before its nearest successor, not in layer 0.
    (dolist (v vnodes)
      (let ((succ (gethash v outs)))
        (when (and succ (not (find v chains :key #'ch-tgt)))
          (setf (gethash v layer)
                (max (gethash v layer)
                     (1- (reduce #'min succ :key (lambda (ch) (gethash (ch-tgt ch) layer)))))))))
    (dolist (v vnodes) (setf (vn-layer v) (gethash v layer)))
    (1+ (reduce #'max vnodes :key #'vn-layer :initial-value 0))))

;;; ---------------------------------------------------------------- phase 3

(defun edge-weight (a b)
  (cond ((and (real-p a) (real-p b)) 1d0)
        ((or (real-p a) (real-p b)) 2d0)
        (t 8d0)))

(defun split-long-edges (chains)
  "Returns the list of dummy vnodes created."
  (let ((dummies '()))
    (dolist (ch chains)
      (let ((path (list (ch-src ch))))
        (loop for l from (1+ (vn-layer (ch-src ch))) below (vn-layer (ch-tgt ch))
              do (let ((d (make-vnode :layer l)))
                   (push d dummies)
                   (push d path)))
        (push (ch-tgt ch) path)
        (setf path (nreverse path))
        (setf (ch-vnodes ch) path)
        (setf (ch-vedges ch)
              (loop for (a b) on path while b
                    collect (let ((e (make-vedge :from a :to b :chain ch :weight (edge-weight a b))))
                              (push e (vn-out a))
                              (push e (vn-in b))
                              e)))))
    (dolist (d dummies) (setf (vn-in d) (nreverse (vn-in d)) (vn-out d) (nreverse (vn-out d))))
    dummies))

;;; ---------------------------------------------------------------- phase 4

(defun set-positions (layers)
  (loop for layer across layers
        do (loop for v in layer for i from 0 do (setf (vn-pos v) i))))

(defun crossings-between (layer)
  (let ((pairs (loop for v in layer
                     nconc (loop for e in (vn-out v) collect (cons (vn-pos v) (vn-pos (ve-to e)))))))
    (setf pairs (sort pairs (lambda (a b) (or (< (car a) (car b))
                                               (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))
    (let ((v (coerce (mapcar #'cdr pairs) 'vector)) (count 0))
      (loop for i from 0 below (length v)
            do (loop for j from (1+ i) below (length v)
                     when (< (aref v j) (aref v i)) do (incf count)))
      count)))

(defun total-crossings (layers)
  (loop for i from 0 below (1- (length layers)) sum (crossings-between (aref layers i))))

(defun barycenter-sort (layer neighbours)
  (let ((keyed (loop for v in layer
                     collect (let ((ns (funcall neighbours v)))
                               (cons (if ns
                                         (/ (reduce #'+ ns :key #'vn-pos) (float (length ns) 1d0))
                                         (float (vn-pos v) 1d0))
                                     v)))))
    (mapcar #'cdr (stable-sort keyed #'< :key #'car))))

(defun order-layers (layers)
  (let ((n (length layers)))
    ;; Initial order: depth-first discovery from layer 0, so a chain of
    ;; successors starts out adjacent.
    (let ((counter 0) (seen (make-hash-table :test #'eq)))
      (labels ((visit (v)
                 (unless (gethash v seen)
                   (setf (gethash v seen) t (vn-order v) (incf counter))
                   (dolist (e (vn-out v)) (visit (ve-to e))))))
        (loop for layer across layers do (dolist (v layer) (visit v))))
      (loop for i from 0 below n
            do (setf (aref layers i) (stable-sort (copy-list (aref layers i)) #'< :key #'vn-order))))
    (set-positions layers)
    (let ((best (copy-seq layers)) (best-count (total-crossings layers)))
      (flet ((preds (v) (mapcar #'ve-from (vn-in v)))
             (succs (v) (mapcar #'ve-to (vn-out v)))
             (keep-if-better ()
               (let ((c (total-crossings layers)))
                 (when (< c best-count) (setf best-count c best (copy-seq layers))))))
        (loop repeat 12
              until (zerop best-count)
              do (loop for i from 1 below n
                       do (setf (aref layers i) (barycenter-sort (aref layers i) #'preds))
                          (set-positions layers))
                 (keep-if-better)
                 (loop for i from (- n 2) downto 0
                       do (setf (aref layers i) (barycenter-sort (aref layers i) #'succs))
                          (set-positions layers))
                 (keep-if-better))
        (replace layers best)
        (set-positions layers)
        ;; Greedy transposition of neighbours while it helps.
        (flet ((local (i)
                 (+ (if (> i 0) (crossings-between (aref layers (1- i))) 0)
                    (if (< i (1- n)) (crossings-between (aref layers i)) 0))))
          (loop repeat 8
                for improved = nil
                do (loop for i from 0 below n
                         for layer = (aref layers i)
                         when (< (length layer) 80)
                           do (loop for j from 0 below (1- (length (aref layers i)))
                                    do (let* ((vec (coerce (aref layers i) 'vector))
                                              (before (local i)))
                                         (rotatef (aref vec j) (aref vec (1+ j)))
                                         (setf (aref layers i) (coerce vec 'list))
                                         (set-positions layers)
                                         (if (< (local i) before)
                                             (setf improved t)
                                             (progn (rotatef (aref vec j) (aref vec (1+ j)))
                                                    (setf (aref layers i) (coerce vec 'list))
                                                    (set-positions layers))))))
                while improved))))
    layers))

;;; ---------------------------------------------------------------- phase 6

(defun pav (desired offsets)
  "Isotonic regression: minimise sum (p_i - d_i)^2 subject to
p_{i+1} - p_i >= offsets_{i+1} - offsets_i.  Returns positions."
  (let* ((n (length desired))
         (y (map 'vector #'- desired offsets))
         (blocks '()))                  ; stack of (sum count start)
    (dotimes (i n)
      (push (list (aref y i) 1 i) blocks)
      (loop while (and (cdr blocks)
                       (< (/ (first (first blocks)) (second (first blocks)))
                          (/ (first (second blocks)) (second (second blocks)))))
            do (let ((top (pop blocks)) (under (pop blocks)))
                 (push (list (+ (first top) (first under))
                             (+ (second top) (second under))
                             (third under))
                       blocks))))
    (let ((z (make-array n)))
      (dolist (b blocks)
        (loop for i from (third b) repeat (second b)
              do (setf (aref z i) (/ (first b) (second b)))))
      (map 'vector #'+ z offsets))))

(defun place-across (layers sp)
  (labels ((spacing (a b)
             (cond ((and (real-p a) (real-p b)) (getf sp :node-node))
                   ((or (real-p a) (real-p b)) (getf sp :edge-node))
                   (t (getf sp :edge-edge))))
           (offsets (layer)
             (let ((acc 0d0) (prev nil))
               (map 'vector (lambda (v)
                              (when prev
                                (incf acc (+ (/ (vn-breadth prev) 2) (spacing prev v)
                                             (/ (vn-breadth v) 2))))
                              (setf prev v)
                              acc)
                    layer))))
    ;; Packed start.
    (loop for layer across layers
          do (let ((off (offsets layer)))
               (loop for v in layer for o across off do (setf (vn-c v) o))))
    (let ((n (length layers)))
      (loop for iter from 0 below 30
            do (loop for k from 0 below n
                     for i = (if (evenp iter) k (- n 1 k))
                     for layer = (aref layers i)
                     do (let ((desired
                                (map 'vector
                                     (lambda (v)
                                       (let ((sum 0d0) (w 0d0))
                                         (dolist (e (vn-in v))
                                           (incf sum (* (ve-weight e) (vn-c (ve-from e))))
                                           (incf w (ve-weight e)))
                                         (dolist (e (vn-out v))
                                           (incf sum (* (ve-weight e) (vn-c (ve-to e))))
                                           (incf w (ve-weight e)))
                                         (if (plusp w) (/ sum w) (vn-c v))))
                                     layer)))
                          (when layer
                            (loop for v in layer
                                  for p across (pav desired (offsets layer))
                                  do (setf (vn-c v) p)))))))
    ;; Normalise so the top edge of everything is 0.
    (let ((lo (loop for layer across layers
                    minimize (loop for v in layer minimize (- (vn-c v) (/ (vn-breadth v) 2))))))
      (loop for layer across layers do (dolist (v layer) (decf (vn-c v) lo))))))

;;; ------------------------------------------------------- phases 5, 7, 8, 9

(defun component-layout (vnodes chains sp routing horizontal port-pos)
  "Lay out one connected component.  Fills VN-U, VN-C and CH-POINTS (U . V).
PORT-POS is a hash port -> (pu . pv), the port's box relative to its node.
Returns (values extent-u extent-v)."
  (break-cycles vnodes chains)
  (let* ((nlayers (assign-layers vnodes chains))
         (dummies (split-long-edges chains))
         (layers (make-array nlayers :initial-element nil)))
    (dolist (v (append vnodes dummies)) (push v (aref layers (vn-layer v))))
    (loop for i from 0 below nlayers do (setf (aref layers i) (nreverse (aref layers i))))
    (order-layers layers)
    (flet ((plen (p) (if horizontal (port-width p) (port-height p)))
           (pbr (p) (if horizontal (port-height p) (port-width p))))
      ;; Phase 5: port sides and protrusion.
      (let ((side (make-hash-table :test #'eq)))
        (dolist (ch chains)
          (when (ch-sport ch) (unless (gethash (ch-sport ch) side) (setf (gethash (ch-sport ch) side) :east)))
          (when (ch-tport ch) (unless (gethash (ch-tport ch) side) (setf (gethash (ch-tport ch) side) :west))))
        ;; A port used in both directions keeps the side it got first; the
        ;; other direction's edges attach to the node border instead.
        (dolist (ch chains)
          (when (and (ch-sport ch) (not (eq (gethash (ch-sport ch) side) :east)))
            (setf (ch-sport ch) nil))
          (when (and (ch-tport ch) (not (eq (gethash (ch-tport ch) side) :west)))
            (setf (ch-tport ch) nil)))
        (maphash (lambda (p s)
                   (let ((vn (find (port-owner p) vnodes :key #'vn-node)))
                     (if (eq s :east)
                         (setf (vn-east vn) (max (vn-east vn) (plen p)))
                         (setf (vn-west vn) (max (vn-west vn) (plen p))))))
                 side)
        ;; Phase 6.
        (place-across layers sp)
        ;; Phase 7: attachment V for every chain end.
        (dolist (vn vnodes)
          (let ((items (list :east nil :west nil)))
            ;; item = (key port . ends) where ends are (chain . which)
            (dolist (ch chains)
              (when (eq (ch-src ch) vn)
                (let ((key (vn-c (second (ch-vnodes ch)))) (p (ch-sport ch)))
                  (push (list key p (cons ch :src)) (getf items :east))))
              (when (eq (ch-tgt ch) vn)
                (let ((key (vn-c (car (last (ch-vnodes ch) 2)))) (p (ch-tport ch)))
                  (push (list key p (cons ch :tgt)) (getf items :west)))))
            (loop for s in '(:east :west)
                  do (let ((merged '()))
                       ;; One slot per port, however many edges use it.
                       (dolist (it (reverse (getf items s)))
                         (let ((existing (and (second it) (find (second it) merged :key #'second))))
                           (if existing
                               (progn (push (third it) (cddr existing))
                                      (push (first it) (car existing)))
                               (push (list (list (first it)) (second it) (third it)) merged))))
                       (setf merged
                             (stable-sort (nreverse merged) #'<
                                          :key (lambda (m) (/ (reduce #'+ (first m)) (length (first m))))))
                       (let ((k (length merged)) (top (- (vn-c vn) (/ (vn-breadth vn) 2))))
                         (loop for m in merged for i from 1
                               do (let ((v (+ top (* (vn-breadth vn) (/ i (+ k 1d0)))))
                                        (p (second m)))
                                    (when p
                                      (setf (gethash p port-pos)
                                            (cons (if (eq s :east) (vn-len vn) (- (plen p)))
                                                  (- v top (/ (pbr p) 2)))))
                                    (dolist (end (cddr m))
                                      (if (eq (cdr end) :src)
                                          (setf (ch-sv (car end)) v)
                                          (setf (ch-tv (car end)) v))))))))))
        ;; Phase 8: channel slots.
        (flet ((v1 (e) (if (eq (ve-from e) (ch-src (ve-chain e))) (ch-sv (ve-chain e)) (vn-c (ve-from e))))
               (v2 (e) (if (eq (ve-to e) (ch-tgt (ve-chain e))) (ch-tv (ve-chain e)) (vn-c (ve-to e)))))
          (let ((nslots (make-array nlayers :initial-element 0))
                (ee (getf sp :edge-edge)))
            (when (eq routing :orthogonal)
              (loop for i from 0 below (1- nlayers)
                    do (let* ((segs (loop for v in (aref layers i)
                                          nconc (loop for e in (vn-out v)
                                                      when (> (abs (- (v1 e) (v2 e))) *eps*)
                                                        collect e)))
                              (down (remove-if-not (lambda (e) (> (v2 e) (v1 e))) segs))
                              (up (remove-if (lambda (e) (> (v2 e) (v1 e))) segs))
                              ;; left-to-right: downward segments that start higher go
                              ;; further right; upward ones that start lower go further right.
                              (ordered (append (sort down #'> :key #'v1) (sort up #'< :key #'v1)))
                              (placed '()))
                         (dolist (e ordered)
                           (let ((lo (min (v1 e) (v2 e))) (hi (max (v1 e) (v2 e))) (slot 0))
                             (dolist (q placed)
                               (when (and (< (- lo (/ ee 2)) (+ (max (v1 q) (v2 q)) (/ ee 2)))
                                          (< (- (min (v1 q) (v2 q)) (/ ee 2)) (+ hi (/ ee 2))))
                                 (setf slot (max slot (1+ (ve-slot q))))))
                             (setf (ve-slot e) slot (ve-gap e) i)
                             (push e placed)
                             (setf (aref nslots i) (max (aref nslots i) (1+ slot))))))))
            ;; Phase 9: U coordinates.
            (let ((width (make-array nlayers :initial-element 0d0))
                  (start (make-array nlayers :initial-element 0d0))
                  (gap (make-array nlayers :initial-element 0d0)))
              (loop for i from 0 below nlayers
                    do (setf (aref width i)
                             (loop for v in (aref layers i)
                                   maximize (+ (vn-west v) (vn-len v) (vn-east v)) into m
                                   finally (return (float m 1d0))))
                       (setf (aref gap i)
                             (max (getf sp :between-layers)
                                  (if (plusp (aref nslots i))
                                      (+ (* (1- (aref nslots i)) ee) (* 2 (getf sp :edge-node)))
                                      0d0))))
              (loop for i from 1 below nlayers
                    do (setf (aref start i) (+ (aref start (1- i)) (aref width (1- i)) (aref gap (1- i)))))
              (loop for i from 0 below nlayers
                    do (dolist (v (aref layers i))
                         (if (real-p v)
                             (setf (vn-u v) (+ (aref start i)
                                               (/ (- (aref width i) (vn-west v) (vn-len v) (vn-east v)) 2)
                                               (vn-west v)))
                             (setf (vn-u v) (aref start i) (vn-len v) (aref width i)))))
              ;; Points.
              (dolist (ch chains)
                (let* ((src (ch-src ch)) (tgt (ch-tgt ch))
                       (spt (cons (+ (vn-u src) (vn-len src) (if (ch-sport ch) (plen (ch-sport ch)) 0))
                                  (ch-sv ch)))
                       (tpt (cons (- (vn-u tgt) (if (ch-tport ch) (plen (ch-tport ch)) 0))
                                  (ch-tv ch)))
                       (pts (list spt)))
                  (if (eq routing :orthogonal)
                      (dolist (e (ch-vedges ch))
                        (when (ve-slot e)
                          (let* ((i (ve-gap e))
                                 (n (aref nslots i))
                                 (x (+ (aref start i) (aref width i)
                                       (/ (- (aref gap i) (* (1- n) ee)) 2)
                                       (* (ve-slot e) ee))))
                            (push (cons x (v1 e)) pts)
                            (push (cons x (v2 e)) pts))))
                      (dolist (d (butlast (cdr (ch-vnodes ch))))
                        (push (cons (+ (vn-u d) (/ (vn-len d) 2)) (vn-c d)) pts)))
                  (push tpt pts)
                  (setf pts (nreverse pts))
                  (setf (ch-points ch) (if (ch-reversed ch) (reverse pts) pts))))
              (values (if (plusp nlayers)
                          (+ (aref start (1- nlayers)) (aref width (1- nlayers)))
                          0d0)
                      (loop for layer across layers
                            maximize (loop for v in layer
                                           maximize (+ (vn-c v) (/ (vn-breadth v) 2)))
                            into m
                            finally (return (float m 1d0)))))))))))

;;; ------------------------------------------------------------ the container

(defun direction-of (node)
  (case (option-keyword node "direction" :right)
    ((:down) :down) ((:left) :left) ((:up) :up) (t :right)))

(defun abs->real-box (u v len br extent dir)
  (declare (ignore br))
  (ecase dir
    (:right (values u v))
    (:left (values (- extent u len) v))
    (:down (values v u))
    (:up (values v (- extent u len)))))

(defun abs->real-point (u v extent dir)
  (ecase dir
    (:right (cons u v))
    (:left (cons (- extent u) v))
    (:down (cons v u))
    (:up (cons v (- extent u)))))

(defun spacings (node)
  (list :node-node (option-number node "spacing.nodeNode" 20d0)
        :between-layers (option-number node "spacing.nodeNodeBetweenLayers" 20d0)
        :edge-edge (option-number node "spacing.edgeEdge" 10d0)
        :edge-node (option-number node "spacing.edgeNode" 10d0)
        :component (option-number node "spacing.componentComponent" 20d0)))

(defun components (vnodes chains)
  (let ((parent (make-hash-table :test #'eq)))
    (labels ((find-root (x)
               (let ((p (gethash x parent x)))
                 (if (eq p x) x (setf (gethash x parent) (find-root p))))))
      (dolist (ch chains)
        (let ((a (find-root (ch-src ch))) (b (find-root (ch-tgt ch))))
          (unless (eq a b) (setf (gethash a parent) b))))
      (let ((groups '()))
        (dolist (v vnodes)
          (let* ((r (find-root v)) (g (assoc r groups)))
            (if g (push v (cdr g)) (push (list r v) groups))))
        (loop for (r . vs) in (nreverse groups)
              collect (cons (nreverse vs)
                            (remove-if-not (lambda (ch) (eq (find-root (ch-src ch)) r)) chains)))))))

(defun layout-layered (container infos)
  "Lay out CONTAINER's children.  INFOS are (edge src tgt sport tport) with
SRC/TGT children of CONTAINER."
  (let* ((dir (direction-of container))
         (horizontal (member dir '(:right :left)))
         (sp (spacings container))
         (routing (case (option-keyword container "edgeRouting" :orthogonal)
                    ((:polyline :splines) :polyline) (t :orthogonal)))
         (table (make-hash-table :test #'eq))
         (port-pos (make-hash-table :test #'eq))
         (vnodes (loop for n in (node-children container)
                       collect (let ((w (or (node-width n) 0d0)) (h (or (node-height n) 0d0)))
                                 (setf (gethash n table)
                                       (make-vnode :node n :len (if horizontal w h)
                                                   :breadth (if horizontal h w))))))
         (loops '()) (chains '()))
    (dolist (info infos)
      (destructuring-bind (edge s tg sport tport) info
        (if (eq s tg)
            (push info loops)
            (push (make-chain :edge edge :src (gethash s table) :tgt (gethash tg table)
                              :sport sport :tport tport)
                  chains))))
    (setf chains (nreverse chains) loops (nreverse loops))
    ;; Components, stacked across the flow.
    (let ((extent-u 0d0) (voff 0d0) (first t))
      (dolist (comp (if (option-keyword container "separateConnectedComponents" :true)
                        (if (eq (option-keyword container "separateConnectedComponents" :true) :false)
                            (list (cons vnodes chains))
                            (components vnodes chains))
                        (list (cons vnodes chains))))
        (multiple-value-bind (cu cv)
            (component-layout (car comp) (cdr comp) sp routing horizontal port-pos)
          (unless first (incf voff (getf sp :component)))
          (setf first nil)
          (dolist (v (car comp)) (incf (vn-c v) voff))
          (dolist (ch (cdr comp))
            (setf (ch-points ch) (mapcar (lambda (p) (cons (car p) (+ (cdr p) voff))) (ch-points ch))))
          (setf extent-u (max extent-u cu))
          (incf voff cv)))
      ;; Self loops, on the east side of their node.
      (let ((loop-points '()) (ee (getf sp :edge-edge)))
        (dolist (n (node-children container))
          (let* ((mine (remove-if-not (lambda (i) (eq (second i) n)) loops))
                 (k (length mine)) (vn (gethash n table)))
            (loop for info in mine for i from 1
                  do (let* ((e (+ (vn-u vn) (vn-len vn) (vn-east vn)))
                            (off (* i ee))
                            (q (* (/ (vn-breadth vn) 2) (/ i (+ k 1d0))))
                            (a (- (vn-c vn) q)) (b (+ (vn-c vn) q)))
                       (setf extent-u (max extent-u (+ e off)))
                       (push (cons info (list (cons e a) (cons (+ e off) a)
                                              (cons (+ e off) b) (cons e b)))
                             loop-points)))))
        ;; To real coordinates.
        (multiple-value-bind (pt pl pb pr) (option-padding container 12d0)
          (let ((w (if horizontal extent-u voff)) (h (if horizontal voff extent-u)))
            (dolist (vn vnodes)
              (let ((n (vn-node vn)))
                (multiple-value-bind (x y)
                    (abs->real-box (vn-u vn) (- (vn-c vn) (/ (vn-breadth vn) 2))
                                   (vn-len vn) (vn-breadth vn) extent-u dir)
                  (setf (node-x n) (+ pl x) (node-y n) (+ pt y)))
                (dolist (p (node-ports n))
                  (let ((rel (gethash p port-pos)))
                    (when rel
                      (multiple-value-bind (x y)
                          (abs->real-box (car rel) (cdr rel)
                                         (if horizontal (port-width p) (port-height p))
                                         (if horizontal (port-height p) (port-width p))
                                         (vn-len vn) dir)
                        (setf (port-x p) x (port-y p) y)))))))
            (flet ((real (p) (let ((r (abs->real-point (car p) (cdr p) extent-u dir)))
                               (cons (+ pl (car r)) (+ pt (cdr r))))))
              (dolist (ch chains)
                (push-section (ch-edge ch) (mapcar #'real (ch-points ch)) container))
              (dolist (lp (reverse loop-points))
                (push-section (first (car lp)) (mapcar #'real (cdr lp)) container)))
            (setf (node-width container) (+ pl w pr)
                  (node-height container) (+ pt h pb))))))))

(defun push-section (edge points container)
  (setf points (remove-collinear points))
  (let ((k (length (edge-sections edge))))
    (setf (edge-container edge) container)
    (setf (edge-sections edge)
          (append (edge-sections edge)
                  (list (%make-section
                         :id (format nil "~A_s~D" (edge-id edge) k)
                         :start (first points) :end (car (last points))
                         :bends (butlast (cdr points))
                         :source (and (edge-sources edge) (element-id (first (edge-sources edge))))
                         :target (and (edge-targets edge) (element-id (first (edge-targets edge))))))))))

(defun remove-collinear (points)
  (let ((out (list (first points))))
    (loop for (p q) on (cdr points)
          do (let ((a (first out)))
               (cond ((null q) (push p out))
                     ((and (< (abs (- (car a) (car p))) *eps*) (< (abs (- (cdr a) (cdr p))) *eps*)))
                     ((or (and (< (abs (- (car a) (car p))) *eps*) (< (abs (- (car p) (car q))) *eps*))
                          (and (< (abs (- (cdr a) (cdr p))) *eps*) (< (abs (- (cdr p) (cdr q))) *eps*))))
                     (t (push p out)))))
    (nreverse out)))
