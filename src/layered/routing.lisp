;;;; routing.lisp -- phase 5: layer positions and edge routes.
;;;;
;;;; Layers become columns.  The space between two columns is sized by the
;;;; routing that runs through it.  Orthogonal routing follows ELK's router as
;;;; Weber (2019, Section 2.3) describes it, after Sander: every edge between
;;;; two layers needs at most one vertical segment; edges sharing a port share
;;;; it (a hypernode).  Each pair of hypernodes that would overlap is compared
;;;; in both left/right orders by the crossings and conflicts (parallel
;;;; horizontals on top of each other) each order causes; the cheaper order
;;;; becomes a weighted dependency, equal ones a two-cycle.  Ordering the
;;;; dependency graph with a weighted greedy cycle breaker and taking longest
;;;; paths gives each hypernode its slot.
;;;;
;;;; Ports facing away from the next layer (north, south, or the wrong side)
;;;; leave with a short local detour into the free space beside their node.

(in-package #:trellis)

(defparameter *conflict-penalty* 16)

;;; ---- columns -------------------------------------------------------------------

(defun layer-width (layer)
  (reduce #'max layer :key (lambda (n) (+ (pad-left (lnode-margin n)) (lnode-width n)
                                          (pad-right (lnode-margin n))))
                      :initial-value 0d0))

(defun place-in-column (layer x width)
  "Centre LAYER's nodes in the column [X, X+WIDTH]."
  (loop for n across layer
        do (let ((ext (+ (pad-left (lnode-margin n)) (lnode-width n) (pad-right (lnode-margin n)))))
             (setf (lnode-x n) (+ x (/ (- width ext) 2) (pad-left (lnode-margin n)))))))

;;; ---- leaving and entering ports ----------------------------------------------------

(defun gap-gutter (lg node above)
  "A y in the free space just above (or below) NODE in its layer."
  (let* ((m (lnode-margin node))
         (s (max 1d0 (/ (lspacing lg :edge-node) 2))))
    (if above
        (- (lnode-y node) (pad-top m) (min s (max 1d0 (lspacing lg :node-node))))
        (+ (lnode-y node) (lnode-height node) (pad-bottom m)
           (min s (max 1d0 (lspacing lg :node-node)))))))

(defun exit-path (lg port toward-y facing)
  "Points leading from PORT's anchor to where the edge can run horizontally,
   leaving toward FACING (:east or :west), and the y of that horizontal.
   Ports on another side step round their node into the free space beside it
   (only edges the port dummies did not take care of come here).  TOWARD-Y is
   where the edge is heading."
  (let* ((n (lport-node port))
         (a (port-abs port))
         (side (lport-side port))
         (stub (max 2d0 (/ (lspacing lg :edge-node) 2))))
    (cond ((eq side facing)
           (values (list a) (py a)))
          ((member side '(:north :south))
           (let ((y (gap-gutter lg n (eq side :north))))
             (values (list a (pt (px a) y)) y)))
          (t
           (let* ((above (< toward-y (+ (lnode-y n) (/ (lnode-height n) 2))))
                  (y (gap-gutter lg n above))
                  (x (if (eq facing :east) (- (px a) stub) (+ (px a) stub))))
             (values (list a (pt x (py a)) (pt x y)) y))))))

(defun edge-facings (e)
  "Which way E leaves its source and enters its target: across a gap, east
   then west; an in-layer edge turns back to the side its ports are on."
  (if (in-layer-p e)
      (let ((side (if (eq (lport-side (ledge-source e)) :east) :east :west)))
        (values side side))
      (values :east :west)))

(defun end-path (lg e end)
  "Points from the source (END :source) or to the target (:target) of piece E
   up to where it runs horizontally, and that y.  Through a north/south port
   dummy, the edge turns at the dummy's height straight above or below its
   real port."
  (multiple-value-bind (sf tf) (edge-facings e)
    (let ((port (if (eq end :source) (ledge-source e) (ledge-target e)))
          (via (if (eq end :source) (ledge-source-via e) (ledge-target-via e)))
          (other (if (eq end :source) (ledge-target e) (ledge-source e))))
      (if via
          (let ((a (port-abs via)) (y (py (port-abs port))))
            (values (list a (pt (px a) y)) y))
          (exit-path lg port (py (port-abs other)) (if (eq end :source) sf tf))))))

;;; ---- one gap -------------------------------------------------------------------------

(defstruct (hseg (:constructor make-hseg ()))
  (edges '()) (lefts '()) (rights '())
  (lo 0d0) (hi 0d0) (slot 0) (straight nil))

(defun gap-edges (left right)
  "The pieces routed in the gap between layers LEFT and RIGHT (vectors, either
   may be empty): edges from one to the other, and in-layer edges turning
   back into the gap from either side."
  (let ((out '()))
    (loop for n across left
          do (dolist (e (node-outgoing n))
               (if (in-layer-p e)
                   (when (eq (lport-side (ledge-source e)) :east) (push e out))
                   (push e out))))
    (loop for n across right
          do (dolist (e (node-outgoing n))
               (when (and (in-layer-p e) (eq (lport-side (ledge-source e)) :west)) (push e out))))
    (nreverse out)))

(defun route-gap (lg left right x0 &key edge-of-drawing)
  "Route the pieces in the gap after layer LEFT, which starts at X0.  Returns
   the gap's width.  Each piece gets its slot and the heights it runs at on
   either side; points are made once every column is placed (LEDGE-ROUTE)."
  (let* ((edges (gap-edges left right))
         (style (getf (lgraph-spacing lg) :routing))
         (en (lspacing lg :edge-node-between)) (ee (lspacing lg :edge-edge-between))
         (nn (if edge-of-drawing 0d0 (lspacing lg :node-node-between))))
    (when (null edges) (return-from route-gap nn))
    ;; the side of the gap each end is on, and the height it runs at there
    (let ((info (make-hash-table :test 'eq)))
      (dolist (e edges)
        (multiple-value-bind (sf tf) (edge-facings e)
          (let ((sy (nth-value 1 (end-path lg e :source)))
                (ty (nth-value 1 (end-path lg e :target))))
            ;; facing east means the end is on the gap's left
            (setf (gethash e info) (list (if (eq sf :east) :left :right) sy
                                         (if (eq tf :east) :left :right) ty)))))
      ;; hypernodes: union pieces sharing a port
      (let ((parent (make-hash-table :test 'eq)))
        (labels ((find-root (e) (let ((p (gethash e parent e)))
                                  (if (eq p e) e (setf (gethash e parent) (find-root p)))))
                 (join (a b) (let ((ra (find-root a)) (rb (find-root b)))
                               (unless (eq ra rb) (setf (gethash ra parent) rb)))))
          (unless (eq style :orthogonal)
            ;; sloped routes only share in-layer turns
            (setf edges (remove-if-not #'in-layer-p edges)))
          (let ((by-port (make-hash-table :test 'eq)))
            (dolist (e edges)
              (dolist (p (list (or (ledge-source-via e) (ledge-source e)) (or (ledge-target-via e) (ledge-target e))))
                (let ((prev (gethash p by-port)))
                  (if prev (join prev e) (setf (gethash p by-port) e))))))
          (let ((groups (make-hash-table :test 'eq)) (segs '()))
            (dolist (e edges) (push e (gethash (find-root e) groups)))
            (maphash (lambda (k es)
                       (declare (ignore k))
                       (let ((h (make-hseg)))
                         (setf (hseg-edges h) es)
                         (dolist (e es)
                           (destructuring-bind (s-side sy t-side ty) (gethash e info)
                             (if (eq s-side :left) (pushnew sy (hseg-lefts h) :test #'approx=)
                                 (pushnew sy (hseg-rights h) :test #'approx=))
                             (if (eq t-side :left) (pushnew ty (hseg-lefts h) :test #'approx=)
                                 (pushnew ty (hseg-rights h) :test #'approx=))))
                         (let ((all (append (hseg-lefts h) (hseg-rights h))))
                           (setf (hseg-lo h) (reduce #'min all) (hseg-hi h) (reduce #'max all)))
                         (setf (hseg-straight h)
                               (and (= 1 (length (hseg-lefts h))) (= 1 (length (hseg-rights h)))
                                    (approx= (first (hseg-lefts h)) (first (hseg-rights h)) 1d-3)))
                         (push h segs)))
                     groups)
            (setf segs (sort segs #'< :key #'hseg-lo))
            (let* ((bent (coerce (remove-if #'hseg-straight segs) 'vector))
                   (slots (assign-slots bent ee)))
              (dolist (h segs)
                (dolist (e (hseg-edges h))
                  (destructuring-bind (s-side sy t-side ty) (gethash e info)
                    (declare (ignore s-side t-side))
                    (setf (ledge-points e)
                          (list :slot (if (hseg-straight h) nil (hseg-slot h)) :sy sy :ty ty)))))
              (let* ((needed (if (zerop slots) 0d0 (+ (* 2 en) (* (1- slots) ee))))
                     (width (if (eq style :orthogonal)
                                (max nn needed)
                                (max nn needed (polyline-gap-width lg (gap-edges left right)))))
                     (start (+ x0 (/ (- width needed) 2) en)))
                (dolist (e edges)
                  (let ((slot (getf (ledge-points e) :slot)))
                    (setf (getf (ledge-points e) :slot-x)
                          (and slot (+ start (* slot ee))))))
                width))))))))

(defun polyline-gap-width (lg edges)
  "Room for sloped edges: wide enough that steep edges do not hug the nodes."
  (let ((steep 0d0))
    (dolist (e edges)
      (unless (in-layer-p e)
        (let ((dy (abs (- (py (port-abs (ledge-source e))) (py (port-abs (ledge-target e)))))))
          (setf steep (max steep dy)))))
    (+ (lspacing lg :node-node-between) (* 0.1d0 (min steep 400d0)))))

(defun assign-slots (segs ee)
  "Order the hypernodes SEGS (a vector) into slots; returns the slot count.

   Every overlapping pair is compared in both orders (Weber's dependency
   graph).  A pair where one order is cheaper gets a weighted dependency; a
   pair where both cost the same -- typically one edge going down and one going
   up, which cross either way -- only needs different slots, so it is left out
   of the ordering instead of becoming a two-cycle.  The dependencies are made
   acyclic by a weighted greedy cycle breaker, then slots are handed out in
   topological order (earliest-starting first), each hypernode taking the
   lowest slot right of everything it must follow and apart from everything
   it overlaps."
  (declare (ignore ee))
  (let* ((n (length segs))
         (tol 1d-3)
         (close 0.5d0)
         (out (make-array n :initial-element '()))   ; strict preferences (j . weight)
         (in (make-array n :initial-element '()))
         (overlaps (make-array n :initial-element '())))
    (flet ((cost (a b)
             ;; crossings and conflicts with A left of B
             (+ (count-if (lambda (y) (and (< (+ (hseg-lo b) tol) y) (< y (- (hseg-hi b) tol))))
                          (hseg-rights a))
                (count-if (lambda (y) (and (< (+ (hseg-lo a) tol) y) (< y (- (hseg-hi a) tol))))
                          (hseg-lefts b))
                (* *conflict-penalty*
                   (loop for y in (hseg-rights a)
                         sum (count-if (lambda (z) (< (abs (- y z)) close)) (hseg-lefts b)))))))
      (loop for i below n
            do (loop for j from (1+ i) below n
                     for a = (aref segs i) for b = (aref segs j)
                     while (<= (hseg-lo b) (+ (hseg-hi a) close))
                     do (when (<= (hseg-lo a) (+ (hseg-hi b) close))
                          (push j (aref overlaps i)) (push i (aref overlaps j))
                          (let ((w (- (cost b a) (cost a b))))
                            (cond ((> w 0) (push (cons j w) (aref out i)) (push (cons i w) (aref in j)))
                                  ((< w 0) (push (cons i (- w)) (aref out j)) (push (cons j (- w)) (aref in i)))))))))
    ;; weighted greedy order of the strict preferences (Eades et al.)
    (let ((rank (make-array n :initial-element nil))
          (wout (make-array n :initial-element 0)) (win (make-array n :initial-element 0))
          (live-outs (make-array n :initial-element 0)) (live-ins (make-array n :initial-element 0))
          (front 0) (back (1- n)) (sinks '()) (sources '()))
      (dotimes (i n)
        (setf (aref wout i) (reduce #'+ (aref out i) :key #'cdr)
              (aref win i) (reduce #'+ (aref in i) :key #'cdr)
              (aref live-outs i) (length (aref out i))
              (aref live-ins i) (length (aref in i)))
        (cond ((zerop (aref live-outs i)) (push i sinks))
              ((zerop (aref live-ins i)) (push i sources))))
      (flet ((take (i at-front)
               (setf (aref rank i) (if at-front (prog1 front (incf front)) (prog1 back (decf back))))
               (dolist (o (aref out i))
                 (let ((j (car o)))
                   (unless (aref rank j)
                     (decf (aref win j) (cdr o))
                     (when (zerop (decf (aref live-ins j))) (push j sources)))))
               (dolist (o (aref in i))
                 (let ((j (car o)))
                   (unless (aref rank j)
                     (decf (aref wout j) (cdr o))
                     (when (zerop (decf (aref live-outs j))) (push j sinks)))))))
        (loop
          (loop while (or sinks sources)
                do (loop while sinks
                         do (let ((i (pop sinks))) (unless (aref rank i) (take i nil))))
                   (loop while sources
                         do (let ((i (pop sources))) (unless (aref rank i) (take i t)))))
          (let ((best nil))
            (dotimes (i n)
              (unless (aref rank i)
                (when (or (null best) (> (- (aref wout i) (aref win i)) (- (aref wout best) (aref win best))))
                  (setf best i))))
            (if best (take best t) (return)))))
      ;; constraints in rank order, then slots in topological order
      (let ((preds (make-array n :initial-element '()))
            (succs (make-array n :initial-element '()))
            (indeg (make-array n :initial-element 0)))
        (dotimes (i n)
          (dolist (o (aref out i))
            (let ((j (car o)))
              (multiple-value-bind (u v) (if (< (aref rank i) (aref rank j)) (values i j) (values j i))
                (push u (aref preds v)) (push v (aref succs u)) (incf (aref indeg v))))))
        (let ((h (make-heap)) (slots 0) (assigned (make-array n :initial-element nil))
              (key (make-array n)))
          ;; downward hypernodes from the bottom up, then upward ones from the top
          ;; down: the order their own dependencies run in
          (dotimes (i n)
            (let ((sg (aref segs i)))
              (setf (aref key i)
                    (if (or (null (hseg-lefts sg)) (null (hseg-rights sg)))
                        (+ 3d9 (hseg-lo sg))   ; in-layer turns last
                        (let ((dl (/ (reduce #'+ (hseg-lefts sg)) (length (hseg-lefts sg))))
                              (dr (/ (reduce #'+ (hseg-rights sg)) (length (hseg-rights sg)))))
                          (if (>= dr dl) (- 1d9 (hseg-hi sg)) (+ 2d9 (hseg-lo sg))))))))
          (dotimes (i n) (when (zerop (aref indeg i)) (heap-push h (aref key i) i)))
          (loop until (heap-empty-p h)
                do (let* ((i (heap-pop h))
                          (s (reduce #'max (aref preds i) :key (lambda (j) (1+ (hseg-slot (aref segs j))))
                                                          :initial-value 0))
                          (taken (loop for j in (aref overlaps i)
                                       when (aref assigned j) collect (hseg-slot (aref segs j)))))
                     (loop while (member s taken) do (incf s))
                     (setf (hseg-slot (aref segs i)) s (aref assigned i) t slots (max slots (1+ s)))
                     (dolist (j (aref succs i))
                       (when (zerop (decf (aref indeg j))) (heap-push h (aref key j) j)))))
          slots)))))

;;; ---- all gaps ---------------------------------------------------------------------------

(defun route-layers (lg)
  "Place the columns and route every gap, including the space before the
   first layer and after the last, which only in-layer turns use."
  (let* ((layers (lgraph-layers lg)) (k (length layers)) (none (vector)) (x 0d0))
    (let ((g (route-gap lg none (if (plusp k) (aref layers 0) none) x :edge-of-drawing t)))
      (setf (gethash -1 (lgraph-gaps lg)) (cons x g))
      (incf x g))
    (loop for i from 0 below k
          for layer = (aref layers i)
          do (let ((w (layer-width layer)))
               (place-in-column layer x w)
               (setf (gethash i (lgraph-columns lg)) (cons x w))
               (incf x w)
               (let ((g (route-gap lg layer (if (< i (1- k)) (aref layers (1+ i)) none) x
                                   :edge-of-drawing (= i (1- k)))))
                 (setf (gethash i (lgraph-gaps lg)) (cons x g))
                 (incf x g))))))

(defun ledge-route (lg e)
  "The points of piece E, from source to target, absolute internal coordinates."
  (let* ((info (ledge-points e))
         (slot-x (getf info :slot-x)))
    (append (end-path lg e :source)
            (when slot-x (list (pt slot-x (getf info :sy)) (pt slot-x (getf info :ty))))
            (reverse (end-path lg e :target)))))
