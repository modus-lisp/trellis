;;;; engine.lisp -- running algorithms over a hierarchy.
;;;;
;;;; LAYOUT works bottom-up: a compound node is laid out after its children, so
;;;; their sizes are known.  Each compound node's ALGORITHM option picks what
;;;; arranges its children (layered when unset).
;;;;
;;;; Which level routes an edge: the edge's first source and first target
;;;; belong to two nodes; when those are siblings their parent routes it, and
;;;; when one is the other's parent that parent routes it.  Algorithms write
;;;; routes in the coordinates of the level that routes them (its "frame"),
;;;; because a frame's own position is only known once the level above has been
;;;; laid out.  When everything is placed, routes are moved into the
;;;; coordinates of the node that contains the edge.  Edges no level can route
;;;; -- between unrelated parts of the hierarchy, or excluded by noLayout -- get
;;;; a straight route at the end.

(in-package #:trellis)

(defvar *algorithms* (make-hash-table :test 'eq))

(defmacro defalgorithm (name (node) &body body)
  "Define layout algorithm NAME: BODY places NODE's children (relative to NODE,
   padding included), routes the edges in (FRAME-EDGES NODE) with SET-ROUTE,
   and sets NODE's size."
  `(setf (gethash ,name *algorithms*) (lambda (,node) ,@body)))

(defun algorithms ()
  "The names of the available layout algorithms."
  (sort (hash-keys *algorithms*) #'string< :key #'symbol-name))

(defvar *frame-edges* nil "node -> edges it routes")
(defvar *edge-frames* nil "edge -> the node whose coordinates its route is in")

(defun frame-edges (node) (gethash node *frame-edges*))

(defun set-curve-route (edge points frame)
  "Give EDGE a curved route: POINTS are Bezier control points (start, then
   control, control, point for each piece), in FRAME's content coordinates."
  (setf (edge-sections edge)
        (list (make-section (first points) (car (last points)) (butlast (rest points)) t))
        (gethash edge *edge-frames*) frame))

(defun set-route (edge points frame)
  "Give EDGE the route POINTS, in FRAME's content coordinates."
  (let ((pts (remove-collinear points)))
    (when (null (cdr pts)) (setf pts (list (first pts) (first pts))))
    (setf (edge-sections edge)
          (list (make-section (first pts) (car (last pts)) (butlast (rest pts))))
          (gethash edge *edge-frames*) frame)))

(defun edge-frame (edge)
  "The node that routes EDGE, or NIL."
  (let ((s (first (edge-sources edge))) (tt (first (edge-targets edge))))
    (when (and s tt)
      (let ((a (endpoint-node s)) (b (endpoint-node tt)))
        (cond ((and (node-parent a) (eq (node-parent a) (node-parent b))) (node-parent a))
              ((eq a (node-parent b)) a)
              ((eq b (node-parent a)) b)
              (t nil))))))

(defun algorithm-of (node)
  (let ((a (option node :algorithm)))
    ;; ELK also accepts short ids such as "layered" or "elk.force"
    (when (stringp a)
      (setf a (enum-keyword (if (and (> (length a) 4) (string-equal "elk." a :end2 4)) (subseq a 4) a))))
    (cond ((and (keywordp a) (gethash a *algorithms*)) a)
          ((null a) :layered)
          (t a))))

(defun excluded-p (element) (opt-bool element :no-layout nil))

(defun layout (graph &rest options)
  "Lay out GRAPH (its root node) in place and return it.  OPTIONS are set on the
   root first, e.g. (layout g :algorithm :layered :direction :down)."
  (apply-options graph options)
  (let ((*frame-edges* (make-hash-table :test 'eq))
        (*edge-frames* (make-hash-table :test 'eq))
        (*ports-placed* (make-hash-table :test 'eq))
        (*inherited-options* (make-hash-table :test 'eq))
        (splits '()))
    (dolist (e (all-edges graph)) (setf (edge-sections e) '()))
    (map-nodes (lambda (n) (setf (node-ties n) '())) graph :include-root t)
    (unwind-protect
         (progn
           (setf splits (split-cross-hierarchy-edges graph))
           (dolist (e (all-edges graph))
             (let ((f (edge-frame e)))
               (when (and f (not (excluded-p e)))
                 (push e (gethash f *frame-edges*)))))
           (maphash (lambda (k v) (setf (gethash k *frame-edges*) (nreverse v))) *frame-edges*)
           (layout-node graph)
           (finish-routes graph)
           (join-cross-hierarchy-edges splits))
      (remove-split-pieces splits)))
  graph)

;;; ---- edges across the hierarchy -----------------------------------------------
;;;
;;; An edge between nodes in different compound nodes is routed in pieces, one
;;; per level it passes through: a temporary port is put on every compound node
;;; on the way, and each level routes its piece like any other edge, from or to
;;; its own ports.  The pieces are joined afterwards and the temporary ports and
;;; edges removed.

(defun lowest-common-ancestor (a b)
  (loop for x = a then (node-parent x) while x
        when (ancestor-p x b) return x))

(defun split-cross-hierarchy-edges (graph)
  "Split edges no single level can route.  Returns ((edge . pieces) ...)."
  (let ((splits '()))
    (dolist (e (all-edges graph))
      (let ((s (first (edge-sources e))) (tt (first (edge-targets e))))
        (when (and s tt (null (edge-frame e)) (not (excluded-p e))
                   (not (excluded-p (endpoint-node s))) (not (excluded-p (endpoint-node tt))))
          (let* ((sa (endpoint-node s)) (ta (endpoint-node tt))
                 (lca (lowest-common-ancestor sa ta)))
            (when lca
              (flet ((climb (n)
                       ;; compound nodes from N's parent up to the child of LCA
                       (if (eq n lca) '()
                           (loop for x = (node-parent n) then (node-parent x)
                                 until (or (null x) (eq x lca)) collect x)))
                     (temp-port (node)
                       (let ((p (%make-port :id (fresh-id "__xh") :node node)))
                         (setf (node-ports node) (append (node-ports node) (list p)))
                         p))
                     (piece (container from to)
                       (let ((pe (%make-edge :id (fresh-id "__xe") :container container
                                             :sources (list from) :targets (list to))))
                         (setf (element-options pe) (element-options e))
                         (setf (node-edges container) (append (node-edges container) (list pe)))
                         pe)))
                (let* ((ups (climb sa)) (downs (climb ta))
                       (up-ports (mapcar #'temp-port ups))
                       (down-ports (mapcar #'temp-port downs))
                       (pieces '()) (at s))
                  (loop for node in ups for p in up-ports
                        do (push (piece node at p) pieces) (setf at p))
                  (let ((rest (reverse (mapcar #'cons downs down-ports))))
                    (push (piece lca at (if rest (cdr (first rest)) tt)) pieces)
                    (loop for ((node . p) next) on rest
                          do (push (piece node p (if next (cdr next) tt)) pieces)))
                  (push (cons e (nreverse pieces)) splits))))))))
    (nreverse splits)))

(defun join-cross-hierarchy-edges (splits)
  (dolist (sp splits)
    (destructuring-bind (e . pieces) sp
      (when (every #'edge-sections pieces)
        (let* ((c (content-origin (edge-container e)))
               (curve (every (lambda (pe) (section-curve (first (edge-sections pe)))) pieces))
               (abs (loop for pe in pieces for first = t then nil
                          append (let* ((o (content-origin (edge-container pe)))
                                        (pts (mapcar (lambda (p) (pt+ o p))
                                                     (if curve
                                                         (section-points (first (edge-sections pe)))
                                                         (section-path (first (edge-sections pe)))))))
                                   ;; curves join at the shared port: drop the repeat
                                   (if (and curve (not first)) (rest pts) pts))))
               (rel (mapcar (lambda (p) (pt (- (px p) (px c)) (- (py p) (py c)))) abs)))
          (unless curve (setf rel (remove-collinear rel)))
          (when (null (cdr rel)) (setf rel (list (first rel) (first rel))))
          (setf (edge-sections e) (list (make-section (first rel) (car (last rel)) (butlast (rest rel)) curve)))
          (place-edge-labels-midway e (section-path (first (edge-sections e)))))))))

(defun remove-split-pieces (splits)
  (dolist (sp splits)
    (dolist (pe (cdr sp))
      (let ((c (edge-container pe)))
        (setf (node-edges c) (remove pe (node-edges c))))
      (dolist (end (append (edge-sources pe) (edge-targets pe)))
        (when (and (port-p end) (eql 0 (search "__xh" (port-id end))))
          (let ((n (port-node end)))
            (setf (node-ports n) (remove end (node-ports n)))))))
    ;; an edge whose pieces were not all routed still needs a route
    (let ((e (car sp)))
      (when (and (null (edge-sections e)) (edge-sources e) (edge-targets e))
        (straight-route e)))))

(defun inherited-option-p (key)
  "Options a compound takes from an ancestor laying out its whole hierarchy:
   those describing how a graph is laid out, not how one node looks."
  (let ((name (symbol-name key)))
    (or (member key '(:algorithm :direction :edge-routing :thoroughness :random-seed
                      :merge-edges :aspect-ratio :separate-connected-components))
        (some (lambda (prefix) (and (>= (length name) (length prefix)) (string= prefix name :end2 (length prefix))))
              '("SPACING-" "CYCLE-BREAKING-" "LAYERING-STRATEGY" "CROSSING-MINIMIZATION-"
                "NODE-PLACEMENT-" "GREEDY-SWITCH-" "EDGE-LABELS-")))))

(defun inherit-hierarchy-options (node)
  "With hierarchyHandling INCLUDE_CHILDREN, ELK lays a whole subtree out with
   its top's settings.  Here each level is still laid out on its own, so a
   compound inside such a subtree takes the layout options it does not set
   from the subtree's top."
  (let ((top (loop for a = (node-parent node) then (node-parent a)
                   while a
                   do (case (opt-enum a :hierarchy-handling :inherit)
                        (:include-children (return a))
                        (:separate-children (return nil))))))
    (when (and top (not (eq (opt-enum node :hierarchy-handling :inherit) :separate-children)))
      (let ((inh (make-hash-table :test 'eq)))
        ;; the top's own settings, and what the top itself inherited
        (dolist (source (list (gethash top *inherited-options*) (element-options top)))
          (when source
            (maphash (lambda (k v)
                       (when (and (inherited-option-p k) (not (option-set-p node k)))
                         (setf (gethash k inh) v)))
                     source)))
        (setf (gethash node *inherited-options*) inh)))))

(defun layout-node (node)
  (inherit-hierarchy-options node)
  (dolist (c (node-children node))
    (when (node-children c) (layout-node c)))
  (when (or (node-children node) (null (node-parent node)))
    (let* ((name (algorithm-of node))
           (fn (gethash name *algorithms*)))
      (unless fn (error "trellis: no layout algorithm ~S" name))
      (funcall fn node))))

;;; ---- after layout ----------------------------------------------------------

(defun finish-routes (graph)
  (dolist (e (all-edges graph))
    (let ((frame (gethash e *edge-frames*)) (container (edge-container e)))
      (cond ((and frame (edge-sections e))
             (unless (eq frame container)
               (move-route e (pt+ (content-origin frame)
                                  (let ((c (content-origin container)))
                                    (pt (- (px c)) (- (py c))))))))
            ((and (edge-sources e) (edge-targets e))
             (straight-route e))))))

(defun move-route (edge delta)
  (flet ((mv (p) (pt+ p delta)))
    (dolist (s (edge-sections edge))
      (setf (section-start s) (mv (section-start s))
            (section-end s) (mv (section-end s))
            (section-bends s) (mapcar #'mv (section-bends s))))
    (dolist (l (edge-labels edge))
      (incf (label-x l) (px delta)) (incf (label-y l) (py delta)))))

(defun shape-center (shape)
  (let ((o (absolute-position shape)))
    (pt (+ (px o) (/ (max 0d0 (shape-width shape)) 2))
        (+ (py o) (/ (max 0d0 (shape-height shape)) 2)))))

(defun clip-to-box (from to shape)
  "Where the segment FROM -> TO (FROM inside SHAPE) leaves SHAPE's box."
  (let* ((o (absolute-position shape))
         (x0 (px o)) (y0 (py o))
         (x1 (+ x0 (max 0d0 (shape-width shape)))) (y1 (+ y0 (max 0d0 (shape-height shape))))
         (dx (- (px to) (px from))) (dy (- (py to) (py from)))
         (tmax 1d0))
    (flet ((lim (dv lo hi v)
             (cond ((> dv 0) (setf tmax (min tmax (/ (- hi v) dv))))
                   ((< dv 0) (setf tmax (min tmax (/ (- lo v) dv)))))))
      (lim dx x0 x1 (px from))
      (lim dy y0 y1 (py from)))
    (setf tmax (clamp tmax 0d0 1d0))
    (pt (+ (px from) (* tmax dx)) (+ (py from) (* tmax dy)))))

(defun straight-route (edge)
  "A straight route between the boxes of EDGE's first source and target, in the
   coordinates of its container."
  (let* ((s (first (edge-sources edge))) (tt (first (edge-targets edge)))
         (a (shape-center s)) (b (shape-center tt))
         (start (if (and (port-p s) (zerop (port-width s))) a (clip-to-box a b s)))
         (end (if (and (port-p tt) (zerop (port-width tt))) b (clip-to-box b a tt)))
         (c (content-origin (edge-container edge)))
         (rel (lambda (p) (pt (- (px p) (px c)) (- (py p) (py c))))))
    (when (ancestor-p (endpoint-node tt) (endpoint-node s)) (setf end (clip-out b a tt)))
    (when (ancestor-p (endpoint-node s) (endpoint-node tt)) (setf start (clip-out a b s)))
    (setf (edge-sections edge)
          (list (make-section (funcall rel start) (funcall rel end))))
    (place-edge-labels-midway edge (list (funcall rel start) (funcall rel end)))))

(defun clip-out (inner other shape)
  "For an edge from SHAPE to something inside it: the point of SHAPE's border
   nearest INNER, going away from OTHER... simply the nearest border point."
  (declare (ignore other))
  (let* ((o (absolute-position shape))
         (x0 (px o)) (y0 (py o))
         (x1 (+ x0 (max 0d0 (shape-width shape)))) (y1 (+ y0 (max 0d0 (shape-height shape))))
         (x (px inner)) (y (py inner))
         (dl (- x x0)) (dr (- x1 x)) (dt (- y y0)) (db (- y1 y))
         (m (min dl dr dt db)))
    (cond ((= m dl) (pt x0 y)) ((= m dr) (pt x1 y)) ((= m dt) (pt x y0)) (t (pt x y1)))))

(defun place-edge-labels-midway (edge points)
  "Put EDGE's labels next to the middle of the route POINTS (frame coordinates)."
  (when (and (edge-labels edge) (cdr points))
    (let* ((lens (loop for (a b) on points while b
                       collect (sqrt (+ (expt (- (px b) (px a)) 2) (expt (- (py b) (py a)) 2)))))
           (half (/ (reduce #'+ lens) 2))
           (mid (loop for (a b) on points while b for l in lens
                      when (or (>= l half) (null (cddr (member a points))))
                        return (let ((f (if (> l 0) (/ half l) 0d0)))
                                 (pt (+ (px a) (* f (- (px b) (px a))))
                                     (+ (py a) (* f (- (py b) (py a))))))
                      do (decf half l)))
           (y (+ (py mid) 2d0)))
      (dolist (l (edge-labels edge))
        (setf (label-x l) (- (px mid) (/ (label-width l) 2))
              (label-y l) y)
        (incf y (label-height l))))))

;;; ---- helpers for algorithms ------------------------------------------------

(defun graph-padding (node default)
  (let ((p (opt-pad node :padding default)))
    (pad (max 0d0 (pad-top p)) (max 0d0 (pad-left p))
         (max 0d0 (pad-bottom p)) (max 0d0 (pad-right p)))))

(defun layout-children (node)
  "NODE's children that take part in layout."
  (remove-if #'excluded-p (node-children node)))

(defun fit-node-to-children (node padding &key (min-width 0d0) (min-height 0d0))
  "Shift NODE's children so their extents start at the padding, and size NODE
   to hold them.  Returns the shift as (dx . dy)."
  (let ((r nil))
    (dolist (c (node-children node))
      (let ((e (node-extent c)))
        (setf r (rect-union r (rect (+ (node-x c) (rect-x e)) (+ (node-y c) (rect-y e))
                                    (rect-w e) (rect-h e))))))
    (unless r (setf r (rect 0d0 0d0 0d0 0d0)))
    (let ((dx (- (pad-left padding) (rect-x r))) (dy (- (pad-top padding) (rect-y r))))
      (dolist (c (node-children node)) (incf (node-x c) dx) (incf (node-y c) dy))
      (resize-graph node (+ (rect-w r) (pad-left padding) (pad-right padding))
                    (+ (rect-h r) (pad-top padding) (pad-bottom padding))
                    :min-width min-width :min-height min-height)
      (pt dx dy))))

(defun resize-graph (node width height &key (min-width 0d0) (min-height 0d0))
  "Set the size of NODE after laying out its content, honouring nodeSize
   options on it.  Content that needs more room always gets it."
  (let ((w (max width min-width)) (h (max height min-height)))
    (when (member :minimum-size (size-constraints node))
      (let ((m (option node :node-size-minimum)))
        (when (consp m) (setf w (max w (sane (px m))) h (max h (sane (py m)))))))
    (when (and (opt-bool node :node-size-fixed-graph-size nil) (node-parent node))
      (setf w (max w (node-width node)) h (max h (node-height node))))
    (setf (node-width node) w (node-height node) h)))

(defun route-frame-edges-straight (node)
  "Route NODE's edges as straight lines between their shapes."
  (let ((o (content-origin node)))
    (declare (ignorable o))
    (dolist (e (frame-edges node))
      (let* ((s (first (edge-sources e))) (tt (first (edge-targets e)))
             (a (local-center s node)) (b (local-center tt node)))
        (multiple-value-bind (start end) (values (local-clip a b s node) (local-clip b a tt node))
          (set-route e (list start end) node)
          (place-edge-labels-midway e (list start end)))))))

(defun local-origin (shape frame)
  "SHAPE's origin in FRAME's content coordinates (SHAPE is FRAME, a child of
   FRAME, or a port of one of those)."
  (etypecase shape
    (node (if (eq shape frame) (pt 0 0) (pt (node-x shape) (node-y shape))))
    (port (pt+ (local-origin (port-node shape) frame) (pt (port-x shape) (port-y shape))))))

(defun local-center (shape frame)
  (let ((o (local-origin shape frame)))
    (pt (+ (px o) (/ (max 0d0 (shape-width shape)) 2))
        (+ (py o) (/ (max 0d0 (shape-height shape)) 2)))))

(defun local-clip (from to shape frame)
  (let* ((o (local-origin shape frame))
         (x0 (px o)) (y0 (py o))
         (x1 (+ x0 (max 0d0 (shape-width shape)))) (y1 (+ y0 (max 0d0 (shape-height shape)))))
    (if (eq shape frame)
        ;; an edge from the frame itself to a child: start on the nearest border
        (let* ((x (px to)) (y (py to))
               (dl (- x x0)) (dr (- x1 x)) (dt (- y y0)) (db (- y1 y)) (m (min dl dr dt db)))
          (cond ((= m dl) (pt x0 y)) ((= m dr) (pt x1 y)) ((= m dt) (pt x y0)) (t (pt x y1))))
        (let* ((dx (- (px to) (px from))) (dy (- (py to) (py from))) (tmax 1d0))
          (flet ((lim (dv lo hi v)
                   (cond ((> dv 0) (setf tmax (min tmax (/ (- hi v) dv))))
                         ((< dv 0) (setf tmax (min tmax (/ (- lo v) dv)))))))
            (lim dx x0 x1 (px from)) (lim dy y0 y1 (py from)))
          (setf tmax (clamp tmax 0d0 1d0))
          (pt (+ (px from) (* tmax dx)) (+ (py from) (* tmax dy)))))))
