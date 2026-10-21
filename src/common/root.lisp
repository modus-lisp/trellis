;;;; common/root.lisp -- org.eclipse.elk.alg.common (see tools/classnames.tsv)
;;;;
;;;; Methods of the root package: Point, Quadrant, TEdge, TTriangle, Tree,
;;;; BowyerWatsonTriangulation, NaiveMinST, NodeMicroLayout,
;;;; RectilinearConvexHull and its two nested event handlers.
;;;;
;;;; Java sets whose elements compare by CONTENT (HashSet<TEdge>,
;;;; HashSet<TTriangle>, HashMap<KVector,...>) are kept here as insertion-
;;;; ordered JLISTs with JEQUALS membership.  Java's HashSet iteration order
;;;; depends on hashCode buckets, which this port does not reproduce; see
;;;; NOTES for the consequence on tie-breaking.

(in-package #:elk)

;;; ------------------------------------------------------------ small helpers

;;; spore's InternalProperties.FUZZINESS, used by TTriangle.inCircumcircle.
;;; Defined here because the root package loads before the spore code that
;;; also reads it; the value is the same constant as in the Java source.
(defjconst common-internal-properties.fuzziness 0.0001d0)

(defun %root-entry (entries key)
  "The (key . value) cell of KEY in an insertion-ordered, JEQUALS-keyed map, or NIL."
  (find key entries :key #'car :test #'jequals))

(defun %root-map-put (entries key value)
  (let ((cell (%root-entry entries key)))
    (if cell
        (setf (cdr cell) value)
        (vector-push-extend (cons key value) entries))
    entries))

(defun %root-set-add (set x)
  "Set.add on a JLIST-backed set with content equality.  Returns T if added."
  (if (jl-contains set x) nil (progn (jl-add set x) t)))

(defjgeneric cost 1)

(defun %root-call-cost (cost-function edge)
  "ICostFunction.cost: callers may pass a Lisp function or an object implementing the interface."
  (if (functionp cost-function)
      (funcall cost-function edge)
      (cost/1 cost-function edge)))

;;; ------------------------------------------------------------ Quadrant (enum methods)

(defjmethod is-upper ((this common-quadrant))
  (or (eq this common-quadrant.q1) (eq this common-quadrant.q2)))

(defjmethod is-left ((this common-quadrant))
  (or (eq this common-quadrant.q1) (eq this common-quadrant.q4)))

(defjstatic common-quadrant.is-both-left-or-both-right (q1 q2)
  (or (and (eq q1 common-quadrant.q1) (eq q2 common-quadrant.q4))
      (and (eq q1 common-quadrant.q4) (eq q2 common-quadrant.q1))
      (and (eq q1 common-quadrant.q3) (eq q2 common-quadrant.q2))
      (and (eq q1 common-quadrant.q2) (eq q2 common-quadrant.q3))))

(defjstatic common-quadrant.is-one-left-one-right (q1 q2)
  (or (and (eq q1 common-quadrant.q1) (eq q2 common-quadrant.q2))
      (and (eq q1 common-quadrant.q1) (eq q2 common-quadrant.q3))
      (and (eq q1 common-quadrant.q4) (eq q2 common-quadrant.q3))
      (and (eq q1 common-quadrant.q4) (eq q2 common-quadrant.q2))))

;;; ------------------------------------------------------------ Point

(defjctor common-point (x y)
  (setf (.x this) x (.y this) y))

(defjctor common-point (x y quadrant)
  (this-ctor common-point this x y)
  (setf (.quadrant this) quadrant))

(defjstatic common-point.from (v)
  (new-common-point/2 (.x v) (.y v)))

(defjmethod to-string ((this common-point))
  (jstr "(" (.x this) ", " (.y this) (if (.convex this) "cx" "") (.quadrant this) ")"))

(defjmethod equals ((this common-point) obj)
  (and obj
       (typep obj 'common-point)
       (jequals (.x this) (.x obj))
       (jequals (.y this) (.y obj))))

(defjmethod hash-code ((this common-point))
  (sxhash (list (.x this) (.y this))))

;;; ------------------------------------------------------------ TEdge

(defjctor t-edge (u v)
  (setf (.u this) u (.v this) v))

(defjmethod equals ((this t-edge) obj)
  (and (typep obj 't-edge)
       (or (and (jequals (.u this) (.u obj)) (jequals (.v this) (.v obj)))
           (and (jequals (.u this) (.v obj)) (jequals (.v this) (.u obj))))
       t))

(defjmethod hash-code ((this t-edge))
  (+ (if (.u this) (hash-code/0 (.u this)) 0) (if (.v this) (hash-code/0 (.v this)) 0)))

;;; ------------------------------------------------------------ TTriangle

(defjctor t-triangle (a b c)
  (setf (.a this) a (.b this) b (.c this) c)
  (setf (.t-edges this) (jlist (new-t-edge/2 a b) (new-t-edge/2 b c) (new-t-edge/2 c a)))
  (setf (.vertices this) (jlist a b c))
  (setf (.circumcenter this) (%tt-calculate-circumcenter this))
  this)

(defun %tt-calculate-circumcenter (this)
  (let* ((a (.a this)) (b (.b this)) (c (.c this))
         (ab (sub (clone b) a))
         (ac (sub (clone c) a))
         (bc (sub (clone c) b))
         (e (+ (* (.x ab) (+ (.x a) (.x b))) (* (.y ab) (+ (.y a) (.y b)))))
         (f (+ (* (.x ac) (+ (.x a) (.x c))) (* (.y ac) (+ (.y a) (.y c)))))
         (g (* 2 (- (* (.x ab) (.y bc)) (* (.y ab) (.x bc)))))
         (px (/ (- (* (.y ac) e) (* (.y ab) f)) g))
         (py (/ (- (* (.x ab) f) (* (.x ac) e)) g)))
    (new-k-vector/2 px py)))

(defjmethod get-circumcenter ((this t-triangle)) (.circumcenter this))

(defjmethod in-circumcircle ((this t-triangle) v)
  ;; Fuzziness prevents non-deterministic results from double imprecision.
  (< (%cm-fuzzy-compare (distance (.circumcenter this) v)
                        (distance (.circumcenter this) (.a this))
                        common-internal-properties.fuzziness)
     0))

(defjmethod equals ((this t-triangle) obj)
  (and (typep obj 't-triangle)
       (contains this (.a obj))
       (contains this (.b obj))
       (contains this (.c obj))
       t))

(defjmethod hash-code ((this t-triangle))
  ;; Independent of vertex order, as in Java.
  (+ (if (.a this) (hash-code/0 (.a this)) 0)
     (if (.b this) (hash-code/0 (.b this)) 0)
     (if (.c this) (hash-code/0 (.c this)) 0)))

(defjmethod contains ((this t-triangle) (edge t-edge))
  (let ((es (.t-edges this)))
    (or (jequals edge (aref es 0)) (jequals edge (aref es 1)) (jequals edge (aref es 2)))))

(defjmethod contains ((this t-triangle) (vertex k-vector))
  (let ((vs (.vertices this)))
    (or (jequals vertex (aref vs 0)) (jequals vertex (aref vs 1)) (jequals vertex (aref vs 2)))))

;;; ------------------------------------------------------------ Tree

(defjctor tree (n)
  (setf (.node this) n (.children this) (new-jlist)))

;;; ------------------------------------------------------------ BowyerWatsonTriangulation

(defjstatic bowyer-watson-triangulation.triangulate (vertices)
  (bowyer-watson-triangulation.triangulate/2 vertices nil))

(defjstatic bowyer-watson-triangulation.triangulate (vertices debug-output-file)
  ;; debug-output-file: SVG debugging is a no-op in this port.
  (declare (ignore debug-output-file))
  (let* ((topleft (new-k-vector/2 double.positive-infinity double.positive-infinity))
         (bottomright (new-k-vector/2 double.negative-infinity double.negative-infinity)))
    (jdo (v vertices)
      (setf (.x topleft) (math.min (.x topleft) (.x v)))
      (setf (.y topleft) (math.min (.y topleft) (.y v)))
      (setf (.x bottomright) (math.max (.x bottomright) (.x v)))
      (setf (.y bottomright) (math.max (.y bottomright) (.y v))))
    (let* ((size (new-k-vector/2 (- (.x bottomright) (.x topleft)) (- (.y bottomright) (.y topleft))))
           (wiggleroom 50d0)
           (sa (new-k-vector/2 (- (.x topleft) wiggleroom) (- (.y topleft) (.x size) wiggleroom)))
           (sb (new-k-vector/2 (- (.x topleft) wiggleroom) (+ (.y bottomright) (.x size) wiggleroom)))
           (sc (new-k-vector/2 (+ (.x bottomright) (/ (.y size) 2) wiggleroom)
                               (+ (.y topleft) (/ (.y size) 2))))
           (super-triangle (new-t-triangle/3 sa sb sc))
           (triangulation (new-jlist))
           (invalid-triangles (new-jlist))
           (boundary (new-jlist)))
      (%root-set-add triangulation super-triangle)
      (jdo (vertex vertices)
        (jl-clear invalid-triangles)
        (jdo (triangle triangulation)
          (when (in-circumcircle/1 triangle vertex)
            (jl-add invalid-triangles triangle)))
        (jl-clear boundary)
        (jdo (triangle invalid-triangles)
          (jdo (t-edge (.t-edges triangle))
            (let ((on-boundary t))
              (jdo (other invalid-triangles)
                (when (and (not (eq other triangle)) (contains other t-edge))
                  (setf on-boundary nil)))
              (when on-boundary
                (jl-add boundary t-edge)))))
        ;; triangulation.removeAll(invalidTriangles)
        (jl-remove-if triangulation (lambda (tri) (jl-contains invalid-triangles tri)))
        (jdo (tedge boundary)
          (%root-set-add triangulation (new-t-triangle/3 vertex (.u tedge) (.v tedge)))))
      (let ((t-edges (new-jlist)))
        (jdo (triangle triangulation)
          (jdo (tedge (.t-edges triangle))
            (%root-set-add t-edges tedge)))
        (jl-remove-if t-edges
                      (lambda (tedge)
                        (or (contains super-triangle (.u tedge))
                            (contains super-triangle (.v tedge)))))
        t-edges))))

;;; ------------------------------------------------------------ NaiveMinST

(defjstatic naive-min-st.create-spanning-tree (t-edges root cost-function)
  (naive-min-st.create-spanning-tree/4 t-edges root cost-function nil))

(defjstatic naive-min-st.create-spanning-tree (t-edges root cost-function debug-output-file)
  (declare (ignore debug-output-file))
  (let ((weight (new-jlist)))
    (jdo (edge t-edges)
      (%root-map-put weight edge (%root-call-cost cost-function edge)))
    (flet ((weight-of (edge) (cdr (%root-entry weight edge))))
      ;; List sorted by weight (stable), then a LinkedHashSet of the same order.
      (let* ((edge-list (jlist-from t-edges))
             (edges (new-jlist)))
        (jl-sort edge-list (lambda (e1 e2) (double.compare (weight-of e1) (weight-of e2))))
        (jdo (e edge-list)
          (%root-set-add edges e))
        (let* ((min-st (new-tree/1 root))
               (tree-nodes (new-jlist)))
          (%root-map-put tree-nodes root min-st)
          (flet ((in-tree (k) (%root-entry tree-nodes k)))
            (loop while (plusp (length edges)) do
              (let ((next-edge nil) (next-node nil) (node-in-tree nil)
                    (min-weight double.positive-infinity))
                (block scan
                  (loop for edge across edges do
                    (when (<= (weight-of edge) min-weight)
                      (when (and (in-tree (.u edge)) (not (in-tree (.v edge))))
                        (setf next-node (.v edge) node-in-tree (.u edge) next-edge edge)
                        (return-from scan))
                      (when (in-tree (.v edge))
                        (unless (in-tree (.u edge))
                          (setf next-node (.u edge) node-in-tree (.v edge) next-edge edge)
                          (return-from scan))))))
                (when (null next-edge) (return))
                (let ((sub-tree (new-tree/1 next-node)))
                  (jl-add (.children (cdr (in-tree node-in-tree))) sub-tree)
                  (%root-map-put tree-nodes next-node sub-tree)
                  (jl-remove edges next-edge)))))
          min-st)))))

;;; ------------------------------------------------------------ NodeMicroLayout

(defjctor node-micro-layout (adapter)
  (setf (.adapter this) adapter))

(defjstatic node-micro-layout.for-graph (graph)
  (new-node-micro-layout/1
   (if (typep graph 'elk-node)
       (elk-graph-adapters.adapt graph)
       graph)))

(defjmethod execute ((this node-micro-layout))
  (node-dimension-calculation.sort-port-lists (.adapter this))
  (node-dimension-calculation.calculate-label-and-node-sizes (.adapter this))
  (node-dimension-calculation.calculate-node-margins (.adapter this)))

;;; ------------------------------------------------------------ RectilinearConvexHull

(defjconst common-rectilinear-convex-hull.right-high-first
  (lambda (p1 p2)
    (if (= (.x p1) (.x p2))
        (double.compare (.y p2) (.y p1))
        (double.compare (.x p1) (.x p2)))))

(defjconst common-rectilinear-convex-hull.right-low-first
  (lambda (p1 p2)
    (if (= (.x p1) (.x p2))
        (double.compare (.y p1) (.y p2))
        (double.compare (.x p1) (.x p2)))))

(defjconst common-rectilinear-convex-hull.left-high-first
  (lambda (p1 p2)
    (if (= (.x p1) (.x p2))
        (double.compare (.y p2) (.y p1))
        (double.compare (.x p2) (.x p1)))))

(defjconst common-rectilinear-convex-hull.left-low-first
  (lambda (p1 p2)
    (if (= (.x p1) (.x p2))
        (double.compare (.y p1) (.y p2))
        (double.compare (.x p2) (.x p1)))))

(defjconst common-rectilinear-convex-hull.right-special-order
  (lambda (p1 p2)
    (if (= (.x p1) (.x p2))
        (let* ((q1 (.quadrant p1))
               (q2 (.quadrant p2))
               (val (if (is-left q1) 1 -1)))
          (cond ((and (or (eq q1 q2) (common-quadrant.is-both-left-or-both-right q1 q2))
                      (.convex p1) (not (.convex p2)))
                 val)
                ((and (or (eq q1 q2) (common-quadrant.is-both-left-or-both-right q1 q2))
                      (not (.convex p1)) (.convex p2))
                 (- val))
                (t (integer.compare (ordinal q1) (ordinal q2)))))
        (double.compare (.x p1) (.x p2)))))

(defjstatic common-rectilinear-convex-hull.of (points)
  (let ((rch (new-common-rectilinear-convex-hull/0)))
    (jdo (p points)
      (when (or (null (.x-max1 rch)) (>= (.x p) (.x (.x-max1 rch))))
        (setf (.x-max2 rch) (.x-max1 rch))
        (setf (.x-max1 rch) p))
      (when (or (null (.x-min1 rch)) (<= (.x p) (.x (.x-min1 rch))))
        (setf (.x-min2 rch) (.x-min1 rch))
        (setf (.x-min1 rch) p))
      (when (or (null (.y-max1 rch)) (>= (.y p) (.y (.y-max1 rch))))
        (setf (.y-max2 rch) (.y-max1 rch))
        (setf (.y-max1 rch) p))
      (when (or (null (.y-min1 rch)) (<= (.y p) (.y (.y-min1 rch))))
        (setf (.y-min2 rch) (.y-min1 rch))
        (setf (.y-min1 rch) p)))
    (let ((q1 (new-common-maximal-elements-event-handler/1 common-quadrant.q1))
          (q4 (new-common-maximal-elements-event-handler/1 common-quadrant.q4))
          (q2 (new-common-maximal-elements-event-handler/1 common-quadrant.q2))
          (q3 (new-common-maximal-elements-event-handler/1 common-quadrant.q3)))
      (common-scanline.execute points common-rectilinear-convex-hull.right-low-first q1)
      (common-scanline.execute points common-rectilinear-convex-hull.right-high-first q4)
      (common-scanline.execute points common-rectilinear-convex-hull.left-low-first q2)
      (common-scanline.execute points common-rectilinear-convex-hull.left-high-first q3)
      (common-rectilinear-convex-hull.add-concave-corners (.points q1) common-quadrant.q1)
      (common-rectilinear-convex-hull.add-concave-corners (.points q2) common-quadrant.q2)
      (common-rectilinear-convex-hull.add-concave-corners (.points q3) common-quadrant.q3)
      (common-rectilinear-convex-hull.add-concave-corners (.points q4) common-quadrant.q4)
      (let ((hull (.hull rch)))
        (jl-clear hull)
        (jl-add-all hull (.points q1))
        (jl-add-all hull (jl-reverse (jl-copy (.points q2))))
        (jl-add-all hull (.points q3))
        (jl-add-all hull (jl-reverse (jl-copy (.points q4)))))
      rch)))

(defjmethod get-hull ((this common-rectilinear-convex-hull)) (.hull this))

(defjmethod split-into-rectangles ((this common-rectilinear-convex-hull))
  (let ((handler (new-common-rectangle-event-handler/1 this)))
    (common-scanline.execute (.hull this) common-rectilinear-convex-hull.right-special-order handler)
    (when (.queued handler)
      (jl-add (.rects handler) (.queued handler)))
    (.rects handler)))

(defjstatic common-rectilinear-convex-hull.add-concave-corners (pts q)
  ;; Java's ListIterator insertion: a concave corner goes between each pair
  ;; of neighbouring points.
  (let ((prev (aref pts 0))
        (i 1))
    (loop while (< i (length pts)) do
      (let* ((next (aref pts i))
             (p (new-common-point/3 (.x next) (.y prev) q)))
        (jl-add-at pts i p)
        (setf (.convex p) nil)
        (setf prev next)
        (incf i 2)))))

;;; MaximalElementsEventHandler

(defjctor common-maximal-elements-event-handler (quadrant)
  (setf (.quadrant this) quadrant)
  (enum-case quadrant
    ((common-quadrant.q1 common-quadrant.q2)
     (setf (.compare this) (lambda (a b) (double.compare b a)))
     (setf (.maximal-y this) double.positive-infinity))
    ((common-quadrant.q3 common-quadrant.q4)
     (setf (.compare this) (lambda (a b) (double.compare a b)))
     (setf (.maximal-y this) double.negative-infinity))))

(defjmethod handle ((this common-maximal-elements-event-handler) p)
  (when (> (funcall (.compare this) (.y p) (.maximal-y this)) 0)
    (jl-add (.points this) (new-common-point/3 (.x p) (.y p) (.quadrant this)))
    (setf (.maximal-y this) (.y p))))

;;; RectangleEventHandler (inner class: OUTER is the enclosing hull)

(defjctor common-rectangle-event-handler (outer)
  (setf (.outer this) outer)
  (setf (.last-x this) (math.min (.x (.x-min1 outer)) (.x (.x-min2 outer)))))

(defjmethod handle ((this common-rectangle-event-handler) p)
  (let ((queued (.queued this)))
    (when (and queued
               (or (/= (.x p) (.x (.queued-pnt this)))
                   (common-quadrant.is-one-left-one-right (.quadrant (.queued-pnt this)) (.quadrant p))))
      (jl-add (.rects this) queued)
      (setf (.last-x this) (+ (.x queued) (.width queued)))
      (setf (.queued this) nil)
      (setf (.queued-pnt this) nil)))
  (if (is-upper (.quadrant p))
      (setf (.min-y this) p)
      (setf (.max-y this) p))
  (let ((qd (.quadrant p))
        (cv (.convex p)))
    (when (or (and (eq qd common-quadrant.q1) (not cv))
              (and (eq qd common-quadrant.q2) cv)
              (and (eq qd common-quadrant.q3) cv)
              (and (eq qd common-quadrant.q4) (not cv)))
      (when (and (.min-y this) (.max-y this))
        (setf (.queued this)
              (new-elk-rectangle/4 (.last-x this) (.y (.min-y this))
                                   (- (.x p) (.last-x this))
                                   (- (.y (.max-y this)) (.y (.min-y this)))))
        (setf (.queued-pnt this) p)))))
