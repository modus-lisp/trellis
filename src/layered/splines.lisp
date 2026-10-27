;;;; splines.lisp -- curved edges, and wire ties.
;;;;
;;;; A spline route is built from knots: the points an edge must pass, each
;;;; with the direction it passes in.  Ports are left and entered along their
;;;; normal, long-edge dummies are crossed horizontally, a north/south port
;;;; dummy is where the edge turns from vertical to horizontal, and an in-layer
;;;; edge turns back at its slot in the gap beside the layer.  Neighbouring
;;;; knots are joined by cubic Beziers whose control points lie along the
;;;; knots' directions, never further than the next knot along that direction,
;;;; so a curve between two columns stays inside the gap between them.
;;;;
;;;; Wire ties bundle edges.  Edges entering the same node run together through
;;;; a tie in the gap before it and fan out from there to their ports; edges
;;;; leaving the same node gather at a tie in the gap after it.  Within a tie
;;;; the wires keep the order of where they come from, so they do not cross
;;;; there, and the tie is drawn as a short band across the bundle.

(in-package #:trellis)

(defparameter *tie-spread* 2.5d0 "Distance between wires held by a tie.")

(defun side-normal (side)
  (ecase side (:north (pt 0 -1)) (:south (pt 0 1)) (:east (pt 1 0)) (:west (pt -1 0))))

(defun neg (v) (pt (- (px v)) (- (py v))))

(defun facing-dir (facing) (if (eq facing :east) (pt 1 0) (pt -1 0)))

;;; ---- knots ----------------------------------------------------------------------

(defun column-edge (lg node facing)
  "The x of the side FACING of NODE's layer column."
  (let ((c (gethash (lnode-layer node) (lgraph-columns lg))))
    (if (eq facing :east) (+ (car c) (cdr c)) (car c))))

(defun end-knots (lg e end)
  "Knots at the source or target end of piece E, in travel order from source
   to target.  Each end runs straight to the edge of its layer's column, so
   curves only bend in the gaps between columns."
  (multiple-value-bind (knots facing) (end-knots-at-port lg e end)
    (let* ((node (lport-node (if (eq end :source) (ledge-source e) (ledge-target e))))
           (x (column-edge lg node facing))
           (last (if (eq end :source) (car (last knots)) (first knots)))
           (y (py (car last)))
           (dir (cdr last)))
      (if (approx= x (px (car last)) 1d-6)
          knots
          (if (eq end :source)
              (append knots (list (cons (pt x y) dir)))
              (cons (cons (pt x y) dir) knots))))))

(defun end-knots-at-port (lg e end)
  "Knots from the port itself, and which side of the column the end faces."
  (multiple-value-bind (sf tf) (edge-facings e)
   (values
    (let ((via (if (eq end :source) (ledge-source-via e) (ledge-target-via e)))
          (port (if (eq end :source) (ledge-source e) (ledge-target e))))
      (if via
          (let* ((a (port-abs via)) (y (py (port-abs port))) (corner (pt (px a) y)))
            (if (eq end :source)
                (list (cons a (side-normal (lport-side via))) (cons corner (pt 1 0)))
                (list (cons corner (pt 1 0)) (cons a (neg (side-normal (lport-side via)))))))
          (let ((path (end-path lg e end)))
            ;; PATH runs from the port outward
            (if (eq end :source)
                (append (list (cons (first path) (facing-dir sf)))
                        (mapcar (lambda (p) (cons p (facing-dir sf))) (rest path)))
                ;; arriving: travel is opposite to the side the edge comes in on
                (let ((dir (if (eq tf :west) (pt 1 0) (pt -1 0))))
                  (reverse (cons (cons (first path) dir)
                                 (mapcar (lambda (p) (cons p dir)) (rest path)))))))))
    (if (eq end :source) sf tf))))

(defun piece-knots (lg e ties)
  (let* ((info (ledge-points e))
         (slot-x (getf info :slot-x))
         (src-tie (getf (gethash e ties) :source))
         (tgt-tie (getf (gethash e ties) :target)))
    (append (end-knots lg e :source)
            (when src-tie (list (cons src-tie (pt 1 0))))
            (when (and slot-x (in-layer-p e))
              (let ((sy (getf info :sy)) (ty (getf info :ty)))
                (list (cons (pt slot-x (/ (+ sy ty) 2)) (pt 0 (if (> ty sy) 1 -1))))))
            (when tgt-tie (list (cons tgt-tie (pt 1 0))))
            (end-knots lg e :target))))

(defun chain-knots (lg pieces ties)
  (let ((knots '()))
    (dolist (e pieces)
      (dolist (k (piece-knots lg e ties))
        ;; a dummy's two ports meet at one point: keep it once
        (unless (and knots (approx= (px (car k)) (px (car (first knots))) 1d-6)
                     (approx= (py (car k)) (py (car (first knots))) 1d-6))
          (push k knots))))
    (setf knots (nreverse knots))
    (if (ledge-reversed (first pieces))
        (mapcar (lambda (k) (cons (car k) (neg (cdr k)))) (reverse knots))
        knots)))

(defun curve-through (knots)
  "Bezier control points through KNOTS: start, then control, control, point
   for each piece."
  (let ((out (list (car (first knots)))))
    (loop for (a b) on knots while b
          do (let* ((pa (car a)) (ta (cdr a)) (pb (car b)) (tb (cdr b))
                    (dx (- (px pb) (px pa))) (dy (- (py pb) (py pa)))
                    ;; reach along each knot's direction: half the way there
                    (da (* 0.5d0 (abs (+ (* dx (px ta)) (* dy (py ta))))))
                    (db (* 0.5d0 (abs (+ (* dx (px tb)) (* dy (py tb)))))))
               (push (pt (+ (px pa) (* da (px ta))) (+ (py pa) (* da (py ta)))) out)
               (push (pt (- (px pb) (* db (px tb))) (- (py pb) (* db (py tb)))) out)
               (push pb out)))
    (nreverse out)))

;;; ---- wire ties --------------------------------------------------------------------

(defun bundle-owner (n)
  "The node a bundle gathers at: port dummies stand for their node."
  (or (lnode-data n :unit-owner) (lnode-data n :partner) n))

(defun crossing-pieces (lg origin)
  (remove-if #'in-layer-p (chain lg origin)))

(defun plan-ties (lg origins)
  "Ties for the chains of ORIGINS.  Returns a table piece -> (:source point
   :target point), and the tie bands, in internal coordinates.  Pieces run
   left to right, so a bundle is the pieces leaving one node into the gap
   after it, or entering one node from the gap before it."
  (let ((ties (make-hash-table :test 'eq)) (bands '())
        (into (make-hash-table :test 'eq)) (out-of (make-hash-table :test 'eq))
        (tied-into (make-hash-table :test 'eq)))
    (dolist (o origins)
      (let ((pieces (crossing-pieces lg o)))
        (when pieces
          (push (first pieces) (gethash (bundle-owner (src-node (first pieces))) out-of))
          (push (car (last pieces)) (gethash (bundle-owner (tgt-node (car (last pieces)))) into)))))
    (maphash (lambda (n members)
               (declare (ignore n))
               (when (cdr members) (dolist (e members) (setf (gethash e tied-into) t))))
             into)
    (labels ((gap-x (layer fraction)
               (let ((g (gethash layer (lgraph-gaps lg))))
                 (and g (+ (car g) (* fraction (cdr g))))))
             (end-y (e end)
               (py (port-abs (if (eq end :source) (ledge-source e) (ledge-target e)))))
             (tie (members end x)
               ;; order the wires by where their other end is, so they do not
               ;; cross inside the tie
               (let* ((far (if (eq end :source) :target :source))
                      (sorted (sort (copy-list members) #'< :key (lambda (e) (end-y e far))))
                      (k (length sorted))
                      (center (/ (reduce #'+ sorted :key (lambda (e) (end-y e end))) k))
                      (top (- center (* *tie-spread* (/ (1- k) 2)))))
                 (loop for e in sorted for i from 0
                       do (setf (getf (gethash e ties) end) (pt x (+ top (* i *tie-spread*)))))
                 (push (cons (pt x (- top 3d0)) (pt x (+ top (* (1- k) *tie-spread*) 3d0))) bands))))
      ;; a piece tied at both ends shares its gap: source tie first
      (maphash (lambda (n members)
                 (when (cdr members)
                   (let ((x (gap-x (lnode-layer n)
                                   (if (some (lambda (e) (gethash e tied-into)) members) 0.3d0 0.5d0))))
                     (when x (tie members :source x)))))
               out-of)
      (maphash (lambda (n members)
                 (when (cdr members)
                   (let ((x (gap-x (1- (lnode-layer n))
                                   (if (some (lambda (e) (getf (gethash e ties) :source)) members) 0.7d0 0.5d0))))
                     (when x (tie members :target x)))))
               into))
    (values ties bands)))
