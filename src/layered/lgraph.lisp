;;;; lgraph.lisp -- the layered algorithm's own graph.
;;;;
;;;; The layered algorithm works left to right: layers are columns along x, and
;;;; nodes in a layer are ordered along y.  Graphs laid out in another
;;;; direction are turned into this frame on the way in and back on the way out
;;;; (DIRECTION below), so every phase is written once.
;;;;
;;;; Every edge end is a port.  Edges attached to a node without a port get a
;;;; port of their own, so ordering and routing never special-case nodes.

(in-package #:trellis)

(defstruct (lnode (:constructor make-lnode (&key kind origin width height)))
  (kind :normal)            ; :normal :long-edge :label :external
  origin                    ; the node, edge or port this stands for
  (x 0d0 :type double-float) (y 0d0 :type double-float)
  (width 0d0 :type double-float) (height 0d0 :type double-float)
  (margin (pad))            ; room for labels and ports around the box
  (ports '())               ; lports
  (layer -1 :type fixnum)
  (order 0 :type fixnum)
  (constraint nil)          ; nil :first :last :first-separate :last-separate
  (partition nil)
  (port-constraints :free)  ; :free :fixed-side :fixed-order :fixed-pos
  (index 0 :type fixnum)    ; position in the lgraph's node list
  (model-order 0)           ; order in the input
  (component 0)
  (scratch nil))            ; plist for phase scratch

(defstruct (lport (:constructor make-lport (&key node origin side)))
  node origin
  (side :east)
  (x 0d0 :type double-float) (y 0d0 :type double-float)   ; relative to node
  (width 0d0 :type double-float) (height 0d0 :type double-float)
  (incoming '()) (outgoing '())
  (fixed nil)               ; position given and kept
  (rank 0d0 :type double-float))

(defstruct (ledge (:constructor make-ledge (&key source target origin)))
  source target             ; lports
  origin                    ; the graph edge, for chains the same for each piece
  (reversed nil)
  (points '())              ; route, internal coordinates
  (thickness 1d0)
  (weight 1d0)
  (labels '())
  (source-via nil)          ; the north/south port this piece really starts at
  (target-via nil))         ; ... or ends at, when routed through a port dummy

(defstruct (lgraph (:constructor make-lgraph ()))
  (nodes '())
  (edges '())
  (layers #())              ; vector of vectors of lnodes
  (self-loops '())
  options-node              ; the graph node whose options apply
  (direction :right)
  (spacing nil)             ; plist of spacings
  (chains (make-hash-table :test 'eq)) ; graph edge -> its ledges, source to target
  (next-index 0)
  (gaps (make-hash-table))  ; gap after layer i (-1: before the first) -> (x . width)
  (columns (make-hash-table)) ; layer i -> (x . width)
  (rng nil))

(defmethod print-object ((n lnode) s)
  (print-unreadable-object (n s :type t)
    (format s "~A ~A L~D/~D" (lnode-kind n)
            (let ((o (lnode-origin n)))
              (typecase o (node (node-id o)) (edge (edge-id o)) (port (port-id o)) (t "")))
            (lnode-layer n) (lnode-order n))))

(defun lnode-id (n)
  (let ((o (lnode-origin n)))
    (typecase o (node (node-id o)) (edge (edge-id o)) (port (port-id o)) (t "?"))))

(defun add-lport (node side &key origin)
  (let ((p (make-lport :node node :origin origin :side side)))
    (setf (lnode-ports node) (append (lnode-ports node) (list p)))
    p))

(defun connect (source target &key origin)
  "A new ledge from lport SOURCE to lport TARGET."
  (let ((e (make-ledge :source source :target target :origin origin)))
    (push e (lport-outgoing source))
    (push e (lport-incoming target))
    e))

(defun disconnect (e)
  (setf (lport-outgoing (ledge-source e)) (remove e (lport-outgoing (ledge-source e)))
        (lport-incoming (ledge-target e)) (remove e (lport-incoming (ledge-target e)))))

(defun reverse-ledge (e)
  "Turn E around, remembering that it was."
  (disconnect e)
  (rotatef (ledge-source e) (ledge-target e))
  (push e (lport-outgoing (ledge-source e)))
  (push e (lport-incoming (ledge-target e)))
  (setf (ledge-reversed e) (not (ledge-reversed e)))
  e)

(defun src-node (e) (lport-node (ledge-source e)))
(defun tgt-node (e) (lport-node (ledge-target e)))

(defun node-outgoing (n) (loop for p in (lnode-ports n) append (lport-outgoing p)))
(defun node-incoming (n) (loop for p in (lnode-ports n) append (lport-incoming p)))

(defun dummy-p (n) (not (eq (lnode-kind n) :normal)))

(defun lnode-data (n key &optional default) (getf (lnode-scratch n) key default))
(defun (setf lnode-data) (v n key) (setf (getf (lnode-scratch n) key) v))

;;; ---- port anchors ------------------------------------------------------------

(defun port-anchor (p)
  "Where an edge meets port P, relative to its node: the middle of the port's
   outer side."
  (let ((x (lport-x p)) (y (lport-y p)) (w (lport-width p)) (h (lport-height p)))
    (ecase (lport-side p)
      (:east (pt (+ x w) (+ y (/ h 2))))
      (:west (pt x (+ y (/ h 2))))
      (:north (pt (+ x (/ w 2)) y))
      (:south (pt (+ x (/ w 2)) (+ y h))))))

(defun port-offset (p)
  "P's anchor along the in-layer axis (y), relative to its node's top."
  (py (port-anchor p)))

(defun port-abs (p)
  (let ((a (port-anchor p)) (n (lport-node p)))
    (pt (+ (lnode-x n) (px a)) (+ (lnode-y n) (py a)))))

;;; ---- directions ----------------------------------------------------------------
;;;
;;; Internal frame: layers left to right.  For each direction, how a box and a
;;; side map between the graph's frame ("outer") and the internal one.

(defun layered-direction (node)
  (let ((d (opt-enum node :direction :right '(:right :left :down :up))))
    d))

(defun transposed-p (dir) (member dir '(:down :up)))

(defun side-in (dir side)
  "An outer side as an internal side."
  (ecase dir
    (:right side)
    (:left (case side (:east :west) (:west :east) (t side)))
    (:down (ecase side (:north :west) (:south :east) (:west :north) (:east :south)))
    (:up (ecase side (:south :west) (:north :east) (:west :north) (:east :south)))))

(defun side-out (dir side)
  "An internal side as an outer side."
  (or (find-if (lambda (s) (eq (side-in dir s) side)) '(:north :east :south :west))
      side))

(defun size-in (dir w h) (if (transposed-p dir) (values h w) (values w h)))

(defun pad-in (dir p)
  "Outer margins/padding P in the internal frame."
  (let ((top (pad-top p)) (left (pad-left p)) (bottom (pad-bottom p)) (right (pad-right p)))
    (ecase dir
      (:right (pad top left bottom right))
      (:left (pad top right bottom left))
      (:down (pad left top right bottom))
      (:up (pad left bottom right top)))))

(defun box-in (dir x y w h fw fh)
  "A box (x y w h) inside an outer frame of size FW x FH, as an internal box."
  (ecase dir
    (:right (values x y w h))
    (:left (values (- fw x w) y w h))
    (:down (values y x h w))
    (:up (values (- fh y h) x h w))))

(defun box-out (dir x y w h fw fh)
  "An internal box inside an internal frame of size FW x FH, as an outer box."
  (ecase dir
    (:right (values x y w h))
    (:left (values (- fw x w) y w h))
    (:down (values y x h w))
    (:up (values y (- fw x w) h w))))

(defun point-out (dir p fw fh)
  (multiple-value-bind (x y) (box-out dir (px p) (py p) 0d0 0d0 fw fh)
    (pt x y)))
