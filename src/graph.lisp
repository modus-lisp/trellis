;;;; graph.lisp -- the graph model.
;;;;
;;;; A graph is its root NODE.  Nodes hold child nodes, ports, labels and the
;;;; edges they contain.  Coordinates follow ELK: a node's X/Y are relative to
;;;; its parent's top-left corner, a port's and a node label's relative to their
;;;; node, a port label's to its port, and an edge's sections and labels to the
;;;; node that contains the edge.

(in-package #:trellis)

(defstruct (element (:constructor nil))
  (id nil)
  (options nil)          ; hash-table keyword -> parsed value, or NIL
  (raw-options '())      ; ((json-key . json-value) ...) as read, for writing back
  (labels '()))

(defstruct (shape (:include element) (:constructor nil))
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (width 0d0 :type double-float)
  (height 0d0 :type double-float)
  (positioned nil))      ; true when the input gave a position

(defstruct (node (:include shape) (:constructor %make-node))
  (parent nil)
  (children '())
  (ports '())
  (edges '())
  (ties '()))           ; wire ties drawn across bundled edges: ((x1 . y1) . (x2 . y2))

(defstruct (port (:include shape) (:constructor %make-port))
  (node nil))

(defstruct (label (:include shape) (:constructor %make-label))
  (text "")
  (owner nil))

(defstruct (edge (:include element) (:constructor %make-edge))
  (sources '())
  (targets '())
  (sections '())
  (container nil))

(defstruct (section (:constructor make-section (start end &optional bends curve)))
  start end (bends '())
  (curve nil))          ; true when the points are cubic Bezier control points

(defun section-points (s)
  "The section's points: start, bends, end.  For a curve these are Bezier
   control points: start, then (control control point) for each piece."
  (append (list (section-start s)) (section-bends s) (list (section-end s))))

(defun bezier-point (p0 p1 p2 p3 u)
  (let* ((v (- 1 u)) (a (* v v v)) (b (* 3 v v u)) (c (* 3 v u u)) (e (* u u u)))
    (pt (+ (* a (px p0)) (* b (px p1)) (* c (px p2)) (* e (px p3)))
        (+ (* a (py p0)) (* b (py p1)) (* c (py p2)) (* e (py p3))))))

(defun section-path (s &key (steps 8))
  "The section as a polyline: its points, or a curve sampled STEPS times per
   Bezier piece."
  (let ((pts (section-points s)))
    (if (and (section-curve s) (= 1 (mod (length pts) 3)))
        (cons (first pts)
              (loop for (p0 p1 p2 p3) on pts by #'cdddr
                    while p3
                    append (loop for i from 1 to steps
                                 collect (bezier-point p0 p1 p2 p3 (/ i (d steps))))))
        pts)))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t) (format stream "~A" (node-id n))))
(defmethod print-object ((p port) stream)
  (print-unreadable-object (p stream :type t) (format stream "~A" (port-id p))))
(defmethod print-object ((e edge) stream)
  (print-unreadable-object (e stream :type t) (format stream "~A" (edge-id e))))
(defmethod print-object ((l label) stream)
  (print-unreadable-object (l stream :type t) (format stream "~S" (label-text l))))

;;; ---- building --------------------------------------------------------------

(defvar *id-counter* 0)

(defun fresh-id (prefix)
  (format nil "~A~D" prefix (incf *id-counter*)))

(defun apply-options (element options)
  (loop for (k v) on options by #'cddr do (set-option element k v))
  element)

(defun make-graph (&rest options &key id &allow-other-keys)
  "A new, empty graph: its root node.  OPTIONS are layout options, e.g.
   (make-graph :algorithm :layered :direction :down)."
  (remf options :id)
  (apply-options (%make-node :id (or id "root")) options))

(defun %place (shape x y width height)
  (when (or x y) (setf (shape-positioned shape) t))
  (setf (shape-x shape) (sane x) (shape-y shape) (sane y)
        (shape-width shape) (sane width) (shape-height shape) (sane height))
  shape)

(defun add-node (parent &key id x y (width 0) (height 0) labels options)
  "Add a child node to PARENT.  LABELS is a list of strings; OPTIONS a plist."
  (let ((n (%place (%make-node :id (or id (fresh-id "n")) :parent parent)
                   x y width height)))
    (setf (node-children parent) (append (node-children parent) (list n)))
    (dolist (l labels) (add-label n l))
    (apply-options n options)))

(defun add-port (node &key id x y (width 0) (height 0) side labels options)
  "Add a port to NODE.  SIDE is :north, :east, :south or :west."
  (let ((p (%place (%make-port :id (or id (fresh-id "p")) :node node)
                   x y width height)))
    (setf (node-ports node) (append (node-ports node) (list p)))
    (when side (set-option p :port-side side))
    (dolist (l labels) (add-label p l))
    (apply-options p options)))

(defun add-label (element text &key id x y width height options)
  "Add a label to ELEMENT.  Without WIDTH/HEIGHT its size is estimated from TEXT."
  (let ((l (%place (%make-label :id id :text (or text "") :owner element)
                   x y (or width (text-width text)) (or height (text-height text)))))
    (setf (element-labels element) (append (element-labels element) (list l)))
    (apply-options l options)))

(defun add-edge (parent source target &key id labels options)
  "Add an edge from SOURCE to TARGET (nodes or ports) contained in PARENT."
  (let ((e (%make-edge :id (or id (fresh-id "e")) :container parent
                       :sources (if (listp source) source (list source))
                       :targets (if (listp target) target (list target)))))
    (setf (node-edges parent) (append (node-edges parent) (list e)))
    (dolist (l labels) (add-label e l))
    (apply-options e options)))

(defun text-width (text) (* 7d0 (length (or text ""))))
(defun text-height (text) (declare (ignore text)) 15d0)

;;; ---- walking ---------------------------------------------------------------

(defun map-nodes (function graph &key (include-root nil))
  "Call FUNCTION on every node below GRAPH, parents before children."
  (labels ((walk (n)
             (dolist (c (node-children n)) (funcall function c) (walk c))))
    (when include-root (funcall function graph))
    (walk graph)))

(defun all-edges (graph)
  (let ((out '()))
    (map-nodes (lambda (n) (dolist (e (node-edges n)) (push e out))) graph :include-root t)
    (nreverse out)))

(defun graph-root (element)
  (loop for n = (element-node element) then (node-parent n)
        when (null (node-parent n)) return n))

(defun element-node (element)
  "The node an element belongs to: itself for a node, the owner otherwise."
  (etypecase element
    (node element)
    (port (port-node element))
    (edge (edge-container element))
    (label (element-node (label-owner element)))))

(defun find-element (graph id)
  "The node, port, edge or label with ID in GRAPH, or NIL."
  (labels ((in-labels (el) (find id (element-labels el) :key #'element-id :test #'equal))
           (walk (n)
             (when (equal (node-id n) id) (return-from find-element n))
             (let ((l (in-labels n))) (when l (return-from find-element l)))
             (dolist (p (node-ports n))
               (when (equal (port-id p) id) (return-from find-element p))
               (let ((l (in-labels p))) (when l (return-from find-element l))))
             (dolist (e (node-edges n))
               (when (equal (edge-id e) id) (return-from find-element e))
               (let ((l (in-labels e))) (when l (return-from find-element l))))
             (mapc #'walk (node-children n))))
    (walk graph)
    nil))

(defun ancestor-p (a n)
  "True when node A is N or one of N's ancestors."
  (loop for x = n then (node-parent x) while x thereis (eq x a)))

(defun depth (n) (loop for x = (node-parent n) then (node-parent x) while x count t))

(defun absolute-position (element)
  "The root-relative position of ELEMENT's origin, as (x . y).  The root's own
   position does not count, as in ELK."
  (etypecase element
    (node (let ((x 0d0) (y 0d0))
            (loop for n = element then (node-parent n)
                  while (node-parent n)
                  do (incf x (node-x n)) (incf y (node-y n)))
            (pt x y)))
    (port (pt+ (absolute-position (port-node element))
               (pt (port-x element) (port-y element))))))

(defun content-origin (node)
  "Root-relative origin of NODE's coordinate system for its children and edges."
  (if (node-parent node) (absolute-position node) (pt 0 0)))

(defun endpoint-node (endpoint)
  (if (port-p endpoint) (port-node endpoint) endpoint))
