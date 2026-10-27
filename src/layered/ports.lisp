;;;; ports.lisp -- edges at ports that do not face the next layer.
;;;;
;;;; Schulze, Spönemann and von Hanxleden (2014), Section 4, the global
;;;; approach: such edges get dummy nodes that crossing reduction places along
;;;; with everything else, instead of being routed around their node locally.
;;;;
;;;; Inverted ports (4.1).  An edge leaving a west-side port, or entering an
;;;; east-side one, is split by a dummy in the port's own layer: the piece
;;;; between port and dummy is an in-layer edge, routed in the space beside the
;;;; layer; the rest is an ordinary edge.
;;;;
;;;; North and south ports (4.2, the fixed-order approach).  A node's edges at
;;;; those sides are moved to dummies stacked above (north) or below (south)
;;;; it, which give the heights the edges turn at.  An incoming and an outgoing
;;;; port can share a dummy; the outermost ports get the outermost dummies, so
;;;; the edges nest instead of crossing.  The node and its dummies form a layout
;;;; unit that crossing reduction keeps together and in order.

(in-package #:trellis)

(defun in-layer-p (e) (= (lnode-layer (src-node e)) (lnode-layer (tgt-node e))))

(defun cross-layer (edges) (remove-if #'in-layer-p edges))

(defun port-dummy (lg kind layer thickness)
  (let* ((w (make-lnode :kind kind :width 0d0 :height thickness))
         (in (add-lport w :west)) (out (add-lport w :east)))
    (dolist (p (list in out)) (setf (lport-y p) (/ thickness 2) (lport-fixed p) t))
    (setf (lnode-port-constraints w) :fixed-pos (lnode-layer w) layer)
    (add-lnode lg w)
    (values w in out)))

(defun insert-into-layer (lg node after)
  "Put NODE into its layer right after AFTER (or at the end)."
  (let* ((layer (aref (lgraph-layers lg) (lnode-layer node)))
         (pos (if after (1+ (position after layer)) (length layer))))
    (vector-push-extend node layer)
    (replace layer layer :start1 (1+ pos) :start2 pos :end2 (1- (length layer)))
    (setf (aref layer pos) node)
    (loop for n across layer for i from 0 do (setf (lnode-order n) i))))

;;; ---- inverted ports ------------------------------------------------------------

(defun insert-inverted-port-dummies (lg)
  (dolist (e (copy-list (lgraph-edges lg)))
    (unless (in-layer-p e)
      (let ((s (src-node e)))
        (when (and (eq (lport-side (ledge-source e)) :west) (eq (lnode-kind s) :normal))
          (multiple-value-bind (w in out) (port-dummy lg :inverted (lnode-layer s) (ledge-thickness e))
            (setf (lnode-data w :partner) s (lnode-model-order w) (lnode-model-order s))
            (insert-into-layer lg w s)
            ;; port -> w is now in-layer, w -> target the ordinary piece
            (split-ledge lg e w in out))))))
  (dolist (e (copy-list (lgraph-edges lg)))
    (unless (in-layer-p e)
      (let ((tn (tgt-node e)))
        (when (and (eq (lport-side (ledge-target e)) :east) (eq (lnode-kind tn) :normal))
          (multiple-value-bind (w in out) (port-dummy lg :inverted (lnode-layer tn) (ledge-thickness e))
            (setf (lnode-data w :partner) tn (lnode-model-order w) (lnode-model-order tn))
            (insert-into-layer lg w tn)
            (split-ledge lg e w in out)))))))

;;; ---- north and south ports -------------------------------------------------------

(defun side-ports-left-to-right (n side)
  (let ((ps (remove side (lnode-ports n) :key #'lport-side :test-not #'eq)))
    ;; clockwise order runs north left-to-right, south right-to-left
    (if (eq side :south) (reverse ps) ps)))

(defun order-free-side-ports (n side)
  "Ports on SIDE of a node whose order is free: incoming ones toward the
   preceding layer, outgoing toward the next (Schulze et al., by the balance
   of incoming and outgoing edges)."
  (let* ((ps (side-ports-left-to-right n side))
         (sorted (stable-sort (copy-list ps) #'<
                              :key (lambda (p) (- (length (lport-outgoing p)) (length (lport-incoming p)))))))
    (unless (equal ps sorted)
      (let ((clockwise (if (eq side :south) (reverse sorted) sorted)))
        (setf (lnode-ports n)
              (loop for p in (lnode-ports n)
                    collect (if (eq (lport-side p) side) (pop clockwise) p)))))))

(defun insert-north-south-dummies (lg)
  (dolist (v (copy-list (lgraph-nodes lg)))
    (when (eq (lnode-kind v) :normal)
      (let ((above '()) (below '()))
        (dolist (side '(:north :south))
          (unless (fixed-port-order-p v) (order-free-side-ports v side))
          (let* ((ps (remove-if-not (lambda (p) (or (cross-layer (lport-incoming p)) (cross-layer (lport-outgoing p))))
                                    (side-ports-left-to-right v side)))
                 ;; incoming edges come from the left: the rightmost port
                 ;; turns outermost; outgoing edges leave to the right: the
                 ;; leftmost turns outermost
                 (ins (remove-if-not (lambda (p) (cross-layer (lport-incoming p))) ps))
                 (outs (reverse (remove-if-not (lambda (p) (cross-layer (lport-outgoing p))) ps)))
                 (levels (max (length ins) (length outs)))
                 (dummies '()))
            (dotimes (k levels)
              (let ((pin (nth k ins)) (po (nth k outs)))
                (multiple-value-bind (w in out)
                    (port-dummy lg :north-south (lnode-layer v)
                                (max 1d0 (reduce #'max (append (and pin (lport-incoming pin)) (and po (lport-outgoing po)))
                                                 :key #'ledge-thickness :initial-value 0d0)))
                  (setf (lnode-data w :owner) v (lnode-model-order w) (lnode-model-order v))
                  (when pin
                    (dolist (e (cross-layer (lport-incoming pin)))
                      (setf (lport-incoming pin) (remove e (lport-incoming pin))
                            (ledge-target e) in (ledge-target-via e) pin)
                      (push e (lport-incoming in))))
                  (when po
                    (dolist (e (cross-layer (lport-outgoing po)))
                      (setf (lport-outgoing po) (remove e (lport-outgoing po))
                            (ledge-source e) out (ledge-source-via e) po)
                      (push e (lport-outgoing out))))
                  (push w dummies))))
            ;; DUMMIES is outermost first
            (if (eq side :north)
                (setf above dummies)
                (setf below (reverse dummies)))))
        (when (or above below)
          (let ((unit (append above (list v) below)))
            (setf (lnode-data v :unit) unit)
            (dolist (w unit) (unless (eq w v) (setf (lnode-data w :unit-owner) v)))
            ;; put the unit together in the layer, in order
            (let* ((layer (aref (lgraph-layers lg) (lnode-layer v)))
                   (rest (remove-if (lambda (n) (member n unit)) (coerce layer 'list)))
                   (pos (position v layer))
                   (before (count-if (lambda (n) (and (not (member n unit)) (< (position n layer) pos))) layer)))
              (dolist (w unit) (unless (or (eq w v) (find w layer)) (vector-push-extend w layer)))
              (let ((new (append (subseq rest 0 before) unit (nthcdr before rest))))
                (loop for n in new for i from 0 do (setf (aref layer i) n (lnode-order n) i))))))))))

(defun unit-members (n)
  "The layout unit N leads, or NIL if N leads none."
  (lnode-data n :unit))

(defun unit-member-p (n)
  (or (lnode-data n :unit) (lnode-data n :unit-owner)))

(defun regroup-units (layer)
  "Make every layout unit in LAYER contiguous again, at its owner's place."
  (when (some #'unit-member-p layer)
    (let ((out '()))
      (loop for n across layer
            do (cond ((lnode-data n :unit-owner))
                     ((unit-members n) (dolist (m (unit-members n)) (push m out)))
                     (t (push n out))))
      (loop for n in (nreverse out) for i from 0 do (setf (aref layer i) n (lnode-order n) i)))))

;;; ---- ports joined by in-layer edges ---------------------------------------------------

(defun order-in-layer-ports (lg)
  "Ports joined to dummies of their own layer go to the ends of their side,
   ordered so the in-layer edges nest (Schulze et al., Figure 10): partners
   above toward the top, nearest first; partners below toward the bottom,
   nearest last."
  (loop for layer across (lgraph-layers lg)
        do (loop for n across layer
                 when (and (eq (lnode-kind n) :normal) (not (fixed-port-order-p n)))
                   do (dolist (side '(:east :west))
                        (let* ((ps (remove side (lnode-ports n) :key #'lport-side :test-not #'eq))
                               (top-down (if (eq side :west) (reverse ps) ps)))
                          (when (some (lambda (p) (some #'in-layer-p (append (lport-incoming p) (lport-outgoing p)))) ps)
                            (let ((keyed
                                    (loop for p in top-down for i from 0
                                          collect (let* ((partners (loop for e in (append (lport-incoming p) (lport-outgoing p))
                                                                         when (in-layer-p e)
                                                                           collect (lnode-order (if (eq (src-node e) n) (tgt-node e) (src-node e))))))
                                                    (cons (if partners
                                                              (let ((d (- (lnode-order n) (/ (reduce #'+ partners) (length partners)))))
                                                                (if (> d 0) (+ -100000 d) (+ 100000 d)))
                                                              i)
                                                          p)))))
                              (let* ((sorted (mapcar #'cdr (stable-sort keyed #'< :key #'car)))
                                     (clockwise (if (eq side :west) (reverse sorted) sorted)))
                                (setf (lnode-ports n)
                                      (loop for p in (lnode-ports n)
                                            collect (if (eq (lport-side p) side) (pop clockwise) p)))))))))))
