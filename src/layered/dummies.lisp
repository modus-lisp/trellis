;;;; dummies.lisp -- dummy nodes: edge labels and long edges.
;;;;
;;;; An edge with centre labels is split by a dummy node as large as its labels,
;;;; so a layer makes room for them; the edge then spans at least two layers.
;;;; After layering, an edge spanning more than one layer is split into a chain
;;;; of long-edge dummies, one per layer crossed, so every edge connects
;;;; neighbouring layers (Sugiyama et al.).  Each graph edge keeps its pieces,
;;;; in order, in the lgraph's CHAINS.

(in-package #:trellis)

(defun chain (lg origin) (gethash origin (lgraph-chains lg)))

(defun init-chains (lg)
  (clrhash (lgraph-chains lg))
  (dolist (e (lgraph-edges lg))
    (setf (gethash (ledge-origin e) (lgraph-chains lg)) (list e))))

(defun split-ledge (lg e node in-port out-port)
  "Route E through NODE: E now ends at IN-PORT and a new piece runs from
   OUT-PORT to E's old target.  Returns the new piece."
  (let* ((target (ledge-target e))
         (rest (make-ledge :source out-port :target target :origin (ledge-origin e))))
    (setf (lport-incoming target) (remove e (lport-incoming target))
          (ledge-target e) in-port)
    (push e (lport-incoming in-port))
    (push rest (lport-outgoing out-port))
    (push rest (lport-incoming target))
    (setf (ledge-reversed rest) (ledge-reversed e)
          (ledge-thickness rest) (ledge-thickness e)
          (ledge-weight rest) (ledge-weight e))
    (push rest (lgraph-edges lg))
    ;; keep the chain in source-to-target order of the internal direction
    (let* ((c (chain lg (ledge-origin e))) (pos (position e c)))
      (setf (gethash (ledge-origin e) (lgraph-chains lg))
            (append (subseq c 0 (1+ pos)) (list rest) (subseq c (1+ pos)))))
    rest))

(defun add-lnode (lg node)
  (setf (lnode-index node) (lgraph-next-index lg))
  (incf (lgraph-next-index lg))
  (push node (lgraph-nodes lg))
  node)

(defun insert-label-dummies (lg)
  "Split edges with centre labels by a node holding the labels."
  (let* ((dir (lgraph-direction lg))
         (spacing (getf (lgraph-spacing lg) :edge-label 2d0))
         (label-label (getf (lgraph-spacing lg) :label-label 0d0)))
    (dolist (e (copy-list (lgraph-edges lg)))
      (let ((labels (ledge-labels e)))
        (when labels
          (let* ((ow (reduce #'max labels :key (lambda (l) (max 0d0 (label-width l)))))
                 (oh (+ (reduce #'+ labels :key (lambda (l) (max 0d0 (label-height l))))
                        (* label-label (1- (length labels))))))
            (multiple-value-bind (w h) (size-in dir ow oh)
              (let* ((thick (ledge-thickness e))
                     (d (make-lnode :kind :label :origin (ledge-origin e)
                                    :width w :height (+ h spacing thick)))
                     (in (add-lport d :west)) (out (add-lport d :east)))
                (setf (lnode-port-constraints d) :fixed-pos
                      (lnode-data d :labels) labels
                      (lnode-data d :label-size) (list w h))
                ;; the edge runs along the dummy's far side from the labels
                (dolist (p (list in out))
                  (setf (lport-y p) (+ h spacing (/ thick 2)) (lport-fixed p) t))
                (setf (lport-x out) w)
                (setf (lnode-model-order d) (lnode-model-order (src-node e)))
                (add-lnode lg d)
                (split-ledge lg e d in out)))))))))

(defun insert-long-edge-dummies (lg)
  "Split every edge spanning more than one layer."
  (dolist (e (copy-list (lgraph-edges lg)))
    (let ((from (lnode-layer (src-node e))) (to (lnode-layer (tgt-node e)))
          (piece e))
      (loop for layer from (1+ from) below to
            do (let* ((thick (ledge-thickness piece))
                      (d (make-lnode :kind :long-edge :origin (ledge-origin piece)
                                     :width 0d0 :height thick))
                      (in (add-lport d :west)) (out (add-lport d :east)))
                 (setf (lnode-port-constraints d) :fixed-pos
                       (lnode-layer d) layer
                       (lnode-model-order d) (lnode-model-order (src-node e)))
                 (dolist (p (list in out))
                   (setf (lport-y p) (/ thick 2) (lport-fixed p) t))
                 (add-lnode lg d)
                 (let ((layer-vec (aref (lgraph-layers lg) layer)))
                   (setf (lnode-order d) (fill-pointer layer-vec))
                   (vector-push-extend d layer-vec))
                 (setf piece (split-ledge lg piece d in out)))))))
