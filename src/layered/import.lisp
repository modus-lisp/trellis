;;;; import.lisp -- from a node's children to an lgraph.

(in-package #:trellis)

(defun edge-thickness (e) (max 0d0 (opt-num e :edge-thickness 1d0)))

(defun edge-label-placement (label)
  (opt-enum label :edge-labels-placement :center '(:center :head :tail)))

(defun node-port-constraints (node)
  "The port constraint level the layered algorithm works with for NODE."
  (let ((pc (port-constraints node)))
    (case pc
      ((:fixed-pos :fixed-ratio) :fixed-pos)
      (:fixed-order :fixed-order)
      (:fixed-side :fixed-side)
      ;; as in ELK, declared sides only count from fixed-side up
      (t :free))))

(defun layer-constraint (node)
  (let ((c (opt-enum node :layering-layer-constraint :none
                     '(:none :first :last :first-separate :last-separate))))
    (if (eq c :none) nil c)))

(defun import-layered (parent)
  "An lgraph for PARENT's children and the edges PARENT routes."
  (let* ((lg (make-lgraph))
         (dir (layered-direction parent))
         (map (make-hash-table :test 'eq))       ; node/port -> lnode/lport
         (externals (make-hash-table :test 'eq)) ; parent port -> lnode
         (partitioning (opt-bool parent :partitioning-activate nil))
         (nodes '()))
    (setf (lgraph-options-node lg) parent (lgraph-direction lg) dir)
    ;; nodes
    (loop for c in (layout-children parent) for i from 0
          do (micro-layout c parent)
             (multiple-value-bind (w h) (size-in dir (max 0d0 (node-width c)) (max 0d0 (node-height c)))
               (let ((n (make-lnode :kind :normal :origin c :width w :height h))
                     (pc (node-port-constraints c)))
                 (setf (lnode-margin n) (pad-in dir (node-margins c))
                       (lnode-constraint n) (layer-constraint c)
                       (lnode-partition n) (and partitioning (opt-int c :partitioning-partition nil))
                       (lnode-port-constraints n) pc
                       (lnode-model-order n) i
                       (gethash c map) n)
                 (dolist (p (node-ports c))
                   (let* ((side (port-side p))
                          (lp (add-lport n (if (or (eq side :undefined) (eq pc :free))
                                               :undecided
                                               (side-in dir side))
                                         :origin p)))
                     (when (and (eq side :undefined) (not (eq pc :free)))
                       (setf (lport-side lp) (side-in dir (infer-port-side p))))
                     (multiple-value-bind (x y pw ph)
                         (box-in dir (port-x p) (port-y p) (max 0d0 (port-width p)) (max 0d0 (port-height p))
                                 (max 0d0 (node-width c)) (max 0d0 (node-height c)))
                       (setf (lport-x lp) x (lport-y lp) y (lport-width lp) pw (lport-height lp) ph
                             (lport-fixed lp) (eq pc :fixed-pos)))
                     (setf (gethash p map) lp)))
                 (push n nodes))))
    ;; edges
    (labels ((external (endpoint as-source)
             ;; the lnode standing for PARENT or one of its ports
             (let ((key (if (port-p endpoint) endpoint (gensym))))
               (or (gethash key externals)
                   (let* ((side (if (port-p endpoint) (port-side endpoint) :undefined))
                          (n (make-lnode :kind :external :origin endpoint))
                          (first (case side
                                   (:west t) (:east nil) (t as-source))))
                     (multiple-value-bind (w h)
                         (if (port-p endpoint)
                             (size-in dir (max 0d0 (port-width endpoint)) (max 0d0 (port-height endpoint)))
                             (values 0d0 0d0))
                       (declare (ignore w))
                       (setf (lnode-height n) h))
                     (setf (lnode-constraint n) (if first :first-separate :last-separate)
                           (lnode-port-constraints n) :fixed-pos)
                     (let ((lp (add-lport n (if first :east :west) :origin endpoint)))
                       (setf (lport-y lp) 0d0 (lport-height lp) (lnode-height n) (lport-fixed lp) t))
                     (push n nodes)
                     (setf (gethash key externals) n)))))
           (endpoint-port (endpoint as-source)
             (cond ((eq endpoint parent) (first (lnode-ports (external endpoint as-source))))
                   ((and (port-p endpoint) (eq (port-node endpoint) parent))
                    (first (lnode-ports (external endpoint as-source))))
                   ((port-p endpoint) (gethash endpoint map))
                   (t (let ((n (gethash endpoint map)))
                        (and n (add-lport n :undecided)))))))
      (dolist (e (frame-edges parent))
        (let* ((s (first (edge-sources e))) (tt (first (edge-targets e))))
          (unless (or (excluded-p (endpoint-node s)) (excluded-p (endpoint-node tt))
                      (and (not (eq (endpoint-node s) parent)) (not (gethash (endpoint-node s) map)))
                      (and (not (eq (endpoint-node tt) parent)) (not (gethash (endpoint-node tt) map))))
            (let ((sp (endpoint-port s t)) (tp (endpoint-port tt nil)))
              (when (and sp tp)
                (if (eq (lport-node sp) (lport-node tp))
                    (push (list e sp tp) (lgraph-self-loops lg))
                    (let ((le (connect sp tp :origin e)))
                      (setf (ledge-thickness le) (edge-thickness e)
                            (ledge-labels le) (remove-if-not (lambda (l) (eq (edge-label-placement l) :center))
                                                             (edge-labels e)))
                      (push le (lgraph-edges lg))))))))))
    (setf (lgraph-nodes lg) (nreverse nodes)
          (lgraph-edges lg) (nreverse (lgraph-edges lg))
          (lgraph-self-loops lg) (nreverse (lgraph-self-loops lg)))
    (loop for n in (lgraph-nodes lg) for i from 0 do (setf (lnode-index n) i))
    (setf (lgraph-next-index lg) (length (lgraph-nodes lg)))
    lg))

(defun assign-free-port-sides (lg)
  "Ports whose side is still open go where their edges flow: west for more
   incoming, east otherwise (Schulze et al., Section 2)."
  (dolist (n (lgraph-nodes lg))
    (dolist (p (lnode-ports n))
      (when (eq (lport-side p) :undecided)
        (let ((in (length (lport-incoming p))) (out (length (lport-outgoing p))))
          (setf (lport-side p)
                (cond ((> in out) :west)
                      ((< in out) :east)
                      ((lport-origin p)
                       (let ((s (port-side* (lport-origin p))))
                         (side-in (lgraph-direction lg) s)))
                      (t :east))))))))
