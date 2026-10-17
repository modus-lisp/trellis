;;;; layout.lisp -- the driver: which container owns each edge, bottom-up
;;;; recursion through the hierarchy, defaults, labels.

(in-package #:diagram)

(defun ancestors (n)
  (loop for p = (node-parent n) then (node-parent p) while p collect p))

(defun lift (n container)
  "The child of CONTAINER that is N or contains N."
  (loop for x = n then (node-parent x)
        while x
        when (eq (node-parent x) container) return x))

(defun assign-edge-containers (root)
  "Hash container -> list of (edge src-child tgt-child src-port tgt-port).
An edge belongs to the deepest node that properly contains both endpoints;
an endpoint deeper than that is LIFTED to the child that holds it (the edge
then attaches to that child's boundary).  Edges between a node and its own
descendant are hierarchical and are not routed."
  (let ((map (make-hash-table :test #'eq)))
    (labels ((walk (n)
               (dolist (e (node-edges n))
                 (setf (edge-sections e) nil (edge-container e) nil)
                 (dolist (s (edge-sources e))
                   (dolist (tg (edge-targets e))
                     (let* ((sn (element-node s)) (tn (element-node tg))
                            (tanc (ancestors tn))
                            (lca (find-if (lambda (a) (member a tanc)) (ancestors sn))))
                       (when lca
                         (let ((ls (lift sn lca)) (lt (lift tn lca)))
                           (unless (and (eq ls lt) (not (eq sn tn)))
                             (push (list e ls lt
                                         (and (port-p s) (eq ls sn) s)
                                         (and (port-p tg) (eq lt tn) tg))
                                   (gethash lca map)))))))))
               (mapc #'walk (node-children n))))
      (walk root))
    (maphash (lambda (k v) (setf (gethash k map) (nreverse v))) map)
    map))

(defun apply-default-sizes (n)
  "ELK leaves an unsized leaf at 0x0.  A leaf with no size gets one that
fits its first label, which is what a hand-written graph means."
  (unless (node-children n)
    (let ((l (first (node-labels n))))
      (unless (node-width n) (setf (node-width n) (if l (+ 20d0 (label-width l)) 40d0)))
      (unless (node-height n) (setf (node-height n) (if l (+ 16d0 (label-height l)) 30d0)))))
  (mapc #'apply-default-sizes (node-children n)))

(defun place-node-labels (n)
  (dolist (l (node-labels n))
    (unless (label-x l)
      (setf (label-x l) (/ (- (or (node-width n) 0d0) (label-width l)) 2)))
    (unless (label-y l)
      (setf (label-y l) (if (node-children n)
                            4d0
                            (/ (- (or (node-height n) 0d0) (label-height l)) 2)))))
  (mapc #'place-node-labels (node-children n)))

(defun place-edge-labels (root)
  (labels ((walk (n)
             (dolist (e (node-edges n))
               (let ((s (first (edge-sections e))))
                 (when (and s (edge-labels e))
                   (let* ((pts (append (list (section-start s)) (section-bends s) (list (section-end s))))
                          (best nil) (best-len -1d0))
                     (loop for (a b) on pts while b
                           do (let ((d (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b))))))
                                (when (> d best-len) (setf best-len d best (cons a b)))))
                     (let ((mx (/ (+ (car (car best)) (car (cdr best))) 2))
                           (my (/ (+ (cdr (car best)) (cdr (cdr best))) 2))
                           (off 2d0))
                       (dolist (l (edge-labels e))
                         (unless (label-x l) (setf (label-x l) (- mx (/ (label-width l) 2))))
                         (unless (label-y l) (setf (label-y l) (- my (label-height l) off)))
                         (incf off (+ 2d0 (label-height l)))))))))
             (mapc #'walk (node-children n))))
    (walk root)))

(defun layout (graph &key (algorithm nil))
  "Lay out GRAPH (a NODE, a parsed JSON object, or a JSON string) in place.
Returns the NODE."
  (unless (node-p graph) (setf graph (json->graph graph)))
  (when algorithm (push (cons "algorithm" (string algorithm)) (node-options graph)))
  (apply-default-sizes graph)
  (let ((edges (assign-edge-containers graph)))
    (labels ((run (n)
               (mapc #'run (node-children n))
               (when (node-children n)
                 (let ((infos (gethash n edges))
                       (alg (string-downcase (string (or (option n "algorithm") "layered")))))
                   (cond ((search "fixed" alg) (layout-fixed n infos))
                         ((or (search "box" alg) (search "rectpacking" alg)) (layout-box n infos))
                         (t (layout-layered n infos)))))))
      (run graph)))
  (unless (node-x graph) (setf (node-x graph) 0d0))
  (unless (node-y graph) (setf (node-y graph) 0d0))
  (place-node-labels graph)
  (place-edge-labels graph)
  graph)

(defun layout-json (json-string &key (indent 2))
  "elkjs-style: JSON graph in, laid-out JSON graph out."
  (write-json (graph->json (layout (json->graph json-string))) nil indent))
