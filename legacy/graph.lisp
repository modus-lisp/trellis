;;;; graph.lisp -- the ELK graph model: nodes, ports, labels, edges, sections.
;;;;
;;;; Mirrors ELK's JSON format (https://eclipse.dev/elk/documentation/tooldevelopers/graphdatastructure/jsonformat.html):
;;;; a node has children, ports, labels and edges; an edge has sources and
;;;; targets (node or port ids) and, after layout, sections of points.
;;;; Positions are relative to the parent node; edge points are relative to
;;;; the edge's CONTAINER, which layout computes and reports.

(in-package #:diagram)

(defstruct (node (:constructor %make-node))
  id (x nil) (y nil) (width nil) (height nil)
  children ports labels edges
  options parent json)

(defstruct (port (:constructor %make-port))
  id (x nil) (y nil) (width 0d0) (height 0d0)
  labels options owner json)

(defstruct (label (:constructor %make-label))
  text (x nil) (y nil) (width nil) (height nil) json)

(defstruct (edge (:constructor %make-edge))
  id sources targets labels sections container options json)

(defstruct (section (:constructor %make-section))
  id start bends end source target)

;;; Options.  Keys are normalised so "org.eclipse.elk.direction", "elk.direction"
;;; and "direction" are the same option, and "elk.layered.spacing.x" may be
;;; written "spacing.x".  Lookup walks up the parent chain.

(defun normalize-option-key (key)
  (let ((k (string-downcase (if (symbolp key) (symbol-name key) (princ-to-string key)))))
    (loop for prefix in '("org.eclipse." "elk." "layered.")
          do (when (and (> (length k) (length prefix))
                        (string= prefix k :end2 (length prefix)))
               (setf k (subseq k (length prefix)))))
    (remove #\- k)))

(defun parse-options (json-opts)
  (when (json-object-p json-opts)
    (loop for (k . v) in (cdr json-opts)
          collect (cons (normalize-option-key k) v))))

(defun option (node key &optional default)
  (let ((k (normalize-option-key key)))
    (loop for n = node then (node-parent n)
          while n
          do (let ((cell (assoc k (node-options n) :test #'string=)))
               (when cell (return-from option (cdr cell)))))
    default))

(defun to-number (v default)
  (cond ((realp v) (float v 1d0))
        ((stringp v)
         (let ((*read-eval* nil))
           (let ((x (ignore-errors (read-from-string v))))
             (if (realp x) (float x 1d0) default))))
        (t default)))

(defun option-number (node key default)
  (to-number (option node key) default))

(defun option-keyword (node key default)
  (let ((v (option node key)))
    (if (or (stringp v) (symbolp v))
        (intern (string-upcase (string v)) :keyword)
        default)))

(defun option-padding (node default)
  "Returns (values top left bottom right)."
  (let ((v (option node "padding")))
    (cond ((realp v) (let ((p (float v 1d0))) (values p p p p)))
          ((stringp v)
           (let ((top default) (left default) (bottom default) (right default))
             (dolist (part (split-string (string-trim "[] " v) #\,))
               (let ((eq (position #\= part)))
                 (when eq
                   (let ((name (string-downcase (string-trim " " (subseq part 0 eq))))
                         (num (to-number (subseq part (1+ eq)) default)))
                     (cond ((string= name "top") (setf top num))
                           ((string= name "left") (setf left num))
                           ((string= name "bottom") (setf bottom num))
                           ((string= name "right") (setf right num)))))))
             (values top left bottom right)))
          (t (values default default default default)))))

(defun split-string (s ch)
  (loop with start = 0
        for p = (position ch s :start start)
        collect (subseq s start p)
        while p
        do (setf start (1+ p))))

;;; JSON -> graph

(defun json-id (v)
  (cond ((stringp v) v)
        ((null v) nil)
        (t (princ-to-string v))))

(defun json-number (v)
  (and (realp v) (float v 1d0)))

(defun json-list (v)
  (cond ((vectorp v) (coerce v 'list))
        ((null v) nil)
        (t (list v))))

(defun estimate-text-width (text) (* 7d0 (length text)))

(defun json->label (j)
  (let ((text (or (jget j "text") "")))
    (%make-label :text text
                 :x (json-number (jget j "x")) :y (json-number (jget j "y"))
                 :width (or (json-number (jget j "width")) (estimate-text-width text))
                 :height (or (json-number (jget j "height")) 14d0)
                 :json j)))

(defun json->graph (json)
  "Build a NODE tree from parsed ELK JSON (or a JSON string)."
  (when (stringp json) (setf json (parse-json json)))
  (let ((ids (make-hash-table :test #'equal))
        (pending '()))
    (labels ((build (j parent)
               (let ((n (%make-node :id (json-id (jget j "id")) :parent parent :json j
                                    :x (json-number (jget j "x")) :y (json-number (jget j "y"))
                                    :width (json-number (jget j "width"))
                                    :height (json-number (jget j "height"))
                                    :options (append (parse-options (jget j "layoutOptions"))
                                                     (parse-options (jget j "properties"))))))
                 (when (node-id n) (setf (gethash (node-id n) ids) n))
                 (setf (node-labels n) (mapcar #'json->label (json-list (jget j "labels"))))
                 (setf (node-ports n)
                       (loop for pj in (json-list (jget j "ports"))
                             collect (let ((p (%make-port
                                               :id (json-id (jget pj "id")) :owner n :json pj
                                               :x (json-number (jget pj "x"))
                                               :y (json-number (jget pj "y"))
                                               :width (or (json-number (jget pj "width")) 0d0)
                                               :height (or (json-number (jget pj "height")) 0d0)
                                               :options (parse-options (jget pj "layoutOptions"))
                                               :labels (mapcar #'json->label
                                                               (json-list (jget pj "labels"))))))
                                       (when (port-id p) (setf (gethash (port-id p) ids) p))
                                       p)))
                 (setf (node-children n)
                       (loop for cj in (json-list (jget j "children")) collect (build cj n)))
                 (setf (node-edges n)
                       (loop for ej in (json-list (jget j "edges"))
                             collect (let ((e (%make-edge
                                               :id (json-id (jget ej "id")) :json ej
                                               :options (parse-options (jget ej "layoutOptions"))
                                               :labels (mapcar #'json->label
                                                               (json-list (jget ej "labels"))))))
                                       (push (cons e ej) pending)
                                       e)))
                 n)))
      (let ((root (build json nil)))
        ;; Resolve endpoints once every id is known (edges may point forward).
        (flet ((resolve (id)
                 (or (gethash (json-id id) ids)
                     (error "Edge endpoint ~S does not name a node or port" id))))
          (loop for (e . ej) in pending
                do (setf (edge-sources e)
                         (mapcar #'resolve
                                 (or (json-list (jget ej "sources"))
                                     (json-list (or (jget ej "sourcePort") (jget ej "source")))))
                         (edge-targets e)
                         (mapcar #'resolve
                                 (or (json-list (jget ej "targets"))
                                     (json-list (or (jget ej "targetPort") (jget ej "target"))))))))
        root))))

;;; graph -> JSON (updates the original JSON objects in place, so fields this
;;; library does not know about survive a round trip)

(defun point->json (p) (jobject "x" (car p) "y" (cdr p)))

(defun graph->json (node)
  (labels ((obj (json-slot id)
             (or json-slot (jobject "id" id)))
           (put-box (j x y w h)
             (when x (jset j "x" x))
             (when y (jset j "y" y))
             (when w (jset j "width" w))
             (when h (jset j "height" h)))
           (labels-json (labels)
             (coerce (loop for l in labels
                           collect (let ((j (or (label-json l) (jobject "text" (label-text l)))))
                                     (put-box j (label-x l) (label-y l) (label-width l) (label-height l))
                                     j))
                     'vector))
           (emit (n)
             (let ((j (obj (node-json n) (node-id n))))
               (setf (node-json n) j)
               (put-box j (node-x n) (node-y n) (node-width n) (node-height n))
               (when (node-labels n) (jset j "labels" (labels-json (node-labels n))))
               (when (node-ports n)
                 (jset j "ports"
                       (coerce (loop for p in (node-ports n)
                                     collect (let ((pj (obj (port-json p) (port-id p))))
                                               (setf (port-json p) pj)
                                               (put-box pj (port-x p) (port-y p)
                                                        (port-width p) (port-height p))
                                               (when (port-labels p)
                                                 (jset pj "labels" (labels-json (port-labels p))))
                                               pj))
                               'vector)))
               (when (node-children n)
                 (jset j "children" (coerce (mapcar #'emit (node-children n)) 'vector)))
               (when (node-edges n)
                 (jset j "edges"
                       (coerce
                        (loop for e in (node-edges n)
                              collect (let ((ej (or (edge-json e)
                                                    (jobject "id" (edge-id e)
                                                             "sources" (coerce (mapcar #'element-id (edge-sources e)) 'vector)
                                                             "targets" (coerce (mapcar #'element-id (edge-targets e)) 'vector)))))
                                        (setf (edge-json e) ej)
                                        (when (edge-container e)
                                          (jset ej "container" (node-id (edge-container e))))
                                        (when (edge-sections e)
                                          (jset ej "sections"
                                                (coerce
                                                 (loop for s in (edge-sections e)
                                                       collect (let ((sj (jobject "id" (section-id s)
                                                                                  "startPoint" (point->json (section-start s))
                                                                                  "endPoint" (point->json (section-end s)))))
                                                                 (when (section-bends s)
                                                                   (jset sj "bendPoints"
                                                                         (coerce (mapcar #'point->json (section-bends s)) 'vector)))
                                                                 (when (section-source s)
                                                                   (jset sj "incomingShape" (section-source s)))
                                                                 (when (section-target s)
                                                                   (jset sj "outgoingShape" (section-target s)))
                                                                 sj))
                                                 'vector)))
                                        (when (edge-labels e) (jset ej "labels" (labels-json (edge-labels e))))
                                        ej))
                        'vector)))
               j)))
    (emit node)))

(defun element-id (x)
  (etypecase x (node (node-id x)) (port (port-id x))))

(defun element-node (x)
  (etypecase x (node x) (port (port-owner x))))

(defun find-element (root id)
  "Find the node or port named ID under ROOT."
  (labels ((walk (n)
             (when (equal (node-id n) id) (return-from find-element n))
             (dolist (p (node-ports n)) (when (equal (port-id p) id) (return-from find-element p)))
             (mapc #'walk (node-children n))))
    (walk root)
    nil))

;;; A Lisp-side constructor: plists in, JSON-shaped data out, then the normal
;;; parse.  (:id "a" :width 30 :height 30 :layout-options (:direction "DOWN"))

(defun camel-case (keyword)
  (let ((s (string-downcase (symbol-name keyword))) (up nil))
    (with-output-to-string (out)
      (loop for c across s
            do (cond ((char= c #\-) (setf up t))
                     (up (write-char (char-upcase c) out) (setf up nil))
                     (t (write-char c out)))))))

(defun plist->json (plist)
  (cons :object
        (loop for (k v) on plist by #'cddr
              collect (cons (camel-case k)
                            (case k
                              ((:children :ports :labels :edges)
                               (coerce (mapcar #'plist->json v) 'vector))
                              ((:sources :targets)
                               (coerce (mapcar #'json-id (if (listp v) v (list v))) 'vector))
                              ((:layout-options :properties)
                               (cons :object
                                     (loop for (ok ov) on v by #'cddr
                                           collect (cons (if (symbolp ok) (camel-case ok) ok)
                                                         (if (and ov (symbolp ov)) (symbol-name ov) ov)))))
                              (t v))))))

(defun plist->graph (plist) (json->graph (plist->json plist)))
