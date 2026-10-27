;;;; json.lisp -- reading and writing ELK JSON.
;;;;
;;;; The format elkjs and ELK's JsonImporter use: a node has "id", "x", "y",
;;;; "width", "height", "children", "ports", "labels", "edges" and
;;;; "layoutOptions"; an edge has "sources"/"targets" (ids of nodes or ports),
;;;; or the older "source"/"target" with "sourcePort"/"targetPort".  Laid-out
;;;; edges get "sections" with startPoint, endPoint and bendPoints.
;;;;
;;;; Gson writes NaN and Infinity unquoted, which is not JSON; those tokens are
;;;; quoted before parsing and read as missing values.

(in-package #:trellis)

(defun quote-bare-numbers (text)
  "TEXT with bare NaN / Infinity / -Infinity tokens outside strings quoted."
  (if (not (or (search "NaN" text) (search "Infinity" text)))
      text
      (with-output-to-string (out)
        (let ((n (length text)) (i 0) (in-string nil))
          (loop while (< i n)
                do (let ((c (char text i)))
                     (cond (in-string
                            (write-char c out)
                            (cond ((char= c #\\)
                                   (when (< (1+ i) n) (write-char (char text (1+ i)) out))
                                   (incf i))
                                  ((char= c #\") (setf in-string nil))))
                           ((char= c #\") (setf in-string t) (write-char c out))
                           (t (let ((tok (find-if (lambda (tok)
                                                    (and (<= (+ i (length tok)) n)
                                                         (string= tok text :start2 i
                                                                           :end2 (+ i (length tok)))))
                                                  '("-Infinity" "Infinity" "NaN"))))
                                (if tok
                                    (progn (format out "\"~A\"" tok)
                                           (incf i (1- (length tok))))
                                    (write-char c out)))))
                     (incf i)))))))

(defun json-get (object key &optional default)
  (if (hash-table-p object)
      (multiple-value-bind (v found) (gethash key object)
        (if (and found (not (eq v 'null))) v default))
      default))

(defun json-num (object key)
  (let ((v (json-get object key)))
    (cond ((realp v) v)
          ((stringp v) (parse-number v))
          (t nil))))

(defun json-list (object key)
  (let ((v (json-get object key)))
    (if (vectorp v) (coerce v 'list) '())))

(defun json-id (v)
  (cond ((stringp v) v)
        ((integerp v) (princ-to-string v))
        ((realp v) (princ-to-string v))
        (t nil)))

(defun read-json-text (source)
  (etypecase source
    (string source)
    (pathname (with-open-file (in source :external-format :utf-8)
                (let ((s (make-string (file-length in))))
                  (subseq s 0 (read-sequence s in)))))
    (stream (with-output-to-string (out)
              (loop for line = (read-line source nil) while line
                    do (write-line line out))))))

(defun read-options (element object)
  (let ((opts (or (json-get object "layoutOptions") (json-get object "properties"))))
    (when (hash-table-p opts)
      (maphash (lambda (k v)
                 (push (cons k v) (element-raw-options element))
                 (unless (eq v 'null) (set-option element k v)))
               opts)
      (setf (element-raw-options element) (nreverse (element-raw-options element))))))

(defun read-shape (shape object)
  (let ((x (json-num object "x")) (y (json-num object "y")))
    (setf (shape-positioned shape) (and (or x y) t)
          (shape-x shape) (sane x) (shape-y shape) (sane y)
          (shape-width shape) (sane (json-num object "width"))
          (shape-height shape) (sane (json-num object "height")))
    (read-options shape object)
    shape))

(defun read-labels (element object)
  (setf (element-labels element)
        (loop for lo in (json-list object "labels")
              when (hash-table-p lo)
                collect (let ((l (%make-label :id (json-id (json-get lo "id"))
                                              :text (let ((tx (json-get lo "text")))
                                                      (if (stringp tx) tx ""))
                                              :owner element)))
                          (read-shape l lo)
                          ;; a label given only its text gets an estimated size
                          (unless (json-num lo "width") (setf (label-width l) (text-width (label-text l))))
                          (unless (json-num lo "height") (setf (label-height l) (text-height (label-text l))))
                          (read-labels l lo)
                          l))))

(defun json-point (object)
  (and (hash-table-p object)
       (pt (sane (json-num object "x")) (sane (json-num object "y")))))

(defun read-sections (edge-object)
  "An edge's laid-out sections, if the JSON has them."
  (loop for so in (json-list edge-object "sections")
        for start = (json-point (json-get so "startPoint"))
        for end = (json-point (json-get so "endPoint"))
        when (and start end)
          collect (make-section start end
                                (remove nil (mapcar #'json-point (json-list so "bendPoints"))))))

(defun read-elk-json (source)
  "Read a graph in ELK JSON from a string, pathname or stream."
  (let* ((text (quote-bare-numbers (read-json-text source)))
         (json (json-simple:parse text :max-depth nil))
         (index (make-hash-table :test 'equal))
         (pending '()))
    (unless (hash-table-p json) (error "ELK JSON: the top level is not an object"))
    (labels ((note (id element)
               (when (and id (not (gethash id index))) (setf (gethash id index) element)))
             (read-node (object parent)
               (let ((n (%make-node :id (or (json-id (json-get object "id")) (fresh-id "n"))
                                    :parent parent)))
                 (read-shape n object)
                 (note (node-id n) n)
                 (read-labels n object)
                 (setf (node-ports n)
                       (loop for po in (json-list object "ports")
                             when (hash-table-p po)
                               collect (let ((p (%make-port :id (or (json-id (json-get po "id"))
                                                                    (fresh-id "p"))
                                                            :node n)))
                                         (read-shape p po)
                                         (read-labels p po)
                                         (note (port-id p) p)
                                         p)))
                 (setf (node-children n)
                       (loop for co in (json-list object "children")
                             when (hash-table-p co) collect (read-node co n)))
                 (dolist (eo (json-list object "edges"))
                   (when (hash-table-p eo)
                     (let ((e (%make-edge :id (or (json-id (json-get eo "id")) (fresh-id "e"))
                                          :container n)))
                       (read-options e eo)
                       (read-labels e eo)
                       (setf (edge-sections e) (read-sections eo))
                       (push (cons e eo) pending)
                       (push e (node-edges n)))))
                 (setf (node-edges n) (nreverse (node-edges n)))
                 n)))
      (let ((root (read-node json nil)))
        (flet ((resolve (ids)
                 (loop for id in ids
                       for el = (gethash (json-id id) index)
                       when el collect el)))
          (dolist (pe pending)
            (destructuring-bind (e . eo) pe
              (setf (edge-sources e)
                    (resolve (or (json-list eo "sources")
                                 (let ((s (or (json-get eo "sourcePort") (json-get eo "source"))))
                                   (and s (list s))))))
              (setf (edge-targets e)
                    (resolve (or (json-list eo "targets")
                                 (let ((s (or (json-get eo "targetPort") (json-get eo "target"))))
                                   (and s (list s)))))))))
        root))))

;;; ---- writing ---------------------------------------------------------------

(defun json-object (&rest pairs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr do (setf (gethash k h) v))
    h))

(defun write-option-value (v)
  (cond ((eq v t) "true")
        ((null v) "false")
        ((pad-p v) (format nil "[top=~F,left=~F,bottom=~F,right=~F]"
                           (pad-top v) (pad-left v) (pad-bottom v) (pad-right v)))
        ((and (consp v) (realp (car v)) (realp (cdr v))) (format nil "(~F,~F)" (car v) (cdr v)))
        ((listp v) (format nil "[~{~A~^, ~}]"
                           (mapcar (lambda (x) (substitute #\_ #\- (string-upcase (string x)))) v)))
        ((keywordp v) (substitute #\_ #\- (symbol-name v)))
        ((stringp v) v)
        (t (princ-to-string v))))

(defun options-json (element)
  (let ((h (make-hash-table :test 'equal))
        (raw-keys (make-hash-table :test 'eq)))
    (dolist (r (element-raw-options element))
      (setf (gethash (car r) h) (cdr r)
            (gethash (option-key (car r)) raw-keys) t))
    (let ((opts (element-options element)))
      (when opts
        (maphash (lambda (k v)
                   (unless (gethash k raw-keys)
                     (setf (gethash (string-downcase (symbol-name k)) h)
                           (write-option-value v))))
                 opts)))
    (and (plusp (hash-table-count h)) h)))

(defun num-json (x) (sane x))

(defun jarr (list) (coerce list 'simple-vector))

(defun shape-json (shape h)
  (setf (gethash "x" h) (num-json (shape-x shape))
        (gethash "y" h) (num-json (shape-y shape))
        (gethash "width" h) (num-json (shape-width shape))
        (gethash "height" h) (num-json (shape-height shape)))
  (let ((o (options-json shape))) (when o (setf (gethash "layoutOptions" h) o)))
  (when (element-labels shape)
    (setf (gethash "labels" h) (jarr (mapcar #'label-json (element-labels shape)))))
  h)

(defun label-json (l)
  (let ((h (make-hash-table :test 'equal)))
    (when (label-id l) (setf (gethash "id" h) (label-id l)))
    (setf (gethash "text" h) (label-text l))
    (shape-json l h)))

(defun point-json (p) (json-object "x" (num-json (px p)) "y" (num-json (py p))))

(defun edge-json (e)
  (let ((h (json-object "id" (edge-id e)
                        "sources" (jarr (mapcar #'element-id (edge-sources e)))
                        "targets" (jarr (mapcar #'element-id (edge-targets e))))))
    (let ((o (options-json e))) (when o (setf (gethash "layoutOptions" h) o)))
    (when (element-labels e)
      (setf (gethash "labels" h) (jarr (mapcar #'label-json (element-labels e)))))
    (when (edge-sections e)
      (setf (gethash "sections" h)
            (jarr (loop for s in (edge-sections e) for i from 0
                  collect (let ((sh (json-object "id" (format nil "~A_s~D" (edge-id e) i)
                                                 "startPoint" (point-json (section-start s))
                                                 "endPoint" (point-json (section-end s)))))
                            (when (section-bends s)
                              (setf (gethash "bendPoints" sh)
                                    (jarr (mapcar #'point-json (section-bends s)))))
                            (when (and (edge-sources e) (= i 0))
                              (setf (gethash "incomingShape" sh) (element-id (first (edge-sources e)))))
                            (when (and (edge-targets e) (= i (1- (length (edge-sections e)))))
                              (setf (gethash "outgoingShape" sh) (element-id (first (edge-targets e)))))
                            sh)))))
    h))

(defun node-json (n)
  (let ((h (json-object "id" (node-id n))))
    (shape-json n h)
    (when (node-ports n)
      (setf (gethash "ports" h)
            (jarr (mapcar (lambda (p) (shape-json p (json-object "id" (port-id p)))) (node-ports n)))))
    (when (node-children n) (setf (gethash "children" h) (jarr (mapcar #'node-json (node-children n)))))
    (when (node-edges n) (setf (gethash "edges" h) (jarr (mapcar #'edge-json (node-edges n)))))
    h))

(defun write-elk-json (graph &key stream pretty)
  "Write GRAPH as ELK JSON to STREAM, or return it as a string."
  (json-simple:stringify (node-json graph) :stream stream :pretty pretty))

(defun layout-elk-json (text &rest options)
  "Lay out a graph given as ELK JSON text; return ELK JSON text."
  (let ((g (read-elk-json text)))
    (apply #'layout g options)
    (write-elk-json g)))
