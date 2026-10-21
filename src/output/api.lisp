;;;; api.lisp -- the public interface, exported from TRELLIS (see runtime/package.lisp).
;;;;
;;;;   (read-graph-file path)      -> ELK graph (.elkt, .json, or .elkg via its .json)
;;;;   (read-elkt path)            -> ELK graph from an ELK Graph Text file
;;;;   (layout graph)              -> lays GRAPH out in place and returns it
;;;;   (layout-json text)          -> ELK JSON text in, laid-out ELK JSON text out
;;;;   (graph->svg graph &optional stream)
;;;;                               -> SVG markup of a laid-out graph
;;;;
;;;; The names here are TRELLIS symbols.  The implementation lives in ELK under the
;;;; same names (the layout entry points are ELK's call macros, which is why they
;;;; cannot be defined under the same symbol here).

(in-package #:trellis)

;;; JSON at the boundary is json-simple's representation: objects are hash tables,
;;; arrays simple-vectors, true/false/null are T / NIL / cl:null.  The ELK JSON
;;; importer and writer use the internal forms (:object (key . value)...),
;;; :true / :false / :null, so the two are converted here.

(defun %to-jsimple (x)
  (cond ((and (consp x) (eq (car x) :object))
         (let ((h (make-hash-table :test 'equal)))
           (loop for (k . v) in (cdr x) do (setf (gethash k h) (%to-jsimple v)))
           h))
        ((and (vectorp x) (not (stringp x))) (map 'simple-vector #'%to-jsimple x))
        ((eq x :true) t)
        ((eq x :false) nil)
        ((eq x :null) 'cl:null)
        (t x)))

(defun %from-jsimple (x)
  (cond ((hash-table-p x)
         (cons :object (loop for k being the hash-keys of x using (hash-value v)
                             collect (cons k (%from-jsimple v)))))
        ((and (vectorp x) (not (stringp x))) (map 'vector #'%from-jsimple x))
        ((eq x t) :true)
        ((null x) :false)
        ((eq x 'cl:null) :null)
        (t x)))

(defun read-graph-file (path)
  (elk::read-graph-file path))

(defun read-elkt (path)
  (elk::parse-elkt-file (pathname path)))

(defun layout (graph &optional (progress-monitor (elk::new-basic-progress-monitor/0)))
  "Lay GRAPH out in place with the recursive engine and return it.  The algorithm
for each level comes from the graph's own options (elk.algorithm)."
  (elk::layout/3 (elk::new-recursive-graph-layout-engine/0) graph nil progress-monitor)
  graph)

(defun layout-json (text)
  "Parse ELK JSON TEXT, lay it out, and return the laid-out graph as ELK JSON text."
  (let ((graph (elk::transform (elk::new-json-importer) (%from-jsimple (json-simple:parse text)))))
    (layout graph)
    (json-simple:stringify (%to-jsimple (elk::elk-graph->json-object graph)) :pretty t)))

(defun graph->svg (graph &optional stream)
  "SVG markup of the laid-out GRAPH: a string when STREAM is NIL."
  (elk::elk-graph->svg graph stream))
