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

;;; Candidate layouts.  ELK's crossing minimisation starts from a random order,
;;; so the seed decides between layouts that differ a lot in quality.  With the
;;; root option "trellis.candidates" set to N, LAYOUT-JSON lays the graph out N
;;; times, with seeds S, S+1, ... (S from elk.randomSeed, default 1), and keeps
;;; the result with the fewest edge crossings, then the shortest total edge
;;; length, then the smallest area.  This is not an ELK option.

(defparameter +candidates-option+ "trellis.candidates")

(defun %option-key (options suffix)
  "The key of OPTIONS (a hash table) naming the option whose id ends in SUFFIX."
  (when options
    (loop for k being the hash-keys of options
          when (let ((n (length k)) (m (length suffix)))
                 (and (>= n m) (string= suffix k :start2 (- n m))
                      (or (= n m) (char= #\. (char k (- n m 1))))))
            return k)))

(defun %option-integer (value)
  (typecase value
    (integer value)
    (real (round value))
    (string (ignore-errors (parse-integer value)))))

(defun %edge-segments (json)
  "Every straight segment of every edge section in the laid-out JSON object, as
(edge x1 y1 x2 y2) in absolute coordinates."
  (let ((segments '()))
    (labels ((field (obj key) (cdr (assoc key (cdr obj) :test #'string=)))
             (num (x) (float (or x 0) 1d0))
             (walk (node ox oy)
               (loop for edge across (or (field node "edges") #())
                     do (loop for section across (or (field edge "sections") #())
                              do (let ((points (append (list (field section "startPoint"))
                                                       (coerce (or (field section "bendPoints") #()) 'list)
                                                       (list (field section "endPoint")))))
                                   (loop for (a b) on points while b
                                         do (push (list edge
                                                        (+ ox (num (field a "x"))) (+ oy (num (field a "y")))
                                                        (+ ox (num (field b "x"))) (+ oy (num (field b "y"))))
                                                  segments)))))
               (loop for child across (or (field node "children") #())
                     do (walk child (+ ox (num (field child "x"))) (+ oy (num (field child "y")))))))
      (walk json 0d0 0d0))
    segments))

(defun %segments-cross-p (s1 s2)
  "Whether two segments cross at a point inside both (touching and overlapping
do not count)."
  (destructuring-bind (ax ay bx by) (rest s1)
    (destructuring-bind (cx cy dx dy) (rest s2)
      (flet ((side (px py qx qy rx ry)
               (let ((v (- (* (- qx px) (- ry py)) (* (- qy py) (- rx px)))))
                 (cond ((> v 1d-9) 1) ((< v -1d-9) -1) (t 0)))))
        (let ((d1 (side ax ay bx by cx cy)) (d2 (side ax ay bx by dx dy))
              (d3 (side cx cy dx dy ax ay)) (d4 (side cx cy dx dy bx by)))
          (and (= -1 (* d1 d2)) (= -1 (* d3 d4))))))))

(defun layout-score (json)
  "(crossings edge-length area) of a laid-out ELK JSON object; smaller is better."
  (let* ((segments (coerce (%edge-segments json) 'vector))
         (n (length segments))
         (crossings 0)
         (length 0d0))
    (dotimes (i n)
      (destructuring-bind (ax ay bx by) (rest (aref segments i))
        (incf length (sqrt (+ (expt (- bx ax) 2) (expt (- by ay) 2)))))
      (loop for j from (1+ i) below n
            when (and (not (eq (first (aref segments i)) (first (aref segments j))))
                      (%segments-cross-p (aref segments i) (aref segments j)))
              do (incf crossings)))
    (flet ((field (key) (float (or (cdr (assoc key (cdr json) :test #'string=)) 0) 1d0)))
      (list crossings length (* (field "width") (field "height"))))))

(defun %score< (a b)
  (loop for x in a for y in b
        do (cond ((< x (- y 1d-6)) (return t))
                 ((> x (+ y 1d-6)) (return nil)))
        finally (return nil)))

(defun %layout-jsimple (root)
  "Lay out the json-simple ROOT and return the laid-out ELK JSON object."
  (let ((graph (elk::transform (elk::new-json-importer) (%from-jsimple root))))
    (layout graph)
    (elk::elk-graph->json-object graph)))

(defun layout-json (text &key candidates)
  "Parse ELK JSON TEXT, lay it out, and return the laid-out graph as ELK JSON text.
CANDIDATES, or the root option \"trellis.candidates\", is the number of seeds to
try (see LAYOUT-SCORE); the default is one layout, exactly as ELK would do it."
  (let* ((root (json-simple:parse text))
         (options (gethash "layoutOptions" root))
         (candidates-key (%option-key options +candidates-option+))
         (n (max 1 (or candidates
                       (and candidates-key (%option-integer (gethash candidates-key options)))
                       1))))
    (when candidates-key (remhash candidates-key options))
    (json-simple:stringify
     (%to-jsimple
      (if (= n 1)
          (%layout-jsimple root)
          (let* ((seed-key (or (%option-key options "randomSeed") "elk.randomSeed"))
                 (seed (or (and options (%option-integer (gethash seed-key options))) 1))
                 (best nil) (best-score nil))
            (unless options
              (setf options (make-hash-table :test 'equal)
                    (gethash "layoutOptions" root) options))
            ;; Seed 0 asks ELK for a time-based seed, which would not be repeatable.
            (when (zerop seed) (setf seed 1))
            (dotimes (i n best)
              (setf (gethash seed-key options) (+ seed i))
              (let* ((json (%layout-jsimple root))
                     (score (layout-score json)))
                (when (or (null best) (%score< score best-score))
                  (setf best json best-score score)))))))
     :pretty t)))

(defun graph->svg (graph &optional stream)
  "SVG markup of the laid-out GRAPH: a string when STREAM is NIL."
  (elk::elk-graph->svg graph stream))
