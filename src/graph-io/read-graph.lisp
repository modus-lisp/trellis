;;;; read-graph.lisp -- one entry point for the test models: (read-graph-file path).
;;;;
;;;; .elkt  the ELK Graph Text document (elkt.lisp)
;;;; .json  the ELK JSON graph format (json-graph.lisp, JsonImporter)
;;;; .elkg  the EMF/XMI serialisation.  Every .elkg in the models has a sibling
;;;;        .json with the same graph, so that is read instead.

(in-package #:elk)

(defun read-graph-file (path)
  "Load the graph in PATH and return its root ElkNode."
  (let* ((path (pathname path))
         (type (string-downcase (or (pathname-type path) ""))))
    (cond ((string= type "elkt") (parse-elkt-file path))
          ((string= type "json") (%read-json-graph path))
          ((string= type "elkg")
           (let ((sibling (make-pathname :type "json" :defaults path)))
             (unless (probe-file sibling)
               (error "~A: no sibling .json to read in its place" (namestring path)))
             (%read-json-graph sibling)))
          (t (error "read-graph-file: unsupported graph file type ~S (~A)" type (namestring path))))))

(defun %read-json-graph (path)
  (transform (new-json-importer) (json-parse-file path)))
