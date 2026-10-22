;;;; json.lisp -- write a laid-out ELK graph as ELK JSON.
;;;;
;;;; The inverse of the JSON importer, in the same coordinate convention: every
;;;; node, port and label position is relative to its parent, and edge section
;;;; points are relative to the edge's containing node.  Only what the layout
;;;; produces is written; input options are not echoed back.

(in-package #:elk)

(defun %json-num (x) (float (or x 0) 1d0))

(defun %json-point (x y)
  (make-json-object "x" (%json-num x) "y" (%json-num y)))

(defun %json-label (label)
  (make-json-object "text" (get-text label)
                    "x" (%json-num (get-x label)) "y" (%json-num (get-y label))
                    "width" (%json-num (get-width label)) "height" (%json-num (get-height label))))

(defun %json-labels (labels)
  (let ((arr (make-json-array)))
    (jdo (l labels) (json-array-add arr (%json-label l)))
    arr))

(defun %json-port (port)
  (make-json-object "id" (get-identifier port)
                    "x" (%json-num (get-x port)) "y" (%json-num (get-y port))
                    "width" (%json-num (get-width port)) "height" (%json-num (get-height port))
                    "labels" (%json-labels (get-labels port))))

(defun %json-section (section)
  (make-json-object "startPoint" (%json-point (get-start-x section) (get-start-y section))
                    "endPoint" (%json-point (get-end-x section) (get-end-y section))
                    "bendPoints"
                    (let ((arr (make-json-array)))
                      (jdo (b (get-bend-points section))
                        (json-array-add arr (%json-point (get-x b) (get-y b))))
                      arr)))

(defun %json-id-array (shapes)
  (let ((arr (make-json-array)))
    (jdo (shape shapes) (json-array-add arr (get-identifier shape)))
    arr))

(defun %json-edge (edge)
  (let ((sections (make-json-array)))
    (jdo (s (get-sections edge)) (json-array-add sections (%json-section s)))
    (make-json-object "id" (get-identifier edge)
                      "sources" (%json-id-array (get-sources edge))
                      "targets" (%json-id-array (get-targets edge))
                      "labels" (%json-labels (get-labels edge))
                      "sections" sections)))

(defun %json-node (node)
  (let ((children (make-json-array))
        (ports (make-json-array))
        (edges (make-json-array)))
    (jdo (c (get-children node)) (json-array-add children (%json-node c)))
    (jdo (p (get-ports node)) (json-array-add ports (%json-port p)))
    (jdo (e (get-contained-edges node)) (json-array-add edges (%json-edge e)))
    (make-json-object "id" (get-identifier node)
                      "x" (%json-num (get-x node)) "y" (%json-num (get-y node))
                      "width" (%json-num (get-width node)) "height" (%json-num (get-height node))
                      "labels" (%json-labels (get-labels node))
                      "ports" ports
                      "children" children
                      "edges" edges)))

(defun elk-graph->json-object (root)
  "The laid-out ROOT as an ELK JSON object (see json-write)."
  (%json-node root))
