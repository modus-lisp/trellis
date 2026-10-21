;;;; svg.lisp -- write a laid-out ELK graph as SVG markup.
;;;;
;;;; The layout library produces geometry; rendering it is ~/stencil's job.  This
;;;; file only writes the markup stencil reads: nodes, ports, labels and edges,
;;;; all in absolute coordinates.  It walks the ELK model directly (not the
;;;; legacy structs in legacy/), so what is drawn is what the engine computed.
;;;;
;;;; Coordinates: a node's x/y are relative to its parent, so they are summed up
;;;; the tree.  An edge section's points are relative to the edge's containing
;;;; node, so they are offset by that node's absolute origin.

(in-package #:elk)

(defun %svg-escape (s)
  (with-output-to-string (out)
    (loop for c across (princ-to-string s)
          do (case c
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (t (write-char c out))))))

(defun %svg-num (x)
  "A number with a fixed, locale-independent form."
  (format nil "~,2F" (float (or x 0) 1d0)))

(defun %svg-origin (node table)
  "Absolute (x . y) of NODE: the sum of its ancestors' and its own x/y."
  (or (gethash node table)
      (setf (gethash node table)
            (let ((parent (get-parent node)))
              (if parent
                  (let ((p (%svg-origin parent table)))
                    (cons (+ (car p) (or (get-x node) 0)) (+ (cdr p) (or (get-y node) 0))))
                  (cons (float (or (get-x node) 0) 1d0) (float (or (get-y node) 0) 1d0)))))))

(defun %svg-write-label (label origin stream)
  (let ((x (+ (car origin) (or (get-x label) 0)))
        (y (+ (cdr origin) (or (get-y label) 0))))
    (format stream "<text x=\"~A\" y=\"~A\" font-size=\"11\" font-family=\"sans-serif\">~A</text>~%"
            (%svg-num x) (%svg-num (+ y (* 0.8 (or (get-height label) 14))))
            (%svg-escape (get-text label)))))

(defun %svg-write-node (node origin depth stream)
  (let ((o (%svg-origin node origin)))
    (when (get-parent node)
      (format stream "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" rx=\"3\" fill=\"~A\" stroke=\"#334\" stroke-width=\"1\"/>~%"
              (%svg-num (car o)) (%svg-num (cdr o)) (%svg-num (get-width node)) (%svg-num (get-height node))
              (if (get-children node)
                  (nth (mod depth 3) '("#eef2f8" "#e4ecf4" "#f6f6ee"))
                  "#ffffff")))
    (jdo (label (get-labels node))
      (%svg-write-label label o stream))
    (jdo (port (get-ports node))
      (format stream "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" fill=\"#334\"/>~%"
              (%svg-num (+ (car o) (or (get-x port) 0))) (%svg-num (+ (cdr o) (or (get-y port) 0)))
              (%svg-num (max 2 (get-width port))) (%svg-num (max 2 (get-height port)))))
    (jdo (child (get-children node))
      (%svg-write-node child origin (1+ depth) stream))))

(defun %svg-write-edges (node origin stream)
  (jdo (edge (get-contained-edges node))
    (let* ((container (or (get-containing-node edge) node))
           (c (%svg-origin container origin)))
      (jdo (section (get-sections edge))
        (let ((pts (append (list (cons (get-start-x section) (get-start-y section)))
                           (mapcar (lambda (b) (cons (get-x b) (get-y b))) (coerce (jlist-from (get-bend-points section)) (quote list)))
                           (list (cons (get-end-x section) (get-end-y section))))))
          (format stream "<path d=\"M~{~A~^ L~}\" fill=\"none\" stroke=\"#556\" stroke-width=\"1.2\" marker-end=\"url(#arrow)\"/>~%"
                  (mapcar (lambda (p)
                            (format nil "~A ~A"
                                    (%svg-num (+ (car c) (float (car p) 1d0)))
                                    (%svg-num (+ (cdr c) (float (cdr p) 1d0)))))
                          pts))))
      (jdo (label (get-labels edge))
        (%svg-write-label label c stream))))
  (jdo (child (get-children node))
    (%svg-write-edges child origin stream)))

(defun elk-graph->svg (root &optional stream)
  "Write the laid-out ELK graph ROOT as an SVG document.  Returns the string when
STREAM is NIL.  Run the layout first; this does not lay anything out."
  (if (null stream)
      (with-output-to-string (s) (elk-graph->svg root s))
      (let* ((w (+ 8 (float (or (get-width root) 0) 1d0)))
             (h (+ 8 (float (or (get-height root) 0) 1d0)))
             (origin (make-hash-table :test #'eq)))
        (format stream "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~A\" height=\"~A\" viewBox=\"-4 -4 ~A ~A\">~%"
                (%svg-num w) (%svg-num h) (%svg-num w) (%svg-num h))
        (format stream "<defs><marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"7\" markerHeight=\"7\" orient=\"auto-start-reverse\"><path d=\"M0 0 L10 5 L0 10 z\" fill=\"#556\"/></marker></defs>~%")
        (format stream "<rect x=\"0\" y=\"0\" width=\"~A\" height=\"~A\" fill=\"#fbfbfd\"/>~%"
                (%svg-num (get-width root)) (%svg-num (get-height root)))
        (%svg-write-node root origin 0 stream)
        (%svg-write-edges root origin stream)
        (format stream "</svg>~%")
        nil)))
