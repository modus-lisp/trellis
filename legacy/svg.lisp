;;;; svg.lisp -- draw a laid-out graph.

(in-package #:diagram)

(defun xml-escape (s)
  (with-output-to-string (out)
    (loop for c across s
          do (case c (#\< (write-string "&lt;" out)) (#\> (write-string "&gt;" out))
                     (#\& (write-string "&amp;" out)) (#\" (write-string "&quot;" out))
                     (t (write-char c out))))))

(defun graph->svg (root &optional stream)
  "Render ROOT (already laid out) as an SVG document."
  (if (null stream)
      (with-output-to-string (s) (graph->svg root s))
      (let ((abs (make-hash-table :test #'eq)))
        (labels ((origin (n)
                   (or (gethash n abs)
                       (setf (gethash n abs)
                             (if (node-parent n)
                                 (let ((p (origin (node-parent n))))
                                   (cons (+ (car p) (or (node-x n) 0)) (+ (cdr p) (or (node-y n) 0))))
                                 (cons 0d0 0d0)))))
                 (f (x) (format nil "~,1F" x))
                 (text (l ox oy)
                   (format stream "<text x=\"~A\" y=\"~A\" font-size=\"11\" font-family=\"sans-serif\">~A</text>~%"
                           (f (+ ox (or (label-x l) 0))) (f (+ oy (or (label-y l) 0) (* 0.8 (or (label-height l) 14))))
                           (xml-escape (label-text l))))
                 (draw-node (n depth)
                   (let ((o (origin n)))
                     (when (node-parent n)
                       (format stream "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" rx=\"3\" fill=\"~A\" stroke=\"#334\" stroke-width=\"1\"/>~%"
                               (f (car o)) (f (cdr o)) (f (or (node-width n) 0)) (f (or (node-height n) 0))
                               (if (node-children n)
                                   (nth (mod depth 3) '("#eef2f8" "#e4ecf4" "#f6f6ee"))
                                   "#ffffff")))
                     (dolist (l (node-labels n)) (text l (car o) (cdr o)))
                     (dolist (p (node-ports n))
                       (when (port-x p)
                         (format stream "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\" fill=\"#334\"/>~%"
                                 (f (+ (car o) (port-x p))) (f (+ (cdr o) (port-y p)))
                                 (f (max 2 (port-width p))) (f (max 2 (port-height p))))))
                     (dolist (c (node-children n)) (draw-node c (1+ depth)))))
                 (draw-edges (n)
                   (dolist (e (node-edges n))
                     (let ((o (if (edge-container e) (origin (edge-container e)) '(0d0 . 0d0))))
                       (dolist (s (edge-sections e))
                         (format stream "<path d=\"M~{~A~^ L~}\" fill=\"none\" stroke=\"#556\" stroke-width=\"1.2\" marker-end=\"url(#arrow)\"/>~%"
                                 (mapcar (lambda (p) (format nil "~A ~A" (f (+ (car o) (car p))) (f (+ (cdr o) (cdr p)))))
                                         (append (list (section-start s)) (section-bends s) (list (section-end s))))))
                       (dolist (l (edge-labels e)) (text l (car o) (cdr o)))))
                   (mapc #'draw-edges (node-children n))))
          (format stream "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~A\" height=\"~A\" viewBox=\"-4 -4 ~A ~A\">~%"
                  (f (+ 8 (or (node-width root) 0))) (f (+ 8 (or (node-height root) 0)))
                  (f (+ 8 (or (node-width root) 0))) (f (+ 8 (or (node-height root) 0))))
          (format stream "<defs><marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"7\" markerHeight=\"7\" orient=\"auto-start-reverse\"><path d=\"M0 0 L10 5 L0 10 z\" fill=\"#556\"/></marker></defs>~%")
          (format stream "<rect x=\"0\" y=\"0\" width=\"~A\" height=\"~A\" fill=\"#fbfbfd\"/>~%"
                  (f (or (node-width root) 0)) (f (or (node-height root) 0)))
          (draw-node root 0)
          (draw-edges root)
          (format stream "</svg>~%")
          nil))))
