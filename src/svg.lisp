;;;; svg.lisp -- draw a laid-out graph as SVG markup.

(in-package #:trellis)

(defun xml-escape (s)
  (with-output-to-string (out)
    (loop for c across s
          do (case c
               (#\< (write-string "&lt;" out)) (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out)) (#\" (write-string "&quot;" out))
               (t (write-char c out))))))

(defparameter *themes*
  '((:light :background nil :node "#e8eef9" :compound "#f4f6fb" :stroke "#555" :text "#222"
            :port "#333" :edge "#333" :tie "#8a8f98" :colored nil)
    (:dark :background "#16181d" :node "#22252c" :compound "#1b1e24" :stroke "#3b404a" :text "#c8ccd4"
           :port "#8a909c" :edge "#8a909c" :tie "#b5bac4" :colored t)))

(defparameter *wire-colors*
  '("#e0915a" "#6cc4a4" "#9b8ae6" "#d76a8a" "#6aa7e0" "#c9b458" "#5fb8c9" "#b886d6"))

(defvar *wire-index* nil "node -> its place in the graph, for wire colours")

(defun wire-color (edge theme)
  "Edges are coloured by the node they leave, when the theme colours them:
   nodes take the palette's colours in turn."
  (if (and (getf theme :colored) *wire-index*)
      (let* ((s (first (edge-sources edge)))
             (i (if s (gethash (endpoint-node s) *wire-index* 0) 0)))
        (nth (mod i (length *wire-colors*)) *wire-colors*))
      (getf theme :edge)))

(defun graph->svg (graph &key stream (theme :light))
  "SVG markup for GRAPH's layout, written to STREAM or returned as a string.
   THEME is :light, or :dark with wires coloured by the node they leave."
  (if (null stream)
      (with-output-to-string (s) (graph->svg graph :stream s :theme theme))
      (let ((th (or (cdr (assoc theme *themes*)) (cdr (assoc :light *themes*))))
            (*wire-index* (let ((h (make-hash-table :test 'eq)) (i 0))
                            (map-nodes (lambda (n) (setf (gethash n h) i) (incf i)) graph)
                            h))
            (w (max 1d0 (node-width graph))) (h (max 1d0 (node-height graph))))
        (format stream "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~,1F\" height=\"~,1F\" viewBox=\"0 0 ~,1F ~,1F\" font-family=\"sans-serif\" font-size=\"11\">~%"
                w h w h)
        (when (getf th :background)
          (format stream "<rect x=\"0\" y=\"0\" width=\"~,1F\" height=\"~,1F\" fill=\"~A\"/>~%" w h (getf th :background)))
        (labels ((draw-labels (element ox oy)
                   (dolist (l (element-labels element))
                     ;; an explicit baseline renders the same everywhere
                     (format stream "<text x=\"~,2F\" y=\"~,2F\" fill=\"~A\">~A</text>~%"
                             (+ ox (label-x l)) (+ oy (label-y l) (* 0.8d0 (max 0d0 (label-height l))))
                             (getf th :text) (xml-escape (label-text l)))))
                 (draw-node (n)
                   (let* ((o (absolute-position n)) (x (px o)) (y (py o)))
                     (format stream "<rect x=\"~,2F\" y=\"~,2F\" width=\"~,2F\" height=\"~,2F\" fill=\"~A\" stroke=\"~A\" rx=\"3\"/>~%"
                             x y (max 0d0 (node-width n)) (max 0d0 (node-height n))
                             (getf th (if (node-children n) :compound :node)) (getf th :stroke))
                     (draw-labels n x y)
                     (dolist (p (node-ports n))
                       (let ((po (absolute-position p)))
                         (format stream "<rect x=\"~,2F\" y=\"~,2F\" width=\"~,2F\" height=\"~,2F\" fill=\"~A\"/>~%"
                                 (px po) (py po) (max 1d0 (port-width p)) (max 1d0 (port-height p)) (getf th :port))
                         (draw-labels p (px po) (py po))))
                     (mapc #'draw-node (node-children n))))
                 (draw-edges (n)
                   (let ((o (content-origin n)))
                     (dolist (e (node-edges n))
                       (let ((color (wire-color e th)))
                         (dolist (s (edge-sections e))
                           (let ((pts (mapcar (lambda (p) (format nil "~,2F,~,2F" (+ (px o) (px p)) (+ (py o) (py p))))
                                              (section-points s))))
                             (if (and (section-curve s) (= 1 (mod (length pts) 3)))
                                 (format stream "<path fill=\"none\" stroke=\"~A\" stroke-width=\"1.2\" d=\"M~A~{ C~A ~A ~A~}\"/>~%"
                                         color (first pts) (rest pts))
                                 (format stream "<polyline fill=\"none\" stroke=\"~A\" stroke-width=\"1.2\" points=\"~{~A~^ ~}\"/>~%"
                                         color pts)))))
                       (draw-labels e (px o) (py o)))
                     ;; wire ties: a short band across each bundle
                     (dolist (tie (node-ties n))
                       (format stream "<line x1=\"~,2F\" y1=\"~,2F\" x2=\"~,2F\" y2=\"~,2F\" stroke=\"~A\" stroke-width=\"4\" stroke-linecap=\"round\"/>~%"
                               (+ (px o) (px (car tie))) (+ (py o) (py (car tie)))
                               (+ (px o) (px (cdr tie))) (+ (py o) (py (cdr tie))) (getf th :tie))))
                   (mapc #'draw-edges (node-children n))))
          (mapc #'draw-node (node-children graph))
          (draw-edges graph))
        (format stream "</svg>~%")
        nil)))
