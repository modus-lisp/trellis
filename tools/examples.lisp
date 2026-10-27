;;;; tools/examples.lisp -- lay out examples/*.json, write .svg and .png.
;;;;
;;;;   sbcl --script tools/examples.lisp
;;;;
;;;; The PNGs are rendered by stencil (SVG in pure Common Lisp, through gesso and
;;;; scribe), looked for beside this repository as in a modus-lisp workspace.

(require :asdf)

(defparameter *here*
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(asdf:initialize-source-registry
 `(:source-registry (:tree ,(truename (merge-pathnames "../" *here*))) :inherit-configuration))
(asdf:load-system "trellis")
(asdf:load-system "stencil")

(dolist (in (directory (merge-pathnames "examples/*.json" *here*)))
  (let* ((g (trellis:read-elk-json in))
         ;; the wire-tie example is drawn dark, its wires coloured by source
         (svg (progn (trellis:layout g)
                     (trellis:graph->svg g :theme (if (search "wire" (pathname-name in)) :dark :light))))
         (w (max 1d0 (trellis:node-width g))) (h (max 1d0 (trellis:node-height g)))
         (k (min 2d0 (/ 1600d0 w) (/ 1600d0 h))))
    (with-open-file (out (make-pathname :type "svg" :defaults in) :direction :output :if-exists :supersede)
      (write-string svg out))
    (gesso:write-png (stencil:render-svg svg :width (* k w) :height (* k h))
                     (make-pathname :type "png" :defaults in))
    (format t "~A ~Dx~D ~A~%" (pathname-name in) (round w) (round h) (trellis:check-layout g))))
