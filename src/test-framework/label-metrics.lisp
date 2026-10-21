;;;; label-metrics.lisp -- the label-size measurer for the test framework.
;;;;
;;;; TestConfiguration.computeLabelSize sizes a label that has text but no size,
;;;; from AWT font metrics: the width and height of the string at 10 pt in the
;;;; sans-serif font.  Here the width is gesso's shaped advance and the height is
;;;; the font's ascent plus descent, both at 10 px, from the stencil/gesso/scribe
;;;; stack.  That stack is not a dependency of the library, so it is found at run
;;;; time: when GESSO and SCRIBE are loaded, the measurer is installed into
;;;; *LABEL-SIZE-FUNCTION*; otherwise labels keep the zero size they have now.
;;;;
;;;; The symbols are looked up by name (FIND-SYMBOL), so this file compiles
;;;; without gesso or scribe present.

(in-package #:elk)

(defparameter *label-metrics-family* '("sans-serif")
  "Java's SansSerif, as a CSS family list for scribe:match-font.")

(defun %label-sym (package name)
  (let ((s (find-symbol name package)))
    (unless (and s (fboundp s))
      (error "label metrics: ~A::~A is not available" package name))
    s))

(defun gesso-label-size (text size)
  "(values width height) of TEXT at SIZE px in the sans-serif face, as Java's
getStringBounds reports them for a label."
  (let* ((make-context (%label-sym "GESSO" "MAKE-CONTEXT"))
         (set-font (%label-sym "GESSO" "SET-FONT"))
         (measure-text (%label-sym "GESSO" "MEASURE-TEXT"))
         (match-font (%label-sym "SCRIBE" "MATCH-FONT"))
         (font-ascent (%label-sym "SCRIBE" "FONT-ASCENT"))
         (font-descent (%label-sym "SCRIBE" "FONT-DESCENT"))
         (upem (%label-sym "SCRIBE" "FONT-UNITS-PER-EM"))
         (ctx (funcall make-context 1 1)))
    (funcall set-font ctx (float size 1d0) :family *label-metrics-family*)
    (let* ((width (funcall measure-text ctx text))
           (font (funcall match-font *label-metrics-family*))
           (scale (/ (float size 1d0) (float (funcall upem font) 1d0)))
           (height (* scale (+ (abs (funcall font-ascent font))
                               (abs (funcall font-descent font))))))
      (values (float width 1d0) (float height 1d0)))))

(defun install-label-metrics ()
  "Use the gesso/scribe measurer for labels, if that stack is loaded.  Returns T
when it was installed."
  (when (and (find-package "GESSO") (find-package "SCRIBE")
             (find-symbol "MEASURE-TEXT" "GESSO") (find-symbol "MATCH-FONT" "SCRIBE"))
    (setf *label-size-function* #'gesso-label-size)
    t))
