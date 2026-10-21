;;;; common/utils-classes.lisp -- org.eclipse.elk.alg.common.utils (see tools/classnames.tsv)

(in-package #:elk)

;;; SVGImage: debug output is a no-op in this port, so the object carries only
;;; the file name the caller passed.  Its methods exist so callers compile and
;;; run unchanged (see utils.lisp).
(defjclass svg-image ()
  ((file-name nil)
   (debug nil)
   (max-area 8294400d0)))

;;; UniqueTriple: three values; no equals override, so identity equality as in Java.
(defjclass unique-triple ()
  ((first-v nil)
   (second-v nil)
   (third-v nil)))
