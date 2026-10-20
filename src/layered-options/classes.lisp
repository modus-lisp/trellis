;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered.options

(in-package #:elk)

;; org.eclipse.elk.alg.layered.options.Spacings
(defjclass spacings ()
  ((graph nil)
   (node-type-spacing-options-horizontal nil)
   (node-type-spacing-options-vertical nil)))

;; Spacings.UnspecifiedSpacingException extends RuntimeException
(define-condition unspecified-spacing-exception (runtime-exception) ())

;; LayeredSpacings.LayeredSpacingsBuilder extends AbstractSpacingsBuilder<LayeredSpacingsBuilder>
(defjclass layered-spacings-builder (abstract-spacings-builder) ())
