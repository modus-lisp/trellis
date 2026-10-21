;;;; p3-whitespace-elimination.lisp -- org.eclipse.elk.alg.rectpacking.p3whitespaceelimination.

(in-package #:elk)

;;; ------------------------------------------------------------ WhiteSpaceEliminationStrategy

(defjmethod create ((this white-space-elimination-strategy))
  (enum-case this
    (white-space-elimination-strategy.equal-between-structures (new-equal-whitespace-eliminator/0))
    (white-space-elimination-strategy.to-aspect-ratio (new-to-aspectratio-node-expander/0))
    (t nil)))

;;; ------------------------------------------------------------ RectangleExpansion

(defjstatic rectangle-expansion.expand (rows drawing-width additional-height node-node-spacing)
  (declare (ignore node-node-spacing))
  (let ((height-per-row (/ additional-height (size rows)))
        (index 0))
    (jdo (row rows)
      (set-y row (+ (get-y row) (* height-per-row index)))
      (expand row drawing-width height-per-row)
      (incf index))
    nil))

;;; ------------------------------------------------------------ EqualWhitespaceEliminator

(defjmethod process ((this equal-whitespace-eliminator) graph progress-monitor)
  (begin progress-monitor "Equal Whitespace Eliminator" 1)
  (if (has-property graph rectpacking-internal-properties.rows)
      (rectangle-expansion.expand (get-property graph rectpacking-internal-properties.rows)
                                  (get-property graph rectpacking-internal-properties.drawing-width)
                                  (get-property graph rectpacking-internal-properties.additional-height)
                                  (get-property graph rect-packing-options.spacing-node-node))
      (jthrow 'unsupported-configuration-exception "The graph does not contain rows."))
  (done progress-monitor)
  nil)

(defjmethod get-layout-processor-configuration ((this equal-whitespace-eliminator) graph)
  (declare (ignore graph))
  nil)

;;; ------------------------------------------------------------ ToAspectratioNodeExpander

(defjmethod process ((this to-aspectratio-node-expander) graph progress-monitor)
  (begin progress-monitor "To Aspect Ratio Whitesapce Eliminator" 1)
  (let* ((width (get-property graph rectpacking-internal-properties.drawing-width))
         (height (get-property graph rectpacking-internal-properties.drawing-height))
         (desired-aspect-ratio (get-property graph rect-packing-options.aspect-ratio))
         (additional-height (get-property graph rectpacking-internal-properties.additional-height))
         (aspect-ratio (/ width height)))
    (if (< aspect-ratio desired-aspect-ratio)
        (progn
          (setf width (* height desired-aspect-ratio))
          (set-property graph rectpacking-internal-properties.drawing-width width))
        (progn
          (incf additional-height (- (/ width desired-aspect-ratio) height))
          (set-property graph rectpacking-internal-properties.additional-height additional-height)
          (set-property graph rectpacking-internal-properties.drawing-height (+ height additional-height)))))
  (call-next-method)
  (done progress-monitor)
  nil)
