;;;; p2-compaction.lisp -- org.eclipse.elk.alg.rectpacking.p2packing.Compaction.
;;;;
;;;; Java's Compaction is a static utility; compact() is driven by the row
;;;; filling loop.  The Java `continue` / `break` of compact()'s block loop are
;;;; RETURN-FROM / RETURN-FROM of named blocks below, and the Java `blockId--`
;;;; before a `continue` is kept as written: the loop increments afterwards.

(in-package #:elk)

(defjstatic compaction.compact (row-idx rows bounding-width node-node-spacing row-height-reevaluation)
  (let* ((something-was-changed nil)
         (compact-row-again nil)
         (next-row-index (1+ row-idx))
         (row (jl-get rows row-idx))
         (blocks (get-children row))
         (current-stack nil)
         (block-id 0))
    (block compact-loop
      (loop while (< block-id (get-number-of-assigned-blocks row))
            do (block compact-continue
                 (let ((block (jl-get blocks block-id)))
                   (when (is-fixed block)
                     (return-from compact-continue))
                   (when (is-empty (get-children block))
                     (remove-block row block)
                     (decf block-id)
                     (setf something-was-changed t)
                     (return-from compact-continue))
                   (unless (is-position-fixed block)
                     (when current-stack
                       (update-dimension current-stack))
                     (setf current-stack
                           (new-block-stack/3 (if current-stack
                                                  (+ (get-x current-stack) (get-width current-stack)
                                                     node-node-spacing)
                                                  0d0)
                                              (get-y row) node-node-spacing))
                     (set-location block (+ (get-x current-stack) (get-width current-stack)) (get-y row))
                     (jl-add (get-stacks row) current-stack)
                     (add-block current-stack block)
                     (set-position-fixed block t))
                   (let* ((next-block (compaction.get-next-block rows row block-id next-row-index))
                          (was-from-next-row (and next-block (not (eq (get-parent-row next-block) row)))))
                     (when next-block
                       (if (and (not (is-empty (get-children next-block)))
                                (not (get-property (jl-get (get-children next-block) 0)
                                                   rect-packing-options.in-new-row)))
                           (progn
                             (compaction.use-row-width block bounding-width)
                             (let ((absorbed (compaction.absorb-blocks row block next-block bounding-width
                                                                       node-node-spacing)))
                               (setf something-was-changed (or something-was-changed absorbed))))
                           (progn
                             (remove-block row next-block)
                             (return-from compact-loop)))
                       (when (is-empty (get-children next-block))
                         (when (> (size rows) next-row-index)
                           (remove-block (jl-get rows next-row-index) next-block))
                         (setf next-block nil)
                         (loop while (and (> (size rows) next-row-index)
                                          (is-empty (get-children (jl-get rows next-row-index))))
                               do (jl-remove rows (jl-get rows next-row-index))))
                       (when (null next-block)
                         (decf block-id)
                         (return-from compact-continue))
                       (when (and (not (get-property (jl-get (get-children next-block) 0)
                                                     rect-packing-options.in-new-row))
                                  (compaction.place-below rows row block next-block was-from-next-row
                                                          bounding-width next-row-index node-node-spacing))
                         (setf something-was-changed t)
                         (return-from compact-continue))
                       (if was-from-next-row
                           (let ((old-row-height (get-height row))
                                 (next-block-min-height (get-min-height next-block)))
                             (cond
                               ((and (not (get-property (jl-get (get-children next-block) 0)
                                                        rect-packing-options.in-new-row))
                                     (compaction.place-beside rows row block next-block was-from-next-row
                                                              bounding-width next-row-index node-node-spacing
                                                              row-height-reevaluation))
                                (setf something-was-changed t)
                                (when (< old-row-height next-block-min-height)
                                  (setf compact-row-again t)
                                  (set-parent-row next-block row)
                                  (return-from compact-loop))
                                (return-from compact-continue))
                               ((compaction.use-row-height row block)
                                (set-fixed block t)
                                (setf something-was-changed t)
                                (return-from compact-continue))))
                           (when (compaction.use-row-height row block)
                             (set-fixed block t)
                             (setf something-was-changed t)
                             (return-from compact-continue)))
                       (when something-was-changed
                         (return-from compact-continue)))
                     (if (compaction.use-row-height row block)
                         (progn
                           (set-fixed block t)
                           (setf something-was-changed t)
                           (when next-block
                             (set-position-fixed next-block nil))
                           (return-from compact-continue))
                         (update-dimension (get-stack block))))))
               (incf block-id)))
    (new-pair/2 something-was-changed compact-row-again)))

(defjstatic compaction.get-next-block (rows row block-id next-row-index)
  (cond ((< block-id (- (get-number-of-assigned-blocks row) 1))
         (jl-get (get-children row) (1+ block-id)))
        ((and (< next-row-index (size rows))
              (not (is-empty (get-children (jl-get rows next-row-index)))))
         (jl-get (get-children (jl-get rows next-row-index)) 0))
        (t nil)))

(defjstatic compaction.use-row-height (row block)
  (let ((something-was-changed nil)
        (previous-width (get-width (get-stack block))))
    (when (< (get-height block) (get-height row))
      (let ((target-width (get-width-for-fixed-height (get-stack block) (get-height row))))
        (when (> (get-width (get-stack block)) target-width)
          (place-rects-in (get-stack block) target-width)
          (setf something-was-changed (/= previous-width (get-width (get-stack block)))))))
    something-was-changed))

(defjstatic compaction.use-row-width (block bounding-width)
  (place-rects-in block (- bounding-width (get-x block)))
  (update-dimension (get-stack block))
  nil)

(defjstatic compaction.absorb-blocks (row block next-block bounding-width node-node-spacing)
  (let ((something-was-changed nil)
        (rect (jl-get (get-children next-block) 0)))
    (loop while (initial-placement.place-rect-in-block row block rect bounding-width node-node-spacing)
          do (setf something-was-changed t)
             (remove-child next-block rect)
             (when (is-empty (get-children next-block))
               (return))
             (setf rect (jl-get (get-children next-block) 0)))
    (when (is-empty (get-children next-block))
      (remove-block (get-parent-row next-block) next-block))
    (when something-was-changed
      (update-dimension (get-stack block)))
    something-was-changed))

(defjstatic compaction.place-below (rows row block next-block was-from-next-row bounding-width
                                    next-row-index node-node-spacing)
  (block place-below-body
    (let* ((something-was-changed nil)
           (remaining-width (- bounding-width (get-x block)))
           (current-block-min-height (+ (- (get-y block) (get-y row))
                                        (get-height-for-target-width block remaining-width))))
      (when (> (+ (get-min-width next-block) node-node-spacing) remaining-width)
        (return-from place-below-body nil))
      (let ((next-block-min-height (get-height-for-target-width next-block remaining-width)))
        (when (<= (+ current-block-min-height node-node-spacing next-block-min-height) (get-height row))
          (place-rects-in block (- bounding-width (get-x block)))
          (set-fixed block t)
          (place-rects-in next-block (- bounding-width (get-x block)))
          (set-location next-block (get-x block) (+ (get-y block) (get-height block) node-node-spacing))
          (set-position-fixed next-block t)
          (add-block (get-stack block) next-block)
          (setf something-was-changed t)
          (when was-from-next-row
            (add-block row next-block)
            (set-parent-row next-block row)
            (when (> (size rows) next-row-index)
              (remove-block (jl-get rows next-row-index) next-block)
              (when (is-empty (get-children (jl-get rows next-row-index)))
                (jl-remove-at rows next-row-index))))))
      something-was-changed)))

(defjstatic compaction.place-beside (rows row block next-block was-from-next-row bounding-width
                                     next-row-index node-node-spacing row-height-reevaluation)
  (declare (ignore was-from-next-row))
  (block place-beside-body
    (let* ((something-was-changed nil)
           (current-block-min-width
             (get-width-for-fixed-height (get-stack block)
                                         (- (+ (get-y row) (get-height row)) (get-y (get-stack block)))))
           (should-reevaluate (and (> (get-min-height next-block) (get-height row))
                                   row-height-reevaluation))
           (target-width-of-next-block
             (- bounding-width
                (- (+ (get-x (get-stack block)) current-block-min-width) node-node-spacing)))
           (next-block-height (get-height-for-target-width next-block target-width-of-next-block)))
      (when (and should-reevaluate (> next-block-height (get-min-height next-block)))
        (return-from place-beside-body nil))
      (when should-reevaluate
        (let ((potential-width 0d0))
          (jdo (stack (get-stacks row))
            (incf potential-width
                  (+ (get-width-for-fixed-height stack (get-min-height next-block)) node-node-spacing)))
          (setf target-width-of-next-block (- bounding-width potential-width))))
      (when (< target-width-of-next-block (get-min-width next-block))
        (return-from place-beside-body nil))
      (let ((last-row-optimization
              (and (= next-row-index (1- (size rows)))
                   (>= target-width-of-next-block (get-width (jl-get rows next-row-index))))))
        (when (and (not should-reevaluate)
                   (> next-block-height (get-height row))
                   (not last-row-optimization))
          (return-from place-beside-body nil))
        (when (or last-row-optimization should-reevaluate (<= next-block-height (get-height row)))
          (if (and last-row-optimization (> next-block-height (get-height row)))
              (progn
                (set-height block next-block-height)
                (place-rects-in block (get-width-for-target-height block next-block-height)))
              (progn
                (place-rects-in (get-stack block) current-block-min-width)
                (set-fixed block t)))
          (place-rects-in next-block (- bounding-width (+ (get-x block) (get-width block))))
          (set-location next-block (+ (get-x (get-stack block)) (get-width (get-stack block))) (get-y row))
          (add-block row next-block)
          (when (> (size rows) next-row-index)
            (remove-block (jl-get rows next-row-index) next-block)
            (when (is-empty (get-children (jl-get rows next-row-index)))
              (jl-remove-at rows next-row-index)))
          (setf something-was-changed t)))
      something-was-changed)))
