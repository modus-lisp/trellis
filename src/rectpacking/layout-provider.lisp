;;;; layout-provider.lisp -- org.eclipse.elk.alg.rectpacking top level:
;;;; RectPackingLayoutProvider and InteractiveRectPackingGraphVisitor.
;;;; (RectPackingLayoutPhases is generated in gen-enums.)

(in-package #:elk)

(defjmethod layout ((this rect-packing-layout-provider) layout-graph progress-monitor)
  (block layout-body
    (begin progress-monitor "Rectangle Packing" 1)
    (let* ((padding (get-property layout-graph rect-packing-options.padding))
           (fixed-graph-size (get-property layout-graph rect-packing-options.node-size-fixed-graph-size))
           (node-node-spacing (get-property layout-graph rect-packing-options.spacing-node-node))
           (try-box (get-property layout-graph rect-packing-options.trybox))
           (rectangles (get-children layout-graph)))
      ;; if requested, compute nodes's dimensions, place node labels, ports, port labels, etc.
      (unless (get-property layout-graph rect-packing-options.omit-node-micro-layout)
        (execute (node-micro-layout.for-graph layout-graph)))
      ;; Check whether regions are stackable and do box layout instead.
      (let ((stackable nil))
        (if (and try-box (>= (size rectangles) 3))
            (let ((region1 nil)
                  (region2 (jl-get rectangles 0))
                  (region3 (jl-get rectangles 1))
                  (counter 0))
              (loop while (< (+ counter 2) (size rectangles))
                    do (setf region1 region2)
                       (setf region2 region3)
                       (setf region3 (jl-get rectangles (+ counter 2)))
                       (if (or (>= (get-height region1)
                                   (+ (get-height region2) (get-height region3) node-node-spacing))
                               (>= (get-height region3)
                                   (+ (get-height region1) (get-height region2) node-node-spacing)))
                           (progn
                             (setf stackable t)
                             (return))
                           (incf counter))))
            (setf stackable t))
        (unless stackable
          ;; Set priority to invoke box layout.
          (let ((priority (size rectangles)))
            (jdo (elk-node rectangles)
              (set-property elk-node core-options.priority priority)
              (decf priority)))
          (layout (new-box-layout-provider/0) layout-graph (new-basic-progress-monitor/0))
          (done progress-monitor)
          (return-from layout-body nil))
        (let* ((algorithm (assemble-algorithm this layout-graph))
               (monitor-progress (coerce (/ 1f0 (size algorithm)) 'double-float))
               (slot-index 0))
          ;; Invoke each layout processor
          (jdo (processor algorithm)
            (when (is-canceled progress-monitor)
              (return-from layout-body nil))
            (process processor layout-graph (sub-task progress-monitor monitor-progress))
            (incf slot-index)))
        ;; Content alignment
        (let ((real-width 0d0)
              (real-height 0d0))
          (jdo (rect rectangles)
            (setf real-width (math.max real-width (+ (get-x rect) (get-width rect))))
            (setf real-height (math.max real-height (+ (get-y rect) (get-height rect)))))
          (elk-util.translate layout-graph
                              (new-k-vector (get-property layout-graph rectpacking-internal-properties.drawing-width)
                                            (get-property layout-graph rectpacking-internal-properties.drawing-height))
                              (new-k-vector real-width real-height)))
        ;; Final touch.
        (rect-packing-layout-provider.apply-padding rectangles padding)
        (unless fixed-graph-size
          (elk-util.resize-node layout-graph
                                (+ (get-property layout-graph rectpacking-internal-properties.drawing-width)
                                   (get-horizontal padding))
                                (+ (get-property layout-graph rectpacking-internal-properties.drawing-height)
                                   (get-vertical padding))
                                nil t))
        ;; Do micro layout again since the whitspace elimination and other things might have changed node sizes.
        (unless (get-property layout-graph rect-packing-options.omit-node-micro-layout)
          (execute (node-micro-layout.for-graph layout-graph))))
      (done progress-monitor)
      nil)))

(defjstatic rect-packing-layout-provider.apply-padding (rectangles padding)
  "Shifts all rectangles to the right and bottom according to the specified padding."
  (jdo (rect rectangles)
    (set-location rect (+ (get-x rect) (get-left padding)) (+ (get-y rect) (get-top padding))))
  nil)

(defjmethod assemble-algorithm ((this rect-packing-layout-provider) layout-graph)
  (let ((asm (.algorithm-assembler this)))
    (reset asm)
    (set-phase asm rect-packing-layout-phases.p1-width-approximation
               (get-property layout-graph rect-packing-options.width-approximation-strategy))
    (set-phase asm rect-packing-layout-phases.p2-packing
               (get-property layout-graph rect-packing-options.packing-strategy))
    (set-phase asm rect-packing-layout-phases.p3-whitespace-elimination
               (get-property layout-graph rect-packing-options.white-space-elimination-strategy))
    (add-processor-configuration asm
                                 (get-phase-independent-layout-processor-configuration this layout-graph))
    (build asm layout-graph)))

(defjmethod get-phase-independent-layout-processor-configuration ((this rect-packing-layout-provider)
                                                                  layout-graph)
  ;; Basic configuration
  (let ((configuration (layout-processor-configuration.create)))
    (add-before configuration rect-packing-layout-phases.p1-width-approximation
                rectpacking-intermediate-processor-strategy.min-size-pre-processor)
    (add-before configuration rect-packing-layout-phases.p2-packing
                rectpacking-intermediate-processor-strategy.min-size-post-processor)
    (when (get-property layout-graph rect-packing-options.order-by-size)
      (add-before configuration rect-packing-layout-phases.p1-width-approximation
                  rectpacking-intermediate-processor-strategy.node-size-reorderer))
    (when (get-property layout-graph rect-packing-options.interactive)
      (add-before configuration rect-packing-layout-phases.p1-width-approximation
                  rectpacking-intermediate-processor-strategy.interactive-node-reorderer))
    configuration))

;;; ------------------------------------------------------------ InteractiveRectPackingGraphVisitor

(defjmethod visit ((this interactive-rect-packing-graph-visitor) element)
  (when (typep element 'elk-node)
    (set-interactive-options this element))
  nil)

(defjmethod set-interactive-options ((this interactive-rect-packing-graph-visitor) root)
  (let ((algorithm (get-property root core-options.algorithm))
        (algorithm-id rect-packing-options.algorithm-id))
    (when (and algorithm
               (>= (length algorithm-id) (length algorithm))
               (string= algorithm-id algorithm
                        :start1 (- (length algorithm-id) (length algorithm))))
      (when (> (size (get-children root)) 0)
        (set-property root rect-packing-options.interactive t))))
  nil)
