;;;; p1-width-approximation.lisp -- org.eclipse.elk.alg.rectpacking.p1widthapproximation.

(in-package #:elk)

;;; ------------------------------------------------------------ WidthApproximationStrategy

(defjmethod create ((this width-approximation-strategy))
  (enum-case this
    (width-approximation-strategy.greedy (new-greedy-width-approximator/0))
    (width-approximation-strategy.target-width (new-target-width-width-approximator/0))
    (t (jthrow 'illegal-argument-exception
               "No implementation is available for the width approximator ~A"
               (to-string this)))))

;;; ------------------------------------------------------------ BestCandidateFilter

(defjgeneric filter-list 3)

;;; AreaFilter

(defjmethod filter-list ((this area-filter) candidates aspect-ratio padding)
  (declare (ignore aspect-ratio))
  (let ((remaining-candidates (new-jlist))
        (min-area double.positive-infinity))
    (jdo (opt candidates)
      (setf min-area (math.min min-area
                               (* (+ (get-drawing-width opt) (get-horizontal padding))
                                  (+ (get-drawing-height opt) (get-vertical padding))))))
    (jdo (candidate candidates)
      (when (= (* (+ (get-drawing-width candidate) (get-horizontal padding))
                  (+ (get-drawing-height candidate) (get-vertical padding)))
               min-area)
        (jl-add remaining-candidates candidate)))
    remaining-candidates))

;;; AspectRatioFilter

(defjmethod filter-list ((this aspect-ratio-filter) candidates aspect-ratio padding)
  (let ((remaining-candidates (new-jlist))
        (smallest-deviation double.positive-infinity))
    (jdo (opt candidates)
      (setf smallest-deviation
            (math.min smallest-deviation
                      (math.abs (- (/ (+ (get-drawing-width opt) (get-horizontal padding))
                                      (+ (get-drawing-height opt) (get-vertical padding)))
                                   aspect-ratio)))))
    (jdo (candidate candidates)
      (when (= (math.abs (- (/ (+ (get-drawing-width candidate) (get-horizontal padding))
                               (+ (get-drawing-height candidate) (get-vertical padding)))
                            aspect-ratio))
               smallest-deviation)
        (jl-add remaining-candidates candidate)))
    remaining-candidates))

;;; ScaleMeasureFilter

(defjmethod filter-list ((this scale-measure-filter) candidates aspect-ratio padding)
  (declare (ignore aspect-ratio))
  (let ((remaining-candidates (new-jlist))
        (max-scale double.negative-infinity))
    (jdo (opt candidates)
      (setf max-scale
            (math.max max-scale
                      (drawing-util.compute-scale-measure/3
                       (+ (get-drawing-width opt) (get-horizontal padding))
                       (+ (get-drawing-height opt) (get-vertical padding))
                       (get-desired-aspect-ratio opt)))))
    (jdo (candidate candidates)
      (when (= (drawing-util.compute-scale-measure/3
                (+ (get-drawing-width candidate) (get-horizontal padding))
                (+ (get-drawing-height candidate) (get-vertical padding))
                (get-desired-aspect-ratio candidate))
               max-scale)
        (jl-add remaining-candidates candidate)))
    remaining-candidates))

;;; ------------------------------------------------------------ Calculations

(defjstatic calculations.get-width-lp-ror-lpb (drawing-width x width)
  (math.max drawing-width (+ x width)))

(defjstatic calculations.get-height-lp-ror-lpb (drawing-height y height)
  (math.max drawing-height (+ y height)))

(defjstatic calculations.calculate-yfor-lpr (x placed-rects last-placed node-node-spacing)
  (let ((closest-upper-neighbor nil)
        (closest-neighbor-bottom-border 0d0))
    (jdo (placed-rect placed-rects)
      (let ((placed-rect-bottom-border (+ (get-y placed-rect) (get-height placed-rect))))
        (when (calculations.vertical-order-constraint placed-rect x node-node-spacing)
          (cond ((null closest-upper-neighbor)
                 (setf closest-upper-neighbor placed-rect))
                ((< (- (get-y last-placed) placed-rect-bottom-border)
                    (- (get-y last-placed) closest-neighbor-bottom-border))
                 (setf closest-upper-neighbor placed-rect)))
          (setf closest-neighbor-bottom-border
                (+ (get-y closest-upper-neighbor) (get-height closest-upper-neighbor))))))
    (if (null closest-upper-neighbor)
        0d0
        (+ closest-neighbor-bottom-border node-node-spacing))))

(defjstatic calculations.calculate-xfor-lpb (y placed-rects last-placed node-node-spacing)
  (let ((closest-left-neighbour nil)
        (closest-neighbor-right-border 0d0))
    (jdo (placed-rect placed-rects)
      (let ((placed-rect-right-border (+ (get-x placed-rect) (get-width placed-rect))))
        (when (calculations.horizontal-order-constraint placed-rect y node-node-spacing)
          (cond ((null closest-left-neighbour)
                 (setf closest-left-neighbour placed-rect))
                ((< (- (get-x last-placed) placed-rect-right-border)
                    (- (get-x last-placed) closest-neighbor-right-border))
                 (setf closest-left-neighbour placed-rect)))
          (setf closest-neighbor-right-border
                (+ (get-x closest-left-neighbour) (get-width closest-left-neighbour))))))
    (if (null closest-left-neighbour)
        0d0
        (+ closest-neighbor-right-border node-node-spacing))))

(defjstatic calculations.calculate-area-lpr (last-placed to-place lpr-opt)
  (let* ((last-placed-bottom-border (+ (get-y last-placed) (get-height last-placed)))
         (to-place-bottom-border (+ (get-next-y-coordinate lpr-opt) (get-height to-place)))
         (max-ylpr (math.max last-placed-bottom-border to-place-bottom-border))
         (height-lpr (- max-ylpr (math.min (get-y last-placed) (get-next-y-coordinate lpr-opt))))
         (width-lpr (- (+ (get-next-x-coordinate lpr-opt) (get-width to-place)) (get-x last-placed))))
    (* width-lpr height-lpr)))

(defjstatic calculations.calculate-area-lpb (last-placed to-place lpb-opt)
  (let* ((last-placed-right-border (+ (get-x last-placed) (get-width last-placed)))
         (to-place-right-border (+ (get-next-x-coordinate lpb-opt) (get-width to-place)))
         (max-xlpb (math.max last-placed-right-border to-place-right-border))
         (width-lpb (- max-xlpb (math.min (get-x last-placed) (get-next-x-coordinate lpb-opt))))
         (height-lpb (- (+ (get-next-y-coordinate lpb-opt) (get-height to-place)) (get-y last-placed))))
    (* width-lpb height-lpb)))

(defjstatic calculations.vertical-order-constraint (placed-rect x node-node-spacing)
  (< x (+ (get-x placed-rect) (get-width placed-rect) node-node-spacing)))

(defjstatic calculations.horizontal-order-constraint (placed-rect y-coord-rect-to-place node-node-spacing)
  (< y-coord-rect-to-place (+ (get-y placed-rect) (get-height placed-rect) node-node-spacing)))

;;; ------------------------------------------------------------ AreaApproximation

(defjctor area-approximation (aspect-ratio goal lp-shift)
  (setf (.aspect-ratio this) aspect-ratio
        (.goal this) goal
        (.lp-shift this) lp-shift))

(defjmethod approx-bounding-box ((this area-approximation) rectangles node-node-spacing padding)
  (let* ((first-rect (jl-get rectangles 0))
         (placed-rects (new-jlist))
         (last-placed first-rect)
         (current-values nil))
    (set-x first-rect 0d0)
    (set-y first-rect 0d0)
    (jl-add placed-rects first-rect)
    (setf current-values
          (new-drawing-data/4 (.aspect-ratio this) (get-width first-rect) (get-height first-rect)
                              drawing-data-descriptor.whole-drawing))
    (loop for rectangle-idx from 1 below (size rectangles)
          do (let* ((to-place (jl-get rectangles rectangle-idx))
                    (opt1 (calc-values-for-opt this drawing-data-descriptor.candidate-position-last-placed-right
                                               to-place last-placed current-values placed-rects node-node-spacing))
                    (opt2 (calc-values-for-opt this drawing-data-descriptor.candidate-position-last-placed-below
                                               to-place last-placed current-values placed-rects node-node-spacing))
                    (opt3 (calc-values-for-opt this drawing-data-descriptor.candidate-position-whole-drawing-right
                                               to-place last-placed current-values placed-rects node-node-spacing))
                    (opt4 (calc-values-for-opt this drawing-data-descriptor.candidate-position-whole-drawing-below
                                               to-place last-placed current-values placed-rects node-node-spacing))
                    (best-opt (find-best-candidate this opt1 opt2 opt3 opt4 to-place last-placed padding)))
               (set-x to-place (get-next-x-coordinate best-opt))
               (set-y to-place (get-next-y-coordinate best-opt))
               (set-placement-option best-opt drawing-data-descriptor.whole-drawing)
               (setf current-values best-opt)
               (setf last-placed to-place)
               (jl-add placed-rects to-place)))
    current-values))

(defjmethod find-best-candidate ((this area-approximation) opt1 opt2 opt3 opt4 to-place last-placed padding)
  (let ((candidates (jlist opt1 opt2 opt3 opt4))
        (filters nil))
    (enum-case (.goal this)
      (optimization-goal.max-scale-driven
       (setf filters (jlist (new-scale-measure-filter/0) (new-area-filter/0) (new-aspect-ratio-filter/0))))
      (optimization-goal.aspect-ratio-driven
       (setf filters (jlist (new-aspect-ratio-filter/0) (new-area-filter/0) (new-scale-measure-filter/0))))
      (optimization-goal.area-driven
       (setf filters (jlist (new-area-filter/0) (new-scale-measure-filter/0) (new-aspect-ratio-filter/0))))
      (t nil))
    (jdo (filter filters)
      (when (> (size candidates) 1)
        (setf candidates (filter-list filter candidates (.aspect-ratio this) padding))))
    (cond ((= (size candidates) 1)
           (jl-get candidates (1- (size candidates))))
          ((= (size candidates) 2)
           (check-special-cases this (jl-get candidates 0) (jl-get candidates 1) last-placed to-place))
          (t nil))))

(defjmethod check-special-cases ((this area-approximation) drawing1 drawing2 last-placed to-place)
  (let* ((first-opt (get-placement-option drawing1))
         (second-opt (get-placement-option drawing2))
         (first-lpb-or-cdb (or (eq first-opt drawing-data-descriptor.candidate-position-last-placed-below)
                               (eq first-opt drawing-data-descriptor.candidate-position-whole-drawing-below)))
         (second-lpb-or-cdb (or (eq second-opt drawing-data-descriptor.candidate-position-last-placed-below)
                                (eq second-opt drawing-data-descriptor.candidate-position-whole-drawing-below)))
         (first-lpr-or-cdr (or (eq first-opt drawing-data-descriptor.candidate-position-last-placed-right)
                               (eq first-opt drawing-data-descriptor.candidate-position-whole-drawing-right)))
         (second-lpr-or-cdr (or (eq second-opt drawing-data-descriptor.candidate-position-last-placed-right)
                                (eq second-opt drawing-data-descriptor.candidate-position-whole-drawing-right)))
         (first-lpr-or-lpb (or (eq first-opt drawing-data-descriptor.candidate-position-last-placed-right)
                               (eq first-opt drawing-data-descriptor.candidate-position-last-placed-below)))
         (second-lpr-or-lpb (or (eq second-opt drawing-data-descriptor.candidate-position-last-placed-right)
                                (eq second-opt drawing-data-descriptor.candidate-position-last-placed-below))))
    (cond
      ((and first-lpb-or-cdb second-lpb-or-cdb)
       (if (eq (get-placement-option drawing1) drawing-data-descriptor.candidate-position-whole-drawing-below)
           drawing1
           drawing2))
      ((and first-lpr-or-cdr second-lpr-or-cdr)
       (if (eq (get-placement-option drawing1) drawing-data-descriptor.candidate-position-whole-drawing-right)
           drawing1
           drawing2))
      ((and first-lpr-or-lpb second-lpr-or-lpb)
       (let (lpr-opt lpb-opt)
         (if (eq first-opt drawing-data-descriptor.candidate-position-last-placed-right)
             (setf lpr-opt drawing1 lpb-opt drawing2)
             (setf lpr-opt drawing2 lpb-opt drawing1))
         (let ((area-lpr (calculations.calculate-area-lpr last-placed to-place lpr-opt))
               (area-lpb (calculations.calculate-area-lpb last-placed to-place lpb-opt)))
           (if (<= area-lpr area-lpb)
               (if (eq (get-placement-option drawing1) drawing-data-descriptor.candidate-position-last-placed-right)
                   drawing1
                   drawing2)
               (if (eq (get-placement-option drawing1) drawing-data-descriptor.candidate-position-last-placed-below)
                   drawing1
                   drawing2)))))
      (t drawing1))))

(defjmethod calc-values-for-opt ((this area-approximation) option to-place last-placed drawing placed-rects
                                 node-node-spacing)
  (let* ((x 0d0)
         (y 0d0)
         (drawing-width (get-drawing-width drawing))
         (drawing-height (get-drawing-height drawing))
         (height-to-place (get-height to-place))
         (width-to-place (get-width to-place))
         (width 0d0)
         (height 0d0))
    (enum-case option
      (drawing-data-descriptor.candidate-position-last-placed-right
       (setf x (+ (get-x last-placed) (get-width last-placed) node-node-spacing))
       (if (.lp-shift this)
           (setf y (calculations.calculate-yfor-lpr x placed-rects last-placed node-node-spacing))
           (setf y (get-y last-placed)))
       (setf width (calculations.get-width-lp-ror-lpb drawing-width x width-to-place))
       (setf height (calculations.get-height-lp-ror-lpb drawing-height y height-to-place)))
      (drawing-data-descriptor.candidate-position-last-placed-below
       (setf y (+ (get-y last-placed) (get-height last-placed) node-node-spacing))
       (if (.lp-shift this)
           (setf x (calculations.calculate-xfor-lpb y placed-rects last-placed node-node-spacing))
           (setf x (get-x last-placed)))
       (setf width (calculations.get-width-lp-ror-lpb drawing-width x width-to-place))
       (setf height (calculations.get-height-lp-ror-lpb drawing-height y height-to-place)))
      (drawing-data-descriptor.candidate-position-whole-drawing-right
       (setf x (+ drawing-width node-node-spacing))
       (setf y 0d0)
       (setf width (+ drawing-width node-node-spacing width-to-place))
       (setf height (math.max drawing-height height-to-place)))
      (drawing-data-descriptor.candidate-position-whole-drawing-below
       (setf x 0d0)
       (setf y (+ drawing-height node-node-spacing))
       (setf width (math.max drawing-width width-to-place))
       (setf height (+ drawing-height node-node-spacing height-to-place)))
      (t (jthrow 'illegal-argument-exception "IllegalPlacementOption.")))
    (new-drawing-data/6 (.aspect-ratio this) width height option x y)))

;;; ------------------------------------------------------------ GreedyWidthApproximator

(defjmethod process ((this greedy-width-approximator) graph progress-monitor)
  (begin progress-monitor "Greedy Width Approximator" 1)
  (let* ((aspect-ratio (get-property graph rect-packing-options.aspect-ratio))
         (padding (get-property graph rect-packing-options.padding))
         (goal (get-property graph rect-packing-options.width-approximation-optimization-goal))
         (last-place-shift (get-property graph rect-packing-options.width-approximation-last-place-shift))
         (node-node-spacing (get-property graph rect-packing-options.spacing-node-node))
         (rectangles (get-children graph)))
    (drawing-util.reset-coordinates rectangles)
    (let* ((first-it (new-area-approximation/3 aspect-ratio goal last-place-shift))
           (drawing (approx-bounding-box first-it rectangles node-node-spacing padding)))
      (set-property graph rectpacking-internal-properties.target-width (get-drawing-width drawing))))
  (done progress-monitor)
  nil)

(defjmethod get-layout-processor-configuration ((this greedy-width-approximator) graph)
  (declare (ignore graph))
  nil)

;;; ------------------------------------------------------------ TargetWidthWidthApproximator

(defjmethod process ((this target-width-width-approximator) graph progress-monitor)
  (begin progress-monitor "Target Width Setter" 1)
  (if (has-property graph rect-packing-options.width-approximation-target-width)
      (set-property graph rectpacking-internal-properties.target-width
                    (get-property graph rect-packing-options.width-approximation-target-width))
      (jthrow 'unsupported-configuration-exception
              "A target width has to be set if the TargetWidthWidthApproximator should be used."))
  (done progress-monitor)
  nil)

(defjmethod get-layout-processor-configuration ((this target-width-width-approximator) graph)
  (declare (ignore graph))
  nil)
