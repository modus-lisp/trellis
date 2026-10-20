;;;; port-placement.lisp -- HorizontalPortPlacementSizeCalculator,
;;;; VerticalPortPlacementSizeCalculator, PortPlacementCalculator

(in-package #:elk)

(defproperty port-placement-calculator.port-ratio-or-position "portRatioOrPosition" 0.0)

(defun %ns-ipl-cell (nc side) (gethash side (.inside-port-label-cells nc)))

;;; ------------------------------------------------------------ HorizontalPortPlacementSizeCalculator

(defjstatic horizontal-port-placement-size-calculator.calculate-horizontal-port-placement-size (nc)
  (enum-case (.port-constraints nc)
    (port-constraints.fixed-pos
     (%ns-h-fixed-pos nc port-side.north)
     (%ns-h-fixed-pos nc port-side.south))
    (port-constraints.fixed-ratio
     (%ns-h-fixed-ratio nc port-side.north)
     (%ns-h-fixed-ratio nc port-side.south))
    (t
     (%ns-h-free nc port-side.north)
     (%ns-h-free nc port-side.south))))

(defun %ns-h-fixed-pos (nc side)
  (let ((rightmost 0.0))
    (jdo (pc (ns-port-contexts-get nc side))
      (setf rightmost (math.max rightmost (+ (.x (.port-position pc)) (.x (get-size (.port pc)))))))
    (let ((cell (%ns-ipl-cell nc side)))
      (setf (.left (get-padding cell)) 0.0)
      (setf (.x (get-minimum-content-area-size cell)) rightmost))))

(defun %ns-h-fixed-ratio (nc side)
  (let ((cell (%ns-ipl-cell nc side))
        (pcs (ns-port-contexts-get nc side)))
    (when (jl-is-empty pcs)
      (setf (.left (get-padding cell)) 0.0 (.right (get-padding cell)) 0.0)
      (return-from %ns-h-fixed-ratio))
    (let ((port-labels-inside (contains (.port-labels-placement nc) port-label-placement.inside))
          (min-width 0.0)
          (prev nil) (prev-ratio 0.0) (prev-width 0.0)
          (spm (.surrounding-port-margins nc)))
      (jdo (cur pcs)
        (let ((cur-ratio (get-property (.port cur) port-placement-calculator.port-ratio-or-position))
              (cur-width (.x (get-size (.port cur)))))
          (when (contains (.size-constraints nc) size-constraint.port-labels)
            (%ns-h-setup-port-margins nc side))
          (if (null prev)
              (when (and spm (> (.left spm) 0))
                (setf min-width (math.max min-width
                                          (horizontal-port-placement-size-calculator.min-size-required-to-respect-spacing
                                           (+ (.left spm) (.left (.port-margin cur))) 0.0 cur-ratio))))
              (let ((required (+ prev-width (.right (.port-margin prev)) (.port-port-spacing nc)
                                 (.left (.port-margin cur)))))
                (setf min-width (math.max min-width
                                          (horizontal-port-placement-size-calculator.min-size-required-to-respect-spacing
                                           required prev-ratio cur-ratio)))))
          (setf prev cur prev-ratio cur-ratio prev-width cur-width)))
      (when (and spm (> (.right spm) 0))
        (let ((required (+ prev-width (.right spm))))
          (when port-labels-inside
            (incf required (.right (.port-margin prev))))
          (setf min-width (math.max min-width
                                    (horizontal-port-placement-size-calculator.min-size-required-to-respect-spacing
                                     required prev-ratio 1.0)))))
      (setf (.left (get-padding cell)) 0.0)
      (setf (.x (get-minimum-content-area-size cell)) min-width))))

(defjconst horizontal-port-placement-size-calculator.equality-tolerance 0.01)

(defjstatic horizontal-port-placement-size-calculator.min-size-required-to-respect-spacing
    (spacing first-ratio second-ratio)
  ;; DoubleMath.fuzzyEquals
  (if (or (= first-ratio second-ratio)
          (<= (abs (- first-ratio second-ratio)) horizontal-port-placement-size-calculator.equality-tolerance))
      0.0
      (/ spacing (- second-ratio first-ratio))))

(defun %ns-h-free (nc side)
  (let ((cell (%ns-ipl-cell nc side)))
    (when (jl-is-empty (ns-port-contexts-get nc side))
      (setf (.left (get-padding cell)) 0.0 (.right (get-padding cell)) 0.0)
      (return-from %ns-h-free))
    (setf (.left (get-padding cell)) (.left (.surrounding-port-margins nc)))
    (setf (.right (get-padding cell)) (.right (.surrounding-port-margins nc)))
    (when (contains (.size-constraints nc) size-constraint.port-labels)
      (%ns-h-setup-port-margins nc side))
    (let ((width (%ns-h-port-width-plus-spacing nc side)))
      (when (eq (get-port-alignment nc side) port-alignment.distributed)
        (incf width (* 2 (.port-port-spacing nc))))
      (setf (.x (get-minimum-content-area-size cell)) width))))

(defun %ns-h-setup-port-margins (nc side)
  (let* ((pcs (ns-port-contexts-get nc side))
         (plp (.port-labels-placement nc))
         (outside (contains plp port-label-placement.outside))
         (always-same-side (contains plp port-label-placement.always-same-side))
         (always-same-side-above (contains plp port-label-placement.always-other-same-side))
         (space-efficient (contains plp port-label-placement.space-efficient))
         (uniform (contains (.size-options nc) size-options.uniform-port-spacing))
         (space-efficient-labels (and (not always-same-side) (not always-same-side-above)
                                      (or space-efficient (= (length pcs) 2))))
         (leftmost nil) (rightmost nil))
    (%ns-h-compute-port-margins nc side outside)
    (when outside
      (setf leftmost (jl-first pcs) rightmost (jl-last pcs))
      (setf (.left (.port-margin leftmost)) 0.0)
      (setf (.right (.port-margin rightmost)) 0.0)
      (when (and space-efficient-labels (not (.labels-next-to-port leftmost)))
        (setf (.right (.port-margin leftmost)) 0.0)))
    (when uniform
      (%ns-h-unify-port-margins pcs)
      (when outside
        (setf (.left (.port-margin leftmost)) 0.0)
        (setf (.right (.port-margin rightmost)) 0.0)))))

(defun %ns-h-compute-port-margins (nc side outside)
  (declare (ignore outside))
  (jdo (pc (ns-port-contexts-get nc side))
    (let ((label-width (if (.port-label-cell pc) (get-minimum-width (.port-label-cell pc)) 0.0)))
      (cond ((> label-width 0)
             (if (.labels-next-to-port pc)
                 (let ((port-width (.x (get-size (.port pc)))))
                   (when (> label-width port-width)
                     (let ((overhang (/ (- label-width port-width) 2)))
                       (setf (.left (.port-margin pc)) overhang
                             (.right (.port-margin pc)) overhang))))
                 (setf (.right (.port-margin pc)) (+ (.port-label-spacing-horizontal nc) label-width))))
            ((port-label-placement.is-fixed (.port-labels-placement nc))
             (let ((lb (elk-util.get-labels-bounds (.port pc))))
               (when (< (.x lb) 0)
                 (setf (.left (.port-margin pc)) (- (.x lb))))
               (when (> (+ (.x lb) (.width lb)) (.x (get-size (.port pc))))
                 (setf (.right (.port-margin pc))
                       (- (+ (.x lb) (.width lb)) (.x (get-size (.port pc))))))))))))

(defun %ns-h-unify-port-margins (pcs)
  (let ((max-left 0.0) (max-right 0.0))
    (jdo (pc pcs)
      (setf max-left (math.max max-left (.left (.port-margin pc)))
            max-right (math.max max-right (.right (.port-margin pc)))))
    (jdo (pc pcs)
      (setf (.left (.port-margin pc)) max-left
            (.right (.port-margin pc)) max-right))))

(defun %ns-h-port-width-plus-spacing (nc side)
  (let ((result 0.0)
        (pcs (ns-port-contexts-get nc side)))
    (loop for i from 0 below (length pcs)
          for pc = (aref pcs i)
          do (incf result (+ (.left (.port-margin pc)) (.x (get-size (.port pc))) (.right (.port-margin pc))))
             (when (< (1+ i) (length pcs))
               (incf result (.port-port-spacing nc))))
    result))

;;; ------------------------------------------------------------ VerticalPortPlacementSizeCalculator

(defjstatic vertical-port-placement-size-calculator.calculate-vertical-port-placement-size (nc)
  (enum-case (.port-constraints nc)
    (port-constraints.fixed-pos
     (%ns-v-fixed-pos nc port-side.east)
     (%ns-v-fixed-pos nc port-side.west))
    (port-constraints.fixed-ratio
     (%ns-v-fixed-ratio nc port-side.east)
     (%ns-v-fixed-ratio nc port-side.west))
    (t
     (%ns-v-free nc port-side.east)
     (%ns-v-free nc port-side.west))))

(defun %ns-v-fixed-pos (nc side)
  (let ((bottommost 0.0))
    (jdo (pc (ns-port-contexts-get nc side))
      (setf bottommost (math.max bottommost (+ (.y (.port-position pc)) (.y (get-size (.port pc)))))))
    (let ((cell (%ns-ipl-cell nc side)))
      (setf (.top (get-padding cell)) 0.0)
      (setf (.y (get-minimum-content-area-size cell)) bottommost))))

(defun %ns-v-fixed-ratio (nc side)
  (let ((cell (%ns-ipl-cell nc side))
        (pcs (ns-port-contexts-get nc side)))
    (when (jl-is-empty pcs)
      (setf (.top (get-padding cell)) 0.0 (.bottom (get-padding cell)) 0.0)
      (return-from %ns-v-fixed-ratio))
    (let ((port-labels-inside (contains (.port-labels-placement nc) port-label-placement.inside))
          (min-height 0.0)
          (prev nil) (prev-ratio 0.0) (prev-height 0.0)
          (spm (.surrounding-port-margins nc)))
      (when (contains (.size-constraints nc) size-constraint.port-labels)
        (%ns-v-setup-port-margins nc side))
      (jdo (cur pcs)
        (let ((cur-ratio (get-property (.port cur) port-placement-calculator.port-ratio-or-position))
              (cur-height (.y (get-size (.port cur)))))
          (if (null prev)
              (when (and spm (> (.top spm) 0))
                (setf min-height (math.max min-height
                                           (horizontal-port-placement-size-calculator.min-size-required-to-respect-spacing
                                            (+ (.top spm) (.top (.port-margin cur))) 0.0 cur-ratio))))
              (let ((required (+ prev-height (.bottom (.port-margin prev)) (.port-port-spacing nc)
                                 (.top (.port-margin cur)))))
                (setf min-height (math.max min-height
                                           (horizontal-port-placement-size-calculator.min-size-required-to-respect-spacing
                                            required prev-ratio cur-ratio)))))
          (setf prev cur prev-ratio cur-ratio prev-height cur-height)))
      (when (and spm (> (.bottom spm) 0))
        (let ((required (+ prev-height (.bottom spm))))
          (when port-labels-inside
            (incf required (.bottom (.port-margin prev))))
          (setf min-height (math.max min-height
                                     (horizontal-port-placement-size-calculator.min-size-required-to-respect-spacing
                                      required prev-ratio 1.0)))))
      (setf (.top (get-padding cell)) 0.0)
      (setf (.y (get-minimum-content-area-size cell)) min-height))))

(defun %ns-v-free (nc side)
  (let ((cell (%ns-ipl-cell nc side)))
    (when (jl-is-empty (ns-port-contexts-get nc side))
      (setf (.top (get-padding cell)) 0.0 (.bottom (get-padding cell)) 0.0)
      (return-from %ns-v-free))
    (setf (.top (get-padding cell)) (.top (.surrounding-port-margins nc)))
    (setf (.bottom (get-padding cell)) (.bottom (.surrounding-port-margins nc)))
    (when (contains (.size-constraints nc) size-constraint.port-labels)
      (%ns-v-setup-port-margins nc side))
    (let ((height (%ns-v-port-height-plus-spacing nc side)))
      (when (eq (get-port-alignment nc side) port-alignment.distributed)
        (incf height (* 2 (.port-port-spacing nc))))
      (setf (.y (get-minimum-content-area-size cell)) height))))

(defun %ns-v-setup-port-margins (nc side)
  (let* ((pcs (ns-port-contexts-get nc side))
         (plp (.port-labels-placement nc))
         (outside (contains plp port-label-placement.outside))
         (always-same-side (contains plp port-label-placement.always-same-side))
         (always-same-side-above (contains plp port-label-placement.always-other-same-side))
         (space-efficient (contains plp port-label-placement.space-efficient))
         (uniform (contains (.size-options nc) size-options.uniform-port-spacing))
         (space-efficient-labels (and (not always-same-side) (not always-same-side-above)
                                      (or space-efficient (= (length pcs) 2))))
         (topmost nil) (bottommost nil))
    (%ns-v-compute-port-margins nc side outside)
    (when outside
      (setf topmost (jl-first pcs) bottommost (jl-last pcs))
      (setf (.top (.port-margin topmost)) 0.0)
      (setf (.bottom (.port-margin bottommost)) 0.0)
      (when (and space-efficient-labels (not (.labels-next-to-port topmost)))
        (setf (.bottom (.port-margin topmost)) 0.0)))
    (when uniform
      (%ns-v-unify-port-margins pcs)
      (when outside
        (setf (.top (.port-margin topmost)) 0.0)
        (setf (.bottom (.port-margin bottommost)) 0.0)))))

(defun %ns-v-compute-port-margins (nc side outside)
  (declare (ignore outside))
  (jdo (pc (ns-port-contexts-get nc side))
    (let ((label-height (if (.port-label-cell pc) (get-minimum-height (.port-label-cell pc)) 0.0)))
      (cond ((> label-height 0)
             (if (.labels-next-to-port pc)
                 (let ((port-height (.y (get-size (.port pc)))))
                   (when (> label-height port-height)
                     (if (or (.port-labels-treat-as-group nc)
                             (= (length (get-labels (.port-label-cell pc))) 1))
                         (let ((overhang (/ (- label-height port-height) 2)))
                           (setf (.top (.port-margin pc)) overhang
                                 (.bottom (.port-margin pc)) overhang))
                         (let* ((first-label-height (.y (get-size (jl-get (get-labels (.port-label-cell pc)) 0))))
                                (first-overhang (/ (- first-label-height port-height) 2)))
                           (setf (.top (.port-margin pc)) (math.max 0.0 first-overhang))
                           (setf (.bottom (.port-margin pc)) (- label-height first-overhang port-height))))))
                 (setf (.bottom (.port-margin pc)) (+ (.port-label-spacing-vertical nc) label-height))))
            ((port-label-placement.is-fixed (.port-labels-placement nc))
             (let ((lb (elk-util.get-labels-bounds (.port pc))))
               (when (< (.y lb) 0)
                 (setf (.top (.port-margin pc)) (- (.y lb))))
               (when (> (+ (.y lb) (.height lb)) (.y (get-size (.port pc))))
                 (setf (.bottom (.port-margin pc))
                       (- (+ (.y lb) (.height lb)) (.y (get-size (.port pc))))))))))))

(defun %ns-v-unify-port-margins (pcs)
  (let ((max-top 0.0) (max-bottom 0.0))
    (jdo (pc pcs)
      (setf max-top (math.max max-top (.top (.port-margin pc)))
            max-bottom (math.max max-bottom (.bottom (.port-margin pc)))))
    (jdo (pc pcs)
      (setf (.top (.port-margin pc)) max-top
            (.bottom (.port-margin pc)) max-bottom))))

(defun %ns-v-port-height-plus-spacing (nc side)
  (let ((result 0.0)
        (pcs (ns-port-contexts-get nc side)))
    (loop for i from 0 below (length pcs)
          for pc = (aref pcs i)
          do (incf result (+ (.top (.port-margin pc)) (.y (get-size (.port pc))) (.bottom (.port-margin pc))))
             (when (< (1+ i) (length pcs))
               (incf result (.port-port-spacing nc))))
    result))

;;; ------------------------------------------------------------ PortPlacementCalculator

(defjstatic port-placement-calculator.place-horizontal-ports (nc)
  (enum-case (.port-constraints nc)
    (port-constraints.fixed-pos
     (%ns-place-h-fixed-pos nc port-side.north)
     (%ns-place-h-fixed-pos nc port-side.south))
    (port-constraints.fixed-ratio
     (%ns-place-h-fixed-ratio nc port-side.north)
     (%ns-place-h-fixed-ratio nc port-side.south))
    (t
     (%ns-place-h-free nc port-side.north)
     (%ns-place-h-free nc port-side.south))))

(defun %ns-place-h-fixed-pos (nc side)
  (jdo (pc (ns-port-contexts-get nc side))
    (setf (.y (.port-position pc)) (%ns-horizontal-port-y pc))))

(defun %ns-place-h-fixed-ratio (nc side)
  (let ((node-width (.x (.node-size nc))))
    (jdo (pc (ns-port-contexts-get nc side))
      (setf (.x (.port-position pc))
            (* node-width (get-property (.port pc) port-placement-calculator.port-ratio-or-position)))
      (setf (.y (.port-position pc)) (%ns-horizontal-port-y pc)))))

(defun %ns-free-port-start (nc side available calculated start)
  "The shared part of placeHorizontalFreePorts / placeVerticalFreePorts: returns
(values start-position space-between-ports)."
  (let* ((port-alignment (get-port-alignment nc side))
         (n (length (ns-port-contexts-get nc side)))
         (current start)
         (space-between (.port-port-spacing nc)))
    (when (and (or (eq port-alignment port-alignment.distributed)
                   (eq port-alignment port-alignment.justified))
               (= n 1))
      (setf calculated (%ns-modified-port-placement-size nc port-alignment calculated))
      (setf port-alignment port-alignment.center))
    (if (and (< available calculated)
             (not (contains (.size-options nc) size-options.ports-overhang)))
        (if (eq port-alignment port-alignment.distributed)
            (progn
              (incf space-between (/ (- available calculated) (+ n 1)))
              (incf current space-between))
            (incf space-between (/ (- available calculated) (- n 1))))
        (progn
          (when (< available calculated)
            (setf calculated (%ns-modified-port-placement-size nc port-alignment calculated))
            (setf port-alignment port-alignment.center))
          (enum-case port-alignment
            (port-alignment.begin nil)
            (port-alignment.center (incf current (/ (- available calculated) 2)))
            (port-alignment.end (incf current (- available calculated)))
            (port-alignment.distributed
             (let ((additional (/ (- available calculated) (+ n 1))))
               (incf space-between (math.max 0.0 additional))
               (incf current space-between)))
            (port-alignment.justified
             (let ((additional (/ (- available calculated) (- n 1))))
               (incf space-between (math.max 0.0 additional)))))))
    (values current space-between)))

(defun %ns-place-h-free (nc side)
  (when (jl-is-empty (ns-port-contexts-get nc side))
    (return-from %ns-place-h-free))
  (let* ((cell (%ns-ipl-cell nc side))
         (rect (get-cell-rectangle cell))
         (padding (get-padding cell))
         (available (- (.width rect) (.left padding) (.right padding))))
    (multiple-value-bind (current-x space-between)
        (%ns-free-port-start nc side available (.x (get-minimum-content-area-size cell))
                             (+ (.x rect) (.left padding)))
      (jdo (pc (ns-port-contexts-get nc side))
        (setf (.x (.port-position pc)) (+ current-x (.left (.port-margin pc))))
        (setf (.y (.port-position pc)) (%ns-horizontal-port-y pc))
        (incf current-x (+ (.left (.port-margin pc)) (.x (get-size (.port pc)))
                           (.right (.port-margin pc)) space-between))))))

(defun %ns-horizontal-port-y (pc)
  (let ((port (.port pc)))
    (if (has-property port core-options.port-border-offset)
        (if (eq (get-side port) port-side.north)
            (- (- (.y (get-size port))) (get-property port core-options.port-border-offset))
            (get-property port core-options.port-border-offset))
        (if (eq (get-side port) port-side.north)
            (- (.y (get-size port)))
            0.0))))

(defjstatic port-placement-calculator.place-vertical-ports (nc)
  (enum-case (.port-constraints nc)
    (port-constraints.fixed-pos
     (%ns-place-v-fixed-pos nc port-side.east)
     (%ns-place-v-fixed-pos nc port-side.west))
    (port-constraints.fixed-ratio
     (%ns-place-v-fixed-ratio nc port-side.east)
     (%ns-place-v-fixed-ratio nc port-side.west))
    (t
     (%ns-place-v-free nc port-side.east)
     (%ns-place-v-free nc port-side.west))))

(defun %ns-place-v-fixed-pos (nc side)
  (let ((node-width (.x (.node-size nc))))
    (jdo (pc (ns-port-contexts-get nc side))
      (setf (.x (.port-position pc)) (%ns-vertical-port-x pc node-width)))))

(defun %ns-place-v-fixed-ratio (nc side)
  (let ((node-size (.node-size nc)))
    (jdo (pc (ns-port-contexts-get nc side))
      (setf (.x (.port-position pc)) (%ns-vertical-port-x pc (.x node-size)))
      (setf (.y (.port-position pc))
            (* (.y node-size) (get-property (.port pc) port-placement-calculator.port-ratio-or-position))))))

(defun %ns-place-v-free (nc side)
  (when (jl-is-empty (ns-port-contexts-get nc side))
    (return-from %ns-place-v-free))
  (let* ((cell (%ns-ipl-cell nc side))
         (rect (get-cell-rectangle cell))
         (padding (get-padding cell))
         (available (- (.height rect) (.top padding) (.bottom padding)))
         (node-width (.x (.node-size nc))))
    (multiple-value-bind (current-y space-between)
        (%ns-free-port-start nc side available (.y (get-minimum-content-area-size cell))
                             (+ (.y rect) (.top padding)))
      (jdo (pc (ns-port-contexts-get nc side))
        (setf (.x (.port-position pc)) (%ns-vertical-port-x pc node-width))
        (setf (.y (.port-position pc)) (+ current-y (.top (.port-margin pc))))
        (incf current-y (+ (.top (.port-margin pc)) (.y (get-size (.port pc)))
                           (.bottom (.port-margin pc)) space-between))))))

(defun %ns-vertical-port-x (pc node-width)
  (let ((port (.port pc)))
    (if (has-property port core-options.port-border-offset)
        (if (eq (get-side port) port-side.west)
            (- (- (.x (get-size port))) (get-property port core-options.port-border-offset))
            (+ node-width (get-property port core-options.port-border-offset)))
        (if (eq (get-side port) port-side.west)
            (- (.x (get-size port)))
            node-width))))

(defun %ns-modified-port-placement-size (nc old-alignment current)
  (if (eq old-alignment port-alignment.distributed)
      (- current (* 2 (.port-port-spacing nc)))
      current))
