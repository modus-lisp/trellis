;;;; common/polyomino.lisp -- org.eclipse.elk.alg.common.polyomino
;;;; PolyominoCompactor, its comparators, and ProfileFill.

(in-package #:elk)

;;; ------------------------------------------------------------ comparators
;;; Java Comparator.compare returns -1/0/1 from the same int comparisons.

(defun %polyomino-cmp (a b)
  (cond ((< a b) -1)
        ((= a b) 0)
        (t 1)))

(defun %direction-set (poly)
  "Set<Direction> of the extensions' directions (Collectors.toSet)."
  (let ((dir-set (new-jset)))
    (jdo (ext (get-polyomino-extensions poly))
      (js-add dir-set (get-first ext)))
    dir-set))

(defjmethod compare ((this min-perimeter-comparator) o1 o2)
  (let ((half-peri-1 (+ (get-width o1) (get-height o1)))
        (half-peri-2 (+ (get-width o2) (get-height o2))))
    (%polyomino-cmp half-peri-1 half-peri-2)))

(defjmethod compare ((this min-perimeter-comparator-with-shape) o1 o2)
  (flet ((shape-value (grid)
           (let ((width (get-width grid))
                 (height (get-height grid)))
             (if (< width height)
                 (setf width (* width width))
                 (setf height (* height height)))
             (+ width height))))
    (%polyomino-cmp (shape-value o1) (shape-value o2))))

(defjmethod compare ((this min-num-of-extensions-comparator) o1 o2)
  (%polyomino-cmp (size (get-polyomino-extensions o1))
                  (size (get-polyomino-extensions o2))))

(defjmethod compare ((this min-num-of-extension-directions-comparator) o1 o2)
  (%polyomino-cmp (size (%direction-set o1))
                  (size (%direction-set o2))))

(defjmethod compare ((this single-extension-side-greater-than-rest-comparator) o1 o2)
  (let ((num-dir-1 (if (= (size (%direction-set o1)) 1) 1 0))
        (num-dir-2 (if (= (size (%direction-set o2)) 1) 1 0)))
    (%polyomino-cmp num-dir-1 num-dir-2)))

(defun %corner-case-value (poly)
  (let* ((dir-set (%direction-set poly))
         (num-dir (if (= (size dir-set) 2) 1 0)))
    (when (= num-dir 1)
      (let ((horizontal 0))
        (jdo (d dir-set)
          (when (is-horizontal d)
            (incf horizontal)))
        (when (= (mod horizontal 2) 0)
          (setf num-dir 0))))
    num-dir))

(defjmethod compare ((this corner-cases-greater-than-rest-comparator) o1 o2)
  (%polyomino-cmp (%corner-case-value o1) (%corner-case-value o2)))

;;; ------------------------------------------------------------ ProfileFill

(defjstatic profile-fill.fill-polyomino (poly)
  (let* ((width (get-width poly))
         (north-profile (make-array width :initial-element 0))
         (south-profile (make-array width :initial-element 0))
         (height (get-height poly))
         (east-profile (make-array height :initial-element 0))
         (west-profile (make-array height :initial-element 0)))
    (dotimes (xi width)
      (let ((y 0))
        (loop while (and (< y height) (not (is-blocked poly xi y)))
              do (incf y))
        (setf (aref north-profile xi) y)))
    (dotimes (xi width)
      (let ((y (- height 1)))
        (loop while (and (>= y 0) (not (is-blocked poly xi y)))
              do (decf y))
        (setf (aref south-profile xi) y)))
    (dotimes (yi height)
      (let ((x 0))
        (loop while (and (< x width) (not (is-blocked poly x yi)))
              do (incf x))
        (setf (aref east-profile yi) x)))
    (dotimes (yi height)
      (let ((x (- width 1)))
        (loop while (and (>= x 0) (not (is-blocked poly x yi)))
              do (decf x))
        (setf (aref west-profile yi) x)))
    (dotimes (xi width)
      (dotimes (yi height)
        (when (and (< xi (aref west-profile yi))
                   (> xi (aref east-profile yi))
                   (< yi (aref south-profile xi))
                   (> yi (aref north-profile xi)))
          (set-blocked poly xi yi))))
    nil))

;;; ------------------------------------------------------------ PolyominoCompactor

(defun %comparator-fn (c)
  (lambda (a b) (compare c a b)))

(defjmethod pack-polyominoes ((this polyomino-compactor) poly-holder)
  (let ((polys (get-polyominoes poly-holder))
        (grid (get-grid poly-holder)))
    (enum-case (get-property poly-holder polyomino-options.polyomino-low-level-sort)
      (low-level-sorting-criterion.by-size
       (jl-sort polys (reversed-comparator (%comparator-fn (new-min-perimeter-comparator)))))
      (t ; BY_SIZE_AND_SHAPE and default
       (jl-sort polys (reversed-comparator (%comparator-fn (new-min-perimeter-comparator-with-shape))))))
    (enum-case (get-property poly-holder polyomino-options.polyomino-high-level-sort)
      (high-level-sorting-criterion.corner-cases-than-single-side-last
       (jl-sort polys (%comparator-fn (new-min-num-of-extensions-comparator)))
       (jl-sort polys (%comparator-fn (new-single-extension-side-greater-than-rest-comparator)))
       (jl-sort polys (%comparator-fn (new-corner-cases-greater-than-rest-comparator))))
      (t ; NUM_OF_EXTERNAL_SIDES_THAN_NUM_OF_EXTENSIONS_LAST and default
       (jl-sort polys (%comparator-fn (new-min-num-of-extensions-comparator)))
       (jl-sort polys (%comparator-fn (new-min-num-of-extension-directions-comparator)))))
    (let ((successor-based-on-cost
            (enum-case (get-property poly-holder polyomino-options.polyomino-traversal-strategy)
              (traversal-strategy.spiral
               (new-successor-max-norm-winding-in-math-pos-sense))
              (traversal-strategy.line-by-line
               (new-successor-line-by-line))
              (traversal-strategy.manhattan
               (new-successor-manhattan))
              (traversal-strategy.jitter
               (new-successor-jitter))
              (traversal-strategy.quadrants-manhattan
               (new-successor-quadrants-generic (new-successor-manhattan)))
              (traversal-strategy.quadrants-line-by-line
               (new-successor-quadrants-generic (new-successor-line-by-line)))
              (traversal-strategy.combine-line-by-line-manhattan
               (new-successor-combination
                (new-successor-quadrants-generic (new-successor-line-by-line))
                (new-successor-quadrants-generic (new-successor-manhattan))))
              (traversal-strategy.combine-jitter-manhattan
               (new-successor-combination
                (new-successor-quadrants-generic (new-successor-jitter))
                (new-successor-quadrants-generic (new-successor-manhattan))))
              (t ; QUADRANTS_JITTER and default
               (new-successor-quadrants-generic (new-successor-jitter))))))
      (jdo (poly polys)
        (let ((off-x 0)
              (off-y 0)
              (next (new-pair 0 0)))
          (loop while (intersects-with-center-based grid poly off-x off-y)
                do (setf next (j-apply successor-based-on-cost next poly))
                   (setf off-x (get-first next)
                         off-y (get-second next)))
          (add-filled-cells-from grid poly off-x off-y))))
    nil))
