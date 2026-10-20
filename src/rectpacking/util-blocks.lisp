;;;; util-blocks.lisp -- org.eclipse.elk.alg.rectpacking.util: Block, BlockRow,
;;;; BlockStack, RectRow.
;;;;
;;;; Block is named J-BLOCK: the Java simple name BLOCK is the CL special
;;;; operator, which ELK's package uses, so the class symbol cannot be BLOCK.
;;;; NEW-BLOCK/4 is provided by hand under the mechanical name.

(in-package #:elk)

;;; ------------------------------------------------------------ Block

(defun new-block/4 (x-coord y-coord parent-row node-node-spacing)
  (new-j-block/4 x-coord y-coord parent-row node-node-spacing))
(define-jcall new-block :static)

(defjctor j-block (x-coord y-coord parent-row node-node-spacing)
  (setf (.node-node-spacing this) node-node-spacing
        (.parent-row this) parent-row
        (.x this) x-coord
        (.y this) y-coord
        (.width this) 0d0
        (.height this) 0d0))

(defun %rp-block-adjust-size-add (this rect)
  (let* ((width-of-last-row (get-width/0 (get-last-row this)))
         (nns (.node-node-spacing this))
         (nchildren (jsize (.children this))))
    (setf (.smallest-rect-width this) (math.min (.smallest-rect-width this) (get-width/0 rect)))
    (setf (.width this) (math.max (.width this) width-of-last-row))
    (setf (.min-width this) (math.max (.min-width this)
                                      (+ (get-width/0 rect) (if (= nchildren 1) 0d0 nns))))
    (setf (.smallest-rect-height this) (math.min (.smallest-rect-height this) (get-height/0 rect)))
    (setf (.max-height this) (+ (.max-height this) (get-height/0 rect) (if (= nchildren 1) 0d0 nns)))
    (setf (.min-height this) (math.max (.min-height this) (get-height/0 rect)))
    (let ((total-height (if (> (jsize (.rows this)) 0) (* (jd (1- (jsize (.rows this)))) nns) 0d0)))
      (jdo (row (.rows this))
        (incf total-height (get-height/0 row)))
      (setf (.height this) total-height))
    (setf (.average-height this)
          (- (/ (.max-height this) (jd nchildren))
             (* nns (/ (jd (1- nchildren)) (jd nchildren)))))
    (notify-about-node-change/0 (.parent-row this))))

(defjmethod add-child ((this j-block) rect)
  (when (jl-is-empty (.rows this))
    (jl-add (.rows this) (new-block-row/3 (.x this) (.y this) (.node-node-spacing this))))
  (jl-add (.children this) rect)
  (add-rectangle/1 (jl-get (.rows this) (1- (length (.rows this)))) rect)
  (%rp-block-adjust-size-add this rect))

(defjmethod add-child-in-new-row ((this j-block) rect)
  (jl-add (.children this) rect)
  (let ((last-row (get-last-row/0 this)))
    (jl-add (.rows this) (new-block-row/3 (.x this)
                                          (+ (get-y/0 last-row) (get-height/0 last-row) (.node-node-spacing this))
                                          (.node-node-spacing this))))
  (add-rectangle/1 (get-last-row/0 this) rect)
  (%rp-block-adjust-size-add this rect))

(defjmethod remove-child ((this j-block) rect)
  (jl-remove (.children this) rect)
  (block found
    (jdo (row (.rows this))
      (when (jl-contains (get-nodes/0 row) rect)
        (remove-rectangle/2 row rect t)
        (when (jl-is-empty (get-nodes/0 row))
          (jl-remove (.rows this) row))
        (return-from found))))
  (%rp-block-adjust-size-after-remove this))

(defjmethod set-location ((this j-block) x-coord y-coord)
  (%rp-block-adjust-children-xy this (- x-coord (.x this)) (- y-coord (.y this)))
  (jdo (row (.rows this))
    (set-x/1 row (+ (get-x/0 row) x-coord (- (.x this))))
    (set-y/1 row (+ (get-y/0 row) y-coord (- (.y this)))))
  (setf (.x this) x-coord
        (.y this) y-coord)
  nil)

(defun %rp-block-adjust-children-xy (this x-change y-change)
  (jdo (rect (.children this))
    (set-location/2 rect (+ (get-x/0 rect) x-change) (+ (get-y/0 rect) y-change))))

;;; Overloads of placeRectsIn: (width) public boolean; (width, height)
;;; public boolean; (width, placeRects) private ElkRectangle; (width, height,
;;; placeRects) private boolean.  Arity 2 is split on the second argument.

(defjmethod place-rects-in ((this j-block) width)
  "placeRectsIn(double): true iff the bounds changed."
  (let* ((old-width (.width this))
         (old-height (.height this))
         (bounds (%rp-block-place-rects-bounds this width t)))
    (or (/= (.width bounds) old-width) (/= (.height bounds) old-height))))

(defjmethod place-rects-in ((this j-block) width second)
  (if (numberp second)
      (%rp-block-place-rects-fits this width second t)
      (%rp-block-place-rects-bounds this width second)))

(defun %rp-block-place-rects-fits (this width height place-rects)
  (let ((bounds (%rp-block-place-rects-bounds this width place-rects)))
    (and (<= (.width bounds) width) (<= (.height bounds) height))))

(defun %rp-block-place-rects-bounds (this width place-rects)
  (let ((current-x 0d0)
        (current-y (.y this))
        (current-width 0d0)
        (current-height 0d0)
        (max-height-in-row 0d0)
        (width-in-row 0d0)
        (row 0)
        (nns (.node-node-spacing this)))
    (when place-rects
      (jl-clear (.rows this))
      (jl-add (.rows this) (new-block-row/3 (.x this) (.y this) nns)))
    (let ((index 0))
      (jdo (rect (.children this))
        (when (and (> (+ current-x (get-width/0 rect) (if (> index 0) nns 0d0)) width)
                   (> max-height-in-row 0))
          (setf current-x 0d0)
          (incf current-y (+ max-height-in-row nns))
          (setf current-width (math.max current-width width-in-row))
          (incf current-height (+ max-height-in-row nns))
          (setf max-height-in-row 0d0
                width-in-row 0d0)
          (when place-rects
            (incf row)
            (jl-add (.rows this) (new-block-row/3 (.x this) current-y nns)))
          (setf index 0))
        (incf width-in-row (+ (get-width/0 rect) (if (> index 0) nns 0d0)))
        (setf max-height-in-row (math.max max-height-in-row (get-height/0 rect)))
        (when place-rects
          (add-rectangle/1 (jl-get (.rows this) row) rect))
        (incf current-x (+ (get-width/0 rect) (if (> index 0) nns 0d0)))
        (incf index)))
    (setf current-width (math.max current-width width-in-row))
    (incf current-height max-height-in-row)
    (when place-rects
      (setf (.width this) current-width
            (.height this) current-height)
      (notify-about-node-change/0 (.parent-row this)))
    (make-instance 'elk-rectangle :x (.x this) :y (.y this)
                                  :width current-width :height current-height)))

(defun %rp-block-adjust-size-after-remove (this)
  (let ((new-width 0d0)
        (new-height 0d0)
        (rows-to-delete (new-jlist))
        (nns (.node-node-spacing this))
        (index 0))
    (jdo (row (.rows this))
      (if (jl-is-empty (get-nodes/0 row))
          (jl-add rows-to-delete row)
          (progn
            (setf new-width (math.max new-width (get-width/0 row)))
            (incf new-height (+ (get-height/0 row) (if (> index 0) nns 0d0)))))
      (incf index))
    (jl-remove-if (.rows this) (lambda (r) (find r rows-to-delete :test #'eq)))
    (setf (.height this) new-height
          (.width this) new-width
          (.min-width this) 0d0
          (.min-height this) 0d0
          (.max-height this) 0d0
          (.smallest-rect-height this) double.positive-infinity
          (.smallest-rect-width this) double.positive-infinity)
    (jdo (rect (.children this))
      (setf (.smallest-rect-width this) (math.min (.smallest-rect-width this) (get-width/0 rect)))
      (setf (.min-width this) (math.max (.min-width this) (get-width/0 rect)))
      (setf (.min-height this) (math.max (.min-height this) (get-height/0 rect)))
      (setf (.smallest-rect-height this) (math.min (.smallest-rect-height this) (get-height/0 rect)))
      (incf (.max-height this) (+ (get-height/0 rect) nns)))
    (setf (.average-height this)
          (- (/ (.max-height this) (jd (jsize (.children this))))
             (* nns (/ (jd (1- (jsize (.children this)))) (jd (jsize (.children this)))))))
    (notify-about-node-change/0 (.parent-row this))))

(defjmethod get-width-for-target-height ((this j-block) height)
  (cond ((<= (.max-height this) height) (.min-width this))
        ((%rp-block-place-rects-fits this (.min-width this) height nil) (.min-width this))
        (t
         (let* ((upper-bound (.width this))
                (lower-bound (.min-width this))
                (viable-width (.width this))
                (new-width (+ (/ (- upper-bound lower-bound) 2) lower-bound)))
           (loop while (< (+ lower-bound 1) upper-bound)
                 do (if (%rp-block-place-rects-fits this new-width height nil)
                        (progn (setf viable-width new-width)
                               (setf upper-bound new-width))
                        (setf lower-bound new-width))
                    (setf new-width (+ (/ (- upper-bound lower-bound) 2) lower-bound)))
           viable-width))))

(defjmethod get-height-for-target-width ((this j-block) width)
  (let ((bounds (%rp-block-place-rects-bounds this width nil)))
    (.height bounds)))

(defjmethod expand ((this j-block) additional-width-per-block additional-height-for-block)
  (let* ((width-for-row (+ (.width this) additional-width-per-block))
         (additional-height-for-row (/ additional-height-for-block (jd (jsize (.rows this)))))
         (index 0))
    (incf (.width this) additional-width-per-block)
    (incf (.height this) additional-height-for-block)
    (jdo (row (.rows this))
      (expand/3 row width-for-row additional-height-for-row index)
      (incf index))))

(defjmethod get-children ((this j-block)) (.children this))
(defjmethod get-width ((this j-block)) (.width this))
(defjmethod set-width ((this j-block) w) (setf (.width this) w) nil)
(defjmethod get-height ((this j-block)) (.height this))
(defjmethod set-height ((this j-block) h) (setf (.height this) h) nil)
(defjmethod get-x ((this j-block)) (.x this))
(defjmethod get-y ((this j-block)) (.y this))
(defjmethod get-parent-row ((this j-block)) (.parent-row this))
(defjmethod set-parent-row ((this j-block) parent-row) (setf (.parent-row this) parent-row) nil)
(defjmethod get-min-width ((this j-block)) (.min-width this))
(defjmethod get-min-height ((this j-block)) (.min-height this))
(defjmethod get-max-height ((this j-block)) (.max-height this))
(defjmethod get-smallest-rect-height ((this j-block)) (.smallest-rect-height this))
(defjmethod get-average-height ((this j-block)) (.average-height this))
(defjmethod is-fixed ((this j-block)) (.fixed this))
(defjmethod set-fixed ((this j-block) fixed) (setf (.fixed this) fixed) nil)
(defjmethod is-position-fixed ((this j-block)) (.position-fixed this))
(defjmethod set-position-fixed ((this j-block) v) (setf (.position-fixed this) v) nil)
(defjmethod get-rows ((this j-block)) (.rows this))

(defjmethod get-last-row-new-x ((this j-block))
  (let ((last-row (get-last-row/0 this)))
    (+ (get-x/0 last-row) (get-width/0 last-row))))

(defjmethod get-last-row-y ((this j-block))
  (get-y/0 (get-last-row/0 this)))

(defjmethod get-last-row ((this j-block))
  (jl-get (.rows this) (1- (length (.rows this)))))

(defjmethod get-stack ((this j-block)) (.stack this))
(defjmethod set-stack ((this j-block) stack) (setf (.stack this) stack) nil)
(defjmethod get-smallest-rect-width ((this j-block)) (.smallest-rect-width this))
(defjmethod set-smallest-rect-width ((this j-block) v) (setf (.smallest-rect-width this) v) nil)
(defjmethod reset-block ((this j-block)) (%rp-block-adjust-size-after-remove this))

;;; ------------------------------------------------------------ BlockRow

(defjctor block-row (x y node-node-spacing)
  (setf (.x this) x
        (.y this) y
        (.node-node-spacing this) node-node-spacing))

(defjmethod add-rectangle ((this block-row) rect)
  (set-x/1 rect (+ (.x this) (.width this) (if (jl-is-empty (.rects this)) 0d0 (.node-node-spacing this))))
  (set-y/1 rect (.y this))
  (setf (.height this) (math.max (.height this) (get-height/0 rect)))
  (incf (.width this) (+ (get-width/0 rect) (if (jl-is-empty (.rects this)) 0d0 (.node-node-spacing this))))
  (jl-add (.rects this) rect)
  t)

(defjmethod remove-rectangle ((this block-row) rect update)
  (jl-remove (.rects this) rect)
  (when update
    (update-row/0 this))
  nil)

(defjmethod update-row ((this block-row))
  (let ((width 0d0)
        (height 0d0)
        (nns (.node-node-spacing this)))
    (jdo (rect (.rects this))
      (set-x/1 rect (+ (.x this) width))
      (set-y/1 rect (.y this))
      (incf width (+ (get-width/0 rect) nns))
      (setf height (math.max height (+ (get-height/0 rect) nns))))
    (setf (.width this) (- width nns))
    (setf (.height this) (- height nns))
    nil))

(defjmethod expand ((this block-row) width-for-row additional-height-for-row index)
  (let* ((additional-width-for-rect (/ (- width-for-row (.width this)) (jd (jsize (.rects this)))))
         (i 0))
    (incf (.height this) additional-height-for-row)
    (setf (.width this) width-for-row)
    (jdo (rect (.rects this))
      (let ((old-width (get-width/0 rect))
            (old-height (get-height/0 rect)))
        (set-x/1 rect (+ (get-x/0 rect) (* i additional-width-for-rect)))
        (set-y/1 rect (+ (get-y/0 rect) (* index additional-height-for-row)))
        (set-width/1 rect (+ (get-width/0 rect) additional-width-for-rect))
        (set-height/1 rect (.height this))
        (incf i)
        (let ((new-width (get-width/0 rect))
              (new-height (get-height/0 rect)))
          (elk-util.translate rect (new-k-vector/2 new-width new-height)
                              (new-k-vector/2 old-width old-height)))))
    nil))

(defjmethod get-y ((this block-row)) (.y this))
(defjmethod set-y ((this block-row) y) (setf (.y this) y) nil)
(defjmethod get-width ((this block-row)) (.width this))
(defjmethod get-height ((this block-row)) (.height this))
(defjmethod get-nodes ((this block-row)) (.rects this))
(defjmethod get-x ((this block-row)) (.x this))
(defjmethod set-x ((this block-row) x) (setf (.x this) x) nil)

;;; ------------------------------------------------------------ BlockStack

(defjctor block-stack (x y node-node-spacing)
  (setf (.blocks this) (new-jlist)
        (.x this) x
        (.y this) y
        (.node-node-spacing this) node-node-spacing))

(defjmethod add-block ((this block-stack) block)
  (set-stack/1 block this)
  (setf (.width this) (math.max (.width this) (get-width/0 block)))
  (incf (.height this) (+ (get-height/0 block) (if (jl-is-empty (.blocks this)) 0d0 (.node-node-spacing this))))
  (jl-add (.blocks this) block)
  nil)

(defjmethod update-dimension ((this block-stack))
  (let ((height 0d0)
        (width 0d0)
        (index 0))
    (jdo (block (.blocks this))
      (setf width (math.max width (get-width/0 block)))
      (incf height (+ (get-height/0 block) (if (> index 0) (.node-node-spacing this) 0d0)))
      (incf index))
    (setf (.height this) height
          (.width this) width)
    nil))

(defjmethod set-location ((this block-stack) x y)
  (let ((x-diff (- x (.x this)))
        (y-diff (- y (.y this))))
    (jdo (block (.blocks this))
      (set-location/2 block (+ (get-x/0 block) x-diff) (+ (get-y/0 block) y-diff)))
    (setf (.x this) x
          (.y this) y)
    nil))

(defjmethod get-width-for-fixed-height ((this block-stack) height)
  (if (= (jsize (.blocks this)) 1)
      (get-width-for-target-height/1 (jl-get (.blocks this) 0) height)
      (let* ((min-width (%rp-block-stack-minimum-width this))
             (total-height 0d0)
             (upper-bound (.width this))
             (lower-bound min-width)
             (viable-width (.width this))
             (new-width (+ (/ (- upper-bound lower-bound) 2) lower-bound)))
        (loop while (< (+ lower-bound 1) upper-bound)
              do (setf total-height 0d0)
                 (jdo (block (.blocks this))
                   (incf total-height (get-height-for-target-width/1 block new-width)))
                 (if (< total-height height)
                     (progn (setf viable-width new-width)
                            (setf upper-bound new-width))
                     (setf lower-bound new-width))
                 (setf new-width (+ (/ (- upper-bound lower-bound) 2) lower-bound)))
        viable-width)))

(defun %rp-block-stack-minimum-width (this)
  (let ((min-width 0d0))
    (jdo (block (.blocks this))
      (setf min-width (math.max min-width (get-min-width/0 block))))
    min-width))

(defjmethod place-rects-in ((this block-stack) target-width)
  (let ((current-y (get-y/0 this))
        (current-height 0d0)
        (current-width 0d0))
    (jdo (block (.blocks this))
      (set-location/2 block (.x this) current-y)
      (place-rects-in/1 block target-width)
      (setf current-width (math.max current-width (get-width/0 block)))
      (incf current-y (+ (get-height/0 block) (.node-node-spacing this)))
      (setf current-height current-y))
    (setf (.width this) current-width
          (.height this) current-height)
    nil))

(defjmethod expand ((this block-stack) additional-width additional-height)
  (let ((index 0)
        (additional-height-per-block (/ additional-height (jd (jsize (.blocks this))))))
    (jdo (block (.blocks this))
      (set-location/2 block (get-x/0 block) (+ (get-y/0 block) (* index additional-height-per-block)))
      (expand/2 block (+ (- (get-width/0 this) (get-width/0 block)) additional-width) additional-height-per-block)
      (incf index))
    nil))

(defjmethod get-blocks ((this block-stack)) (.blocks this))
(defjmethod get-x ((this block-stack)) (.x this))
(defjmethod get-y ((this block-stack)) (.y this))
(defjmethod get-width ((this block-stack)) (.width this))
(defjmethod set-width ((this block-stack) w) (setf (.width this) w) nil)
(defjmethod get-height ((this block-stack)) (.height this))
(defjmethod set-height ((this block-stack) h) (setf (.height this) h) nil)

;;; ------------------------------------------------------------ RectRow

(defjctor rect-row (y node-node-spacing)
  (setf (.y this) y
        (.node-node-spacing this) node-node-spacing))

(defjmethod notify-about-node-change ((this rect-row))
  (let ((total-stack-width 0d0)
        (new-max-height double.negative-infinity)
        (index 0))
    (jdo (child (.children this))
      (incf total-stack-width (+ (get-width/0 child) (if (> index 0) (.node-node-spacing this) 0d0)))
      (setf new-max-height (math.max new-max-height (get-height/0 child)))
      (incf index))
    (setf (.width this) total-stack-width
          (.height this) new-max-height)
    nil))

(defjmethod expand ((this rect-row) width additional-height)
  (let* ((additional-width (- width (.width this)))
         (additional-width-per-stack (/ additional-width (jd (jsize (.stacks this)))))
         (index 0))
    (jdo (stack (.stacks this))
      (let ((additional-height-for-stack (+ (- (get-height/0 this) (get-height/0 stack)) additional-height)))
        (set-location/2 stack (+ (get-x/0 stack) (* index additional-width-per-stack)) (get-y/0 stack))
        (expand/2 stack additional-width-per-stack additional-height-for-stack))
      (incf index))
    nil))

(defjmethod calculate-block-stacks ((this rect-row))
  (let ((current-x -1d0))
    (jdo (block (.children this))
      (if (/= (get-x/0 block) current-x)
          (progn
            (jl-add (.stacks this) (new-block-stack/3 (get-x/0 block) (.y this) (.node-node-spacing this)))
            (add-block/1 (jl-get (.stacks this) (1- (length (.stacks this)))) block)
            (setf current-x (get-x/0 block)))
          (add-block/1 (jl-get (.stacks this) (1- (length (.stacks this)))) block)))
    nil))

(defjmethod get-first-block ((this rect-row)) (jl-get (.children this) 0))
(defjmethod get-last-block ((this rect-row)) (jl-get (.children this) (1- (length (.children this)))))
(defjmethod get-number-of-assigned-blocks ((this rect-row)) (length (.children this)))

(defjmethod add-block ((this rect-row) block)
  (setf (.height this) (math.max (.height this) (get-height/0 block)))
  (incf (.width this) (+ (get-width/0 block) (if (jl-is-empty (.children this)) 0d0 (.node-node-spacing this))))
  (jl-add (.children this) block)
  nil)

(defjmethod remove-block ((this rect-row) block)
  (jl-remove (.children this) block)
  (decf (.width this) (+ (get-width/0 block) (if (jl-is-empty (.children this)) 0d0 (.node-node-spacing this))))
  (let ((new-max-height double.min-value))
    (jdo (child (.children this))
      (setf new-max-height (math.max new-max-height (get-height/0 child))))
    (setf (.height this) new-max-height))
  nil)

(defjmethod get-height ((this rect-row)) (.height this))
(defjmethod set-height ((this rect-row) h) (setf (.height this) h) nil)
(defjmethod get-width ((this rect-row)) (.width this))
(defjmethod set-width ((this rect-row) w) (setf (.width this) w) nil)
(defjmethod get-y ((this rect-row)) (.y this))
(defjmethod set-y ((this rect-row) y)
  (let ((y-change (- y (.y this))))
    (jdo (stack (.stacks this))
      (set-location/2 stack (get-x/0 stack) (+ (get-y/0 stack) y-change))))
  (setf (.y this) y)
  nil)
(defjmethod get-children ((this rect-row)) (.children this))
(defjmethod get-stacks ((this rect-row)) (.stacks this))
(defjmethod get-potential-additional-width-to-get-last-block ((this rect-row))
  (.potential-additional-width-to-get-last-block this))
(defjmethod set-potential-additional-width-to-get-last-block ((this rect-row) v)
  (setf (.potential-additional-width-to-get-last-block this) v)
  nil)
(defjmethod reset-stacks ((this rect-row))
  (setf (.stacks this) (new-jlist))
  nil)
