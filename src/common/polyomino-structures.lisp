;;;; common/polyomino-structures.lisp -- org.eclipse.elk.alg.common.polyomino.structures
;;;;
;;;; TwoBitGrid stores two bits per cell.  Java packs 32 cells into one 64-bit
;;;; long; here each 64-bit word is two 32-bit halves (elements 2w and 2w+1 of
;;;; the row), so every value stays a fixnum (no bignums on the 64-bit path).
;;;; Cell x of word w: half = (x mod 32) >= 16, shift = 2 * ((x mod 32) mod 16).
;;;; That is bit-for-bit the Java field at bit 2*(x mod 32) of the long, because
;;;; a two-bit field never straddles the 32-bit boundary (positions are even).
;;;; The Java operations (>>>, &, |, ~mask) are reproduced with explicit masks.

(in-package #:elk)

;;; ------------------------------------------------------------ Direction

(defjmethod is-horizontal ((this common-direction))
  ;; EAST and WEST are horizontal; NORTH and SOUTH are not.
  (or (eq this common-direction.east) (eq this common-direction.west)))

;;; ------------------------------------------------------------ TwoBitGrid

(defun %two-bit-make-rows (width height)
  "Java: new long[height][(int) Math.ceil(width / 32.0)], each word as 2 halves."
  (let ((rows (make-array height))
        (halves (* 2 (ceiling width 32))))
    (dotimes (y height)
      (setf (svref rows y) (make-array halves :initial-element 0)))
    rows))

(defun %two-bit-oob (this x y)
  (jthrow 'index-out-of-bounds-exception
          "Grid is only of size ~D*~D. Requested point (~D, ~D) is out of bounds."
          (.x-size this) (.y-size this) x y))

(defun %two-bit-locate (this x y)
  "Java retrieve/set: (values element-index shift).  Throws where Java's
ArrayIndexOutOfBoundsException would be turned into IndexOutOfBoundsException."
  (let* ((rest (logand x #x1f))
         (element (+ (* 2 (ash x -5)) (if (>= rest 16) 1 0)))
         (grid (.grid this)))
    (unless (and (<= 0 y) (< y (length grid))
                 (<= 0 element) (< element (length (svref grid y))))
      (%two-bit-oob this x y))
    (values element (* 2 (logand rest 15)))))

(defun %two-bit-retrieve (this x y)
  (multiple-value-bind (element shift) (%two-bit-locate this x y)
    (logand (ash (svref (svref (.grid this) y) element) (- shift)) 3)))

(defun %two-bit-set (this x y msb lsb)
  (when (>= x (.x-size this))
    (%two-bit-oob this x y))
  (multiple-value-bind (element shift) (%two-bit-locate this x y)
    (let* ((row (svref (.grid this) y))
           (lsb-mask (ash 1 shift))
           (msb-mask (ash 1 (+ shift 1)))
           (v (svref row element)))
      ;; Java: word |= mask  or  word &= ~mask  (lognot is taken on the
      ;; non-negative word, so logandc2 gives the same bits)
      (setf v (if lsb (logior v lsb-mask) (logandc2 v lsb-mask)))
      (setf v (if msb (logior v msb-mask) (logandc2 v msb-mask)))
      (setf (svref row element) v))))

(defjctor two-bit-grid ()
  (this-ctor two-bit-grid this 0 0))

(defjctor two-bit-grid (width height)
  (setf (.grid this) (%two-bit-make-rows width height)
        (.x-size this) width
        (.y-size this) height))

(defjmethod get-width ((this two-bit-grid)) (.x-size this))
(defjmethod get-height ((this two-bit-grid)) (.y-size this))

(defjmethod is-empty ((this two-bit-grid) x y)
  (= (%two-bit-retrieve this x y) 0))

(defjmethod is-blocked ((this two-bit-grid) x y)
  (= (%two-bit-retrieve this x y) 1))

(defjmethod is-weakly-blocked ((this two-bit-grid) x y)
  (= (%two-bit-retrieve this x y) 2))

(defjmethod in-bounds ((this two-bit-grid) x y)
  (and (>= x 0) (>= y 0) (< x (.x-size this)) (< y (.y-size this))))

(defjmethod reinitialize ((this two-bit-grid) width height)
  (setf (.grid this) (%two-bit-make-rows width height)
        (.x-size this) width
        (.y-size this) height))

(defjmethod set-empty ((this two-bit-grid) x y)
  (%two-bit-set this x y nil nil))

(defjmethod set-blocked ((this two-bit-grid) x y)
  (%two-bit-set this x y nil t))

(defjmethod set-weakly-blocked ((this two-bit-grid) x y)
  (%two-bit-set this x y t nil))

(defjmethod to-string ((this two-bit-grid))
  (flet ((inc-mod-ten (num) (if (> num 8) 0 (+ num 1))))
    (let ((out (make-string-output-stream)))
      (write-string " " out)
      (let ((count 0))
        (dotimes (x (.x-size this))
          (format out "~D" count)
          (setf count (inc-mod-ten count))))
      (format out "~%")
      (let ((count 0))
        (dotimes (y (.y-size this))
          (format out "~D" count)
          (setf count (inc-mod-ten count))
          (dotimes (x (.x-size this))
            (let ((item (%two-bit-retrieve this x y)))
              (write-string (cond ((= item 0) "_") ((= item 1) "X") (t "0")) out)))
          (format out "~%")))
      (let ((s (get-output-stream-string out)))
        (subseq s 0 (- (length s) 1))))))

;;; ------------------------------------------------------------ PlanarGrid

(defjctor planar-grid ()
  ;; PlanarGrid(): super(0, 0) and xCenter = yCenter = 0 explicitly
  ;; (not (0-1)>>1 as the two-argument constructor would give).
  (super-ctor two-bit-grid this 0 0)
  (setf (.x-center this) 0
        (.y-center this) 0))

(defjctor planar-grid (width height)
  (super-ctor two-bit-grid this width height)
  (setf (.x-center this) (ash (- width 1) -1)
        (.y-center this) (ash (- height 1) -1)))

(defjmethod get-center-x ((this planar-grid)) (.x-center this))
(defjmethod get-center-y ((this planar-grid)) (.y-center this))

(defjmethod is-empty-center-based ((this planar-grid) x y)
  (is-empty this (+ x (.x-center this)) (+ y (.y-center this))))

(defjmethod is-blocked-center-based ((this planar-grid) x y)
  (is-blocked this (+ x (.x-center this)) (+ y (.y-center this))))

(defjmethod is-weakly-blocked-center-based ((this planar-grid) x y)
  (is-weakly-blocked this (+ x (.x-center this)) (+ y (.y-center this))))

(defjmethod in-bounds-center-based ((this planar-grid) x y)
  (in-bounds this (+ x (.x-center this)) (+ y (.y-center this))))

(defjmethod set-blocked-center-based ((this planar-grid) x y)
  (set-blocked this (+ x (.x-center this)) (+ y (.y-center this))))

(defjmethod set-empty-center-based ((this planar-grid) x y)
  (set-empty this (+ x (.x-center this)) (+ y (.y-center this))))

(defjmethod set-weakly-blocked-center-based ((this planar-grid) x y)
  (set-weakly-blocked this (+ x (.x-center this)) (+ y (.y-center this))))

(defjmethod reinitialize ((this planar-grid) width height)
  (call-next-method)
  (setf (.x-center this) (ash (- width 1) -1)
        (.y-center this) (ash (- height 1) -1)))

(defjmethod weakly-block-area ((this planar-grid) x-upper-left y-upper-left
                               x-bottom-right y-bottom-right)
  (loop for yi from y-upper-left to y-bottom-right
        do (loop for xi from x-upper-left to x-bottom-right
                 do (unless (is-blocked this xi yi)
                      (set-weakly-blocked this xi yi)))))

(defjmethod weakly-intersects-area ((this planar-grid) x-upper-left y-upper-left
                                    x-bottom-right y-bottom-right)
  (loop for yi from y-upper-left to y-bottom-right
        do (loop for xi from x-upper-left to x-bottom-right
                 do (when (is-blocked this xi yi)
                      (return-from weakly-intersects-area t))))
  nil)

;;; PlanarGrid.intersectsWithCenterBased(PlanarGrid, int, int).  The Polyomino
;;; overload below calls this one explicitly (Java's (PlanarGrid) cast), so the
;;; body lives in a plain function and is not reached through dispatch.
(defun %planar-intersects-grid (this other x-offset y-offset)
  (dotimes (x (get-width other))
    (let ((x-translated (+ (- x (get-center-x other)) x-offset)))
      (dotimes (y (get-height other))
        (let ((y-translated (+ (- y (get-center-y other)) y-offset)))
          (when (and (in-bounds-center-based this x-translated y-translated)
                     (or (and (not (is-empty other x y))
                              (is-blocked-center-based this x-translated y-translated))
                         (and (is-blocked other x y)
                              (not (is-empty-center-based this x-translated y-translated)))))
            (return-from %planar-intersects-grid t))))))
  nil)

(defjmethod intersects-with-center-based ((this planar-grid) (other planar-grid) x-offset y-offset)
  (%planar-intersects-grid this other x-offset y-offset))

(defjmethod intersects-with-center-based ((this planar-grid) (other polyomino) x-offset y-offset)
  (block intersects
    (when (%planar-intersects-grid this other x-offset y-offset)
      (return-from intersects t))
    (jdo (ext (get-polyomino-extensions other))
      (let* ((left-x (+ (- (get-center-x this) (get-center-x other)) x-offset))
             (right-x (+ left-x (get-width other)))
             (top-y (+ (- (get-center-y this) (get-center-y other)) y-offset))
             (bottom-y (+ top-y (get-height other)))
             (intersects
               (enum-case (get-first ext)
                 (common-direction.north
                  (weakly-intersects-area this (+ left-x (get-second ext)) 0
                                          (+ left-x (get-third ext)) (- top-y 1)))
                 (common-direction.east
                  (weakly-intersects-area this right-x (+ top-y (get-second ext))
                                          (- (get-width this) 1) (+ top-y (get-third ext))))
                 (common-direction.south
                  (weakly-intersects-area this (+ left-x (get-second ext)) bottom-y
                                          (+ left-x (get-third ext)) (- (get-height this) 1)))
                 (t ; WEST
                  (weakly-intersects-area this 0 (+ top-y (get-second ext))
                                          (- left-x 1) (+ top-y (get-third ext)))))))
        (when intersects
          (return-from intersects t))))
    nil))

(defun %planar-add-filled-cells-from (this other x-offset y-offset)
  (dotimes (x (get-width other))
    (let ((x-translated (+ (- x (get-center-x other)) x-offset)))
      (dotimes (y (get-height other))
        (let ((y-translated (+ (- y (get-center-y other)) y-offset)))
          (if (is-blocked other x y)
              (unless (is-weakly-blocked-center-based this x-translated y-translated)
                (set-blocked-center-based this x-translated y-translated))
              (when (is-weakly-blocked other x y)
                (unless (is-blocked-center-based this x-translated y-translated)
                  (set-weakly-blocked-center-based this x-translated y-translated))))))))
  nil)

(defjmethod add-filled-cells-from ((this planar-grid) (other planar-grid) x-offset y-offset)
  (%planar-add-filled-cells-from this other x-offset y-offset))

(defjmethod add-filled-cells-from ((this planar-grid) (other polyomino) x-offset y-offset)
  (%planar-add-filled-cells-from this other x-offset y-offset)
  (set-x other (+ (- (.x-center this) (get-center-x other)) x-offset))
  (set-y other (+ (- (.y-center this) (get-center-y other)) y-offset))
  (jdo (ext (get-polyomino-extensions other))
    (enum-case (get-first ext)
      (common-direction.north
       (weakly-block-area this (+ (get-x other) (get-second ext)) 0
                          (+ (get-x other) (get-third ext)) (- (get-y other) 1)))
      (common-direction.east
       (weakly-block-area this (+ (get-x other) (get-width other)) (+ (get-y other) (get-second ext))
                          (- (get-width this) 1) (+ (get-y other) (get-third ext))))
      (common-direction.south
       (weakly-block-area this (+ (get-x other) (get-second ext)) (+ (get-y other) (get-height other))
                          (+ (get-x other) (get-third ext)) (- (get-height this) 1)))
      (t ; WEST
       (weakly-block-area this 0 (+ (get-y other) (get-second ext))
                          (- (get-x other) 1) (+ (get-y other) (get-third ext))))))
  nil)

(defjmethod get-filled-bounds ((this planar-grid))
  (let ((grid-width (get-width this))
        (grid-height (get-height this))
        (min-x integer.max-value)
        (max-x integer.min-value)
        (min-y integer.max-value)
        (max-y integer.min-value))
    (dotimes (xi grid-width)
      (dotimes (yi grid-height)
        (when (is-blocked this xi yi)
          (setf min-x (math.min min-x xi)
                max-x (math.max max-x xi)
                min-y (math.min min-y yi)
                max-y (math.max max-y yi)))))
    (new-quadruple min-x min-y (+ (- max-x min-x) 1) (+ (- max-y min-y) 1))))

;;; ------------------------------------------------------------ Polyomino

(defjctor polyomino (width height extensions)
  (super-ctor planar-grid this width height)
  (setf (.polyomino-extensions this) extensions))

(defjctor polyomino (width height)
  (this-ctor polyomino this width height (new-jlist)))

(defjctor polyomino ()
  (this-ctor polyomino this 0 0))

(defjmethod get-x ((this polyomino)) (.x this))
(defjmethod get-y ((this polyomino)) (.y this))
(defjmethod set-x ((this polyomino) val) (setf (.x this) val))
(defjmethod set-y ((this polyomino) val) (setf (.y this) val))

(defjmethod get-polyomino-extensions ((this polyomino)) (.polyomino-extensions this))

(defjmethod add-extension ((this polyomino) dir offset width)
  (jl-add (.polyomino-extensions this) (new-unique-triple dir offset width)))

;;; ------------------------------------------------------------ Polyominoes

(defjctor polyominoes (polys aspect-ratio)
  (this-ctor polyominoes this polys aspect-ratio nil))

(defjctor polyominoes (polys aspect-ratio fill)
  (let ((grid-width 0)
        (grid-height 0))
    (jdo (poly polys)
      (when fill
        (profile-fill.fill-polyomino poly))
      (jl-add (.polys this) poly)
      (incf grid-width (get-width poly))
      (incf grid-height (get-height poly)))
    (when (> (size (.polys this)) 0)
      (let ((poly (jl-get (.polys this) 0)))
        (incf grid-width (get-width poly))
        (incf grid-height (get-height poly))))
    (setf grid-width (* grid-width 2)
          grid-height (* grid-height 2))
    (if (> aspect-ratio 1.0d0)
        (setf grid-width (to-int (math.ceil (* grid-width aspect-ratio))))
        (setf grid-height (to-int (math.ceil (/ grid-height aspect-ratio)))))
    (setf (.grid this) (new-planar-grid grid-width grid-height))))

(defjmethod get-polyominoes ((this polyominoes)) (.polys this))
(defjmethod get-grid ((this polyominoes)) (.grid this))
