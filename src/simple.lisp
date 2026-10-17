;;;; simple.lisp -- the small algorithms: fixed (keep positions) and box (pack rows).
;;;; Edges in both are straight, from boundary to boundary.

(in-package #:diagram)

(defun clip-to-box (cx cy tx ty w h)
  "Point where the ray from the box centre (CX,CY) toward (TX,TY) leaves the box."
  (let ((dx (- tx cx)) (dy (- ty cy)))
    (if (and (< (abs dx) 1d-9) (< (abs dy) 1d-9))
        (cons cx cy)
        (let ((s (min (if (> (abs dx) 1d-9) (/ (/ w 2) (abs dx)) most-positive-double-float)
                      (if (> (abs dy) 1d-9) (/ (/ h 2) (abs dy)) most-positive-double-float))))
          (cons (+ cx (* s dx)) (+ cy (* s dy)))))))

(defun element-box (el)
  "(values x y w h) of a child node or of a port, relative to the node's parent."
  (etypecase el
    (node (values (or (node-x el) 0d0) (or (node-y el) 0d0)
                  (or (node-width el) 0d0) (or (node-height el) 0d0)))
    (port (let ((n (port-owner el)))
            (values (+ (or (node-x n) 0d0) (or (port-x el) 0d0))
                    (+ (or (node-y n) 0d0) (or (port-y el) 0d0))
                    (port-width el) (port-height el))))))

(defun straight-edges (container infos)
  (dolist (info infos)
    (destructuring-bind (edge s tg sport tport) info
      (multiple-value-bind (sx sy sw sh) (element-box (or sport s))
        (multiple-value-bind (tx ty tw th) (element-box (or tport tg))
          (let ((scx (+ sx (/ sw 2))) (scy (+ sy (/ sh 2)))
                (tcx (+ tx (/ tw 2))) (tcy (+ ty (/ th 2))))
            (push-section edge
                          (list (if sport (cons scx scy) (clip-to-box scx scy tcx tcy sw sh))
                                (if tport (cons tcx tcy) (clip-to-box tcx tcy scx scy tw th)))
                          container)))))))

(defun fit-to-children (container)
  (multiple-value-bind (pt pl pb pr) (option-padding container 12d0)
    (declare (ignore pt pl))
    (let ((w 0d0) (h 0d0))
      (dolist (c (node-children container))
        (setf w (max w (+ (or (node-x c) 0d0) (or (node-width c) 0d0)))
              h (max h (+ (or (node-y c) 0d0) (or (node-height c) 0d0)))))
      (setf (node-width container) (+ w pr) (node-height container) (+ h pb)))))

(defun layout-fixed (container infos)
  (dolist (c (node-children container))
    (unless (node-x c) (setf (node-x c) 0d0))
    (unless (node-y c) (setf (node-y c) 0d0)))
  (fit-to-children container)
  (straight-edges container infos))

(defun layout-box (container infos)
  (multiple-value-bind (pt pl pb pr) (option-padding container 12d0)
    (let* ((sp (option-number container "spacing.nodeNode" 15d0))
           (kids (node-children container))
           (area (reduce #'+ kids :key (lambda (c) (* (+ sp (or (node-width c) 0d0))
                                                      (+ sp (or (node-height c) 0d0))))))
           (target (max (sqrt area)
                        (reduce #'max kids :key (lambda (c) (or (node-width c) 0d0)) :initial-value 0d0)))
           (x 0d0) (y 0d0) (row-h 0d0) (max-w 0d0))
      (dolist (c kids)
        (let ((w (or (node-width c) 0d0)) (h (or (node-height c) 0d0)))
          (when (and (> x 0) (> (+ x w) target))
            (setf x 0d0 y (+ y row-h sp) row-h 0d0))
          (setf (node-x c) (+ pl x) (node-y c) (+ pt y))
          (setf max-w (max max-w (+ x w)) row-h (max row-h h) x (+ x w sp))))
      (setf (node-width container) (+ pl max-w pr)
            (node-height container) (+ pt y row-h pb))
      (straight-edges container infos))))
