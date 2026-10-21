;;;; issue143-and318-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.issues.Issue143And318Test.

(in-package #:elk)

(defparameter %b-143-proximity-threshold 2d0)
(defparameter %b-143-tolerance 0.01d0)

(defun %b-143-gather-horizontal-edge-segments (graph)
  "Segments as (x1 y1 x2 y2) with x1 <= x2."
  (let ((segments (new-jlist)))
    (jdo (edge (get-contained-edges graph))
      (jdo (section (get-sections edge))
        (let ((bend-points (elk-util.create-vector-chain section))
              (prev-point nil)
              (curr-point nil))
          (jdo (bend-point bend-points)
            ;; Advance to new point
            (setf prev-point curr-point
                  curr-point bend-point)
            (when prev-point
              ;; Check if the current segment is horizontal
              (when (< (abs (- (.y prev-point) (.y curr-point))) %b-143-tolerance)
                ;; Be sure to always go from smaller x values to larger x values
                (if (< (.x prev-point) (.x curr-point))
                    (jl-add segments (list (.x prev-point) (.y prev-point)
                                           (.x curr-point) (.y curr-point)))
                    (jl-add segments (list (.x curr-point) (.y curr-point)
                                           (.x prev-point) (.y prev-point))))))))))
    segments))

(defun %b-143-intersect-horizontally-p (seg1 seg2)
  (destructuring-bind (x1a y1a x2a y2a) seg1
    (declare (ignore y1a y2a))
    (destructuring-bind (x1b y1b x2b y2b) seg2
      (declare (ignore y1b y2b))
      (assert-true (< x1a x2a))
      (assert-true (< x1b x2b))
      (and (< x1a x2b) (< x1b x2a)))))

(deflayouttest issue143-and318-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tickets/layered/143_smallVerticalEdgeSegmentSpacing.elkt")
                          (model-resource-path "tickets/layered/318_overlappingHorizontalEdgeSegments.elkt")))
  (:test test-for-close-edge-proximities () (graph)
    (let* ((segments (%b-143-gather-horizontal-edge-segments graph))
           (count (length segments)))
      ;; Pairwise comparison of all segments
      (dotimes (i count)
        (let ((seg1 (jl-get segments i)))
          (loop for j from (1+ i) below count
                do (let ((seg2 (jl-get segments j)))
                     (when (%b-143-intersect-horizontally-p seg1 seg2)
                       (%b-assert-not-equals-delta nil (second seg1) (second seg2)
                                                   %b-143-proximity-threshold)))))))))
