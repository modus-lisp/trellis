;;;; nubs-self-loop.lisp -- org.eclipse.elk.alg.layered.p5edges.splines.NubsSelfLoop
;;;;
;;;; Java's varargs KVector... constructor is taken here as a sequence of
;;;; vectors: (new-nubs-self-loop dim (jlist ...)).

(in-package #:elk)

(defjconst nubs-self-loop.fraction 1.3d0)
(defjconst nubs-self-loop.half 0.5d0)

;; private NubsSelfLoop(NubsSelfLoop nubs)
(defjctor nubs-self-loop (nubs)
  (super-ctor nub-spline this nubs)
  (setf (.first-label-position this) (.first-label-position nubs)))

;; NubsSelfLoop(int dimension, List<KVector>)
(defjctor nubs-self-loop (dimension k-vectors)
  (super-ctor nub-spline this t dimension k-vectors))

;; NubsSelfLoop(boolean clamped, int dimension, List<KVector>)
(defjctor nubs-self-loop (clamped dimension k-vectors)
  (super-ctor nub-spline this clamped dimension k-vectors))

(defjmethod get-first-label-position ((this nubs-self-loop))
  (.first-label-position this))

(defjmethod set-first-label-position ((this nubs-self-loop) position)
  (setf (.first-label-position this) position)
  nil)

(defun %nubs-port-position (port)
  (add (clone (get-position port)) (get-anchor port)))

(defjstatic nubs-self-loop.create-side-self-loop (source target length)
  (let* ((direction (splines-math.port-side-to-direction (get-side source)))
         (source-pos (%nubs-port-position source))
         (target-pos (%nubs-port-position target))
         (first-cp (add (new-k-vector/1 source-pos)
                        (scale (new-k-vector/1 direction) length)))
         (third-cp (add (new-k-vector/1 target-pos)
                        (scale (new-k-vector/1 direction) length)))
         (mid (scale (sub (new-k-vector/1 first-cp) third-cp) (/ 1.0d0 2.0d0)))
         (second-cp (add (add (new-k-vector/1 third-cp) mid)
                         (scale (new-k-vector/1 direction) (j-length mid)))))
    (new-nubs-self-loop nub-spline.dim (jlist source-pos first-cp second-cp third-cp target-pos))))

(defjstatic nubs-self-loop.create-corner-self-loop (source target source-height target-height)
  (let* ((source-dir (splines-math.port-side-to-direction (get-side source)))
         (target-dir (splines-math.port-side-to-direction (get-side target)))
         (source-pos (%nubs-port-position source))
         (target-pos (%nubs-port-position target))
         (first-cp (add (clone source-pos)
                        (scale (new-k-vector/1 source-dir) (* nubs-self-loop.fraction source-height))))
         (third-cp (add (clone target-pos)
                        (scale (new-k-vector/1 target-dir) (* nubs-self-loop.fraction target-height))))
         (corner-x 0.0d0)
         (corner-y 0.0d0))
    (enum-case (get-side source)
      (port-side.west
       (setf corner-x (- (* 2 (- (.x source-pos) source-height))
                         (* nubs-self-loop.half (+ (.x first-cp) (.x third-cp))))))
      (port-side.east
       (setf corner-x (- (* 2 (+ (.x source-pos) source-height))
                         (* nubs-self-loop.half (+ (.x first-cp) (.x third-cp))))))
      (port-side.north
       (setf corner-y (- (* 2 (- (.y source-pos) source-height))
                         (* nubs-self-loop.half (+ (.y first-cp) (.y third-cp))))))
      (port-side.south
       (setf corner-y (- (* 2 (+ (.y source-pos) source-height))
                         (* nubs-self-loop.half (+ (.y first-cp) (.y third-cp))))))
      (t nil))
    (enum-case (get-side target)
      (port-side.west
       (setf corner-x (- (* 2 (- (.x target-pos) target-height))
                         (* nubs-self-loop.half (+ (.x third-cp) (.x first-cp))))))
      (port-side.east
       (setf corner-x (- (* 2 (+ (.x target-pos) target-height))
                         (* nubs-self-loop.half (+ (.x third-cp) (.x first-cp))))))
      (port-side.north
       (setf corner-y (- (* 2 (- (.y target-pos) target-height))
                         (* nubs-self-loop.half (+ (.y third-cp) (.y first-cp))))))
      (port-side.south
       (setf corner-y (- (* 2 (+ (.y target-pos) target-height))
                         (* nubs-self-loop.half (+ (.y third-cp) (.y first-cp))))))
      (t nil))
    (let ((second-cp (new-k-vector/2 corner-x corner-y)))
      (new-nubs-self-loop nub-spline.dim
                          (jlist source-pos first-cp second-cp third-cp target-pos)))))

(defjstatic nubs-self-loop.create-across-self-loop (source target source-bend-point corner-bend-points target-bend-point)
  (let* ((source-pos (%nubs-port-position source))
         (target-pos (%nubs-port-position target))
         (third-cp (jl-get corner-bend-points 0))
         (fourth-cp (jl-get corner-bend-points 1))
         (middle-cp (scale (add (clone fourth-cp) third-cp) nubs-self-loop.half)))
    (new-nubs-self-loop nub-spline.dim
                        (jlist source-pos source-bend-point third-cp middle-cp fourth-cp
                               target-bend-point target-pos))))

(defjstatic nubs-self-loop.create-three-side-self-loop (source target source-bend-point corner-bend-points target-bend-point)
  (let* ((source-pos (%nubs-port-position source))
         (target-pos (%nubs-port-position target))
         (third-cp (jl-get corner-bend-points 0))
         (fourth-cp (jl-get corner-bend-points 1))
         (fifth-cp (jl-get corner-bend-points 2)))
    (new-nubs-self-loop nub-spline.dim
                        (jlist source-pos source-bend-point third-cp fourth-cp fifth-cp
                               target-bend-point target-pos))))

(defjstatic nubs-self-loop.create-four-side-self-loop (source target source-bend-point corner-bend-points target-bend-point)
  (let* ((source-pos (%nubs-port-position source))
         (target-pos (%nubs-port-position target))
         (third-cp (jl-get corner-bend-points 0))
         (fourth-cp (jl-get corner-bend-points 1))
         (fifth-cp (jl-get corner-bend-points 2))
         (sixth-cp (jl-get corner-bend-points 3)))
    (new-nubs-self-loop nub-spline.dim
                        (jlist source-pos source-bend-point third-cp fourth-cp fifth-cp sixth-cp
                               target-bend-point target-pos))))
