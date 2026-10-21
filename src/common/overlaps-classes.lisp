;;;; overlaps-classes.lisp -- org.eclipse.elk.alg.common.overlaps classes

(in-package #:elk)

(defjinterface i-rectangle-strip-overlap-removal-strategy)

(defjclass greedy-rectangle-strip-overlap-remover (i-rectangle-strip-overlap-removal-strategy) ())

(defjclass rectangle-node ()
  ((original-rectangle nil)
   (rectangle nil)
   (overlapping-nodes (new-jlist))))

(defjclass rectangle-strip-overlap-remover ()
  ((overlap-removal-direction nil)
   (gap-vertical 5d0)
   (gap-horizontal 5d0)
   (start-coordinate 0d0)
   (overlap-removal-strategy nil)
   (rectangle-nodes (new-jlist))))
