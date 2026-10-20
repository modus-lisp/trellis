;;;; radial/classes.lisp -- org.eclipse.elk.alg.radial: class and interface definitions.

(in-package #:elk)

;;; ---------------------------------------------------------------- interfaces

(defjinterface i-annulus-wedge-criteria)
(defjgeneric calculate-wedge-space 1)

(defjinterface i-radial-sorter)
(defjgeneric initialize 1)

(defjinterface i-evaluation)
(defjgeneric evaluate 1)

(defjinterface i-overlap-removal)
(defjgeneric remove-overlaps 2)

(defjinterface i-radial-compactor)
(defjgeneric compact 1)

(defjinterface i-radial-rotator)

(defjinterface i-radial-edge-router)
(defjgeneric route-edges 1)

;;; ---------------------------------------------------------------- InternalProperties

(defproperty radial-internal-properties.root-node "root")

;;; ---------------------------------------------------------------- p1position

(defjclass eades-radial (i-layout-phase)
  ((radius 0.0d0)
   (sorter nil)
   (annulus-wedge-criteria nil)
   (optimizer nil)
   (root nil)))

;;; ---------------------------------------------------------------- p1position.wedge

(defjclass annulus-wedge-by-leafs (i-annulus-wedge-criteria) ())
(defjclass annulus-wedge-by-node-space (i-annulus-wedge-criteria) ())

;;; ---------------------------------------------------------------- p2routing

(defjclass straight-line-edge-router (i-layout-phase i-radial-edge-router) ())

;;; ---------------------------------------------------------------- sorting

(defjclass id-sorter (i-radial-sorter) ())

(defjconst polar-coordinate-sorter.degree-45 (* 0.25d0 pi))
(defjconst polar-coordinate-sorter.degree-90 (* 0.5d0 pi))
(defjconst polar-coordinate-sorter.degree-135 (* 0.75d0 pi))
(defjconst polar-coordinate-sorter.degree-225 (* 1.25d0 pi))
(defjconst polar-coordinate-sorter.degree-270 (* 1.5d0 pi))
(defjconst polar-coordinate-sorter.degree-315 (* 1.75d0 pi))

(defjclass polar-coordinate-sorter (i-radial-sorter)
  ((comp-right (radial-util.create-polar-comparator 0d0 0d0))
   (comp-left (radial-util.create-polar-comparator pi 0d0))
   (comp-top (radial-util.create-polar-comparator polar-coordinate-sorter.degree-270 0d0))
   (comp-bottom (radial-util.create-polar-comparator polar-coordinate-sorter.degree-90 0d0))
   (id-sorter nil)))

;;; ---------------------------------------------------------------- intermediate

(defjclass calculate-graph-size (i-layout-processor) ())

(defjclass edge-angle-calculator (i-layout-processor) ())

;;; ---------------------------------------------------------------- intermediate.rotation

(defjclass angle-rotation (i-radial-rotator) ())

(defjclass general-rotator (i-layout-processor) ())

;;; ---------------------------------------------------------------- intermediate.optimization

(defjclass crossing-minimization-position (i-evaluation)
  ((root nil)))

(defjclass edge-length-optimization (i-evaluation) ())

(defjclass edge-length-position-optimization (i-evaluation) ())

;;; ---------------------------------------------------------------- intermediate.compaction

;; AbstractRadiusExtensionCompaction: shared state and geometry of the two
;; compactors and of the overlap removal.
(defjclass abstract-radius-extension-compaction ()
  ((compaction-step 1)
   (spacing 0.0d0)
   (root nil)))

(defjclass radial-compaction (abstract-radius-extension-compaction i-radial-compactor)
  ((sorter nil)
   (last-radius 0.0d0)))

;; The Java HashMultimap is modelled as a hash table from node to a list of
;; nodes (set semantics are irrelevant to the boolean overlap tests).
(defjclass annulus-wedge-compaction (abstract-radius-extension-compaction i-radial-compactor)
  ((left-contour (new-jmap))
   (right-contour (new-jmap))
   (sorter nil)
   (root nil)))

(defjclass general-compactor (i-layout-processor) ())

;;; ---------------------------------------------------------------- intermediate.overlaps

(defjclass radius-extension-overlap-removal (abstract-radius-extension-compaction
                                             i-layout-processor i-overlap-removal)
  ((sorter nil)))

;;; ---------------------------------------------------------------- top level

(defjclass radial-layout-provider (abstract-layout-provider)
  ((algorithm-assembler (algorithm-assembler.create 'radial-layout-phases))))
