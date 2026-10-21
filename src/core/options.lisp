;;;; options.lisp -- org.eclipse.elk.core.options: the hand-written methods and
;;;; static sets of the option enums.  The constants themselves are generated
;;;; (src/gen-enums/enums.lisp); TopdownSizeApproximator's per-constant bodies
;;;; are dispatched with ENUM-CASE on the constant.

(in-package #:elk)

(defjclass topdown-size-approximator-util () ())

(defun %intersection-size (group set)
  "Sets.intersection(group, set).size() for a Java list GROUP of constants."
  (count-if (lambda (x) (contains set x)) group))

;;; ---------------------------------------------------------------- Direction

(defproperty label-side.label-side "org.eclipse.elk.labelSide" label-side.unknown)

(defjmethod is-horizontal ((this core-direction))
  (or (eq this core-direction.left) (eq this core-direction.right)))

(defjmethod is-vertical ((this core-direction))
  (or (eq this core-direction.up) (eq this core-direction.down)))

(defjmethod opposite ((this core-direction))
  (enum-case this
    (core-direction.left core-direction.right)
    (core-direction.right core-direction.left)
    (core-direction.up core-direction.down)
    (core-direction.down core-direction.up)
    (t core-direction.undefined)))

;;; ---------------------------------------------------------------- ContentAlignment

(defjstatic content-alignment.center-center ()
  (enum-set.of content-alignment.v-center content-alignment.h-center))

(defjstatic content-alignment.top-left ()
  (enum-set.of content-alignment.v-top content-alignment.h-left))

(defjstatic content-alignment.bottom-right ()
  (enum-set.of content-alignment.v-bottom content-alignment.h-right))

(defjstatic content-alignment.top-center ()
  (enum-set.of content-alignment.v-top content-alignment.h-center))

;;; ---------------------------------------------------------------- EdgeLabelPlacement

(defjmethod is-end-label-placement ((this edge-label-placement))
  (or (eq this edge-label-placement.head) (eq this edge-label-placement.tail)))

;;; ---------------------------------------------------------------- LabelSide

(defjmethod opposite ((this label-side))
  (enum-case this
    (label-side.above label-side.below)
    (label-side.below label-side.above)
    (label-side.inline label-side.inline)
    (t label-side.unknown)))

;;; ---------------------------------------------------------------- NodeLabelPlacement

;; valueOf(int) overloads the generated valueOf(String); both are kept.
(defjstatic node-label-placement.value-of (x)
  (if (integerp x)
      (aref (node-label-placement.values) x)
      (or (enum-value-of 'node-label-placement x)
          (jthrow 'illegal-argument-exception "No enum constant NodeLabelPlacement.~A" x))))

(defjstatic node-label-placement.fixed ()
  (enum-set.none-of 'node-label-placement))

(defjstatic node-label-placement.inside-top-left ()
  (enum-set.of node-label-placement.inside node-label-placement.v-top node-label-placement.h-left))
(defjstatic node-label-placement.inside-top-center ()
  (enum-set.of node-label-placement.inside node-label-placement.v-top node-label-placement.h-center))
(defjstatic node-label-placement.inside-top-right ()
  (enum-set.of node-label-placement.inside node-label-placement.v-top node-label-placement.h-right))
(defjstatic node-label-placement.inside-center ()
  (enum-set.of node-label-placement.inside node-label-placement.v-center node-label-placement.h-center))
(defjstatic node-label-placement.inside-bottom-left ()
  (enum-set.of node-label-placement.inside node-label-placement.v-bottom node-label-placement.h-left))
(defjstatic node-label-placement.inside-bottom-center ()
  (enum-set.of node-label-placement.inside node-label-placement.v-bottom node-label-placement.h-center))
(defjstatic node-label-placement.inside-bottom-right ()
  (enum-set.of node-label-placement.inside node-label-placement.v-bottom node-label-placement.h-right))
(defjstatic node-label-placement.outside-top-left ()
  (enum-set.of node-label-placement.outside node-label-placement.v-top node-label-placement.h-left))
(defjstatic node-label-placement.outside-top-center ()
  (enum-set.of node-label-placement.outside node-label-placement.v-top node-label-placement.h-center))
(defjstatic node-label-placement.outside-top-right ()
  (enum-set.of node-label-placement.outside node-label-placement.v-top node-label-placement.h-right))
(defjstatic node-label-placement.outside-bottom-left ()
  (enum-set.of node-label-placement.outside node-label-placement.v-bottom node-label-placement.h-left))
(defjstatic node-label-placement.outside-bottom-center ()
  (enum-set.of node-label-placement.outside node-label-placement.v-bottom node-label-placement.h-center))
(defjstatic node-label-placement.outside-bottom-right ()
  (enum-set.of node-label-placement.outside node-label-placement.v-bottom node-label-placement.h-right))

(defjstatic node-label-placement.is-valid (placement)
  (and (<= (%intersection-size (list node-label-placement.inside node-label-placement.outside) placement) 1)
       (<= (%intersection-size (list node-label-placement.h-left node-label-placement.h-center
                                     node-label-placement.h-right) placement) 1)
       (<= (%intersection-size (list node-label-placement.v-top node-label-placement.v-center
                                     node-label-placement.v-bottom) placement) 1)
       t))

;;; ---------------------------------------------------------------- PortConstraints

(defjmethod is-pos-fixed ((this port-constraints)) (eq this port-constraints.fixed-pos))
(defjmethod is-ratio-fixed ((this port-constraints)) (eq this port-constraints.fixed-ratio))
(defjmethod is-order-fixed ((this port-constraints))
  (and (member this (list port-constraints.fixed-order port-constraints.fixed-ratio
                          port-constraints.fixed-pos))
       t))
(defjmethod is-side-fixed ((this port-constraints))
  (not (or (eq this port-constraints.free) (eq this port-constraints.undefined))))

;;; ---------------------------------------------------------------- PortLabelPlacement

(defjstatic port-label-placement.fixed () (enum-set.none-of 'port-label-placement))
(defjstatic port-label-placement.inside () (enum-set.of port-label-placement.inside))
(defjstatic port-label-placement.outside () (enum-set.of port-label-placement.outside))

(defjstatic port-label-placement.is-fixed (placement)
  (and (not (contains placement port-label-placement.inside))
       (not (contains placement port-label-placement.outside))))

(defjstatic port-label-placement.is-valid (placement)
  (and (<= (%intersection-size (list port-label-placement.inside port-label-placement.outside)
                               placement) 1)
       (<= (%intersection-size (list port-label-placement.always-same-side
                                     port-label-placement.always-other-same-side
                                     port-label-placement.space-efficient) placement) 1)
       t))

;;; ---------------------------------------------------------------- PortSide

(defparameter port-side.sides-none (enum-set.none-of 'port-side))
(defparameter port-side.sides-north (enum-set.of port-side.north))
(defparameter port-side.sides-east (enum-set.of port-side.east))
(defparameter port-side.sides-south (enum-set.of port-side.south))
(defparameter port-side.sides-west (enum-set.of port-side.west))
(defparameter port-side.sides-north-south (enum-set.of port-side.north port-side.south))
(defparameter port-side.sides-east-west (enum-set.of port-side.east port-side.west))
(defparameter port-side.sides-north-west (enum-set.of port-side.north port-side.west))
(defparameter port-side.sides-north-east (enum-set.of port-side.north port-side.east))
(defparameter port-side.sides-south-west (enum-set.of port-side.south port-side.west))
(defparameter port-side.sides-east-south (enum-set.of port-side.east port-side.south))
(defparameter port-side.sides-north-east-west (enum-set.of port-side.north port-side.east port-side.west))
(defparameter port-side.sides-east-south-west (enum-set.of port-side.east port-side.south port-side.west))
(defparameter port-side.sides-north-south-west (enum-set.of port-side.north port-side.south port-side.west))
(defparameter port-side.sides-north-east-south (enum-set.of port-side.north port-side.east port-side.south))
(defparameter port-side.sides-north-east-south-west
  (enum-set.of port-side.north port-side.east port-side.south port-side.west))

(defjmethod right ((this port-side))
  (enum-case this
    (port-side.north port-side.east)
    (port-side.east port-side.south)
    (port-side.south port-side.west)
    (port-side.west port-side.north)
    (t port-side.undefined)))

(defjmethod left ((this port-side))
  (enum-case this
    (port-side.north port-side.west)
    (port-side.east port-side.north)
    (port-side.south port-side.east)
    (port-side.west port-side.south)
    (t port-side.undefined)))

(defjmethod opposed ((this port-side))
  (enum-case this
    (port-side.north port-side.south)
    (port-side.east port-side.west)
    (port-side.south port-side.north)
    (port-side.west port-side.east)
    (t port-side.undefined)))

(defjmethod are-adjacent ((this port-side) other)
  (if (eq this port-side.undefined)
      nil
      (or (eq (left this) other) (eq (right this) other))))

(defjstatic port-side.from-direction (direction)
  (enum-case direction
    (core-direction.up port-side.north)
    (core-direction.right port-side.east)
    (core-direction.down port-side.south)
    (core-direction.left port-side.west)
    (t port-side.undefined)))

(defjstatic port-side.is-vertical (side)
  (or (eq side port-side.north) (eq side port-side.south)))

(defjstatic port-side.is-horizontal (side)
  (or (eq side port-side.west) (eq side port-side.east)))

;;; ---------------------------------------------------------------- SizeConstraint

(defjstatic size-constraint.fixed () (enum-set.none-of 'size-constraint))
(defjstatic size-constraint.minimum-size () (enum-set.of size-constraint.minimum-size))
(defjstatic size-constraint.minimum-size-with-ports ()
  (enum-set.of size-constraint.ports size-constraint.minimum-size))
(defjstatic size-constraint.free () (enum-set.all-of 'size-constraint))

;;; ---------------------------------------------------------------- SizeOptions

;; valueOf(int) overloads the generated valueOf(String); both are kept.
(defjstatic size-options.value-of (x)
  (if (integerp x)
      (aref (size-options.values) x)
      (or (enum-value-of 'size-options x)
          (jthrow 'illegal-argument-exception "No enum constant SizeOptions.~A" x))))

;;; ---------------------------------------------------------------- TopdownSizeApproximator

;; ITopdownSizeApproximator.getSize(ElkNode): one body per constant.
(defjmethod get-size ((this topdown-size-approximator) node)
  (enum-case this
    (topdown-size-approximator.count-children
     (let ((size (* (get-property node core-options.topdown-hierarchical-node-width)
                    (math.sqrt (size (get-children node))))))
       (new-k-vector size (/ size (get-property node core-options.topdown-hierarchical-node-aspect-ratio)))))
    (topdown-size-approximator.lookahead-layout
     (%tsa-lookahead-layout node))
    (topdown-size-approximator.fixed-integer-ratio-boxes
     (%tsa-fixed-integer-ratio-boxes node))
    (topdown-size-approximator.layout-next-level
     (%tsa-layout-next-level node))))

(defun %tsa-lookahead-layout (original-graph)
  (let* ((algorithm-data (get-property original-graph core-options.resolved-algorithm))
         (node (create-elk-node elk-graph-factory.e-instance))
         (old-to-new (new-jmap)))
    (copy-properties node original-graph)
    (jdo (child (get-children original-graph))
      (let ((new-child (create-elk-node elk-graph-factory.e-instance)))
        (set-parent new-child node)
        (copy-properties new-child child)
        (let ((size (get-size topdown-size-approximator.count-children child)))
          (set-dimensions new-child
                          (math.max (get-width child) (.x size))
                          (math.max (get-height child) (.y size))))
        (jm-put old-to-new child new-child)))
    (jdo (child (get-children original-graph))
      (jdo (edge (get-outgoing-edges child))
        (let* ((new-src (jm-get old-to-new child))
               (new-tar (jm-get old-to-new (jl-get (get-targets edge) 0)))
               (new-edge (create-elk-edge elk-graph-factory.e-instance)))
          (add (get-sources new-edge) new-src)
          (add (get-targets new-edge) new-tar)
          (set-containing-node new-edge (get-parent new-src))
          (copy-properties new-edge edge))))
    (let ((provider (fetch (get-instance-pool algorithm-data))))
      (handler-case
          (progn
            (layout provider node (new-null-elk-progress-monitor))
            (release (get-instance-pool algorithm-data) provider))
        (error (condition)
          (dispose provider)
          (error condition))))
    (unless (or (has-property node core-options.child-area-width)
                (has-property node core-options.child-area-height))
      (elk-util.compute-child-area-dimensions node))
    (let* ((desired-width (get-property node core-options.child-area-width))
           (desired-height (get-property node core-options.child-area-height))
           (desired-aspect-ratio (/ desired-width desired-height))
           (base-size (get-property node core-options.topdown-hierarchical-node-width))
           (padding (get-property node core-options.padding))
           (min-width (+ (.left padding) (.right padding) 1))
           (min-height (+ (.top padding) (.bottom padding) 1)))
      (new-k-vector (math.max min-width base-size)
                    (math.max min-height (/ base-size desired-aspect-ratio))))))

(defun %tsa-fixed-integer-ratio-boxes (original-graph)
  (let* ((base-width (get-property original-graph core-options.topdown-hierarchical-node-width))
         (base-height (/ base-width (get-property original-graph
                                                  core-options.topdown-hierarchical-node-aspect-ratio)))
         (multiplier (topdown-size-approximator-util.get-size-category-multiplier original-graph))
         (padding (get-property original-graph core-options.padding))
         (node-node-spacing (get-default core-options.spacing-node-node)))
    (when (get-parent original-graph)
      (setf node-node-spacing (get-property (get-parent original-graph) core-options.spacing-node-node)))
    (let ((result-size (scale (new-k-vector base-width base-height) multiplier)))
      (add result-size (new-k-vector (- (+ (.left padding) (.right padding)) node-node-spacing)
                                     (- (+ (.top padding) (.bottom padding)) node-node-spacing))))))

(defun %tsa-layout-next-level (original-graph)
  (jdo (child-node (get-children original-graph))
    (if (and (get-property child-node core-options.topdown-size-approximator)
             (plusp (size (get-children child-node))))
        (let* ((approximator (get-property child-node core-options.topdown-size-approximator))
               (size (get-size approximator child-node))
               (padding (get-property child-node core-options.padding)))
          (set-dimensions child-node
                          (math.max (get-width child-node)
                                    (+ (.x size) (.left padding) (.right padding)))
                          (math.max (get-height child-node)
                                    (+ (.y size) (.top padding) (.bottom padding)))))
        (unless (zerop (size (get-children child-node)))
          (set-dimensions child-node
                          (get-property child-node core-options.topdown-hierarchical-node-width)
                          (/ (get-property child-node core-options.topdown-hierarchical-node-width)
                             (get-property child-node core-options.topdown-hierarchical-node-aspect-ratio))))))
  (let* ((algorithm-data (get-property original-graph core-options.resolved-algorithm))
         (provider (fetch (get-instance-pool algorithm-data))))
    (handler-case
        (progn
          (layout provider original-graph (new-null-elk-progress-monitor))
          (release (get-instance-pool algorithm-data) provider))
      (error (condition)
        (dispose provider)
        (error condition)))
    (set-property original-graph core-options.algorithm fixed-layouter-options.algorithm-id)
    (visit (new-layout-algorithm-resolver) original-graph)
    (elk-util.compute-child-area-dimensions original-graph)
    (new-k-vector (get-property original-graph core-options.child-area-width)
                  (get-property original-graph core-options.child-area-height))))

;;; TopdownSizeApproximatorUtil

(defjstatic topdown-size-approximator-util.get-size-category-multiplier (original-graph)
  (let* ((parent (get-parent original-graph))
         (this-graph-size (topdown-size-approximator-util.get-graph-size original-graph))
         (categories (get-property original-graph core-options.topdown-size-categories)))
    (if parent
        (let ((size-min-found integer.max-value)
              (size-max-found integer.min-value))
          (jdo (child (get-children parent))
            (let ((size (topdown-size-approximator-util.get-graph-size child)))
              (when (> size size-max-found) (setf size-max-found size))
              (when (< size size-min-found) (setf size-min-found size))))
          (let* ((size-min 1d0)
                 (size-max (math.pow 4 categories)))
            (when (> size-max-found size-max) (setf size-max size-max-found))
            (let* ((x (/ (- (math.log size-max) (math.log size-min)) categories))
                   (factor (math.exp x))
                   (cutoff (* size-min factor)))
              (dotimes (i categories (math.pow 2 (1- categories)))
                (if (< this-graph-size cutoff)
                    (return (math.pow 2 i))
                    (setf cutoff (* cutoff factor)))))))
        1d0)))

(defjstatic topdown-size-approximator-util.get-graph-size (original-graph)
  (let ((sum 0)
        (hierarchical-node-weight
          (get-property original-graph core-options.topdown-size-categories-hierarchical-node-weight)))
    (jdo (child (get-children original-graph))
      (if (plusp (size (get-children child)))
          (incf sum hierarchical-node-weight)
          (incf sum 1)))
    sum))
