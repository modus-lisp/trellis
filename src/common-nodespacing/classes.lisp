;;;; classes.lisp -- classes of org.eclipse.elk.alg.common.nodespacing(.cellsystem/.internal)

(in-package #:elk)

;;; cellsystem

(defjclass cell ()
  ((padding (new-elk-padding))
   (cell-rectangle (new-elk-rectangle))
   (contributes-to-minimum-width nil)
   (contributes-to-minimum-height nil)))

(defjclass atomic-cell (cell)
  ((minimum-content-area-size (new-k-vector))))

(defjclass container-cell (cell) ())

(defjclass label-cell (cell)
  ((horizontal-layout-mode t)
   (horizontal-alignment horizontal-label-alignment.center)
   (vertical-alignment vertical-label-alignment.center)
   (gap 0.0)
   (labels (new-jlist 2))
   (minimum-content-area-size (new-k-vector))))

(defjclass strip-container-cell (container-cell)
  ((container-mode nil)
   (symmetrical nil)
   (gap 0.0)
   ;; Cell[ContainerArea.values().length]
   (cells (make-array 3 :initial-element nil))))

(defjclass grid-container-cell (container-cell)
  ((gap 0.0)
   (tabular nil)
   (symmetrical nil)
   ;; Cell[ROWS][COLUMNS]
   (cells (make-array '(3 3) :initial-element nil))
   (center-cell-minimum-size nil)
   (only-center-cell-contributes-to-minimum-size nil)
   (center-cell-rect (new-elk-rectangle))))

;;; internal

(defjclass node-context ()
  ((node nil)
   (node-size nil)
   (treat-as-compound-node nil)
   (size-constraints nil)
   (size-options nil)
   (port-constraints nil)
   (port-labels-placement nil)
   (port-labels-treat-as-group nil)
   (node-label-placement nil)
   (node-labels-padding nil)
   (node-label-spacing 0.0)
   (label-label-spacing 0.0)
   (label-cell-spacing 0.0)
   (port-port-spacing 0.0)
   (port-label-spacing-horizontal 0.0)
   (port-label-spacing-vertical 0.0)
   (surrounding-port-margins nil)
   (topdown-layout nil)
   ;; TreeMultimap<PortSide, PortContext>: see NS-PORT-CONTEXTS
   (port-contexts (new-ns-port-contexts))
   (node-container nil)
   (node-container-middle-row nil)
   (inside-node-label-container nil)
   ;; EnumMaps, as eq hash tables keyed by enum constant (iterate in ordinal order!)
   (inside-port-label-cells (new-jmap))
   (outside-node-label-containers (new-jmap))
   (node-label-cells (new-jmap))))

(defjclass port-context ()
  ((parent-node-context nil)
   (port nil)
   (port-position nil)
   (labels-next-to-port nil)
   (port-margin (new-elk-margin))
   (port-label-cell nil)))

(defjclass node-margin-calculator ()
  ((adapter nil)
   (include-labels t)
   (include-ports t)
   (include-port-labels t)
   (include-edge-head-tail-labels t)))
