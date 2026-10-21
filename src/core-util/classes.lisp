;;;; classes.lisp -- every class and interface of org.eclipse.elk.core.util
;;;; (and its subpackages), labels, comments and validation.

(in-package #:elk)

;;; ------------------------------------------------------------ util

(defjinterface i-elk-cancel-indicator)
(defjinterface i-elk-progress-monitor (i-elk-cancel-indicator))
(defjinterface i-graph-element-visitor)
(defjinterface i-data-object)
(defjinterface i-factory)

(defjclass basic-progress-monitor (i-elk-progress-monitor)
  ((parent-monitor nil)
   (children (new-jlist))
   (max-levels -1)
   (task-name nil)
   (closed nil)
   (total-work 0.0)
   (completed-work 0.0)
   (current-child-work -1.0)
   (record-logs nil)
   (persist-logs nil)
   (log-messages nil)
   (log-graphs nil)
   (debug-folder nil)
   (log-file nil)
   (record-execution-time nil)
   (start-time 0)
   (total-time 0.0)))

(defjclass null-elk-progress-monitor (i-elk-progress-monitor) ())

(defjclass abstract-random-list-accessor ()
  ((list (new-jlist))))

(defjclass algorithm-factory (i-factory)
  ((clazz nil)
   (parameter nil)))

(defjclass default-factory (i-factory)
  ((clazz nil)))

(defjclass instance-pool ()
  ((factory nil)
   (instances (new-jlist))
   (limit -1)))

(defjclass logged-graph ()
  ((graph nil)
   (graph-type nil)
   (tag nil)))

(defjclass maybe ()
  ((object nil)))

(defjclass pair ()
  ((first nil)
   (second nil)))

(defjclass first-comparator () ())
(defjclass second-comparator () ())

(defjclass core-util-triple ()
  ((first nil) (second nil) (third nil)))

(defjclass quadruple ()
  ((first nil) (second nil) (third nil) (fourth nil)))

(define-condition wrapped-exception (runtime-exception)
  ((cause :initarg :cause :initform nil :reader wrapped-exception-cause)))

(defjclass exclusive-bounds () ())
(defjclass exclusive-lower-bound ()
  ((exclusive-lower-bound 0.0)))
(defjclass exclusive-upper-bound ()
  ((exclusive-upper-bound 0.0)))

(defjclass individual-spacings (map-property-holder i-data-object) ())

(defjclass elk-spacings () ())

(defjclass abstract-spacings-builder ()
  ((base-spacing 0.0)
   (overwrite nil)
   (filters nil)
   ;; IProperty -> factor, insertion ordered (an alist (property . factor))
   (factor-map nil)))

(defjclass elk-core-spacings-builder (abstract-spacings-builder) ())

(defjclass property-constants-delegator ()
  ((property-delegates (make-hash-table :test 'equal))))

;; Layout providers; AbstractLayoutProvider is defined by module core.
(defjclass box-layout-provider (abstract-layout-provider) ())
(defjclass fixed-layout-provider (abstract-layout-provider) ())
(defjclass random-layout-provider (abstract-layout-provider) ())

;; BoxLayoutProvider.Group
(defjclass group ()
  ((node nil)
   (groups nil)
   (size nil)
   (bottom nil)
   (right nil)))

;;; ------------------------------------------------------------ util.adapters

(defjinterface graph-adapters)
(defjinterface graph-element-adapter)
(defjinterface graph-adapter (graph-element-adapter))
(defjinterface node-adapter (graph-element-adapter))
(defjinterface port-adapter (graph-element-adapter))
(defjinterface label-adapter (graph-element-adapter))
(defjinterface edge-adapter)

(defjclass elk-graph-adapters () ())

(defjclass abstract-elk-graph-element-adapter (graph-element-adapter)
  ((element nil)
   (id 0)))

(defjclass elk-graph-adapter (abstract-elk-graph-element-adapter graph-adapter)
  ((child-nodes nil)))

(defjclass elk-node-adapter (abstract-elk-graph-element-adapter node-adapter)
  ((parent-graph-adapter nil)
   (label-adapters nil)
   (port-adapters nil)
   (incoming-edge-adapters nil)
   (outgoing-edge-adapters nil)))

(defjclass elk-label-adapter (abstract-elk-graph-element-adapter label-adapter) ())

(defjclass elk-port-adapter (abstract-elk-graph-element-adapter port-adapter)
  ((label-adapters nil)
   (incoming-edge-adapters nil)
   (outgoing-edge-adapters nil)))

(defjclass elk-edge-adapter (edge-adapter)
  ((element nil)
   (label-adapters nil)))

(defjclass core-port-comparator () ())

;;; ------------------------------------------------------------ util.internal / persistence / selection

(defjclass layout-option-proxy (i-property-value-proxy)
  ((value nil)))

(defjclass elk-graph-xmi-helper () ((resource nil)))
(defjclass elk-graph-resource () ((uri nil) (contents (new-jlist)) (encoding nil)))
(defjclass elk-graph-resource-factory () ())
(defjclass elk-graph-xmi-save () ((helper nil)))

(defjclass selection-iterator ()
  ((visited nil)
   (root nil)
   ;; AbstractTreeIterator state: a stack of (vector . next-index)
   (data nil)
   (started nil)))

(defjclass default-selection-iterator (selection-iterator)
  ((add-ports nil)
   (follow-edge-direction nil)))

;;; ------------------------------------------------------------ labels

(defjinterface i-label-manager)

;;; ------------------------------------------------------------ validation

(defjinterface i-validating-graph-element-visitor (i-graph-element-visitor))

(defjclass graph-issue ()
  ((element nil)
   (message nil)
   (severity nil)))

(define-condition graph-validation-exception (runtime-exception)
  ((issues :initarg :issues :initform nil :reader graph-validation-exception-issues)))

(defjclass graph-validator (i-validating-graph-element-visitor)
  ((issues (new-jlist))
   ;; LayoutAlgorithmData -> validator, insertion ordered
   (algorithm-specific-validators (make-hash-table :test 'eq))))

(defjclass layout-option-validator (i-validating-graph-element-visitor)
  ((issues (new-jlist))))

;;; ------------------------------------------------------------ comments

(defjinterface i-bounds-provider)
(defjinterface i-data-provider)
(defjinterface i-decider)
(defjinterface i-explicit-attachment-provider)
(defjinterface i-filter)
(defjinterface i-matcher)

(defjclass abstract-normalized-matcher (i-matcher)
  ((worst-raw-value 0.0)
   (best-raw-value 1.0)
   (normalization-function nil)))

(defjclass aggregated-match-decider (i-decider)
  ((aggregator #'%cu-agg-max)
   (lower-boundary 0.0)
   (include-lower-boundary nil)))

(defjclass alignment-matcher (abstract-normalized-matcher)
  ((bounds-provider nil)))

(defjclass distance-matcher (abstract-normalized-matcher)
  ((bounds-provider nil)))

(defjclass comment-attacher ()
  ((include-hierarchy t)
   (explicit-attachments-disable-heuristics t)
   (explicit-attachment-provider (lambda (a) (declare (ignore a)) nil))
   (bounds-provider nil)
   (filters (new-jlist))
   (matchers (new-jlist))
   (decider (new-aggregated-match-decider/0))))

(defjclass elk-graph-bounds-provider (i-bounds-provider) ())

(defjclass elk-graph-data-provider (i-data-provider)
  ((graph nil)))

(defjclass node-reference-matcher (i-matcher)
  ((comment-text-function nil)
   (target-name-function nil)
   (bounds-provider nil)
   (max-distance -1.0)
   (fuzzy nil)
   (found-attachments (make-hash-table :test 'eq))))

(defjclass size-filter (i-filter)
  ((bounds-provider nil)
   (max-area -1.0)))

(defjclass text-prefix-filter (i-filter)
  ((comment-text-provider nil)
   (prefixes (new-jlist))
   (reject-comment-on-prefix-match t)
   (case-sensitive nil)))

;; java.awt.geom.Rectangle2D.Double, used by the comment attachment code.
(defjclass rectangle2-d-double ()
  ((x 0.0) (y 0.0) (width 0.0) (height 0.0)))

;; Anonymous classes of IBoundsProvider.cached() / IDataProvider.cached() /
;; lambda-backed functional interfaces.
(defjclass %cu-cached-bounds-provider (i-bounds-provider)
  ((delegate nil)
   (comment-bounds-cache (make-hash-table :test 'eq))
   (target-bounds-cache (make-hash-table :test 'eq))))

(defjclass %cu-cached-data-provider (i-data-provider)
  ((delegate nil)
   (comments-cache nil)
   (targets-cache nil)
   (comment-targets-cache (make-hash-table :test 'eq))
   (sub-provider-cache nil)))
