;;;; classes.lisp -- classes, interfaces and exceptions of org.eclipse.elk.core
;;;; (root package, alg, data, options, testing).
;;;;
;;;; LayoutOptionData and LayoutAlgorithmData are defined in
;;;; src/runtime/metadata.lisp; their Java API is added in data.lisp.

(in-package #:elk)

;;; ---------------------------------------------------------------- root package

;; IGraphLayoutEngine: void layout(ElkNode, IElkProgressMonitor)
(defjinterface i-graph-layout-engine)
(defjgeneric layout 2)

;; AbstractLayoutProvider
(defjclass abstract-layout-provider (i-graph-layout-engine) ())

;; ITopdownLayoutProvider: KVector getPredictedGraphSize(ElkNode)
(defjinterface i-topdown-layout-provider)
(defjgeneric get-predicted-graph-size 1)

;; RecursiveGraphLayoutEngine
(defjclass recursive-graph-layout-engine (i-graph-layout-engine) ())

;; LayoutConfigurator (implements IGraphElementVisitor, a marker here: visit/1
;; is dispatched on the visitor's own class).
(defjclass layout-configurator ()
  ((element-option-map (make-hash-table :test 'eq))   ; ElkGraphElement -> MapPropertyHolder
   (class-option-map (make-hash-table :test 'eq))     ; class-name symbol -> MapPropertyHolder
   (clear-layout nil)
   (option-filters (new-jlist))))                     ; IOptionFilter = (lambda (element property)) -> boolean

;; IOptionFilter / IPropertyHolderOptionFilter are functional interfaces:
;; closures of two arguments.  The interfaces exist as markers only.
(defjinterface i-option-filter)
(defjinterface i-property-holder-option-filter)

(define-condition unsupported-configuration-exception (runtime-exception)
  ((cause :initarg :cause :initform nil :reader exception-cause)))
(define-condition unsupported-graph-exception (runtime-exception)
  ((cause :initarg :cause :initform nil :reader exception-cause)))

(defjstatic new-unsupported-configuration-exception () (make-condition 'unsupported-configuration-exception))
(defjstatic new-unsupported-configuration-exception (message)
  (make-condition 'unsupported-configuration-exception :message message))
(defjstatic new-unsupported-configuration-exception (message cause)
  (make-condition 'unsupported-configuration-exception :message message :cause cause))
(defjstatic new-unsupported-graph-exception () (make-condition 'unsupported-graph-exception))
(defjstatic new-unsupported-graph-exception (message)
  (make-condition 'unsupported-graph-exception :message message))
(defjstatic new-unsupported-graph-exception (message cause)
  (make-condition 'unsupported-graph-exception :message message :cause cause))

;;; ---------------------------------------------------------------- alg

;; ILayoutProcessor<G>: void process(G graph, IElkProgressMonitor monitor)
(defjinterface i-layout-processor)
(defjgeneric process 2)
;; ILayoutPhase<P,G> extends ILayoutProcessor<G>
(defjinterface i-layout-phase (i-layout-processor))
(defjgeneric get-layout-processor-configuration 1)
;; ILayoutProcessorFactory<G>: ILayoutProcessor<G> create()
(defjinterface i-layout-processor-factory)
(defjgeneric create 0)
;; ILayoutPhaseFactory<P,G> extends ILayoutProcessorFactory<G>
(defjinterface i-layout-phase-factory (i-layout-processor-factory))

;; AbstractRandomListAccessor (core.util) is folded into the two classes below
;; as a LIST slot plus the %RLA- helpers in alg.lisp.

;; AlgorithmAssembler<P, G>
(defjclass algorithm-assembler ()
  ((list (new-jlist))
   (enable-caching t)
   (fail-on-missing-phase t)
   (processor-comparator nil)        ; comparator function; EnumBasedFactoryComparator by default
   (phases-enum-class nil)           ; symbol naming the phase enum
   (number-of-phases 0)
   (configured-phases nil)           ; enum-set
   (additional-processors nil)       ; layout-processor-configuration
   (cache (make-hash-table :test 'eq))))

;; LayoutProcessorConfiguration<P, G>
(defjclass layout-processor-configuration ()
  ((list (new-jlist))
   (current-index -1)))

;; EnumBasedFactoryComparator<G>
(defjclass enum-based-factory-comparator () ())

;;; ---------------------------------------------------------------- data

(defjinterface i-layout-meta-data)
(defjinterface i-layout-meta-data-provider)
(defjgeneric j-apply 1)

;; LayoutCategoryData
(defjclass layout-category-data (i-layout-meta-data)
  ((id nil)
   (name nil)
   (description nil)
   (layouters (new-jlist))))

;; The three Builder inner classes (classnames.tsv maps all of them to BUILDER,
;; which cannot be three classes; they get their outer class's name as prefix).
(defjclass layout-option-data-builder ()
  ((id nil) (group nil) (legacy-ids nil) (default-value nil) (clazz nil) (type nil)
   (name nil) (description nil) (targets nil) (visibility nil) (lower-bound nil) (upper-bound nil)))
(defjclass layout-algorithm-data-builder ()
  ((id nil) (name nil) (provider-factory nil) (category nil) (melk-bundle-name nil)
   (defining-bundle-id nil) (description nil) (image-path nil) (supported-features nil)
   (validator-class nil)))
(defjclass layout-category-data-builder ()
  ((id nil) (name nil) (description nil)))

;; LayoutMetaDataService (singleton).  Options and algorithms live in the
;; global registries of metadata.lisp; the service adds categories and the
;; suffix caches.
(defjclass layout-meta-data-service ()
  ((layout-category-map (make-hash-table :test 'equal))
   (layout-category-list (new-jlist))
   (algorithm-suffix-map (make-hash-table :test 'equal))
   (option-suffix-map (make-hash-table :test 'equal))
   (option-suffix-stamp nil)))

;; LayoutMetaDataService.Registry (inner class) and its Triple
(defjclass registry ()
  ((service nil)
   (option-dependencies (new-jlist))
   (option-support (new-jlist))))
(defjclass core-data-triple ()
  ((first-id nil) (second-id nil) (value nil)))

;; The provider factory every LayoutAlgorithmData's instance pool uses
;; (core.util.AlgorithmFactory's behaviour: instantiate, then initialize(parameter)).
(defjclass core-algorithm-provider-factory ()
  ((clazz nil) (parameter nil)))

;; LayoutAlgorithmResolver, DeprecatedLayoutOptionReplacer (IGraphElementVisitors)
(defjclass layout-algorithm-resolver () ())
(defjclass deprecated-layout-option-replacer () ())

;; LayoutDataContentAssist.Proposal
(defjclass proposal ()
  ((proposal nil) (label nil) (data nil)))

;;; ---------------------------------------------------------------- options

(defjinterface i-topdown-size-approximator)
(defjgeneric get-size 1)

;;; ---------------------------------------------------------------- testing

(defjinterface i-white-box-testable)
(defjgeneric set-test-controller 1)

(defjinterface i-layout-execution-listener)
(defjgeneric layout-processor-ready 3)
(defjgeneric layout-processor-finished 3)

(defjclass test-controller ()
  ((layout-algorithm-id nil)
   (current-layout-provider nil)
   (listeners (new-jset))))       ; LinkedHashSet: insertion order, no duplicates
