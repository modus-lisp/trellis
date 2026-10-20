;;;; classes.lisp -- classes of org.eclipse.elk.alg.layered (root package)

(in-package #:elk)

;; org.eclipse.elk.alg.layered.IHierarchyAwareLayoutProcessor -- a marker interface
;; for processors that operate on the whole hierarchy at once.
(defjinterface i-hierarchy-aware-layout-processor)

;; org.eclipse.elk.alg.layered.GraphConfigurator
(defjclass graph-configurator ()
  ((algorithm-assembler (algorithm-assembler.create 'layered-phases))))

;; org.eclipse.elk.alg.layered.ElkLayered
(defjclass elk-layered ()
  ((graph-configurator (new-graph-configurator))
   (components-processor (new-layered-components-processor))
   (compound-graph-preprocessor (new-compound-graph-preprocessor))
   (compound-graph-postprocessor (new-compound-graph-postprocessor))
   (test-controller nil)))

;; ElkLayered.TestExecutionState
(defjclass test-execution-state ()
  ((graphs nil)
   (step 0)))

;; org.eclipse.elk.alg.layered.LayeredLayoutProvider
(defjclass layered-layout-provider (abstract-layout-provider i-white-box-testable)
  ((elk-layered (new-elk-layered))))

;; org.eclipse.elk.alg.layered.InteractiveLayeredGraphVisitor
(defjclass interactive-layered-graph-visitor (i-graph-element-visitor) ())
