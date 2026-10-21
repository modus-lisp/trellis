;;;; testing.lisp -- org.eclipse.elk.core.testing: TestController and the
;;;; white-box hooks it installs on a layout provider.  IWhiteBoxTestable and
;;;; TestController.ILayoutExecutionListener are the interfaces in classes.lisp.

(in-package #:elk)

(defjctor test-controller (layout-algorithm-id)
  (setf (.layout-algorithm-id this) layout-algorithm-id))

(defjmethod get-target-algorithm-id ((this test-controller))
  (.layout-algorithm-id this))

(defjmethod targets ((this test-controller) algorithm-data)
  (and (jequals (get-id algorithm-data) (.layout-algorithm-id this)) t))

(defjmethod install ((this test-controller) layout-provider)
  (cond ((.current-layout-provider this)
         (jthrow 'illegal-state-exception
                 "Test controller may be installed on only one layout provider at a time"))
        ((typep layout-provider 'i-white-box-testable)
         (setf (.current-layout-provider this) layout-provider)
         (set-test-controller layout-provider this))
        (t
         (jthrow 'illegal-argument-exception
                 "Test controllers can only be installed on white-box testable layout algorithms")))
  nil)

(defjmethod uninstall ((this test-controller))
  (when (.current-layout-provider this)
    (set-test-controller (.current-layout-provider this) nil)
    (setf (.current-layout-provider this) nil))
  nil)

(defjmethod notify-processor-ready ((this test-controller) graph processor)
  (jdo (listener (.listeners this))
    (layout-processor-ready listener processor graph nil))
  nil)

(defjmethod notify-processor-finished ((this test-controller) graph processor)
  (jdo (listener (.listeners this))
    (layout-processor-finished listener processor graph nil))
  nil)

(defjmethod notify-root-processor-ready ((this test-controller) graph processor)
  (jdo (listener (.listeners this))
    (layout-processor-ready listener processor graph t))
  nil)

(defjmethod notify-root-processor-finished ((this test-controller) graph processor)
  (jdo (listener (.listeners this))
    (layout-processor-finished listener processor graph t))
  nil)

(defjmethod add-layout-execution-listener ((this test-controller) listener)
  (add (.listeners this) listener)
  nil)

(defjmethod remove-layout-execution-listener ((this test-controller) listener)
  ;; Kept as in ELK: the Java source calls listeners.add(listener) here, so
  ;; removal adds (a no-op for a listener already present).  Not corrected.
  (add (.listeners this) listener)
  nil)
