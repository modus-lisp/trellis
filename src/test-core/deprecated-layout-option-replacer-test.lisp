;;;; deprecated-layout-option-replacer-test.lisp -- port of
;;;; org.eclipse.elk.core.data.DeprecatedLayoutOptionReplacerTest.
;;;; @Before registerLayoutMetaDataProviders(new CoreOptions()) runs first in
;;;; each test body.

(in-package #:elk)

(defun %core-register-core-options ()
  "@Before registerLayoutMetaDataProviders(new CoreOptions()).  The port has no
provider objects: CoreOptions' options are defined at load time by
DEFINE-LAYOUT-OPTION, so there is nothing to register."
  nil)

(defcore-class deprecated-layout-option-replacer-test
  ("testReplacesNextToPortIfPossible"
   (%core-register-core-options)
   (let ((node (elk-graph-util.create-graph))
         (replacer (new-deprecated-layout-option-replacer)))
     (set-property node core-options.port-labels-next-to-port-if-possible t)
     (visit replacer node)
     (assert-false (has-property node core-options.port-labels-next-to-port-if-possible))
     (assert-true (has-property node core-options.port-labels-placement))
     (assert-true (contains (get-property node core-options.port-labels-placement)
                            port-label-placement.next-to-port-if-possible))))
  ("testMovesSpaceEfficientPortLabels"
   (%core-register-core-options)
   (let ((node (elk-graph-util.create-graph))
         (replacer (new-deprecated-layout-option-replacer)))
     (set-property node core-options.node-size-options
                   (enum-set.of size-options.space-efficient-port-labels))
     (visit replacer node)
     (assert-false (contains (get-property node core-options.node-size-options)
                             size-options.space-efficient-port-labels))
     (assert-true (has-property node core-options.port-labels-placement))
     (assert-true (contains (get-property node core-options.port-labels-placement)
                            port-label-placement.space-efficient)))))
