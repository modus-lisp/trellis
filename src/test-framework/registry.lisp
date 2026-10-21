;;;; registry.lisp -- the declarative form of an ELK layout test class, and the
;;;; initialization that LayoutTestRunner does when it is built.
;;;;
;;;; DEFLAYOUTTEST replaces a @RunWith(LayoutTestRunner.class) class and its
;;;; annotations.  Each clause is one annotation or annotated method:
;;;;
;;;;   (:algorithm "org.eclipse.elk.layered")        @Algorithm (repeatable; the value is a form)
;;;;   (:all-algorithms)                             @AllAlgorithms
;;;;   (:default-configuration :nodes t :ports t ...) @DefaultConfiguration (no args = its defaults)
;;;;   (:graph-resources FORM)                       @GraphResourceProvider; FORM -> list of resource paths
;;;;   (:graph NAME () BODY...)                      @GraphProvider; returns an ElkNode
;;;;   (:random-generator-file FORM)                 @RandomGeneratorFile; FORM -> one resource path
;;;;   (:random-generator-options NAME () BODY...)   @RandomGeneratorOptions; returns GeneratorOptions
;;;;   (:configurator NAME (GRAPH) BODY...)          @Configurator; mutates the graph
;;;;   (:configurator-provider NAME () BODY...)      @ConfiguratorProvider; returns a layout-configurator
;;;;   (:test NAME OPTIONS (GRAPH) BODY...)          @Test (GRAPH = the laid-out root ElkNode)
;;;;   (:before-processor NAME OPTIONS (GRAPH) BODY...) @TestBeforeProcessor; OPTIONS :processors (classes...)
;;;;   (:after-processor NAME OPTIONS (GRAPH) BODY...)  @TestAfterProcessor
;;;;
;;;; OPTIONS is a plist: :ignore (@Ignore), :only-on-root (@OnlyOnRootNode),
;;;; :fail-if-not-executed (@FailIfNotExecuted), :processors (whitebox only; the
;;;; processor class names, unevaluated).  Body forms run with JUnit-style
;;;; assertions from assert.lisp.  Example:
;;;;
;;;;   (deflayouttest whitebox-test
;;;;     (:algorithm "org.eclipse.elk.layered")
;;;;     (:default-configuration :nodes t)
;;;;     (:graph-resources (list (model-resource-path "tests/**")))  ; or a literal path list
;;;;     (:graph compound () (make-compound-graph))
;;;;     (:configurator layering (graph) (set-property graph layered-options.layering-strategy ...))
;;;;     (:after-processor check-layering
;;;;        (:processors (network-simplex-layerer) :fail-if-not-executed t) (graph)
;;;;        (assert-true (layer-order-ok-p graph))))

(in-package #:elk)

(defvar *layout-test-classes* '()
  "Registered test classes (LAYOUT-TEST-CLASS), in definition order.")

(defstruct layout-test-class
  name
  (algorithms '())           ; algorithm id strings, in declaration order
  (all-algorithms nil)
  (defaults nil)             ; NIL = no @DefaultConfiguration; else (:nodes :ports :edges :edge-labels)
  (resource-thunks '())      ; (lambda () list-of-resource-paths)
  (random-file-thunks '())   ; (lambda () resource-path)
  (graph-providers '())      ; (name . (lambda () ElkNode))
  (random-option-providers '()) ; (name . (lambda () options))
  (configurators '())        ; (name . (lambda (graph) ...))
  (configurator-providers '()) ; (name . (lambda () layout-configurator))
  (methods '()))             ; TEST-METHODs in declaration order

(defun register-layout-test-class (class)
  (setf *layout-test-classes*
        (append (remove (layout-test-class-name class) *layout-test-classes*
                        :key #'layout-test-class-name)
                (list class)))
  class)

(defun find-layout-test-class (designator)
  (find-if (lambda (c) (string-equal (symbol-name-or-string (layout-test-class-name c))
                                     (symbol-name-or-string designator)))
           *layout-test-classes*))

(defun %defaults-plist (args)
  (list :nodes (getf args :nodes t) :ports (getf args :ports t)
        :edges (getf args :edges t) :edge-labels (getf args :edge-labels nil)))

(defmacro deflayouttest (name &body clauses)
  "Define the ELK layout test class NAME (a special variable holding a
LAYOUT-TEST-CLASS).  See the file header for the clauses."
  (let ((algorithms '()) (all nil) (defaults nil) (resources '()) (random-files '())
        (graphs '()) (random-options '()) (configurators '()) (providers '())
        (methods '()))
    (dolist (clause clauses)
      (destructuring-bind (kind &rest args) clause
        (ecase kind
          (:algorithm (push (first args) algorithms))
          (:all-algorithms (setf all t))
          (:default-configuration (setf defaults `(quote ,(%defaults-plist args))))
          (:graph-resources (push (first args) resources))
          (:random-generator-file (push (first args) random-files))
          (:graph
           (destructuring-bind (gname params &rest body) args
             (push `(cons ,(symbol-name gname) (lambda ,params ,@body)) graphs)))
          (:random-generator-options
           (destructuring-bind (gname params &rest body) args
             (push `(cons ,(symbol-name gname) (lambda ,params ,@body)) random-options)))
          (:configurator
           (destructuring-bind (gname params &rest body) args
             (push `(cons ,(symbol-name gname) (lambda ,params ,@body)) configurators)))
          (:configurator-provider
           (destructuring-bind (gname params &rest body) args
             (push `(cons ,(symbol-name gname) (lambda ,params ,@body)) providers)))
          ((:test :before-processor :after-processor)
           (destructuring-bind (mname options params &rest body) args
             (push `(make-test-method
                     :name ',mname
                     :kind ,(ecase kind (:test :test) (:before-processor :before)
                              (:after-processor :after))
                     :processors ',(getf options :processors)
                     :only-on-root ,(getf options :only-on-root)
                     :fail-if-not-executed ,(getf options :fail-if-not-executed)
                     :ignore ,(getf options :ignore)
                     :function (lambda ,params ,@body))
                   methods))))))
    `(defparameter ,name
       (register-layout-test-class
        (make-layout-test-class
         :name ',name
         :algorithms (list ,@(reverse algorithms))
         :all-algorithms ,all
         :defaults ,defaults
         :resource-thunks (list ,@(loop for r in (reverse resources) collect `(lambda () ,r)))
         :random-file-thunks (list ,@(loop for r in (reverse random-files) collect `(lambda () ,r)))
         :graph-providers (list ,@(reverse graphs))
         :random-option-providers (list ,@(reverse random-options))
         :configurators (list ,@(reverse configurators))
         :configurator-providers (list ,@(reverse providers))
         :methods (list ,@(reverse methods)))))))
