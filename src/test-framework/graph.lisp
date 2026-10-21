;;;; graph.lisp -- org.eclipse.elk.alg.test.framework.graph: where a test's
;;;; input graphs come from.  Four sources, as in the Java framework:
;;;;   :file           GraphFromFile          (.elkt / .elkg via read-graph-file)
;;;;   :method         GraphFromMethod        (a @GraphProvider thunk)
;;;;   :random-file    RandomGraphFromFile    (.elkr configurations)
;;;;   :random-method  RandomGraphFromMethod  (a @RandomGeneratorOptions thunk)
;;;;
;;;; Each graph is provided FRESH for every experiment (ElkNode copies for the
;;;; random sources), so layout never sees a graph another experiment laid out.

(in-package #:elk)

(defstruct test-graph
  kind
  (path nil)        ; :file / :random-file: an abstract-resource-path's file
  (name nil)        ; :method / :random-method: the method's name
  (provider nil)    ; :method: thunk returning an ElkNode
  (graph nil))      ; :random-*: the already generated ElkNode

(defparameter *random-generator-seed* 1337
  "RandomGraphFromMethod: new Random(RANDOM_SEED).")

(defun graph-from-file (path)
  (make-test-graph :kind :file :path path))

(defun graph-from-method (name provider)
  (make-test-graph :kind :method :name name :provider provider))

(defun random-graph-from-file (path graph)
  (make-test-graph :kind :random-file :path path :graph graph))

(defun random-graph-from-method (name graph)
  (make-test-graph :kind :random-method :name name :graph graph))

(defun test-graph-provide (g)
  "TestGraph.provideGraph(test): an ElkNode for one experiment."
  (ecase (test-graph-kind g)
    (:file (read-graph-file (test-graph-path g)))
    (:method (funcall (test-graph-provider g)))
    ((:random-file :random-method) (ecore-util.copy (test-graph-graph g)))))

(defun test-graph-description (g)
  (ecase (test-graph-kind g)
    (:file (format nil "graphFile(~A)" (test-graph-path g)))
    (:method (format nil "graphMethod(~A)" (test-graph-name g)))
    (:random-file (format nil "randomGraphFile(~A)" (test-graph-path g)))
    (:random-method (format nil "randomGraphMethod(~A)" (test-graph-name g)))))

(defparameter *graph-file-extensions* '(".elkt" ".elkg"))
(defparameter *random-graph-file-extensions* '(".elkr"))

(defun %tf-random-graphs-of-file (path)
  "RandomGraphFromFile.randomGraphs(path): every graph each .elkr configuration
generates.  READ-RAND-GRAPH-FILE (the .elkr reader, returning the Configuration
list) and RandomGraphGenerator are not in the port yet; they are called by
their mechanical names, so the first use says exactly what is missing."
  (let ((generator (new-random-graph-generator nil))
        (graphs '()))
    (dolist (config (read-rand-graph-file path))
      (dolist (g (generate generator config))
        (push g graphs)))
    (nreverse graphs)))

(defun %tf-random-graphs-of-method (options)
  (let ((generator (new-random-graph-generator (new-random *random-generator-seed*))))
    (generate generator options)))
