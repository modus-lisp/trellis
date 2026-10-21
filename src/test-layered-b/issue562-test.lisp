;;;; issue562-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue562Test.
;;;;
;;;; A plain JUnit class (no LayoutTestRunner), so it is not a DEFLAYOUTTEST.
;;;; Run with (run-issue562-test).

(in-package #:elk)

(defun %b-issue562-body ()
  (let* ((graph (elk-graph-util.create-graph))
         (n1 (elk-graph-util.create-node graph))
         (p1 (elk-graph-util.create-port n1))
         (p2 (elk-graph-util.create-port n1))
         (e (elk-graph-util.create-simple-edge p1 p2))
         (config (new-layout-configurator)))
    (set-property (configure config n1) core-options.inside-self-loops-activate t)
    (set-property (configure config e) core-options.inside-self-loops-yo t)
    (elk-util.apply-visitors graph config (new-layout-algorithm-resolver))
    (handler-case
        (layout (new-recursive-graph-layout-engine) graph (new-null-elk-progress-monitor))
      (unsupported-configuration-exception (ex)
        (fail (princ-to-string ex))))))

(defun run-issue562-test ()
  (%b-run-plain-tests "issue562-test"
                      (list (cons "test" #'%b-issue562-body))))
