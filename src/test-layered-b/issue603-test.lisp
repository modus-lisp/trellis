;;;; issue603-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue603Test.

(in-package #:elk)

(defun %b-603-do-test-no-label-node-overlaps (node)
  (let ((node-and-label-rects (new-jlist)))
    (jdo (l (get-labels node))
      (jl-add node-and-label-rects (new-elk-rectangle (get-x l) (get-y l) (get-width l) (get-height l))))
    (jdo (n (get-children node))
      (jl-add node-and-label-rects (new-elk-rectangle (get-x n) (get-y n) (get-width n) (get-height n))))
    (let ((count (length node-and-label-rects)))
      (dotimes (first count)
        (loop for second from (1+ first) below count
              do (assert-false (intersects (jl-get node-and-label-rects first)
                                           (jl-get node-and-label-rects second))))))))

(deflayouttest issue603-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%b-with-name-filter (model-resource-path "tickets/layered/**")
                                               (lambda (name) (%b-name-matches-p name "603" ".elkt")))))
  (:configurator configure-node-label-placement (graph)
    (jdo (node (get-children graph))
      (set-property node layered-options.node-labels-placement
                    (node-label-placement.inside-top-center))))
  (:test test-no-label-node-overlaps () (graph)
    (let ((node-queue (new-jlist)))
      (jdo (child (get-children graph)) (jl-add node-queue child))
      (loop until (is-empty node-queue)
            do (let ((node (jl-poll-first node-queue)))
                 (when (and (not (is-empty (get-labels node)))
                            (not (is-empty (get-children node))))
                   (%b-603-do-test-no-label-node-overlaps node))
                 (jdo (child (get-children node))
                   (jl-add node-queue child)))))))
