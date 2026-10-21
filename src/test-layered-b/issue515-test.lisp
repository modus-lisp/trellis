;;;; issue515-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue515Test.

(in-package #:elk)

(deflayouttest issue515-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (%b-with-name-filter (model-resource-path "tickets/layered/**")
                                               (lambda (name) (%b-name-matches-p name "515" ".elkt")))))
  (:test test-no-edge-overlaps () (graph)
    (let ((all-nodes-as-boxes (new-jlist)))
      (jdo (n (get-children graph))
        (jl-add all-nodes-as-boxes (new-elk-rectangle (get-x n) (get-y n) (get-width n) (get-height n))))
      (let ((all-edge-paths (new-jlist)))
        (jdo (edge (get-contained-edges graph))
          (jdo (section (get-sections edge))
            (jl-add all-edge-paths (elk-util.create-vector-chain section))))
        (jdo (path all-edge-paths)
          (let ((points (new-jlist)))
            (jdo (p path) (jl-add points p))
            ;; stream().reduce over consecutive pairs
            (loop for i from 0 below (1- (length points))
                  do (let ((start (jl-get points i))
                           (end (jl-get points (1+ i))))
                       (jdo (box all-nodes-as-boxes)
                         (assert-true "Edge path overlaps with node."
                                      (not (elk-math.intersects box start end))))))))))))
