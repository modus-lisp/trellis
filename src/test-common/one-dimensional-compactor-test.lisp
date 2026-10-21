;;;; one-dimensional-compactor-test.lisp -- port of
;;;; org.eclipse.elk.alg.common.compaction.oned.OneDimensionalCompactorTest.

(in-package #:elk)

(define-plain-test-class one-dimensional-compactor-test)

(defparameter *one-d-epsilon* 0.0001d0)
(defparameter *one-d-spacing* 5d0)

(defjclass one-d-test-spacings-handler (common-i-spacings-handler)
  ((vertical nil)
   (horizontal nil)))

(defjctor one-d-test-spacings-handler () nil)

(defjmethod get-vertical-spacing ((this one-d-test-spacings-handler) c1 c2)
  (funcall (.vertical this) c1 c2))

(defjmethod get-horizontal-spacing ((this one-d-test-spacings-handler) c1 c2)
  (funcall (.horizontal this) c1 c2))

(defun %one-d-spacings (vertical horizontal)
  "An ISpacingsHandler whose two methods are the given closures (the Java test's
anonymous classes capture the nodes, so they are closures here)."
  (let ((handler (new-one-d-test-spacings-handler)))
    (setf (.vertical handler) vertical
          (.horizontal handler) horizontal)
    handler))

(defparameter *one-d-test-spacing-handler*
  (%one-d-spacings (lambda (c1 c2) (declare (ignore c1 c2)) *one-d-spacing*)
                   (lambda (c1 c2) (declare (ignore c1 c2)) *one-d-spacing*)))

(defun %one-d-compacter (graph)
  (set-constraint-algorithm (new-common-one-dimensional-compactor graph)
                            common-one-dimensional-compactor.quadratic-constraints))

(defun %one-d-new-graph ()
  (new-common-c-graph (enum-set.all-of 'core-direction)))

(defun %one-d-node (x y w h graph)
  (create (hitbox (common-c-node.of) (new-elk-rectangle x y w h)) graph))

(defun %one-d-x (node) (.x (.hitbox node)))
(defun %one-d-y (node) (.y (.hitbox node)))

(defun %one-d-message (&rest parts)
  (format nil "~{~A~^ ~}" (mapcar (lambda (p) (name p)) parts)))

(deftest test-left-compaction ()
  (let* ((graph (%one-d-new-graph))
         (left (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (right (%one-d-node 30d0 0d0 20d0 20d0 graph)))
    (finish (compact (change-direction (%one-d-compacter graph) core-direction.left)))
    (%assert-equals-delta 0 (%one-d-x left) *one-d-epsilon*)
    (%assert-equals-delta 20 (%one-d-x right) *one-d-epsilon*)))

(deftest test-left-compaction-equal-y-coordinate ()
  (let* ((graph (%one-d-new-graph))
         (top (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (bot (%one-d-node 30d0 20d0 20d0 20d0 graph)))
    (finish (compact (change-direction
                      (set-constraint-algorithm (new-common-one-dimensional-compactor graph)
                                                common-one-dimensional-compactor.scanline-constraints)
                      core-direction.left)))
    (%assert-equals-delta 0 (%one-d-x top) *one-d-epsilon*)
    (%assert-equals-delta 0 (%one-d-x bot) *one-d-epsilon*)))

(deftest test-left-compaction-spacing-aware ()
  (let* ((graph (%one-d-new-graph))
         (left (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (right (%one-d-node 30d0 (+ 20d0 *one-d-spacing* -1d0) 20d0 20d0 graph)))
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) *one-d-test-spacing-handler*)
                      core-direction.left)))
    (%assert-equals-delta 0 (%one-d-x left) *one-d-epsilon*)
    (%assert-equals-delta 25 (%one-d-x right) *one-d-epsilon*)))

(deftest test-left-compaction-spacing-aware2 ()
  (let* ((graph (%one-d-new-graph))
         (left (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (right (%one-d-node 30d0 (+ 20d0 *one-d-spacing* 1d0) 20d0 20d0 graph)))
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) *one-d-test-spacing-handler*)
                      core-direction.left)))
    (%assert-equals-delta 0 (%one-d-x left) *one-d-epsilon*)
    (%assert-equals-delta 0 (%one-d-x right) *one-d-epsilon*)))

(deftest test-right-compaction ()
  (let* ((graph (%one-d-new-graph))
         (left (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (right (%one-d-node 30d0 0d0 20d0 20d0 graph)))
    (finish (compact (change-direction (%one-d-compacter graph) core-direction.right)))
    (%assert-equals-delta 10 (%one-d-x left) *one-d-epsilon*)
    (%assert-equals-delta 30 (%one-d-x right) *one-d-epsilon*)))

(deftest test-up-compaction ()
  (let* ((graph (%one-d-new-graph))
         (upper (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (lower (%one-d-node 0d0 30d0 20d0 20d0 graph)))
    (finish (compact (change-direction (%one-d-compacter graph) core-direction.up)))
    (%assert-equals-delta 0 (%one-d-y upper) *one-d-epsilon*)
    (%assert-equals-delta 20 (%one-d-y lower) *one-d-epsilon*)))

(deftest test-down-compaction ()
  (let* ((graph (%one-d-new-graph))
         (upper (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (lower (%one-d-node 0d0 30d0 20d0 20d0 graph)))
    (finish (compact (change-direction (%one-d-compacter graph) core-direction.down)))
    (%assert-equals-delta 10 (%one-d-y upper) *one-d-epsilon*)
    (%assert-equals-delta 30 (%one-d-y lower) *one-d-epsilon*)))

(deftest test-left-group-compaction ()
  (let* ((graph (%one-d-new-graph))
         (left (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (upper-right (%one-d-node 40d0 5d0 20d0 20d0 graph))
         (lower-right (%one-d-node 30d0 25d0 20d0 20d0 graph)))
    (create (nodes-list (common-c-group.of) (jlist upper-right lower-right)) graph)
    (finish (compact (change-direction (%one-d-compacter graph) core-direction.left)))
    (%assert-equals-delta 0 (%one-d-x left) *one-d-epsilon*)
    (%assert-equals-delta 20 (%one-d-x upper-right) *one-d-epsilon*)
    (%assert-equals-delta 10 (%one-d-x lower-right) *one-d-epsilon*)))

(deftest test-right-group-compaction ()
  (let* ((graph (%one-d-new-graph))
         (left (%one-d-node 0d0 5d0 20d0 20d0 graph))
         (upper-right (%one-d-node 40d0 0d0 20d0 20d0 graph))
         (lower-right (%one-d-node 10d0 25d0 20d0 20d0 graph)))
    (create (nodes-list (common-c-group.of) (jlist left lower-right)) graph)
    (finish (compact (change-direction (%one-d-compacter graph) core-direction.right)))
    (%assert-equals-delta 20 (%one-d-x left) *one-d-epsilon*)
    (%assert-equals-delta 40 (%one-d-x upper-right) *one-d-epsilon*)
    (%assert-equals-delta 30 (%one-d-x lower-right) *one-d-epsilon*)))

(deftest test-up-group-compaction ()
  (let* ((graph (%one-d-new-graph))
         (upper-left (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (lower-left (%one-d-node 5d0 40d0 20d0 20d0 graph))
         (right (%one-d-node 25d0 30d0 20d0 20d0 graph)))
    (create (nodes-list (common-c-group.of) (jlist lower-left right)) graph)
    (finish (compact (change-direction (%one-d-compacter graph) core-direction.up)))
    (%assert-equals-delta 0 (%one-d-y upper-left) *one-d-epsilon*)
    (%assert-equals-delta 20 (%one-d-y lower-left) *one-d-epsilon*)
    (%assert-equals-delta 10 (%one-d-y right) *one-d-epsilon*)))

(deftest test-down-group-compaction ()
  (let* ((graph (%one-d-new-graph))
         (upper-left (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (lower-left (%one-d-node 5d0 40d0 10d0 20d0 graph))
         (right (%one-d-node 25d0 10d0 20d0 20d0 graph)))
    (create (nodes-list (common-c-group.of) (jlist upper-left right)) graph)
    (finish (compact (change-direction (%one-d-compacter graph) core-direction.down)))
    (%assert-equals-delta 20 (%one-d-y upper-left) *one-d-epsilon*)
    (%assert-equals-delta 40 (%one-d-y lower-left) *one-d-epsilon*)
    (%assert-equals-delta 30 (%one-d-y right) *one-d-epsilon*)))

(deftest test-no-spacing-applied-within-groups ()
  (let* ((graph (%one-d-new-graph))
         (one (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (two (%one-d-node 20d0 10d0 20d0 20d0 graph))
         (three (%one-d-node 40d0 20d0 20d0 20d0 graph)))
    (create (nodes-list (common-c-group.of) (jlist one two three)) graph)
    (let* ((four (%one-d-node 22d0 80d0 20d0 20d0 graph))
           (five (%one-d-node 42d0 90d0 20d0 20d0 graph))
           (six (%one-d-node 62d0 100d0 20d0 20d0 graph)))
      (create (nodes-list (common-c-group.of) (jlist four five six)) graph)
      (finish
       (compact
        (change-direction
         (compact
          (change-direction
           (compact
            (change-direction
             (compact
              (change-direction
               (set-spacings-handler (%one-d-compacter graph) *one-d-test-spacing-handler*)
               core-direction.left))
             core-direction.right))
           core-direction.up))
         core-direction.down)))
      (%assert-equals-delta 0 (%one-d-x one) *one-d-epsilon*)
      (%assert-equals-delta 20 (%one-d-x two) *one-d-epsilon*)
      (%assert-equals-delta 40 (%one-d-x three) *one-d-epsilon*)
      (%assert-equals-delta 0 (%one-d-x four) *one-d-epsilon*)
      (%assert-equals-delta 20 (%one-d-x five) *one-d-epsilon*)
      (%assert-equals-delta 40 (%one-d-x six) *one-d-epsilon*)
      (%assert-equals-delta 0 (%one-d-y one) *one-d-epsilon*)
      (%assert-equals-delta 10 (%one-d-y two) *one-d-epsilon*)
      (%assert-equals-delta 20 (%one-d-y three) *one-d-epsilon*)
      (%assert-equals-delta 35 (%one-d-y four) *one-d-epsilon*)
      (%assert-equals-delta 45 (%one-d-y five) *one-d-epsilon*)
      (%assert-equals-delta 55 (%one-d-y six) *one-d-epsilon*))))

(deftest test-subsequent-directions-compaction ()
  (let* ((graph (%one-d-new-graph))
         (one (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (two (%one-d-node 25d0 0d0 20d0 20d0 graph))
         (three (%one-d-node 0d0 25d0 20d0 20d0 graph))
         (four (%one-d-node 25d0 25d0 20d0 20d0 graph))
         (directions (enum-set.of core-direction.left core-direction.right
                                  core-direction.up core-direction.down)))
    (jdo (d1 directions)
      (jdo (d2 directions)
        (jdo (d3 directions)
          (jdo (d4 directions)
            (finish
             (compact
              (change-direction
               (compact
                (change-direction
                 (compact
                  (change-direction
                   (compact
                    (change-direction
                     (set-spacings-handler (%one-d-compacter graph) *one-d-test-spacing-handler*)
                     d1))
                   d2))
                 d3))
               d4)))
            (let ((current-directions (%one-d-message d1 d2 d3 d4)))
              (%assert-equals-delta-m current-directions 0 (%one-d-x one) *one-d-epsilon*)
              (%assert-equals-delta-m current-directions 0 (%one-d-y one) *one-d-epsilon*)
              (%assert-equals-delta-m current-directions 25 (%one-d-x two) *one-d-epsilon*)
              (%assert-equals-delta-m current-directions 0 (%one-d-y two) *one-d-epsilon*)
              (%assert-equals-delta-m current-directions 0 (%one-d-x three) *one-d-epsilon*)
              (%assert-equals-delta-m current-directions 25 (%one-d-y three) *one-d-epsilon*)
              (%assert-equals-delta-m current-directions 25 (%one-d-x four) *one-d-epsilon*)
              (%assert-equals-delta-m current-directions 25 (%one-d-y four) *one-d-epsilon*))))))))

(deftest test-horizontal-spacings ()
  (let* ((graph (%one-d-new-graph))
         (one (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (two (%one-d-node 50d0 0d0 20d0 20d0 graph))
         (three (%one-d-node 150d0 0d0 20d0 20d0 graph))
         (spacing-handler
           (%one-d-spacings
            (lambda (c1 c2) (declare (ignore c1 c2)) *one-d-spacing*)
            (lambda (c1 c2)
              (cond ((or (eq c1 three) (eq c2 three)) 10)
                    ((or (eq c1 two) (eq c2 two)) 7)
                    ((or (eq c1 one) (eq c2 one)) 5)
                    (t *one-d-spacing*))))))
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) spacing-handler)
                      core-direction.left)))
    (%assert-equals-delta 0 (%one-d-x one) *one-d-epsilon*)
    (%assert-equals-delta 27 (%one-d-x two) *one-d-epsilon*)
    (%assert-equals-delta 57 (%one-d-x three) *one-d-epsilon*)
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) spacing-handler)
                      core-direction.right)))
    (%assert-equals-delta 0 (%one-d-x one) *one-d-epsilon*)
    (%assert-equals-delta 27 (%one-d-x two) *one-d-epsilon*)
    (%assert-equals-delta 57 (%one-d-x three) *one-d-epsilon*)
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) spacing-handler)
                      core-direction.left)))
    (%assert-equals-delta 0 (%one-d-x one) *one-d-epsilon*)
    (%assert-equals-delta 27 (%one-d-x two) *one-d-epsilon*)
    (%assert-equals-delta 57 (%one-d-x three) *one-d-epsilon*)))

(deftest test-vertical-spacing-during-horizontal-compaction ()
  (let* ((graph (%one-d-new-graph))
         (one (%one-d-node 150d0 11d0 20d0 20d0 graph))
         (two (%one-d-node 0d0 40d0 20d0 20d0 graph))
         (three (%one-d-node 150d0 76d0 20d0 20d0 graph))
         (spacing-handler
           (%one-d-spacings
            (lambda (c1 c2)
              (cond ((or (eq c1 three) (eq c2 three)) 15)
                    ((or (eq c1 one) (eq c2 one)) 10)
                    ((or (eq c1 two) (eq c2 two)) 5)
                    (t 0)))
            (lambda (c1 c2) (declare (ignore c1 c2)) 0))))
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) spacing-handler)
                      core-direction.left)))
    (%assert-equals-delta 20 (%one-d-x one) *one-d-epsilon*)
    (%assert-equals-delta 0 (%one-d-x two) *one-d-epsilon*)
    (%assert-equals-delta 0 (%one-d-x three) *one-d-epsilon*)))

(deftest test-vertical-spacings ()
  (let* ((graph (%one-d-new-graph))
         (one (%one-d-node 0d0 0d0 20d0 20d0 graph))
         (two (%one-d-node 0d0 50d0 20d0 20d0 graph))
         (three (%one-d-node 0d0 150d0 20d0 20d0 graph))
         (spacing-handler
           (%one-d-spacings
            (lambda (c1 c2)
              (cond ((or (eq c1 three) (eq c2 three)) 10)
                    ((or (eq c1 two) (eq c2 two)) 7)
                    ((or (eq c1 one) (eq c2 one)) 5)
                    (t 0)))
            (lambda (c1 c2) (declare (ignore c1 c2)) 0))))
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) spacing-handler)
                      core-direction.up)))
    (%assert-equals-delta 0 (%one-d-y one) *one-d-epsilon*)
    (%assert-equals-delta 27 (%one-d-y two) *one-d-epsilon*)
    (%assert-equals-delta 57 (%one-d-y three) *one-d-epsilon*)
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) spacing-handler)
                      core-direction.down)))
    (%assert-equals-delta 0 (%one-d-y one) *one-d-epsilon*)
    (%assert-equals-delta 27 (%one-d-y two) *one-d-epsilon*)
    (%assert-equals-delta 57 (%one-d-y three) *one-d-epsilon*)
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) spacing-handler)
                      core-direction.up)))
    (%assert-equals-delta 0 (%one-d-y one) *one-d-epsilon*)
    (%assert-equals-delta 27 (%one-d-y two) *one-d-epsilon*)
    (%assert-equals-delta 57 (%one-d-y three) *one-d-epsilon*)))

(deftest test-horizontal-spacing-during-vertical-compaction ()
  (let* ((graph (%one-d-new-graph))
         (one (%one-d-node 16d0 150d0 20d0 20d0 graph))
         (two (%one-d-node 40d0 0d0 20d0 20d0 graph))
         (three (%one-d-node 76d0 150d0 20d0 20d0 graph))
         (spacing-handler
           (%one-d-spacings
            (lambda (c1 c2) (declare (ignore c1 c2)) 0)
            (lambda (c1 c2)
              (cond ((or (eq c1 three) (eq c2 three)) 15)
                    ((or (eq c1 two) (eq c2 two)) 10)
                    ((or (eq c1 one) (eq c2 one)) 5)
                    (t 0))))))
    (finish (compact (change-direction
                      (set-spacings-handler (%one-d-compacter graph) spacing-handler)
                      core-direction.up)))
    (%assert-equals-delta 20 (%one-d-y one) *one-d-epsilon*)
    (%assert-equals-delta 0 (%one-d-y two) *one-d-epsilon*)
    (%assert-equals-delta 0 (%one-d-y three) *one-d-epsilon*)))
