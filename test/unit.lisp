;;;; unit.lisp -- unit tests.  (trellis-test:run-unit-tests) => T when all pass.

(in-package #:trellis-test)

(defvar *failures* '())
(defvar *count* 0)

(defmacro check (form &optional description)
  `(progn
     (incf *count*)
     (unless (ignore-errors ,form)
       (push (or ,description ',form) *failures*))))

(defmacro deftest (name &body body)
  `(defun ,name () ,@body))

(defun clean-p (g)
  (every #'zerop (loop for (nil v) on (check-layout g) by #'cddr collect v)))

(defun orthogonal-p (e)
  (loop for (a b) on (section-points (first (edge-sections e))) while b
        always (or (< (abs (- (car a) (car b))) 1d-6) (< (abs (- (cdr a) (cdr b))) 1d-6))))

(defun chain-graph (n &rest options)
  (let* ((g (apply #'make-graph options))
         (nodes (loop for i below n collect (add-node g :id (format nil "n~D" i) :width 40 :height 30))))
    (loop for (a b) on nodes while b do (add-edge g a b))
    (values g nodes)))

;;; ---- options and JSON --------------------------------------------------------

(deftest test-options
  (check (eq (trellis::option-key "org.eclipse.elk.layered.spacing.edgeEdgeBetweenLayers")
             :spacing-edge-edge-between-layers))
  (check (eq (trellis::option-key "elk.direction") :direction))
  (check (eq (trellis::option-key "nodeLabels.placement") :node-labels-placement))
  (check (eq (trellis::parse-option-value "FIXED_SIDE") :fixed-side))
  (check (equal (trellis::parse-option-value "[H_LEFT, V_TOP]") '(:h-left :v-top)))
  (check (eq (trellis::parse-option-value "org.eclipse.elk.layered") :layered))
  (check (= (trellis::parse-option-value "1.5") 1.5d0))
  (check (eq (trellis::parse-option-value "true") t))
  (let ((p (trellis::parse-option-value "[top=1.0,left=2.0,bottom=3.0,right=4.0]")))
    (check (and (= (trellis::pad-top p) 1) (= (trellis::pad-right p) 4))))
  (let ((g (make-graph :direction :down)))
    (check (eq (option g :direction) :down))
    (set-option g "org.eclipse.elk.spacing.nodeNode" "33")
    (check (= (option g :spacing-node-node) 33))))

(deftest test-json-round-trip
  (let* ((text "{\"id\":\"root\",\"layoutOptions\":{\"elk.direction\":\"DOWN\"},
                 \"children\":[{\"id\":\"a\",\"width\":40,\"height\":NaN,
                                \"ports\":[{\"id\":\"pa\",\"width\":5,\"height\":5}],
                                \"labels\":[{\"text\":\"A\",\"width\":10,\"height\":12}]},
                               {\"id\":\"b\",\"width\":40,\"height\":30}],
                 \"edges\":[{\"id\":\"e\",\"sources\":[\"pa\"],\"targets\":[\"b\"],
                             \"labels\":[{\"text\":\"lbl\",\"width\":20,\"height\":10}]}]}")
         (g (read-elk-json text)))
    (check (= (length (node-children g)) 2))
    (check (zerop (node-height (find-element g "a"))) "NaN sizes read as 0")
    (check (eq (first (edge-sources (find-element g "e"))) (find-element g "pa")))
    (layout g)
    (check (clean-p g))
    (let ((g2 (read-elk-json (write-elk-json g))))
      (check (= (node-x (find-element g2 "b")) (node-x (find-element g "b"))))
      (check (= (length (edge-sections (find-element g2 "e"))) 1))
      (check (eq (option g2 :direction) :down)))
    (check (search "\"sections\"" (layout-elk-json text)))))

;;; ---- layered -----------------------------------------------------------------

(deftest test-directions
  (dolist (dir '(:right :left :down :up))
    (multiple-value-bind (g nodes) (chain-graph 4 :direction dir)
      (layout g)
      (check (clean-p g) (list :clean dir))
      (check (every #'orthogonal-p (all-edges g)) (list :orthogonal dir))
      (let ((coords (mapcar (lambda (n) (ecase dir
                                          (:right (node-x n)) (:left (- (node-x n)))
                                          (:down (node-y n)) (:up (- (node-y n)))))
                            nodes)))
        (check (apply #'< coords) (list :flow dir))))))

(deftest test-cycle-and-self-loop
  (let* ((g (make-graph))
         (a (add-node g :width 30 :height 30)) (b (add-node g :width 30 :height 30))
         (c (add-node g :width 30 :height 30)))
    (add-edge g a b) (add-edge g b c) (add-edge g c a) (add-edge g b b)
    (layout g)
    (check (clean-p g))
    (check (every #'orthogonal-p (all-edges g)))))

(deftest test-fixed-ports
  (let* ((g (make-graph))
         (a (add-node g :width 60 :height 60 :options '(:port-constraints :fixed-pos)))
         (pa (add-port a :x 60 :y 10 :width 4 :height 4))
         (b (add-node g :width 40 :height 40)))
    (add-edge g pa b)
    (layout g)
    (check (and (= (port-x pa) 60) (= (port-y pa) 10)) "fixed port keeps its place")
    (check (clean-p g))))

(deftest test-port-sides
  (let* ((g (make-graph))
         (a (add-node g :width 60 :height 60 :options '(:port-constraints :fixed-side)))
         (pn (add-port a :side :north :width 4 :height 4))
         (b (add-node g :width 40 :height 40)))
    (add-edge g pn b)
    (layout g)
    (check (< (port-y pn) 0) "north port stays north")
    (check (clean-p g))
    (check (every #'orthogonal-p (all-edges g)))))

(deftest test-edge-labels
  (let* ((g (make-graph))
         (a (add-node g :width 40 :height 30)) (b (add-node g :width 40 :height 30))
         (e (add-edge g a b :labels '("a label"))))
    (layout g)
    (let ((l (first (edge-labels e))))
      (check (> (label-x l) (+ (node-x a) (node-width a))) "label between the nodes")
      (check (< (+ (label-x l) (label-width l)) (node-x b))))
    (check (clean-p g))))

(deftest test-hierarchy
  (let* ((g (make-graph))
         (outer (add-node g :id "outer"))
         (inner (add-node outer :id "inner" :width 30 :height 30))
         (other (add-node g :id "other" :width 30 :height 30))
         (e (add-edge g inner other)))
    (layout g)
    (check (clean-p g))
    (check (edge-sections e))
    (check (every #'orthogonal-p (all-edges g)))
    (check (> (node-width outer) 30) "compound sized to its child")))

(deftest test-components
  (let* ((g (make-graph)))
    (dotimes (i 6) (add-node g :width 30 :height 20))
    (add-edge g (first (node-children g)) (second (node-children g)))
    (layout g)
    (check (clean-p g))))

(deftest test-degenerate
  (let ((g (make-graph))) (layout g) (check (clean-p g)) (check (plusp (node-width g))))
  (let ((g (make-graph))) (add-node g) (layout g) (check (clean-p g)))
  (let* ((g (make-graph :spacing-node-node -30 :spacing-edge-edge-between-layers -20)))
    (dotimes (i 5) (add-node g :width 0 :height 0))
    (add-edge g (first (node-children g)) (second (node-children g)))
    (layout g)
    (check (clean-p g) "negative spacing and zero sizes")))

(deftest test-determinism
  (flet ((run () (let ((g (read-elk-json (case-file "0001" "in"))))
                   (layout g) (write-elk-json g))))
    (check (string= (run) (run)))))

(deftest test-candidates
  (flet ((crossings (n)
           (let ((g (read-elk-json (asdf:system-relative-pathname "trellis" "examples/modus-lisp.json"))))
             (set-option g :candidates n)
             (layout g)
             (check (clean-p g))
             (getf (layout-metrics g) :crossings))))
    (check (<= (crossings 4) (crossings 1)) "more candidates never do worse")))

(defun example (name)
  (read-elk-json (asdf:system-relative-pathname "trellis" (format nil "examples/~A.json" name))))

(deftest test-port-dummies
  ;; ports on every side, inverted ports and a feedback edge (Schulze et al.)
  (let ((g (example "port-sides")))
    (layout g)
    (check (clean-p g))
    (check (every #'orthogonal-p (all-edges g)))
    (check (zerop (getf (layout-metrics g) :node-hits)) "no edge crosses a node")))

(deftest test-splines-and-ties
  (let ((g (example "wire-ties")))
    (layout g)
    (check (clean-p g))
    (check (every (lambda (e) (let ((s (first (edge-sections e))))
                                (and (section-curve s) (= 1 (mod (length (section-points s)) 3)))))
                  (all-edges g))
           "every route is a Bezier curve")
    (check (zerop (getf (layout-metrics g) :node-hits)) "curves stay in the gaps")
    (check (node-ties g) "bundles get ties"))
  (let ((g (example "wire-ties")))
    (set-option g :wire-ties nil)
    (layout g)
    (check (null (node-ties g)) "no ties unless asked")))

;;; ---- every algorithm ---------------------------------------------------------------

(deftest test-algorithms
  (dolist (alg (algorithms))
    (let* ((g (make-graph :algorithm alg))
           (ns (loop for i below 8 collect (add-node g :width (+ 20 (* 5 i)) :height 20))))
      (loop for (a b) on ns while b do (add-edge g a b))
      (add-edge g (first ns) (fourth ns))
      (layout g)
      (check (or (eq alg :fixed) (clean-p g)) (list :algorithm alg)))))

;;; ---- internals ---------------------------------------------------------------------

(deftest test-network-simplex
  ;; a diamond plus a long chord: optimal ranks put the chord's ends 2 apart
  (let ((r (trellis::network-simplex 4 '((0 1 1 1) (0 2 1 1) (1 3 1 1) (2 3 1 1) (0 3 1 1)))))
    (check (equalp r #(0 1 1 2))))
  (let ((r (trellis::network-simplex 3 '((0 1 1 1) (1 2 1 1) (0 2 1 5)))))
    (check (equalp r #(0 1 2)))))

(deftest test-crossing-count
  ;; the accumulator tree against brute force: random perfect matchings
  ;; between two layers, where crossings are exactly the inversions
  (let ((rng (trellis::make-rng 7)))
    (dotimes (trial 20)
      (let* ((g (make-graph)) (n 8)
             (left (loop for i below n collect (add-node g :width 10 :height 10 :options '(:layering-layer-constraint :first))))
             (right (loop for i below n collect (add-node g :width 10 :height 10 :options '(:layering-layer-constraint :last))))
             (perm (trellis::rng-shuffle rng (coerce (loop for i below n collect i) 'vector))))
        (loop for a in left for i from 0 do (add-edge g a (nth (aref perm i) right)))
        (let ((trellis::*frame-edges* (make-hash-table :test 'eq)))
          (setf (gethash g trellis::*frame-edges*) (node-edges g))
          (let ((lg (trellis::import-layered g)))
            (setf (trellis::lgraph-spacing lg) (trellis::layered-spacing g))
            (trellis::break-cycles lg)
            (trellis::assign-free-port-sides lg)
            (trellis::assign-layers lg)
            (trellis::init-port-orders lg)
            (let* ((layers (trellis::lgraph-layers lg))
                   (fast (trellis::count-crossings-between (aref layers 0) (aref layers 1)))
                   (pairs (loop for v across (aref layers 0)
                                collect (cons (trellis::lnode-order v)
                                              (trellis::lnode-order (trellis::tgt-node (first (trellis::node-outgoing v)))))))
                   (slow (loop for (a . rest) on pairs
                               sum (count-if (lambda (b) (< (* (- (car a) (car b)) (- (cdr a) (cdr b))) 0)) rest))))
              (check (= (length layers) 2))
              (check (= slow fast) (list :crossings slow fast)))))))))

;;; ---- runner ----------------------------------------------------------------------

(defun run-unit-tests ()
  (setf *failures* '() *count* 0)
  (progn
    (dolist (test '(test-options test-json-round-trip test-directions test-cycle-and-self-loop
                    test-fixed-ports test-port-sides test-edge-labels test-hierarchy
                    test-components test-degenerate test-determinism test-candidates test-port-dummies test-splines-and-ties test-algorithms
                    test-network-simplex test-crossing-count))
      (handler-case (funcall test)
        (error (e) (push (list test :error (princ-to-string e)) *failures*)))))
  (format t "~&~D checks, ~D failed~%" *count* (length *failures*))
  (dolist (f (reverse *failures*)) (format t "  FAIL ~S~%" f))
  (null *failures*))
