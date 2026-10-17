;;;; tests.lisp -- geometric checks on laid-out graphs.
;;;; (asdf:test-system "diagram") or sbcl --script test/run.lisp

(defpackage #:diagram-test (:use #:cl #:diagram) (:export #:run))
(in-package #:diagram-test)

(defvar *failures* 0)
(defvar *checks* 0)

(defmacro check (form &optional (msg (format nil "~S" form)))
  `(progn (incf *checks*)
          (unless ,form (incf *failures*) (format t "  FAIL: ~A~%" ,msg))))

(defun graph (&rest plist) (plist->graph plist))
(defun nd (id &optional (w 40) (h 30)) (list :id id :width w :height h))
(defun edg (id s tg) (list :id id :sources (list s) :targets (list tg)))

(defun boxes-overlap-p (a b)
  (and (< (node-x a) (+ (node-x b) (node-width b) -0.01))
       (< (node-x b) (+ (node-x a) (node-width a) -0.01))
       (< (node-y a) (+ (node-y b) (node-height b) -0.01))
       (< (node-y b) (+ (node-y a) (node-height a) -0.01))))

(defun section-points (s)
  (append (list (section-start s)) (section-bends s) (list (section-end s))))

(defun on-boundary-p (pt n &optional (tol 0.5))
  "PT (relative to N's parent) lies on N's border, or on one of its ports."
  (let ((x (car pt)) (y (cdr pt))
        (x0 (node-x n)) (y0 (node-y n))
        (x1 (+ (node-x n) (node-width n))) (y1 (+ (node-y n) (node-height n))))
    (or (and (<= (- x0 tol) x (+ x1 tol)) (<= (- y0 tol) y (+ y1 tol))
             (or (< (abs (- x x0)) tol) (< (abs (- x x1)) tol)
                 (< (abs (- y y0)) tol) (< (abs (- y y1)) tol)))
        (some (lambda (p)
                (let ((px (+ x0 (port-x p))) (py (+ y0 (port-y p))))
                  (and (<= (- px tol) x (+ px (port-width p) tol))
                       (<= (- py tol) y (+ py (port-height p) tol)))))
              (remove nil (node-ports n) :key #'port-x)))))

(defun all-nodes (root)
  (cons root (mapcan #'all-nodes (node-children root))))

(defun check-sane (root &key (orthogonal t))
  "No overlapping siblings, children inside parents, edges orthogonal and
anchored on their endpoints' boundaries."
  (dolist (n (all-nodes root))
    (loop for (a . rest) on (node-children n)
          do (dolist (b rest)
               (check (not (boxes-overlap-p a b))
                      (format nil "~A and ~A overlap" (node-id a) (node-id b))))
             (check (and (>= (node-x a) -0.01) (>= (node-y a) -0.01)
                         (<= (+ (node-x a) (node-width a)) (+ (node-width n) 0.01))
                         (<= (+ (node-y a) (node-height a)) (+ (node-height n) 0.01)))
                    (format nil "~A not inside ~A" (node-id a) (node-id n))))
    (dolist (e (node-edges n))
      (dolist (s (edge-sections e))
        (let ((pts (section-points s)))
          (when orthogonal
            (loop for (p q) on pts while q
                  do (check (or (< (abs (- (car p) (car q))) 0.01) (< (abs (- (cdr p) (cdr q))) 0.01))
                            (format nil "edge ~A has a diagonal segment ~S -> ~S" (edge-id e) p q))))
          (let ((c (edge-container e)))
            (flet ((in-container (el)
                     (let ((nd (if (port-p el) (port-owner el) el)))
                       (loop for x = nd then (node-parent x) while x
                             when (eq (node-parent x) c) return x))))
              (let ((sn (in-container (first (edge-sources e))))
                    (tn (in-container (first (edge-targets e)))))
                (check (on-boundary-p (first pts) sn)
                       (format nil "edge ~A start ~S not on ~A" (edge-id e) (first pts) (node-id sn)))
                (check (on-boundary-p (car (last pts)) tn)
                       (format nil "edge ~A end ~S not on ~A" (edge-id e) (car (last pts)) (node-id tn)))))))))))

(defun find-node (root id) (find id (all-nodes root) :key #'node-id :test #'equal))

(defun test-chain-directions ()
  (dolist (dir '("RIGHT" "DOWN" "LEFT" "UP"))
    (let ((g (layout (graph :id "root" :layout-options (list :direction dir)
                            :children (list (nd "a") (nd "b") (nd "c"))
                            :edges (list (edg "e1" "a" "b") (edg "e2" "b" "c"))))))
      (check-sane g)
      (let ((a (find-node g "a")) (b (find-node g "b")) (c (find-node g "c")))
        (flet ((flow (n) (cond ((string= dir "RIGHT") (node-x n)) ((string= dir "LEFT") (- (node-x n)))
                               ((string= dir "DOWN") (node-y n)) (t (- (node-y n))))))
          (check (< (flow a) (flow b) (flow c)) (format nil "~A: a b c not in flow order" dir)))
        ;; A straight chain should be straight: every edge one segment.
        (dolist (e (node-edges g))
          (check (null (section-bends (first (edge-sections e))))
                 (format nil "~A: chain edge ~A bends" dir (edge-id e))))))))

(defun test-cycle-and-long-edges ()
  (let ((g (layout (graph :id "root"
                          :children (mapcar #'nd '("a" "b" "c" "d" "e"))
                          :edges (list (edg "1" "a" "b") (edg "2" "b" "c") (edg "3" "c" "d")
                                       (edg "4" "d" "a") (edg "5" "a" "e") (edg "6" "a" "d")
                                       (edg "7" "e" "e"))))))
    (check-sane g)
    (dolist (e (node-edges g))
      (check (= 1 (length (edge-sections e))) (format nil "edge ~A has no section" (edge-id e))))))

(defun test-crossings ()
  ;; K(2,2)-ish crossing that a good order removes: a->d, b->c with input order c,d.
  (let ((g (layout (graph :id "root" :children (mapcar #'nd '("a" "b" "c" "d"))
                          :edges (list (edg "1" "a" "c") (edg "2" "b" "d")
                                       (edg "3" "a" "c") (edg "4" "b" "d"))))))
    (check-sane g)
    (check (eq (< (node-y (find-node g "a")) (node-y (find-node g "b")))
               (< (node-y (find-node g "c")) (node-y (find-node g "d"))))
           "parallel edges were ordered to cross")))

(defun test-ports ()
  (let ((g (layout (graph :id "root"
                          :children (list (list :id "a" :width 40 :height 60
                                                :ports (list (list :id "a.o1" :width 6 :height 6)
                                                             (list :id "a.o2" :width 6 :height 6)))
                                          (list :id "b" :width 40 :height 30
                                                :ports (list (list :id "b.i" :width 6 :height 6)))
                                          (nd "c"))
                          :edges (list (edg "1" "a.o1" "b.i") (edg "2" "a.o2" "c"))))))
    (check-sane g)
    (let ((a (find-node g "a")))
      (dolist (p (node-ports a))
        (check (= (port-x p) (node-width a)) (format nil "port ~A not on the east side" (port-id p)))))
    (let ((b (find-node g "b")))
      (check (= (port-x (first (node-ports b))) -6d0) "b's input port not on the west side"))))

(defun test-hierarchy ()
  (let ((g (layout (graph :id "root" :layout-options '(:direction "DOWN")
                          :children (list (nd "x")
                                          (list :id "group" :layout-options '(:direction "RIGHT")
                                                :children (list (nd "g1") (nd "g2"))
                                                :edges (list (edg "in" "g1" "g2")))
                                          (nd "y"))
                          :edges (list (edg "1" "x" "g1") (edg "2" "group" "y"))))))
    (check-sane g)
    (let ((grp (find-node g "group")))
      (check (> (node-width grp) 80) "group was not sized to fit its children")
      (check (< (node-x (find-node g "g1")) (node-x (find-node g "g2"))) "inner direction ignored"))
    (check (< (node-y (find-node g "x")) (node-y (find-node g "group")) (node-y (find-node g "y")))
           "outer DOWN order wrong")))

(defun test-polyline-and-components ()
  (let ((g (layout (graph :id "root" :layout-options '(:edge-routing "POLYLINE")
                          :children (mapcar #'nd '("a" "b" "c" "p" "q"))
                          :edges (list (edg "1" "a" "b") (edg "2" "a" "c") (edg "3" "p" "q"))))))
    (check-sane g :orthogonal nil)))

(defun test-json-roundtrip ()
  (let* ((in "{\"id\":\"root\",\"layoutOptions\":{\"elk.algorithm\":\"layered\",\"elk.direction\":\"DOWN\"},
               \"custom\":[1,2.5,true,null,\"\\u00e9\"],
               \"children\":[{\"id\":\"n1\",\"width\":30,\"height\":30,\"labels\":[{\"text\":\"one\"}]},
                             {\"id\":\"n2\",\"width\":30,\"height\":30}],
               \"edges\":[{\"id\":\"e1\",\"sources\":[\"n1\"],\"targets\":[\"n2\"],\"labels\":[{\"text\":\"go\"}]}]}")
         (out (parse-json (layout-json in)))
         (e (aref (jget out "edges") 0))
         (s (aref (jget e "sections") 0)))
    (check (equalp (jget out "custom") (vector 1 2.5d0 :true :null (string (code-char 233))))
           "unknown fields not preserved")
    (check (equal (jget e "container") "root") "edge container missing")
    (check (= (jget (jget s "startPoint") "x") (jget (jget s "endPoint") "x")) "DOWN chain not vertical")
    (check (realp (jget (aref (jget (aref (jget out "children") 0) "labels") 0) "x")) "node label not placed")
    (check (realp (jget (aref (jget e "labels") 0) "y")) "edge label not placed")))

(defun test-box-and-fixed ()
  (let ((g (layout (graph :id "root" :layout-options '(:algorithm "box")
                          :children (loop for i below 9 collect (nd (format nil "n~D" i)))))))
    (check-sane g))
  (let ((g (layout (graph :id "root" :layout-options '(:algorithm "fixed")
                          :children (list (list :id "a" :x 0 :y 0 :width 20 :height 20)
                                          (list :id "b" :x 100 :y 50 :width 20 :height 20))
                          :edges (list (edg "1" "a" "b"))))))
    (check (= (node-x (find-node g "b")) 100) "fixed moved a node")
    (check-sane g :orthogonal nil)))

(defun random-graph (seed n m)
  (let ((*random-state* (sb-ext:seed-random-state seed)))
    (apply #'graph :id "root"
           :layout-options (list :direction (nth (random 4) '("RIGHT" "DOWN" "LEFT" "UP")))
           :children (loop for i below n collect (nd (format nil "n~D" i) (+ 20 (random 60)) (+ 15 (random 40))))
           :edges (loop for i below m
                        collect (edg (format nil "e~D" i) (format nil "n~D" (random n)) (format nil "n~D" (random n))))
           nil)))

(defun test-random ()
  (loop for seed from 1 to 40
        do (let ((before *failures*))
             (check-sane (layout (random-graph seed (+ 3 (mod seed 17)) (+ 2 (mod (* 7 seed) 31)))))
             (when (> *failures* before) (format t "  (random seed ~D)~%" seed)))))

(defun run ()
  (setf *failures* 0 *checks* 0)
  (dolist (tst '(test-chain-directions test-cycle-and-long-edges test-crossings test-ports
                 test-hierarchy test-polyline-and-components test-json-roundtrip
                 test-box-and-fixed test-random))
    (format t "~A~%" tst)
    (handler-case (funcall tst)
      (error (c) (incf *failures*) (format t "  ERROR: ~A~%" c))))
  (format t "~D checks, ~D failures~%" *checks* *failures*)
  (zerop *failures*))
