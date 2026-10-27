;;;; cycles.lisp -- phase 1: make the graph acyclic.
;;;;
;;;; The greedy heuristic of Eades, Lin and Smyth (1993): repeatedly take sinks
;;;; to the end of a sequence and sources to its front, and otherwise the node
;;;; whose out-degree most exceeds its in-degree to the front.  Edges pointing
;;;; backwards in the sequence are reversed.  Layer constraints and partitions
;;;; come first: a node constrained to the first layer must be a source, one
;;;; constrained to the last a sink, and no edge may run from a higher partition
;;;; to a lower one.

(in-package #:trellis)

(defun break-cycles (lg)
  (let* ((nodes (coerce (lgraph-nodes lg) 'vector))
         (n (length nodes))
         (strategy (opt-enum (lgraph-options-node lg) :cycle-breaking-strategy :greedy))
         (rank (make-array n :initial-element 0)))
    ;; constraints decide some edges outright
    (dolist (e (copy-list (lgraph-edges lg)))
      (let ((s (src-node e)) (tt (tgt-node e)))
        (when (or (member (lnode-constraint tt) '(:first :first-separate))
                  (member (lnode-constraint s) '(:last :last-separate))
                  (and (lnode-partition s) (lnode-partition tt)
                       (> (lnode-partition s) (lnode-partition tt))))
          (unless (or (member (lnode-constraint s) '(:first :first-separate))
                      (member (lnode-constraint tt) '(:last :last-separate))
                      (and (lnode-partition s) (lnode-partition tt)
                           (< (lnode-partition s) (lnode-partition tt))))
            (reverse-ledge e)))))
    (if (eq strategy :depth-first)
        (dfs-order nodes rank)
        (greedy-order nodes rank))
    (dolist (e (copy-list (lgraph-edges lg)))
      (let ((s (src-node e)) (tt (tgt-node e)))
        (when (and (> (aref rank (lnode-index s)) (aref rank (lnode-index tt)))
                   (not (fixed-direction-p e)))
          (reverse-ledge e))))
    lg))

(defun fixed-direction-p (e)
  "Has a constraint already decided E's direction?"
  (let ((s (src-node e)) (tt (tgt-node e)))
    (or (member (lnode-constraint s) '(:first :first-separate))
        (member (lnode-constraint tt) '(:last :last-separate))
        (and (lnode-partition s) (lnode-partition tt)
             (/= (lnode-partition s) (lnode-partition tt))))))

(defun greedy-order (nodes rank)
  "Eades-Lin-Smyth.  Fills RANK with each node's position in the sequence."
  (let* ((n (length nodes))
         (indeg (make-array n :initial-element 0))
         (outdeg (make-array n :initial-element 0))
         (done (make-array n :initial-element nil))
         (front 0) (back (1- n))
         (sources '()) (sinks '()))
    (loop for v across nodes
          do (dolist (e (node-outgoing v))
               (unless (eq (src-node e) (tgt-node e))
                 (incf (aref outdeg (lnode-index v)))
                 (incf (aref indeg (lnode-index (tgt-node e)))))))
    (flet ((bias (v)
             ;; constrained nodes go to their end first
             (case (lnode-constraint v)
               ((:first :first-separate) -1)
               ((:last :last-separate) 1)
               (t 0)))
           (take (i at-front)
             (setf (aref done i) t)
             (if at-front
                 (progn (setf (aref rank i) front) (incf front))
                 (progn (setf (aref rank i) back) (decf back)))
             (let ((v (aref nodes i)))
               (dolist (e (node-outgoing v))
                 (let ((j (lnode-index (tgt-node e))))
                   (unless (or (= i j) (aref done j))
                     (when (zerop (decf (aref indeg j))) (push j sources)))))
               (dolist (e (node-incoming v))
                 (let ((j (lnode-index (src-node e))))
                   (unless (or (= i j) (aref done j))
                     (when (zerop (decf (aref outdeg j))) (push j sinks))))))))
      ;; first-constrained nodes are sources by now; take them first
      (loop for v across nodes
            when (= (bias v) -1) do (take (lnode-index v) t))
      (loop for v across nodes
            when (= (bias v) 1) do (take (lnode-index v) nil))
      (loop for i below n
            unless (aref done i)
              do (cond ((zerop (aref outdeg i)) (push i sinks))
                       ((zerop (aref indeg i)) (push i sources))))
      (setf sources (nreverse sources) sinks (nreverse sinks))
      (loop
        (loop while (or sinks sources)
              do (loop while sinks
                       do (let ((i (pop sinks))) (unless (aref done i) (take i nil))))
                 (loop while sources
                       do (let ((i (pop sources))) (unless (aref done i) (take i t)))))
        (let ((best nil) (best-delta nil))
          (loop for i below n
                unless (aref done i)
                  do (let ((delta (- (aref outdeg i) (aref indeg i))))
                       (when (or (null best) (> delta best-delta))
                         (setf best i best-delta delta))))
          (if best (take best t) (return)))))
    rank))

(defun dfs-order (nodes rank)
  "Depth-first: edges to nodes on the current DFS path are back edges.  RANK
   gets a topological order of the forward edges."
  (let* ((n (length nodes))
         (state (make-array n :initial-element 0))
         (counter n))
    (labels ((visit (v)
               (setf (aref state (lnode-index v)) 1)
               (dolist (e (node-outgoing v))
                 (let ((w (tgt-node e)))
                   (when (zerop (aref state (lnode-index w))) (visit w))))
               (setf (aref state (lnode-index v)) 2
                     (aref rank (lnode-index v)) (decf counter))))
      (loop for v across nodes
            when (and (zerop (aref state (lnode-index v))) (null (node-incoming v)))
              do (visit v))
      (loop for v across nodes
            when (zerop (aref state (lnode-index v))) do (visit v)))
    rank))
