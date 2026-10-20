;;;; intermediate.lisp -- org.eclipse.elk.alg.rectpacking.intermediate.

(in-package #:elk)

;;; Java's Collections.sort / Arrays.sort(Object[], Comparator) on a vector, in
;;; place.  Below 32 elements Java's TimSort is countRunAndMakeAscending followed
;;; by binarySort; that code is reproduced here exactly, because with an
;;; inconsistent comparator (InteractiveNodeReorderer returns -1 for equal
;;; keys) the result depends on that algorithm, not only on the ordering.
;;; From 32 elements up, a stable merge sort gives Java's result for any
;;; consistent comparator.

(defun %rp-count-run-and-make-ascending (a lo hi cmp)
  (let ((run-hi (1+ lo)))
    (when (= run-hi hi)
      (return-from %rp-count-run-and-make-ascending 1))
    (let ((first-cmp (funcall cmp (aref a run-hi) (aref a lo))))
      (incf run-hi)
      (if (< first-cmp 0)
          (progn
            (loop while (and (< run-hi hi) (< (funcall cmp (aref a run-hi) (aref a (1- run-hi))) 0))
                  do (incf run-hi))
            (let ((i lo) (j (1- run-hi)))
              (loop while (< i j)
                    do (rotatef (aref a i) (aref a j))
                       (incf i) (decf j))))
          (loop while (and (< run-hi hi) (>= (funcall cmp (aref a run-hi) (aref a (1- run-hi))) 0))
                do (incf run-hi))))
    (- run-hi lo)))

(defun %rp-binary-sort (a lo hi start cmp)
  (when (= start lo) (incf start))
  (loop while (< start hi)
        do (let ((pivot (aref a start))
                 (left lo)
                 (right start))
             (loop while (< left right)
                   do (let ((mid (ash (+ left right) -1)))
                        (if (< (funcall cmp pivot (aref a mid)) 0)
                            (setf right mid)
                            (setf left (1+ mid)))))
             (loop for p from start above left
                   do (setf (aref a p) (aref a (1- p))))
             (setf (aref a left) pivot))
           (incf start))
  a)

(defun %rp-java-sort (v cmp)
  "Sorts the adjustable vector V in place with comparator CMP (a function returning an int)."
  (let ((n (length v)))
    (when (>= n 2)
      (if (< n 32)
          (%rp-binary-sort v 0 n (%rp-count-run-and-make-ascending v 0 n cmp) cmp)
          (jl-sort v cmp)))
    v))

;;; ------------------------------------------------------------ NodeSizeComparator

(defjmethod compare ((this node-size-comparator) node0 node1)
  "Double.compare(node1.getHeight(), node0.getHeight())"
  (double.compare (get-height/0 node1) (get-height/0 node0)))

;;; ------------------------------------------------------------ NodeSizeReorderer

(defjmethod process ((this node-size-reorderer) graph progress-monitor)
  (declare (ignore progress-monitor))
  ;; ECollections.sort(graph.getChildren(), new NodeSizeComparator())
  (let ((comparator (new-node-size-comparator/0)))
    (%rp-java-sort (.items (get-children/0 graph))
                   (lambda (a b) (compare/2 comparator a b))))
  nil)

;;; ------------------------------------------------------------ InteractiveNodeReorderer

(defjmethod process ((this interactive-node-reorderer) graph progress-monitor)
  (begin/2 progress-monitor "Interactive Node Reorderer" 1)
  (let* ((rectangles (get-children/0 graph))
         (fixed-nodes (new-jlist)))
    (jdo (elk-node rectangles)
      (when (has-property/2 elk-node rect-packing-options.desired-position)
        (jl-add fixed-nodes elk-node)))
    (jdo (elk-node fixed-nodes)
      (j-remove/1 rectangles elk-node))
    (%rp-java-sort fixed-nodes
                   (lambda (a b)
                     (let ((position-a (get-property/2 a rect-packing-options.desired-position))
                           (position-b (get-property/2 b rect-packing-options.desired-position)))
                       (if (= position-a position-b)
                           -1
                           (integer.compare position-a position-b)))))
    (jdo (elk-node fixed-nodes)
      (let ((position (get-property/2 elk-node rect-packing-options.desired-position)))
        (setf position (min position (size/0 rectangles)))
        (add/2 rectangles position elk-node)))
    (let ((index 0))
      (jdo (elk-node rectangles)
        (set-property/3 elk-node rect-packing-options.current-position index)
        (incf index))))
  (done/0 progress-monitor)
  nil)

;;; ------------------------------------------------------------ MinSizePreProcessor

(defjmethod process ((this min-size-pre-processor) graph progress-monitor)
  (begin/2 progress-monitor "Min Size Preprocessing" 1)
  ;; Get minimum size based on children.
  (let ((min-size (elk-util.effective-min-size-constraint-for graph)))
    ;; Get minimum size based on labels.
    (when (get-parent/0 graph)
      ;; only possible if a parent exists.
      (let* ((graph-adapter (elk-graph-adapters.adapt (get-parent/0 graph)))
             (node-adapter (elk-graph-adapters.adapt-single-node graph))
             (min-size2 (node-label-and-size-calculator.process/4 graph-adapter node-adapter nil t)))
        (setf (.x min-size) (math.max (.x min-size) (.x min-size2)))
        (setf (.y min-size) (math.max (.y min-size) (.y min-size2)))))
    (set-property/3 graph rectpacking-internal-properties.min-width (.x min-size))
    (set-property/3 graph rectpacking-internal-properties.min-height (.y min-size)))
  (done/0 progress-monitor)
  nil)

;;; ------------------------------------------------------------ MinSizePostProcessor

(defjmethod process ((this min-size-post-processor) graph progress-monitor)
  (begin/2 progress-monitor "Min Size Postprocessing" 1)
  ;; Remove padding to get the space the algorithm can use.
  (set-property/3 graph rectpacking-internal-properties.target-width
                  (math.max (get-property/2 graph rectpacking-internal-properties.target-width)
                            (get-property/2 graph rectpacking-internal-properties.min-width)))
  (done/0 progress-monitor)
  nil)

;;; ------------------------------------------------------------ IntermediateProcessorStrategy

(defjmethod create ((this rectpacking-intermediate-processor-strategy))
  (enum-case this
    (rectpacking-intermediate-processor-strategy.node-size-reorderer (new-node-size-reorderer/0))
    (rectpacking-intermediate-processor-strategy.interactive-node-reorderer (new-interactive-node-reorderer/0))
    (rectpacking-intermediate-processor-strategy.min-size-pre-processor (new-min-size-pre-processor/0))
    (rectpacking-intermediate-processor-strategy.min-size-post-processor (new-min-size-post-processor/0))
    (t nil)))
