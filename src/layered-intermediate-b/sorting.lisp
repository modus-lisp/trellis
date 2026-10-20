;;;; sorting.lisp -- SortByInputModelProcessor and SemiInteractiveCrossMinProcessor.

(in-package #:elk)

;;; ------------------------------------------------ SortByInputModelProcessor

(defjmethod process ((this sort-by-input-model-processor) graph progress-monitor)
  (begin progress-monitor
         (jstr "Sort By Input Model " (get-property graph layered-options.consider-model-order-strategy))
         1)
  (let ((layer-index 0))
    (jdo (layer (get-layers graph))
      ;; The layer.id is necessary to check whether nodes really connect to the previous layer.
      ;; Feedback long edge dummies do might have a long-edge dummy in the same layer.
      (setf (.id layer) layer-index)
      (let* ((previous-layer-index (if (= layer-index 0) 0 (1- layer-index)))
             (previous-layer (jl-get (get-layers graph) previous-layer-index))
             ;; Sort nodes before port sorting to have sorted nodes for in-layer feedback edge dummies.
             (comparator (new-model-order-node-comparator
                          graph previous-layer
                          (get-property graph layered-options.consider-model-order-strategy)
                          (get-property graph layered-options.consider-model-order-long-edge-strategy)
                          (get-property graph layered-options.consider-model-order-group-model-order-cm-group-order-strategy)
                          t)))
        (sort-by-input-model-processor.insertion-sort (get-nodes layer) comparator)
        (jdo (node (get-nodes layer))
          (when (and (not (eq (get-property node layered-options.port-constraints) port-constraints.fixed-order))
                     (not (eq (get-property node layered-options.port-constraints) port-constraints.fixed-pos)))
            ;; Special case:
            ;; If two edges (of the same node) have the same target node they should be next to each other.
            ;; Therefore all ports that connect to the same node should have the same
            ;; (their minimal) model order.
            ;; Get minimal model order for target node
            (jl-sort (get-ports node)
                     (let ((port-comparator
                             (new-model-order-port-comparator
                              graph previous-layer
                              (get-property graph layered-options.consider-model-order-strategy)
                              (sort-by-input-model-processor.long-edge-target-node-preprocessing node)
                              (get-property graph layered-options.consider-model-order-port-model-order))))
                       (lambda (a b) (compare port-comparator a b))))))))
      ;; Sort nodes after port sorting to also sort dummy feedback nodes from the current layer correctly.
      (let ((comparator (new-model-order-node-comparator
                         graph (jl-get (get-layers graph) (if (= layer-index 0) 0 (1- layer-index)))
                         (get-property graph layered-options.consider-model-order-strategy)
                         (get-property graph layered-options.consider-model-order-long-edge-strategy)
                         (get-property graph layered-options.consider-model-order-group-model-order-cm-group-order-strategy)
                         nil)))
        (sort-by-input-model-processor.insertion-sort (get-nodes layer) comparator))
      (incf layer-index)))
  (done progress-monitor))

(defjstatic sort-by-input-model-processor.long-edge-target-node-preprocessing (node)
  "Map<LNode, Integer>: the minimal model order of the target nodes of NODE's ports."
  (when (has-property node layered-internal-properties.target-node-model-order)
    (return-from sort-by-input-model-processor.long-edge-target-node-preprocessing/1
      (get-property node layered-internal-properties.target-node-model-order)))
  (let ((target-node-model-order (new-jmap)))
    (jdo (p (get-ports node))
      (unless (jl-is-empty (get-outgoing-edges p))
        (let ((target-node (sort-by-input-model-processor.get-target-node p)))
          (set-property p layered-internal-properties.long-edge-target-node target-node)
          (when target-node
            (let ((previous-order integer.max-value))
              (when (jm-contains-key target-node-model-order target-node)
                (setf previous-order (jm-get target-node-model-order target-node)))
              (let ((edge (jl-get (get-outgoing-edges p) 0)))
                (unless (get-property edge layered-internal-properties.reversed)
                  (setf (gethash target-node target-node-model-order)
                        (math.min (get-property edge layered-internal-properties.model-order)
                                  previous-order)))))))))
    (set-property node layered-internal-properties.target-node-model-order target-node-model-order)
    target-node-model-order))

(defjstatic sort-by-input-model-processor.get-target-node (port)
  (block target
    (let* ((node nil)
           (edge (jl-get (get-outgoing-edges port) 0)))
      (loop
        (setf node (get-node (get-target edge)))
        ;; If the dummy node has a target return it.
        (when (has-property node layered-internal-properties.long-edge-target)
          (return-from target (get-node (get-property node layered-internal-properties.long-edge-target))))
        ;; It not the current node might be the target node or one has to iterate manually through to it.
        (cond ((and (not (eq (get-type node) node-type.normal))
                    (not (jl-is-empty (get-outgoing-edges node))))
               (setf edge (jl-get (get-outgoing-edges node) 0)))
              ((not (eq (get-type node) node-type.normal))
               (return-from target nil)))
        (unless (and node (not (eq (get-type node) node-type.normal)))
          (return)))
      node)))

(defjstatic sort-by-input-model-processor.insertion-sort (layer comparator)
  (loop for i from 1 below (length layer)
        do (let* ((temp (jl-get layer i))
                  (j i))
             (loop while (and (> j 0) (> (compare comparator (jl-get layer (1- j)) temp) 0))
                   do (jl-set layer j (jl-get layer (1- j)))
                      (decf j))
             (jl-set layer j temp)))
  (clear-transitive-ordering comparator))

(defjstatic sort-by-input-model-processor.insertion-sort-port (layer comparator)
  (loop for i from 1 below (length layer)
        do (let* ((temp (jl-get layer i))
                  (j i))
             (loop while (and (> j 0) (> (compare comparator (jl-get layer (1- j)) temp) 0))
                   do (jl-set layer j (jl-get layer (1- j)))
                      (decf j))
             (jl-set layer j temp)))
  (clear-transitive-ordering comparator))

;;; ------------------------------------------------ SemiInteractiveCrossMinProcessor

(defjmethod process ((this semi-interactive-cross-min-processor) layered-graph progress-monitor)
  (begin progress-monitor "Semi-Interactive Crossing Minimization Processor" 1)
  (let ((added-constraints nil))
    (jdo (l (get-layers layered-graph))
      ;; #1 extract relevant nodes
      ;; #2 sort them with ascending y coordinate
      ;; #3 introduce pair-wise in-layer constraints
      (let ((candidates (new-jlist)))
        (jdo (n (get-nodes l))
          (when (and (eq (get-type n) node-type.normal)
                     (has-property n layered-options.position))
            (jl-add candidates n)))
        (jl-sort candidates
                 (lambda (n1 n2)
                   (let ((orig-pos1 (get-property n1 layered-options.position))
                         (orig-pos2 (get-property n2 layered-options.position)))
                     (double.compare (.y orig-pos1) (.y orig-pos2)))))
        ;; reduce((prev, cur) -> { prev.IN_LAYER_SUCCESSOR_CONSTRAINTS.add(cur); return cur; })
        (when (not (jl-is-empty candidates))
          (let ((prev (jl-get candidates 0)))
            (loop for i from 1 below (length candidates)
                  do (let ((cur (jl-get candidates i)))
                       (jl-add (get-property prev layered-internal-properties.in-layer-successor-constraints) cur)
                       (setf prev cur))))
          (setf added-constraints t))))
    ;; If we added in-layer successor constraints, make subsequent phases aware
    (when added-constraints
      (set-property layered-graph
                    layered-internal-properties.in-layer-successor-constraints-between-non-dummies t)))
  (done progress-monitor))
