;;;; partitions.lisp -- PartitionPreprocessor and PartitionMidprocessor.

(in-package #:elk)

;;; Guava Multimap<Integer, LNode> (HashMultimap): a hash table key -> jlist.

(defun %lib-multimap-put (table key value)
  (let ((values (gethash key table)))
    (unless values
      (setf values (new-jlist))
      (setf (gethash key table) values))
    (jl-add values value)))

(defun %lib-multimap-get (table key)
  "Multimap.get: the values for KEY, an empty list when there are none."
  (or (gethash key table) (new-jlist)))

;;; ------------------------------------------------ PartitionPreprocessor

(defjconst partition-preprocessor.partition-constraint-edge-priority 1000)

(defjmethod process ((this partition-preprocessor) l-graph monitor)
  (begin monitor "Partition preprocessing" 1)
  ;; Find all edges that must be reversed, and then reverse them (this needs to be a two-step process to avoid
  ;; ConcurrentModificationExceptions)
  (let ((partitioned-nodes (new-jlist)))
    (jdo (l-node (get-layerless-nodes l-graph))
      (when (has-property l-node layered-options.partitioning-partition)
        (jl-add partitioned-nodes l-node)))
    (let ((edges-to-be-reversed (new-jlist)))
      (jdo (l-node (get-layerless-nodes l-graph))
        (when (has-property l-node layered-options.partitioning-partition)
          (jdo (l-edge (get-outgoing-edges l-node))
            (when (must-be-reversed this l-edge partitioned-nodes)
              (jl-add edges-to-be-reversed l-edge)))))
      (jdo (l-edge edges-to-be-reversed)
        (j-reverse this l-edge l-graph))))
  (done monitor))

(defjmethod must-be-reversed ((this partition-preprocessor) l-edge partitioned-nodes)
  (when (has-property (get-node (get-target l-edge)) layered-options.partitioning-partition)
    ;; This is the easy mode, the partitions are connected directly.
    (let ((source-partition (get-property (get-node (get-source l-edge)) layered-options.partitioning-partition))
          (target-partition (get-property (get-node (get-target l-edge)) layered-options.partitioning-partition)))
      ;; We need to reverse an edge if sourcePartition > targetPartition
      (return-from must-be-reversed/2 (> source-partition target-partition))))
  ;; You need to check further in the graph to see if the is a connected partitioned node.
  ;; Traverse the graph breadth first and check if there are partitioned nodes.
  ;; While doing so save the path to the partitioned node and reverse all edges leading to it if the target
  ;; partition is lower than the source partition.
  (let* ((source-partition (get-property (get-node (get-source l-edge)) layered-options.partitioning-partition))
         ;; Remove all partitioned nodes with a higher partition ID than the source partition.
         (partitioned-nodes-with-lower-partition
           (let ((result (new-jlist)))
             (jdo (l-node partitioned-nodes)
               (when (< (get-property l-node layered-options.partitioning-partition) source-partition)
                 (jl-add result l-node)))
             result))
         ;; Find the paths between the source node and all partitioned nodes with a lower partition ID.
         (queue (new-jlist))
         (visited (new-jset)))
    (jl-add queue (get-node (get-source l-edge)))
    (js-add visited (get-node (get-source l-edge)))
    (loop until (jl-is-empty queue)
          do (let ((current-node (jl-poll-first queue)))
               (when (jl-contains partitioned-nodes-with-lower-partition current-node)
                 ;; We found a partitioned node with a lower partition ID, so we need to reverse the edge
                 ;; connecting it to the source node.
                 ;; Currently, we only reverse the outgoing edge from a partitioned node that at some points leads
                 ;; to a partitioned node with a lower partition ID and not the full path.
                 (return-from must-be-reversed/2 t))
               ;; Doing bfs to find all partitioned nodes with a lower partition ID if they can be reached.
               (jdo (outgoing-edge (get-outgoing-edges current-node))
                 (let ((target-node (get-node (get-target outgoing-edge))))
                   (unless (js-contains visited target-node)
                     (js-add visited target-node)
                     (jl-add queue target-node))))))
    ;; No node with a lower partition ID was found, so we do not need to reverse any edges.
    nil))

(defjmethod j-reverse ((this partition-preprocessor) l-edge l-graph)
  (j-reverse l-edge l-graph t)
  ;; If the user has set a priority, add that to our base priority to allow them to fix issues where an edge is
  ;; still being reversed
  (let ((priority partition-preprocessor.partition-constraint-edge-priority))
    (when (has-property l-edge layered-options.priority-direction)
      (incf priority (get-property l-edge layered-options.priority-direction)))
    (set-property l-edge layered-options.priority-direction priority)))

;;; ------------------------------------------------ PartitionMidprocessor

(defjmethod process ((this partition-midprocessor) l-graph monitor)
  (begin monitor "Partition midprocessing" 1)
  ;; Collect nodes which have a partition set
  (let ((partition-to-nodes-map (new-jmap)))
    ;; Go through all layerless nodes and collect all partition IDs in use
    (jdo (node (get-layerless-nodes l-graph))
      (when (has-property node layered-options.partitioning-partition)
        (%lib-multimap-put partition-to-nodes-map
                           (get-property node layered-options.partitioning-partition) node)))
    (when (zerop (hash-table-count partition-to-nodes-map))
      ;; This shouldn't happen, but if it does, there's nothing to do
      (return-from process/2 nil))
    ;; Collect a sorted list of partition IDs
    (let ((sorted-partition-ids (sort (loop for id being the hash-keys of partition-to-nodes-map collect id) #'<)))
      ;; For each pair of consecutive partition IDs (a, b), connect all nodes from partition a to partition b
      (loop for (first-id second-id) on sorted-partition-ids
            while second-id
            do (connect-nodes this
                              (%lib-multimap-get partition-to-nodes-map first-id)
                              (%lib-multimap-get partition-to-nodes-map second-id)))))
  (done monitor))

(defjmethod connect-nodes ((this partition-midprocessor) first-partition second-partition)
  (jdo (node first-partition)
    (let ((source-port (new-l-port)))
      (set-node source-port node)
      (set-side source-port port-side.east)
      (set-property source-port layered-internal-properties.partition-dummy t)
      (let ((label-connected nil))
        (jdo (l-edge (get-outgoing-edges node))
          (when (eq (get-type (get-node (get-target l-edge))) node-type.label)
            (setf label-connected t)))
        (jdo (other-node second-partition)
          (let ((target-port (new-l-port)))
            (set-node target-port other-node)
            (set-side target-port port-side.west)
            (set-property target-port layered-internal-properties.partition-dummy t)
            (let ((edge (new-l-edge)))
              (set-property edge layered-internal-properties.partition-dummy t)
              (set-source edge source-port)
              (set-target edge target-port)
              (when label-connected
                ;; Dummy node to compensate for potential edge label and its influence on the layering.
                (let ((dummy-node (new-l-node (get-graph node))))
                  (jl-add (get-layerless-nodes (get-graph node)) dummy-node)
                  (set-type dummy-node node-type.long-edge)
                  (set-property dummy-node layered-internal-properties.partition-dummy t)
                  (let ((dummy-in (new-l-port))
                        (dummy-out (new-l-port)))
                    (set-node dummy-in dummy-node)
                    (set-side dummy-in port-side.west)
                    (set-node dummy-out dummy-node)
                    (set-side dummy-out port-side.east)
                    (set-target edge dummy-in)
                    (let ((edge2 (new-l-edge)))
                      (set-property edge2 layered-internal-properties.partition-dummy t)
                      (set-source edge2 dummy-out)
                      (set-target edge2 target-port))))))))))))
