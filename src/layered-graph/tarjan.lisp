;;;; tarjan.lisp -- org.eclipse.elk.alg.layered.graph.Tarjan

(in-package #:elk)

(define-jcalls :instance tarjan strongly-connected reset-tarjan)

(defjctor tarjan (edges-to-be-reversed strongly-connected-components node-to-sccid)
  (setf (.edges-to-be-reversed this) edges-to-be-reversed
        (.strongly-connected-components this) strongly-connected-components
        (.node-to-sccid this) node-to-sccid))

(defjmethod tarjan ((this tarjan) graph)
  (setf (.index this) 0)
  (setf (.stack this) (new-jlist))
  (jdo (node (get-layerless-nodes graph))
    (when (= (get-property node layered-internal-properties.tarjan-id) -1)
      (strongly-connected this node)
      (jl-clear (.stack this))))
  nil)

(defjmethod strongly-connected ((this tarjan) v)
  (set-property v layered-internal-properties.tarjan-id (.index this))
  (set-property v layered-internal-properties.tarjan-lowlink (.index this))
  (incf (.index this))
  (jl-add (.stack this) v)                 ; Stack.push
  (set-property v layered-internal-properties.tarjan-on-stack t)
  (let ((reversed (.edges-to-be-reversed this)))
    (jdo (edge (get-connected-edges v))
      (unless (or (and (not (eq (get-node (get-source edge)) v)) (not (contains reversed edge)))
                  (and (eq (get-node (get-source edge)) v) (contains reversed edge)))
        (let ((target (if (eq (get-node (get-target edge)) v)
                          (get-node (get-source edge))
                          (get-node (get-target edge)))))
          (cond ((= (get-property target layered-internal-properties.tarjan-id) -1)
                 (strongly-connected this target)
                 (set-property v layered-internal-properties.tarjan-lowlink
                               (min (get-property v layered-internal-properties.tarjan-lowlink)
                                    (get-property target layered-internal-properties.tarjan-lowlink))))
                ((get-property target layered-internal-properties.tarjan-on-stack)
                 (set-property v layered-internal-properties.tarjan-lowlink
                               (min (get-property v layered-internal-properties.tarjan-lowlink)
                                    (get-property target layered-internal-properties.tarjan-id)))))))))
  (when (= (get-property v layered-internal-properties.tarjan-lowlink)
           (get-property v layered-internal-properties.tarjan-id))
    (let ((s-cc (new-jset))
          (n nil))
      (loop
        (setf n (vector-pop (.stack this)))  ; Stack.pop
        (set-property n layered-internal-properties.tarjan-on-stack nil)
        (js-add s-cc n)
        (when (eq v n) (return)))
      (when (> (jsize s-cc) 1)
        (let ((index (jsize (.strongly-connected-components this))))
          (add (.strongly-connected-components this) s-cc)
          (jdo (node s-cc)
            (jm-put (.node-to-sccid this) node index))))))
  nil)

(defjmethod reset-tarjan ((this tarjan) graph)
  (jdo (n (get-layerless-nodes graph))
    (set-property n layered-internal-properties.tarjan-on-stack nil)
    (set-property n layered-internal-properties.tarjan-lowlink -1)
    (set-property n layered-internal-properties.tarjan-id -1)
    (jl-clear (.stack this))
    (jdo (e (get-connected-edges n))
      (set-property e layered-internal-properties.is-part-of-cycle nil)))
  nil)
