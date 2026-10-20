;;;; self-loop-pre.lisp -- SelfLoopPreProcessor.

(in-package #:elk)

;;; ------------------------------------------------ SelfLoopPreProcessor

(defjmethod process ((this self-loop-pre-processor) graph progress-monitor)
  (begin progress-monitor "Self-Loop pre-processing" 1)
  (jdo (lnode (get-layerless-nodes graph))
    (when (self-loop-holder.needs-self-loop-processing lnode)
      (let ((sl-holder (self-loop-holder.install lnode)))
        (hide-self-loops this sl-holder)
        (hide-ports this sl-holder))))
  (done progress-monitor))

(defjmethod hide-self-loops ((this self-loop-pre-processor) sl-holder)
  (jdo (sl-loop (get-sl-hyper-loops sl-holder))
    (jdo (sl-edge (get-sl-edges sl-loop))
      (hide-self-loop this (get-l-edge sl-edge)))))

(defjmethod hide-self-loop ((this self-loop-pre-processor) l-edge)
  (set-source l-edge nil)
  (set-target l-edge nil))

(defjmethod hide-ports ((this self-loop-pre-processor) sl-holder)
  (let* ((l-node (get-l-node sl-holder))
         (nested-graph (get-nested-graph l-node))
         (order-fixed (is-order-fixed (get-property l-node layered-options.port-constraints)))
         (hierarchy-mode (and nested-graph
                              (contains (get-property nested-graph layered-internal-properties.graph-properties)
                                        graph-properties.external-ports))))
    (when (or order-fixed hierarchy-mode)
      ;; No need to hide any ports
      (return-from hide-ports/1 nil))
    (jdo (sl-port (jm-values (get-sl-port-map sl-holder)))
      (when (had-only-self-loops sl-port)
        ;; Hide the port
        (let ((l-port (get-l-port sl-port)))
          (set-node l-port nil)
          ;; Remember that we actually did so
          (set-hidden sl-port t)
          (set-ports-hidden sl-holder t))))))
