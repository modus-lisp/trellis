;;;; multimap-comments.lisp -- BiLinkedHashMultiMap, CommentNodeMarginCalculator,
;;;; CommentPostprocessor, CommentPreprocessor, ConstraintsPostprocessor.

(in-package #:elk)

;;; ------------------------------------------------------------ BiLinkedHashMultiMap
;;; LinkedHashMaps are CL hash tables (SBCL iterates in insertion order as long
;;; as nothing is removed; keys are never removed here).  LinkedList values are
;;; jlists.

(defjmethod put-all ((this bi-linked-hash-multi-map) key values)
  (jdo (value values)
    (put this key value)))

(defjmethod put ((this bi-linked-hash-multi-map) key value)
  ;; Remove old value.
  (let ((old-key (jm-get (.hash-map-values-to-key this) value)))
    (when old-key
      (let ((values (jm-get (.multi-map-key-to-values this) old-key)))
        (jl-remove values value))))
  ;; Add new value
  (let ((values (jm-get (.multi-map-key-to-values this) key)))
    (when (null values)
      (setf values (new-jlist))
      (jm-put (.multi-map-key-to-values this) key values))
    (jl-add values value)
    (jm-put (.hash-map-values-to-key this) value key)))

(defjmethod get-key ((this bi-linked-hash-multi-map) value)
  (jm-get (.hash-map-values-to-key this) value))

(defjmethod get-values ((this bi-linked-hash-multi-map) key)
  (or (jm-get (.multi-map-key-to-values this) key)
      (new-jlist)))

(defjmethod key-set ((this bi-linked-hash-multi-map))
  (jset-from (jm-keys (.multi-map-key-to-values this))))

(defjmethod is-maximal-key ((this bi-linked-hash-multi-map) key)
  (jdo (other-key (jm-keys (.multi-map-key-to-values this)) t)
    (when (< (jcompare key other-key) 0)
      (return-from is-maximal-key/1 nil))))

(defjmethod is-minimal-key ((this bi-linked-hash-multi-map) key)
  (jdo (other-key (jm-keys (.multi-map-key-to-values this)) t)
    (when (> (jcompare key other-key) 0)
      (return-from is-minimal-key/1 nil))))

;;; ------------------------------------------------------------ CommentNodeMarginCalculator

(defjmethod process ((this comment-node-margin-calculator) layered-graph monitor)
  (begin monitor "Node margin calculation" 1)
  ;; Iterate through the layers to additionally handle comments
  (jdo (layer (get-layers layered-graph))
    (jdo (lnode (get-nodes layer))
      (process-comments this lnode)))
  (done monitor))

(defjmethod process-comments ((this comment-node-margin-calculator) node)
  (let ((margin (get-margin node))
        (top-boxes (get-property node layered-internal-properties.top-comments))
        (bottom-boxes (get-property node layered-internal-properties.bottom-comments)))
    (when (and (null top-boxes) (null bottom-boxes))
      ;; Shortcut if there are no attached comments
      (return-from process-comments/1 nil))
    ;; Retrieve the spacings that apply to this node
    (let ((comment-comment-spacing
            (spacings.get-individual-or-default node layered-options.spacing-comment-comment))
          (comment-node-spacing
            (spacings.get-individual-or-default node layered-options.spacing-comment-node))
          (top-width 0.0)
          (bottom-width 0.0))
      ;; Consider comment boxes that are put on top of the node
      (when top-boxes
        (let ((max-height 0.0))
          (jdo (comment-box top-boxes)
            (setf max-height (math.max max-height (.y (get-size comment-box))))
            (incf top-width (.x (get-size comment-box))))
          (incf top-width (* comment-comment-spacing (- (size top-boxes) 1)))
          (incf (.top margin) (+ max-height comment-node-spacing))))
      ;; Consider comment boxes that are put in the bottom of the node
      (when bottom-boxes
        (let ((max-height 0.0))
          (jdo (comment-box bottom-boxes)
            (setf max-height (math.max max-height (.y (get-size comment-box))))
            (incf bottom-width (.x (get-size comment-box))))
          (incf bottom-width (* comment-comment-spacing (- (size bottom-boxes) 1)))
          (incf (.bottom margin) (+ max-height comment-node-spacing))))
      ;; Check if the maximum width of the comments is wider than the node itself, which the
      ;; comments are centered on
      (let ((max-comment-width (math.max top-width bottom-width)))
        (when (> max-comment-width (.x (get-size node)))
          (let ((protrusion (/ (- max-comment-width (.x (get-size node))) 2)))
            (setf (.left margin) (math.max (.left margin) protrusion))
            (setf (.right margin) (math.max (.right margin) protrusion))))))))

;;; ------------------------------------------------------------ CommentPostprocessor

(defjmethod process ((this comment-postprocessor) layered-graph monitor)
  (begin monitor "Comment post-processing" 1)
  (jdo (layer (get-layers layered-graph))
    (let ((boxes (new-jlist)))
      (jdo (node (get-nodes layer))
        (let ((top-boxes (get-property node layered-internal-properties.top-comments))
              (bottom-boxes (get-property node layered-internal-properties.bottom-comments)))
          (when (or top-boxes bottom-boxes)
            (%cpp-process this node top-boxes bottom-boxes)
            (when top-boxes (jl-add-all boxes top-boxes))
            (when bottom-boxes (jl-add-all boxes bottom-boxes)))))
      (add-all (get-nodes layer) boxes)))
  (done monitor))

(defun %cpp-process (this node top-boxes bottom-boxes)
  "CommentPostprocessor.process(LNode, List, List) (private overload)."
  (let* ((node-pos (get-position node))
         (node-size (get-size node))
         (margin (get-margin node))
         (comment-comment-spacing
           (spacings.get-individual-or-default node layered-options.spacing-comment-comment)))
    (when top-boxes
      ;; determine the total width and maximal height of the top boxes
      (let ((boxes-width (* comment-comment-spacing (- (size top-boxes) 1)))
            (max-height 0.0))
        (jdo (box top-boxes)
          (incf boxes-width (.x (get-size box)))
          (setf max-height (math.max max-height (.y (get-size box)))))
        ;; place the boxes on top of the node, horizontally centered around the node itself
        (let* ((x (- (.x node-pos) (/ (- boxes-width (.x node-size)) 2)))
               (base-line (+ (- (.y node-pos) (.top margin)) max-height))
               (anchor-inc (/ (.x node-size) (+ (size top-boxes) 1)))
               (anchor-x anchor-inc))
          (jdo (box top-boxes)
            (setf (.x (get-position box)) x)
            (setf (.y (get-position box)) (- base-line (.y (get-size box))))
            (incf x (+ (.x (get-size box)) comment-comment-spacing))
            ;; set source and target point for the connecting edge
            (let ((box-port (get-box-port this box)))
              (setf (.x (get-position box-port)) (- (/ (.x (get-size box)) 2) (.x (get-anchor box-port))))
              (setf (.y (get-position box-port)) (.y (get-size box))))
            (let ((node-port (get-property box layered-internal-properties.comment-conn-port)))
              (when (= (get-degree node-port) 1)
                (setf (.x (get-position node-port)) (- anchor-x (.x (get-anchor node-port))))
                (setf (.y (get-position node-port)) 0.0)
                (set-node node-port node)))
            (incf anchor-x anchor-inc)))))
    (when bottom-boxes
      ;; determine the total width and maximal height of the bottom boxes
      (let ((boxes-width (* comment-comment-spacing (- (size bottom-boxes) 1)))
            (max-height 0.0))
        (jdo (box bottom-boxes)
          (incf boxes-width (.x (get-size box)))
          (setf max-height (math.max max-height (.y (get-size box)))))
        ;; place the boxes in the bottom of the node, horizontally centered around the node itself
        (let* ((x (- (.x node-pos) (/ (- boxes-width (.x node-size)) 2)))
               (base-line (- (+ (.y node-pos) (.y node-size) (.bottom margin)) max-height))
               (anchor-inc (/ (.x node-size) (+ (size bottom-boxes) 1)))
               (anchor-x anchor-inc))
          (jdo (box bottom-boxes)
            (setf (.x (get-position box)) x)
            (setf (.y (get-position box)) base-line)
            (incf x (+ (.x (get-size box)) comment-comment-spacing))
            ;; set source and target point for the connecting edge
            (let ((box-port (get-box-port this box)))
              (setf (.x (get-position box-port)) (- (/ (.x (get-size box)) 2) (.x (get-anchor box-port))))
              (setf (.y (get-position box-port)) 0.0))
            (let ((node-port (get-property box layered-internal-properties.comment-conn-port)))
              (when (= (get-degree node-port) 1)
                (setf (.x (get-position node-port)) (- anchor-x (.x (get-anchor node-port))))
                (setf (.y (get-position node-port)) (.y node-size))
                (set-node node-port node)))
            (incf anchor-x anchor-inc)))))))

(defjmethod get-box-port ((this comment-postprocessor) comment-box)
  (let ((node-port (get-property comment-box layered-internal-properties.comment-conn-port)))
    (jdo (port (get-ports comment-box))
      (jdo (edge (get-outgoing-edges port))
        ;; reconnect the edge (has been disconnected by pre-processor)
        (set-target edge node-port)
        (return-from get-box-port/1 port))
      (jdo (edge (get-incoming-edges port))
        ;; reconnect the edge (has been disconnected by pre-processor)
        (set-source edge node-port)
        (return-from get-box-port/1 port)))
    nil))

;;; ------------------------------------------------------------ CommentPreprocessor

(defjmethod process ((this comment-preprocessor) layered-graph monitor)
  (begin monitor "Comment pre-processing" 1)
  (let ((comment-box-count 0)
        (nodes (get-layerless-nodes layered-graph))
        (i 0))
    ;; iterator with remove() -> index loop
    (loop while (< i (length nodes))
          do (let ((node (aref nodes i))
                   (removed nil))
               (when (get-property node layered-options.comment-box)
                 (incf comment-box-count)
                 (let ((edge-count 0) (edge nil) (opposite-port nil))
                   (jdo (port (get-ports node))
                     (incf edge-count (get-degree port))
                     (when (= (length (get-incoming-edges port)) 1)
                       (setf edge (jl-get (get-incoming-edges port) 0))
                       (setf opposite-port (get-source edge)))
                     (when (= (length (get-outgoing-edges port)) 1)
                       (setf edge (jl-get (get-outgoing-edges port) 0))
                       (setf opposite-port (get-target edge))))
                   (if (and (= edge-count 1)
                            (= (get-degree opposite-port) 1)
                            (not (get-property (get-node opposite-port) layered-options.comment-box)))
                       ;; found a comment that has exactly one connection
                       (progn
                         (process-box this node edge opposite-port (get-node opposite-port))
                         (jl-remove-at nodes i)
                         (setf removed t))
                       ;; reverse edges that are oddly connected
                       (let ((rev-edges (new-jlist)))
                         (jdo (port (get-ports node))
                           (jdo (outedge (get-outgoing-edges port))
                             (unless (jl-is-empty (get-outgoing-edges (get-target outedge)))
                               (jl-add rev-edges outedge)))
                           (jdo (inedge (get-incoming-edges port))
                             (unless (jl-is-empty (get-incoming-edges (get-source inedge)))
                               (jl-add rev-edges inedge))))
                         (jdo (re rev-edges)
                           (j-reverse re layered-graph t))))))
               (unless removed (incf i))))
    ;; logging is a no-op
    comment-box-count)
  (done monitor))

(defjmethod process-box ((this comment-preprocessor) box edge opposite-port real-node)
  (let ((top-first nil) (only-top nil) (only-bottom nil) (box-list nil))
    (when (is-side-fixed (get-property real-node layered-options.port-constraints))
      (let ((has-north nil) (has-south nil))
        (block port-loop
          (jdo (port1 (get-ports real-node))
            (jdo (port2 (get-connected-ports port1))
              (unless (get-property (get-node port2) layered-options.comment-box)
                (when (eq (get-side port1) port-side.north)
                  (setf has-north t)
                  (return-from port-loop))
                (when (eq (get-side port1) port-side.south)
                  (setf has-south t)
                  (return-from port-loop))))))
        (setf only-top (and has-south (not has-north)))
        (setf only-bottom (and has-north (not has-south)))))
    (if (and (not only-top) (not only-bottom) (not (jl-is-empty (get-labels real-node))))
        (let ((label-pos 0.0))
          (jdo (label (get-labels real-node))
            (incf label-pos (+ (.y (get-position label)) (/ (.y (get-size label)) 2))))
          (setf label-pos (/ label-pos (length (get-labels real-node))))
          (setf top-first (>= label-pos (/ (.y (get-size real-node)) 2))))
        (setf top-first (not only-bottom)))
    (if top-first
        ;; determine the position to use, favoring the top position
        (let ((top-boxes (get-property real-node layered-internal-properties.top-comments)))
          (cond ((null top-boxes)
                 (setf box-list (new-jlist))
                 (set-property real-node layered-internal-properties.top-comments box-list))
                (only-top (setf box-list top-boxes))
                (t
                 (let ((bottom-boxes (get-property real-node layered-internal-properties.bottom-comments)))
                   (if (null bottom-boxes)
                       (progn
                         (setf box-list (new-jlist))
                         (set-property real-node layered-internal-properties.bottom-comments box-list))
                       (setf box-list (if (<= (size top-boxes) (size bottom-boxes))
                                          top-boxes
                                          bottom-boxes)))))))
        ;; determine the position to use, favoring the bottom position
        (let ((bottom-boxes (get-property real-node layered-internal-properties.bottom-comments)))
          (cond ((null bottom-boxes)
                 (setf box-list (new-jlist))
                 (set-property real-node layered-internal-properties.bottom-comments box-list))
                (only-bottom (setf box-list bottom-boxes))
                (t
                 (let ((top-boxes (get-property real-node layered-internal-properties.top-comments)))
                   (if (null top-boxes)
                       (progn
                         (setf box-list (new-jlist))
                         (set-property real-node layered-internal-properties.top-comments box-list))
                       (setf box-list (if (<= (size bottom-boxes) (size top-boxes))
                                          bottom-boxes
                                          top-boxes))))))))
    ;; add the comment box to one of the two possible lists
    (add box-list box)
    ;; set the opposite port as property for the comment box
    (set-property box layered-internal-properties.comment-conn-port opposite-port)
    ;; detach the edge and the opposite port
    (if (eq (get-target edge) opposite-port)
        (progn
          (set-target edge nil)
          (when (= (get-degree opposite-port) 0)
            (set-node opposite-port nil))
          (remove-hierarchical-port-dummy-node this opposite-port))
        (progn
          (set-source edge nil)
          (when (= (get-degree opposite-port) 0)
            (set-node opposite-port nil))))
    (clear (get-bend-points edge))))

(defjmethod remove-hierarchical-port-dummy-node ((this comment-preprocessor) opposite-port)
  (let ((dummy (get-property opposite-port layered-internal-properties.port-dummy)))
    (when dummy
      (let ((layer (get-layer dummy)))
        (jl-remove (get-nodes layer) dummy)
        (when (jl-is-empty (get-nodes layer))
          (jl-remove (get-layers (get-graph dummy)) layer))))))

;;; ------------------------------------------------------------ ConstraintsPostprocessor

(defjmethod process ((this constraints-postprocessor) graph progress-monitor)
  (begin progress-monitor "Constraints Postprocessor" 1)
  (let ((layer-index 0))
    (jdo (layer (get-layers graph))
      (let ((pos-index 0) (node-layer nil))
        (jdo (current-node (get-nodes layer))
          (when (eq (get-type current-node) node-type.normal)
            (setf node-layer t)
            (set-property current-node layered-options.layering-layer-id layer-index)
            (set-property current-node layered-options.crossing-minimization-position-id pos-index)
            (incf pos-index)))
        ;; layers with no nodes in it should not increase the layer id
        (when node-layer
          (incf layer-index)))))
  (done progress-monitor))
