;;;; high-degree.lisp -- HighDegreeNodeLayeringProcessor.

(in-package #:elk)

;; Private helpers of this file are called before their defjmethod is compiled.
(define-jcalls :instance
  process
  is-high-degree-node
  calculate-information
  prepend-layer
  append-layer
  move-tree
  degree
  has-single-connection
  other
  is-tree-root)

;;; The Java Function<LNode, Iterable<LEdge>> fields INCOMING_EDGES / OUTGOING_EDGES are passed
;;; around as edge selectors; here they are the Lisp functions below, applied with FUNCALL.

(defun %hdnl-incoming-edges (n) (get-incoming-edges n))
(defun %hdnl-outgoing-edges (n) (get-outgoing-edges n))

(defjmethod process ((this high-degree-node-layering-processor) graph progress-monitor)
  (setf (.layered-graph this) graph)
  ;; retrieve some properties
  (setf (.degree-threshold this) (get-property graph layered-options.high-degree-nodes-threshold))
  (setf (.tree-height-threshold this) (get-property graph layered-options.high-degree-nodes-tree-height))
  ;; translate 0 to 'arbitrary height'
  (when (= (.tree-height-threshold this) 0)
    (setf (.tree-height-threshold this) integer.max-value))
  ;; now iterate through all layer.  The Java ListIterator is modelled by an index CURSOR
  ;; (the number of layers before the iterator), which the insertions below advance exactly as
  ;; ListIterator.add does.
  (let* ((layers (get-layers graph))
         (cursor 0))
    (loop while (< cursor (size layers))
          do (let ((lay (jl-get layers cursor)))
               (incf cursor)
               ;; -----------------------------------------------------------------------
               ;;  #1 find high degree nodes and find their incoming and outgoing trees
               ;; -----------------------------------------------------------------------
               (let ((high-degree-nodes '())
                     (inc-max -1)
                     (out-max -1))
                 (jdo (n (get-nodes lay))
                   (when (is-high-degree-node this n)
                     (let ((hdni (calculate-information this n)))
                       (setf inc-max (max inc-max (.inc-trees-max-height hdni)))
                       (setf out-max (max out-max (.out-trees-max-height hdni)))
                       ;; Pair.of(n, hdni) is kept as a cons
                       (push (cons n hdni) high-degree-nodes))))
                 (setf high-degree-nodes (nreverse high-degree-nodes))
                 ;; -----------------------------------------------------------------------
                 ;;  #2 insert layers before the current layer and move the trees
                 ;; -----------------------------------------------------------------------
                 (let ((pre-layers '()))
                   (dotimes (i inc-max)
                     (push (prepend-layer this layers (lambda () cursor) (lambda (v) (setf cursor v)))
                           pre-layers))
                   (dolist (hdn high-degree-nodes)
                     (let ((inc-roots (.inc-tree-roots (cdr hdn))))
                       (when inc-roots
                         (dolist (inc-root inc-roots)
                           (move-tree this inc-root #'%hdnl-incoming-edges pre-layers))))))
                 ;; -----------------------------------------------------------------------
                 ;;  #2 insert layers after the current layer and move the trees
                 ;; -----------------------------------------------------------------------
                 (let ((after-layers '()))
                   (dotimes (i out-max)
                     (setf after-layers
                           (append after-layers
                                   (list (append-layer this layers (lambda () cursor)
                                                       (lambda (v) (setf cursor v)))))))
                   (dolist (hdn high-degree-nodes)
                     (let ((out-roots (.out-tree-roots (cdr hdn))))
                       (when out-roots
                         (dolist (out-root out-roots)
                           (move-tree this out-root #'%hdnl-outgoing-edges after-layers)))))))))
    ;; -----------------------------------------------------------------------
    ;;  #2 it can happen that layers became empty
    ;;     after the previous processing, remove them
    ;; -----------------------------------------------------------------------
    (let ((i 0))
      (loop while (< i (size layers))
            do (if (is-empty (get-nodes (jl-get layers i)))
                   (jl-remove-at layers i)
                   (incf i))))))

(defjmethod is-high-degree-node ((this high-degree-node-layering-processor) node)
  (>= (degree this node) (.degree-threshold this)))

(defjmethod calculate-information ((this high-degree-node-layering-processor) hdn)
  "Returns the calculated trees for the passed node: a list of incoming trees and a list of
outgoing trees and the maximum height of both of them."
  (let ((hdni (new-high-degree-node-information)))
    ;; check for incoming trees
    (jdo (inc-edge (get-incoming-edges hdn))
      (unless (is-self-loop inc-edge)
        (let ((src (get-node (get-source inc-edge))))
          (when (has-single-connection this src #'%hdnl-outgoing-edges)
            (let ((tree-height (is-tree-root this src #'%hdnl-outgoing-edges #'%hdnl-incoming-edges)))
              (unless (= tree-height -1)
                ;; update the maximum tree height
                (setf (.inc-trees-max-height hdni)
                      (max (.inc-trees-max-height hdni) tree-height))
                ;; remember the tree root for later processing
                (unless (.inc-tree-roots hdni)
                  (setf (.inc-tree-roots hdni) (list)))
                (setf (.inc-tree-roots hdni) (append (.inc-tree-roots hdni) (list src)))))))))
    ;; outgoing trees
    (jdo (out-edge (get-outgoing-edges hdn))
      (unless (is-self-loop out-edge)
        (let ((tgt (get-node (get-target out-edge))))
          (when (has-single-connection this tgt #'%hdnl-incoming-edges)
            (let ((tree-height (is-tree-root this tgt #'%hdnl-incoming-edges #'%hdnl-outgoing-edges)))
              (unless (= tree-height -1)
                (setf (.out-trees-max-height hdni)
                      (max (.out-trees-max-height hdni) tree-height))
                (unless (.out-tree-roots hdni)
                  (setf (.out-tree-roots hdni) (list)))
                (setf (.out-tree-roots hdni) (append (.out-tree-roots hdni) (list tgt)))))))))
    hdni))

(defjmethod prepend-layer ((this high-degree-node-layering-processor) layers get-cursor set-cursor)
  "The newly added layer.  The iterator itself is left where it was before (after the layer it
had returned), i.e. the cursor moves one place to the right."
  (let ((l (new-layer (.layered-graph this))))
    (jl-add-at layers (1- (funcall get-cursor)) l)
    (funcall set-cursor (1+ (funcall get-cursor)))
    l))

(defjmethod append-layer ((this high-degree-node-layering-processor) layers get-cursor set-cursor)
  "The newly added layer.  The iterator is left after it."
  (let ((l (new-layer (.layered-graph this))))
    (jl-add-at layers (funcall get-cursor) l)
    (funcall set-cursor (1+ (funcall get-cursor)))
    l))

(defjmethod move-tree ((this high-degree-node-layering-processor) root edges-fun layers)
  "Moves the tree defined by ROOT and EDGES-FUN to the LAYERS.  The layers should be ordered
according to EDGES-FUN: if the tree expands to the left and the tree's root should be inserted
into l_i, the list of layers should be [l_i, l_i-1, l_i-2, ...]."
  (set-layer root (car layers))
  (let ((sub-list (cdr layers)))
    (jdo (e (funcall edges-fun root))
      (let ((other (other this e root)))
        (move-tree this other edges-fun sub-list)))))

(defjmethod degree ((this high-degree-node-layering-processor) node)
  (size (get-connected-edges node)))

(defjmethod has-single-connection ((this high-degree-node-layering-processor) node edge-selector)
  "Whether the edges connect NODE to a single other node: other than checking for degree(node) ==
1, this allows for multiple edges between the two involved nodes."
  (block single-connection
    (let ((connection nil))
      (jdo (e (funcall edge-selector node))
        (if (null connection)
            (setf connection (other this e node))
            (unless (eq (other this e node) connection)
              (return-from single-connection nil))))
      t)))

(defjmethod other ((this high-degree-node-layering-processor) edge node)
  "For an edge (u, v) returns u if node == v and v if node == u."
  (if (eq (get-node (get-source edge)) node)
      (get-node (get-target edge))
      (get-node (get-source edge))))

(defjmethod is-tree-root ((this high-degree-node-layering-processor) root ancestor-edges descendant-edges)
  "Checks whether ROOT is the root node of a subtree with height at most TREE-HEIGHT-THRESHOLD.
This is the case if: (1) root is connected to at most one node, i.e. it can be detached from a
larger graph by removing exactly one edge, (2) root is either a leaf, or all of the connected
nodes are subtrees again.  Returns either the height of the subtree of which ROOT is the root, or
-1 if it is not part of a tree."
  (block tree-root
  ;; exclude the high degree nodes themselves from the 'tree moving' process,
  ;;  i.e. leave them where they are
  (when (is-high-degree-node this root)
    (return-from tree-root -1))
  ;; is it a proper (sub-)root, i.e. does the node have exactly one parent?
  (unless (has-single-connection this root ancestor-edges)
    (return-from tree-root -1))
  ;; is it a leaf?
  (when (is-empty (funcall descendant-edges root))
    (return-from tree-root 1))
  ;; recursively check subtrees
  (let ((current-height 0))
    (jdo (e (funcall descendant-edges root))
      (let* ((other (other this e root))
             (height (is-tree-root this other ancestor-edges descendant-edges)))
        ;; other is not the root of a subtree
        (when (= height -1)
          (return-from tree-root -1))
        (setf current-height (max current-height height))
        ;; if we hit the threshold ... stop
        (when (> current-height (1- (.tree-height-threshold this)))
          (return-from tree-root -1))))
    (1+ current-height))))
