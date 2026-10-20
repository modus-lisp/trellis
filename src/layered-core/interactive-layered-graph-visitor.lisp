;;;; interactive-layered-graph-visitor.lisp --
;;;; org.eclipse.elk.alg.layered.InteractiveLayeredGraphVisitor
;;;;
;;;; Sets pseudo positions on an ElkGraph from the layer/position ids of a
;;;; previous run plus layer/position/relative constraints, and switches the
;;;; layout to the interactive strategies.  Int arithmetic that can overflow in
;;;; Java (comparators that subtract, 2*layerId*MAX_VALUE) wraps as in Java.

(in-package #:elk)

(defjconst interactive-layered-graph-visitor.pseudo-position-spacing integer.max-value)
(defjconst interactive-layered-graph-visitor.last-layer-index integer.max-value)

(defmacro %ilgv-lcc () 'layered-options.layering-layer-choice-constraint)
(defmacro %ilgv-pred () 'layered-options.crossing-minimization-in-layer-pred-of)
(defmacro %ilgv-succ () 'layered-options.crossing-minimization-in-layer-succ-of)
(defmacro %ilgv-pcc () 'layered-options.crossing-minimization-position-choice-constraint)
(defmacro %ilgv-layer-id () 'layered-options.layering-layer-id)

(defun %ilgv-int-cmp (key)
  "Java's (a, b) -> key(a) - key(b) on ints."
  (lambda (a b) (to-int (- (funcall key a) (funcall key b)))))

(defjmethod visit ((this interactive-layered-graph-visitor) element)
  ;; only apply to the root of the graph
  (when (typep element 'elk-node)
    (%ilgv-set-interactive-options-and-pseudo-positions this element)))

(defun %ilgv-set-interactive-options-and-pseudo-positions (this root)
  (unless (is-empty (get-children root))
    (let ((algorithm (get-property root core-options.algorithm)))
      (when (or (null algorithm)
                (let ((id layered-options.algorithm-id))
                  (and (<= (length algorithm) (length id))
                       (string= algorithm id :start2 (- (length id) (length algorithm))))))
        ;; translate LayerConstraints into LayerChoiceConstraints
        (jdo (node (get-children root))
          (when (has-property node layered-options.layering-layer-constraint)
            (let ((constraint (get-property node layered-options.layering-layer-constraint)))
              (set-property node layered-options.layering-layer-constraint layer-constraint.none)
              (enum-case constraint
                (layer-constraint.first
                 (unless (has-property node (%ilgv-lcc))
                   (set-property node (%ilgv-lcc) 0)))
                (layer-constraint.last
                 (unless (has-property node (%ilgv-lcc))
                   (set-property node (%ilgv-lcc) interactive-layered-graph-visitor.last-layer-index)))
                (t nil))))
          ;; (sic: inside the loop, as in Java)
          (%ilgv-set-coordinates this root)
          (%ilgv-set-interactive-strategies root))))))

(defun %ilgv-set-coordinates (this root)
  (let* ((layers (%ilgv-calc-layer-nodes this (get-children root)))
         (direction (get-property root layered-options.direction))
         (layer-id 0))
    (%ilgv-set-coordinate-in-layout-direction layers direction)
    (jdo (layer layers)
      (when (> (jsize layer) 0)
        (%ilgv-set-coordinates-orthogonal-to-layout-direction layer layer-id direction)
        (incf layer-id)))))

(defun %ilgv-calc-layer-nodes (this nodes)
  (declare (ignore this))
  (let ((nodes-without-c (new-jlist))
        (nodes-with-layer-constraint (new-jlist))
        (nodes-with-rc (new-jlist))
        (nodes-with-rc-and-lc (new-jlist)))
    (jdo (node nodes)
      (cond ((and (has-property node (%ilgv-lcc))
                  (or (has-property node (%ilgv-pred)) (has-property node (%ilgv-succ))))
             (jl-add nodes-with-rc-and-lc node))
            ((has-property node (%ilgv-lcc))
             (jl-add nodes-with-layer-constraint node))
            ((or (has-property node (%ilgv-pred)) (has-property node (%ilgv-succ)))
             (jl-add nodes-with-rc node))
            (t (jl-add nodes-without-c node))))
    (%ilgv-update-lists-for-layer-assignment nodes-with-rc-and-lc nodes nodes-with-layer-constraint
                                             nodes-with-rc nodes-without-c)
    (setf nodes-with-rc (%ilgv-sort-rc-nodes nodes-with-rc nodes nodes-without-c))
    (let* ((layer-nodes (%ilgv-initial-layers nodes-without-c))
           (diff (%ilgv-assign-layers-to-nodes-with-property nodes-with-layer-constraint layer-nodes)))
      (%ilgv-assign-layers-to-nodes-with-rc nodes-with-rc layer-nodes nodes diff)
      layer-nodes)))

(defun %ilgv-update-lists-for-layer-assignment (nodes-with-rc-and-lc nodes nodes-with-layer-constraint
                                                nodes-with-rc nodes-without-c)
  (loop for i from 0
        while (< i (jsize nodes-with-rc-and-lc)) do
    (let ((node (jl-get nodes-with-rc-and-lc i))
          (targets (new-jlist)))
      (jl-add nodes-with-layer-constraint node)
      (when (has-property node (%ilgv-pred))
        (jl-add targets (%ilgv-get-elk-node nodes (get-property node (%ilgv-pred)))))
      (when (has-property node (%ilgv-succ))
        (jl-add targets (%ilgv-get-elk-node nodes (get-property node (%ilgv-succ)))))
      (jdo (target targets)
        (unless (has-property target (%ilgv-lcc))
          (set-property target (%ilgv-lcc) (get-property node (%ilgv-lcc)))
          (if (not (jl-contains nodes-with-rc target))
              (progn (jl-add nodes-with-layer-constraint target)
                     (jl-remove nodes-without-c target))
              (progn (jl-add nodes-with-rc-and-lc target)
                     (jl-remove nodes-with-rc target))))))))

(defun %ilgv-layer-value (node)
  (if (has-property node (%ilgv-lcc))
      (get-property node (%ilgv-lcc))
      (get-property node (%ilgv-layer-id))))

(defun %ilgv-assign-layers-to-nodes-with-rc (nodes-with-rc layering all-nodes diff)
  (loop for i from 0
        while (< i (jsize nodes-with-rc)) do
    (let* ((node (jl-get nodes-with-rc i))
           (succ-name (get-property node (%ilgv-pred)))
           (pred-name (get-property node (%ilgv-succ)))
           (succ-node (and succ-name (%ilgv-get-elk-node all-nodes succ-name)))
           (pred-node (and pred-name (%ilgv-get-elk-node all-nodes pred-name))))
      (if (and succ-node pred-node)
          ;; both relative constraints set: the layer ids of the chain may need updating
          (let* ((chain (%ilgv-get-chain-by-all-nodes node all-nodes))
                 (succ-val (%ilgv-layer-value succ-node))
                 (pred-val (%ilgv-layer-value pred-node))
                 (val (if (> succ-val pred-val) succ-val pred-val)))
            (jdo (n chain)
              (let ((old-val (get-property n (%ilgv-layer-id))))
                (unless (= old-val val)
                  (set-property n (%ilgv-layer-id) val)
                  (%ilgv-shift-other-nodes n val layering t)
                  (%ilgv-shift-other-nodes n val layering nil))
                (jl-remove (jl-get layering old-val) n)
                (jl-add (jl-get layering val) n))))
          (let* ((succ-val (if succ-node (%ilgv-layer-value succ-node) -1))
                 (pred-val (if pred-node (%ilgv-layer-value pred-node) -1))
                 (val (if (> succ-val pred-val) succ-val pred-val)))
            (set-property node (%ilgv-layer-id) (- val diff))
            (%ilgv-shift-other-nodes node (- val diff) layering t)
            (%ilgv-shift-other-nodes node (- val diff) layering nil)
            (jl-add (jl-get layering (- val diff)) node))))))

(defun %ilgv-assign-layers-to-nodes-with-property (nodes-with-layer-constraint layering)
  (jl-sort nodes-with-layer-constraint
           (%ilgv-int-cmp (lambda (n) (get-property n (%ilgv-lcc)))))
  (let ((diff 0))
    (jdo (node nodes-with-layer-constraint)
      (let ((current-layer (- (get-property node (%ilgv-lcc)) diff)))
        (cond ((and (< current-layer (jsize layering)) (>= current-layer 0))
               (let ((nodes-of-layer (jl-get layering current-layer)))
                 ;; shift nodes to remove in-layer edges
                 (%ilgv-shift-other-nodes node current-layer layering t)
                 (%ilgv-shift-other-nodes node current-layer layering nil)
                 (jl-add nodes-of-layer node)))
              ((= current-layer -1)
               (jl-add-at layering 0 (jlist node)))
              (t
               (setf diff (+ diff (- current-layer (jsize layering))))
               (jl-add layering (jlist node))))))
    diff))

(defun %ilgv-connected-to-p (shape moved-node)
  (or (eq shape moved-node)
      (and (typep shape 'elk-port) (eq (e-container shape) moved-node))))

(defun %ilgv-node-of-shape (shape)
  (cond ((typep shape 'elk-port) (e-container shape))
        ((typep shape 'elk-node) shape)
        (t nil)))

(defun %ilgv-shift-other-nodes (moved-node layer layer-nodes incoming)
  (let ((nodes-of-layer (jl-get layer-nodes layer))
        (edges (new-jlist))
        (root (get-parent moved-node)))
    (jdo (edge (get-contained-edges root))
      (jdo (target (if incoming (get-targets edge) (get-sources edge)))
        (when (%ilgv-connected-to-p target moved-node)
          (jl-add edges edge))))
    (jdo (edge edges)
      ;; get the connected node
      (let ((node (%ilgv-node-of-shape
                   (if incoming (j-get (get-sources edge) 0) (j-get (get-targets edge) 0)))))
        ;; shift node to the next layer
        (when (jl-contains nodes-of-layer node)
          (jl-remove nodes-of-layer node)
          (set-property node (%ilgv-layer-id) (+ layer 1))
          (if (< (+ layer 1) (jsize layer-nodes))
              (progn
                (jl-add (jl-get layer-nodes (+ layer 1)) node)
                ;; the connected nodes in the target layer must be shifted too
                (%ilgv-shift-other-nodes node (+ layer 1) layer-nodes nil)
                (%ilgv-shift-other-nodes node (+ layer 1) layer-nodes t))
              (jl-add layer-nodes (jlist node))))))))

(defun %ilgv-initial-layers (nodes)
  (jl-sort nodes (%ilgv-int-cmp (lambda (n) (get-property n (%ilgv-layer-id)))))
  (let ((layer-nodes (new-jlist))
        (nodes-of-layer (new-jlist))
        (layer-id 0)
        (current-layer -1))
    (jdo (node nodes)
      (let ((layer (get-property node (%ilgv-layer-id))))
        (when (> layer current-layer)
          (unless (jl-is-empty nodes-of-layer)
            (jl-add layer-nodes nodes-of-layer)
            (incf layer-id))
          (setf nodes-of-layer (new-jlist)
                current-layer layer))
        ;; nodes with layer constraint are added later
        (unless (has-property node (%ilgv-lcc))
          (set-property node (%ilgv-layer-id) layer-id)
          (jl-add nodes-of-layer node))))
    (unless (jl-is-empty nodes-of-layer)
      (jl-add layer-nodes nodes-of-layer))
    layer-nodes))

(defun %ilgv-set-coordinate-in-layout-direction (layers direction)
  (let ((position 0.0)
        (next-position 0.0)
        (spacing interactive-layered-graph-visitor.pseudo-position-spacing))
    (jdo (nodes-of-layer layers)
      (jdo (node nodes-of-layer)
        (enum-case direction
          ((core-direction.undefined core-direction.right)
           (set-x node position)
           (when (>= (+ position (/ (get-width node) 2)) next-position)
             (setf next-position (+ (get-x node) (get-width node) spacing))))
          (core-direction.left
           (set-x node position)
           (when (<= (get-x node) next-position)
             (setf next-position (- (get-x node) spacing))))
          (core-direction.down
           (set-y node position)
           (when (>= (+ position (get-height node)) next-position)
             (setf next-position (+ (get-y node) (get-height node) spacing))))
          (core-direction.up
           (set-y node position)
           (when (<= (get-y node) next-position)
             (setf next-position (- (get-y node) spacing))))
          (t nil)))
      (setf position next-position))))

(defun %ilgv-set-coordinates-orthogonal-to-layout-direction (nodes-of-layer layer-id direction)
  (let ((all-nodes (new-jlist))
        (nodes-without-c (new-jlist))
        (nodes-with-pc-and-rc (new-jlist))
        (nodes-with-pc (new-jlist))
        (nodes-with-one-rc (new-jlist))
        (nodes-with-both-rc (new-jlist))
        (spacing interactive-layered-graph-visitor.pseudo-position-spacing))
    (jdo (node nodes-of-layer)
      (jl-add all-nodes node)
      (cond ((and (has-property node (%ilgv-pcc))
                  (or (has-property node (%ilgv-pred)) (has-property node (%ilgv-succ))))
             (jl-add nodes-with-pc-and-rc node))
            ((has-property node (%ilgv-pcc))
             (jl-add nodes-with-pc node))
            ((and (has-property node (%ilgv-pred)) (has-property node (%ilgv-succ)))
             (jl-add nodes-with-both-rc node))
            ((or (has-property node (%ilgv-pred)) (has-property node (%ilgv-succ)))
             (jl-add nodes-with-one-rc node))
            (t (jl-add nodes-without-c node))))
    ;; sort nodes with constraint by their position id
    (jl-sort nodes-with-pc
             (%ilgv-int-cmp (lambda (n) (get-property n layered-options.crossing-minimization-position-id))))
    ;; each node should only have one relative constraint
    (loop for n from 0
          while (< n (jsize nodes-with-both-rc)) do
      ;; pred cons is translated to succ cons for the following node
      (let* ((node (jl-get nodes-with-both-rc n))
             (target (%ilgv-get-elk-node all-nodes (get-property node (%ilgv-pred)))))
        (set-property node (%ilgv-pred) nil)
        (if (has-property target (%ilgv-pcc))
            (let ((pos-cos-target (get-property target (%ilgv-pcc))))
              (set-property node (%ilgv-pcc) (- pos-cos-target 1))
              (jl-add nodes-with-pc-and-rc node))
            (progn
              (set-property target (%ilgv-succ) (get-identifier node))
              (jl-add nodes-with-one-rc node)
              (if (not (has-property target (%ilgv-pred)))
                  (progn (jl-remove nodes-without-c target)
                         (jl-add nodes-with-one-rc target))
                  (progn (jl-remove nodes-with-one-rc target)
                         (jl-add-at nodes-with-both-rc (+ n 1) target)))))))
    ;; nodes with relative and position constraint
    (%ilgv-handle-nodes-with-pc-and-rc all-nodes nodes-without-c nodes-with-one-rc nodes-with-pc
                                       nodes-with-pc-and-rc)
    ;; determine the order of the nodes
    (setf nodes-with-one-rc (%ilgv-sort-rc-nodes nodes-with-one-rc all-nodes nodes-without-c))
    (%ilgv-sort-nodes-in-layer nodes-with-pc nodes-without-c direction)
    ;; add nodes with relative constraint
    (jdo (node nodes-with-one-rc)
      (let ((targets (make-array 2 :initial-element nil)))
        (if (has-property node (%ilgv-pred))
            (setf (aref targets 0) (%ilgv-get-elk-node all-nodes (get-property node (%ilgv-pred))))
            (setf (aref targets 1) (%ilgv-get-elk-node all-nodes (get-property node (%ilgv-succ)))))
        (dotimes (tt 2)
          (when (aref targets tt)
            (if (not (has-property (aref targets tt) (%ilgv-pcc)))
                ;; add node at the correct position
                (let ((index (if (= tt 0)
                                 (jl-index-of nodes-without-c (aref targets tt))
                                 (+ (jl-index-of nodes-without-c (aref targets tt)) 1))))
                  (jl-add-at nodes-without-c index node))
                ;; set pos cons on node because target has pos cons
                (let* ((val (get-property (aref targets tt) (%ilgv-pcc)))
                       (value (if (= tt 0) (- val 1) (+ val 1)))
                       (node-prop (if (= tt 0) (%ilgv-pred) (%ilgv-succ))))
                  (set-property node (%ilgv-pcc) value)
                  (set-property node node-prop nil)
                  (jl-add nodes-with-pc node)))))))
    ;; sort nodes with constraint by their position constraint
    (jl-sort nodes-with-pc (%ilgv-int-cmp (lambda (n) (get-property n (%ilgv-pcc)))))
    ;; add the nodes with position constraint at the desired position in their layer
    (jdo (node nodes-with-pc)
      (let ((pos (get-property node (%ilgv-pcc))))
        (if (< pos (jsize nodes-without-c))
            (progn
              (when (> pos 0)
                (%ilgv-swap-nodes nodes-without-c pos))
              (jl-add-at nodes-without-c pos node))
            (jl-add nodes-without-c node))))
    ;; set the y/x positions according to the order of the nodes
    (enum-case direction
      ((core-direction.undefined core-direction.right core-direction.left)
       (let ((y-pos (get-y (jl-get nodes-without-c 0))))
         (jdo (node nodes-without-c)
           (set-property node layered-options.position (new-k-vector (get-x node) y-pos))
           (incf y-pos (+ (get-height node) spacing)))))
      ((core-direction.down core-direction.up)
       (let ((x-pos (+ (get-x (jl-get nodes-without-c 0)) (int* (int* 2 layer-id) spacing))))
         (jdo (node nodes-without-c)
           (set-property node layered-options.position (new-k-vector x-pos (get-y node)))
           (incf x-pos (+ (get-width node) spacing)))))
      (t nil))))

(defun %ilgv-handle-nodes-with-pc-and-rc (all-nodes nodes nodes-with-rc nodes-with-position-constraint
                                          nodes-with-pc-and-rc)
  (loop for i from 0
        while (< i (jsize nodes-with-pc-and-rc)) do
    (let* ((node (jl-get nodes-with-pc-and-rc i))
           (targets (new-jlist))
           (vals (new-jlist))
           (val (get-property node (%ilgv-pcc))))
      (jl-add nodes-with-position-constraint node)
      (when (has-property node (%ilgv-pred))
        (jl-add targets (%ilgv-get-elk-node all-nodes (get-property node (%ilgv-pred))))
        (jl-add vals (+ val 1))
        (set-property node (%ilgv-pred) nil))
      (when (has-property node (%ilgv-succ))
        (jl-add targets (%ilgv-get-elk-node all-nodes (get-property node (%ilgv-succ))))
        (jl-add vals (- val 1))
        (set-property node (%ilgv-succ) nil))
      ;; targets should be in the correct lists
      (dotimes (tt (jsize targets))
        (let ((target (jl-get targets tt)))
          (unless (jl-contains nodes-with-position-constraint target)
            (set-property target (%ilgv-pcc) (jl-get vals tt))
            (if (not (jl-contains nodes-with-rc target))
                (progn (jl-add nodes-with-position-constraint target)
                       (jl-remove nodes target))
                (progn (jl-add nodes-with-pc-and-rc target)
                       (jl-remove nodes-with-rc target)))))))))

(defun %ilgv-swap-nodes (nodes position)
  (let* ((pred (jl-get nodes (- position 1)))
         (succ (jl-get nodes position))
         (chain (%ilgv-get-chain-by-layer-nodes pred nodes)))
    (when (jl-contains chain succ)
      (let* ((count (jl-index-of chain succ))
             (end (+ position (- (jsize chain) count)))
             (swap-nodes (new-jlist)))
        (let ((i 0))
          (loop while (< i count) do
            (when (>= end (jsize nodes))
              (return))
            (let ((current-node (jl-get nodes end)))
              (setf chain (%ilgv-get-chain-by-layer-nodes current-node nodes))
              (if (<= (+ (jsize chain) i) count)
                  (progn
                    ;; removeAll: every occurrence of every chain element
                    (jl-remove-if nodes (lambda (x) (jl-contains chain x)))
                    (jl-add-all swap-nodes chain)
                    (incf i (- (jsize chain) 1)))
                  ;; skip chains of relative constraints
                  (incf end (jsize chain))))
            (incf i)))
        (jl-add-all-at nodes (- position count) swap-nodes)))))

(defun %ilgv-sort-rc-nodes (nodes-with-rc all-nodes nodes-without-c)
  (let ((nodes (new-jlist)))
    (loop until (jl-is-empty nodes-with-rc) do
      (let ((circle t))
        (let ((i 0))
          (loop while (< i (jsize nodes-with-rc)) do
            (let* ((cur (jl-get nodes-with-rc i))
                   (target (if (has-property cur (%ilgv-pred))
                               ;; predecessor is defined
                               (%ilgv-get-elk-node all-nodes (get-property cur (%ilgv-pred)))
                               ;; successor is defined
                               (%ilgv-get-elk-node all-nodes (get-property cur (%ilgv-succ))))))
              (unless (jl-contains nodes-with-rc target)
                (jl-add nodes cur)
                (jl-remove-at nodes-with-rc i)
                (decf i)
                (setf circle nil)))
            (incf i)))
        ;; relative constraints create a circle: remove one of the constraints
        (when circle
          (let ((cur (jl-get nodes-with-rc 0)))
            (if (has-property cur (%ilgv-pred))
                (set-property cur (%ilgv-pred) nil)
                (set-property cur (%ilgv-succ) nil))
            (jl-remove-at nodes-with-rc 0)
            (jl-add nodes-without-c cur)))))
    nodes))

(defun %ilgv-sort-nodes-in-layer (nodes-with-position-constraint nodes direction)
  (jl-sort nodes-with-position-constraint (%ilgv-int-cmp (lambda (n) (get-property n (%ilgv-pcc)))))
  ;; other nodes by their y (RIGHT/LEFT) / x (DOWN/UP) coordinate
  (jl-sort nodes
           (lambda (a b)
             (enum-case direction
               ((core-direction.undefined core-direction.right core-direction.left)
                (to-int (- (get-y a) (get-y b))))
               ((core-direction.down core-direction.up)
                (to-int (- (get-x a) (get-x b))))
               (t 0)))))

(defun %ilgv-set-interactive-strategies (parent)
  (set-property parent layered-options.crossing-minimization-semi-interactive t)
  (set-property parent layered-options.layering-strategy layering-strategy.interactive)
  (set-property parent layered-options.cycle-breaking-strategy cycle-breaking-strategy.interactive)
  ;; model order would destroy the ordering created by the constraints
  (set-property parent layered-options.consider-model-order-strategy ordering-strategy.none)
  ;; Java stores FALSE; in this port that removes the entry (the default is false too)
  (set-property parent layered-options.crossing-minimization-force-node-model-order nil)
  (set-property parent layered-options.crossing-minimization-strategy crossing-minimization-strategy.layer-sweep))

(defun %ilgv-get-elk-node (nodes id)
  (jdo (e-n nodes)
    (when (equal id (get-identifier e-n))
      (return-from %ilgv-get-elk-node e-n)))
  nil)

(defun %ilgv-get-chain-by-layer-nodes (node layer-nodes)
  (let ((pos (jl-index-of layer-nodes node))
        (chain-nodes (jlist node)))
    ;; from node to the start
    (loop for i from (- pos 1) downto 0 do
      (if (or (has-property (jl-get layer-nodes i) (%ilgv-pred))
              (has-property (jl-get layer-nodes (+ i 1)) (%ilgv-succ)))
          (jl-add-at chain-nodes 0 (jl-get layer-nodes i))
          (return)))
    ;; from node to the end
    (loop for i from (+ pos 1) below (jsize layer-nodes) do
      (if (or (has-property (jl-get layer-nodes i) (%ilgv-succ))
              (has-property (jl-get layer-nodes (- i 1)) (%ilgv-pred)))
          (jl-add chain-nodes (jl-get layer-nodes i))
          (return)))
    chain-nodes))

(defun %ilgv-get-chain-by-all-nodes (node all-nodes)
  (let ((chain (jlist node))
        (succ-name (get-property node (%ilgv-pred)))
        (pred-name (get-property node (%ilgv-succ))))
    (loop while succ-name do
      (let ((succ-node (%ilgv-get-elk-node all-nodes succ-name)))
        (jl-add chain succ-node)
        (setf succ-name (get-property succ-node (%ilgv-pred)))))
    (loop while pred-name do
      (let ((pred-node (%ilgv-get-elk-node all-nodes pred-name)))
        (jl-add chain pred-node)
        ;; (sic) Java follows PRED_OF here too
        (setf pred-name (get-property pred-node (%ilgv-pred)))))
    chain))
