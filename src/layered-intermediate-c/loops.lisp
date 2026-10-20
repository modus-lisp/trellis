;;;; loops.lisp -- org.eclipse.elk.alg.layered.intermediate.loops

(in-package #:elk)

;;; ------------------------------------------------------------ SelfLoopType

(defjstatic self-loop-type.from-port-sides (port-sides)
  (when (contains port-sides port-side.undefined)
    (jthrow 'illegal-argument-exception "Port sides must not contain UNDEFINED"))
  (case (jsize port-sides)
    (1 self-loop-type.one-side)
    (2 (let ((east-west (and (contains port-sides port-side.east) (contains port-sides port-side.west)))
             (north-south (and (contains port-sides port-side.north) (contains port-sides port-side.south))))
         (if (or east-west north-south)
             self-loop-type.two-sides-opposing
             self-loop-type.two-sides-corner)))
    (3 self-loop-type.three-sides)
    (4 self-loop-type.four-sides)
    (t nil)))

;;; ------------------------------------------------------------ SelfLoopPort

(defjctor self-loop-port (l-port)
  (setf (.l-port this) l-port)
  ;; Check if the port is only incident to self loops
  (setf (.had-only-self-loops this)
        (block all
          (jdo (edge (get-connected-edges l-port))
            (unless (is-self-loop edge) (return-from all nil)))
          t)))

(defjmethod get-l-port ((this self-loop-port)) (.l-port this))
(defjmethod had-only-self-loops ((this self-loop-port)) (.had-only-self-loops this))
(defjmethod is-hidden ((this self-loop-port)) (.is-hidden this))
(defjmethod set-hidden ((this self-loop-port) hidden) (setf (.is-hidden this) hidden))
(defjmethod get-incoming-sl-edges ((this self-loop-port)) (.incoming-sl-edges this))
(defjmethod get-outgoing-sl-edges ((this self-loop-port)) (.outgoing-sl-edges this))
(defjmethod get-sl-net-flow ((this self-loop-port))
  (- (length (.incoming-sl-edges this)) (length (.outgoing-sl-edges this))))

;;; ------------------------------------------------------------ SelfLoopEdge

(defjctor self-loop-edge (l-edge sl-source sl-target)
  (setf (.l-edge this) l-edge
        (.sl-source this) sl-source
        (.sl-target this) sl-target)
  (jl-add (get-outgoing-sl-edges sl-source) this)
  (jl-add (get-incoming-sl-edges sl-target) this))

(defjmethod get-l-edge ((this self-loop-edge)) (.l-edge this))
(defjmethod get-sl-hyper-loop ((this self-loop-edge)) (.sl-hyper-loop this))
(defjmethod set-sl-hyper-loop ((this self-loop-edge) sl-loop) (setf (.sl-hyper-loop this) sl-loop))
(defjmethod get-sl-source ((this self-loop-edge)) (.sl-source this))
(defjmethod get-sl-target ((this self-loop-edge)) (.sl-target this))
(defjmethod is-inline ((this self-loop-edge))
  (jdo (label (get-labels (.l-edge this)))
    (when (get-property label layered-options.edge-labels-inline)
      (return-from is-inline/0 t)))
  nil)
(defjmethod get-label-side ((this self-loop-edge))
  (get-side (get-sl-labels (.sl-hyper-loop this))))

;;; ------------------------------------------------------------ SelfHyperLoop

(defjctor self-hyper-loop (sl-holder)
  (setf (.sl-holder this) sl-holder))

(defjmethod compute-ports-per-side ((this self-hyper-loop))
  ;; Remember ports for each side
  (let ((by-side (new-jmap)))
    (setf (.sl-ports-by-side this) by-side)
    (jdo (sl-port (.sl-ports this))
      (let ((port-side (get-side (get-l-port sl-port))))
        (jl-add (or (gethash port-side by-side)
                    (setf (gethash port-side by-side) (new-jlist)))
                sl-port)))
    ;; Determine this self loop's loop type
    (setf (.self-loop-type this)
          (self-loop-type.from-port-sides (%lic-multimap-key-set by-side)))))

(defun %lic-multimap-key-set (multimap)
  "keySet() of a PortSide multimap, as an enum set."
  (enum-set-of-list 'port-side (loop for k being the hash-keys of multimap collect k)))

(defjmethod get-sl-holder ((this self-hyper-loop)) (.sl-holder this))

(defjmethod add-self-loop-edge ((this self-hyper-loop) sl-edge)
  (when (js-add (.sl-edges this) sl-edge)
    (set-sl-hyper-loop sl-edge this)
    (let ((sl-source (get-sl-source sl-edge)))
      (unless (find sl-source (.sl-ports this))
        (jl-add (.sl-ports this) sl-source)))
    (let ((sl-target (get-sl-target sl-edge)))
      (unless (find sl-target (.sl-ports this))
        (jl-add (.sl-ports this) sl-target)))
    ;; Check if we need to take care of any edge labels
    (let ((l-labels (get-labels (get-l-edge sl-edge))))
      (unless (is-empty l-labels)
        (when (null (.sl-labels this))
          (setf (.sl-labels this) (new-self-hyper-loop-labels this)))
        (add-l-labels (.sl-labels this) l-labels)))))

(defjmethod get-sl-ports ((this self-hyper-loop)) (.sl-ports this))
(defjmethod get-sl-edges ((this self-hyper-loop)) (.sl-edges this))
(defjmethod get-sl-labels ((this self-hyper-loop)) (.sl-labels this))
(defjmethod get-self-loop-type ((this self-hyper-loop)) (.self-loop-type this))
(defjmethod get-sl-ports-by-side ((this self-hyper-loop))
  "The multimap: a hash table PortSide -> list of SelfLoopPort."
  (.sl-ports-by-side this))
(defjmethod get-sl-ports-by-side ((this self-hyper-loop) port-side)
  (or (gethash port-side (.sl-ports-by-side this)) (new-jlist)))
(defjmethod has-sl-ports-on-side ((this self-hyper-loop) port-side)
  (jm-contains-key (.sl-ports-by-side this) port-side))
(defun %lic-sl-port-sides (sl-loop)
  "slLoop.getSLPortsBySide().keySet()"
  (%lic-multimap-key-set (.sl-ports-by-side sl-loop)))
(defjmethod get-leftmost-port ((this self-hyper-loop)) (.leftmost-port this))
(defjmethod set-leftmost-port ((this self-hyper-loop) p) (setf (.leftmost-port this) p))
(defjmethod get-rightmost-port ((this self-hyper-loop)) (.rightmost-port this))
(defjmethod set-rightmost-port ((this self-hyper-loop) p) (setf (.rightmost-port this) p))
(defjmethod get-occupied-port-sides ((this self-hyper-loop)) (.occupied-port-sides this))
(defjmethod get-routing-slot ((this self-hyper-loop) port-side)
  (aref (.routing-slot this) (ordinal port-side)))
(defjmethod set-routing-slot ((this self-hyper-loop) port-side slot)
  (setf (aref (.routing-slot this) (ordinal port-side)) slot)
  (let ((slot-count (get-routing-slot-count (.sl-holder this)))
        (o (ordinal port-side)))
    (setf (aref slot-count o) (max (aref slot-count o) (+ slot 1)))))

;;; ------------------------------------------------------------ SelfLoopHolder

(defjctor self-loop-holder (node)
  (setf (.l-node this) node))

(defjstatic self-loop-holder.install (l-node)
  (let ((holder (new-self-loop-holder l-node)))
    (set-property l-node layered-internal-properties.self-loop-holder holder)
    (%lic-holder-initialize holder)
    holder))

(defjstatic self-loop-holder.needs-self-loop-processing (l-node)
  (if (not (eq (get-type l-node) node-type.normal))
      nil
      (block any
        (jdo (edge (get-outgoing-edges l-node))
          (when (is-self-loop edge) (return-from any t)))
        nil)))

(defconstant +lic-unvisited+ 0)
(defconstant +lic-visited+ 1)

(defun %lic-holder-initialize (this)
  (let ((sl-edges (new-jlist)))
    ;; Create self loop edges and ports for every self loop
    (jdo (l-edge (get-outgoing-edges (.l-node this)))
      (when (is-self-loop l-edge)
        (jl-add sl-edges (new-self-loop-edge l-edge
                                             (%lic-self-loop-port-for this (get-source l-edge))
                                             (%lic-self-loop-port-for this (get-target l-edge))))))
    ;; Reset port IDs for the BFS we're about to run
    (jdo (sl-port (.sl-ports this))
      (setf (.id (get-l-port sl-port)) +lic-unvisited+))
    ;; Run BFS at every port to gather the edges into hyperloops
    (jdo (sl-port (.sl-ports this))
      (when (= (.id (get-l-port sl-port)) +lic-unvisited+)
        (jl-add (.sl-hyper-loops this) (%lic-initialize-hyper-loop this sl-port))))))

(defun %lic-self-loop-port-for (this lport)
  (let ((m (.sl-ports this)))
    (unless (jm-contains-key m lport)
      (jm-put m lport (new-self-loop-port lport)))
    (gethash lport m)))

(defun %lic-initialize-hyper-loop (this sl-port)
  (let ((sl-loop (new-self-hyper-loop this))
        (bfs-queue (new-jlist)))
    (jl-add bfs-queue sl-port)
    (loop until (jl-is-empty bfs-queue)
          do (let ((current (jl-poll-first bfs-queue)))
               (setf (.id (get-l-port current)) +lic-visited+)
               ;; Add each outgoing edge to our hyper loop
               (jdo (sl-edge (get-outgoing-sl-edges current))
                 (add-self-loop-edge sl-loop sl-edge)
                 (let ((tp (get-sl-target sl-edge)))
                   (when (= (.id (get-l-port tp)) +lic-unvisited+)
                     (jl-add bfs-queue tp))))
               ;; Add each incoming edge to our hyper loop
               (jdo (sl-edge (get-incoming-sl-edges current))
                 (add-self-loop-edge sl-loop sl-edge)
                 (let ((sp (get-sl-source sl-edge)))
                   (when (= (.id (get-l-port sp)) +lic-unvisited+)
                     (jl-add bfs-queue sp))))))
    sl-loop))

(defjmethod get-l-node ((this self-loop-holder)) (.l-node this))
(defjmethod get-sl-hyper-loops ((this self-loop-holder)) (.sl-hyper-loops this))
(defjmethod get-sl-port-map ((this self-loop-holder)) (.sl-ports this))
(defjmethod are-ports-hidden ((this self-loop-holder)) (.are-ports-hidden this))
(defjmethod set-ports-hidden ((this self-loop-holder) hidden) (setf (.are-ports-hidden this) hidden))
(defjmethod get-routing-slot-count ((this self-loop-holder)) (.routing-slot-count this))

;;; ------------------------------------------------------------ SelfHyperLoopLabels

(defjctor self-hyper-loop-labels (sl-loop)
  ;; Initialize our properties from the graph
  (let ((l-node (get-l-node (get-sl-holder sl-loop))))
    (setf (.layout-direction this) (get-property (get-graph l-node) layered-options.direction))
    (setf (.label-label-spacing this)
          (l-graph-util.get-individual-or-inherited l-node layered-options.spacing-label-label))))

(defjmethod add-l-labels ((this self-hyper-loop-labels) new-l-labels)
  (jdo (new-l-label new-l-labels)
    (jl-add (.l-labels this) new-l-label)
    (%lic-update-size this new-l-label)))

(defjmethod get-l-labels ((this self-hyper-loop-labels)) (.l-labels this))

(defun %lic-update-size (this new-l-label)
  (let ((new-size (get-size new-l-label))
        (size (.size this)))
    (if (is-horizontal (.layout-direction this))
        (progn
          ;; The labels will be stacked vertically
          (setf (.x size) (math.max (.x size) (.x new-size)))
          (incf (.y size) (.y new-size))
          ;; Add a label-label spacing if we already had labels
          (when (> (length (.l-labels this)) 1)
            (incf (.y size) (.label-label-spacing this))))
        (progn
          ;; The labels will be stacked horizontally
          (incf (.x size) (.x new-size))
          (setf (.y size) (math.max (.y size) (.y new-size)))
          (when (> (length (.l-labels this)) 1)
            (incf (.x size) (.label-label-spacing this)))))))

(defjmethod apply-label-management ((this self-hyper-loop-labels) label-manager target-width)
  (j-set (.size this)
         (label-management-processor.do-manage-labels
          label-manager (.l-labels this) target-width (.label-label-spacing this)
          (is-vertical (.layout-direction this)))))

(defjmethod apply-placement ((this self-hyper-loop-labels) offset)
  (if (is-horizontal (.layout-direction this))
      (%lic-apply-placement-horizontal this offset)
      (%lic-apply-placement-vertical this offset)))

(defun %lic-apply-placement-horizontal (this offset)
  (let ((x (.x (.position this)))
        (y (.y (.position this)))
        (size (.size this))
        (alignment (.alignment this))
        (side (.side this)))
    (jdo (l-label (.l-labels this))
      (let ((label-pos (get-position l-label)))
        ;; X coordinate depends on alignment and / or side
        (cond ((or (eq alignment layered-alignment.left) (eq side port-side.east))
               (setf (.x label-pos) x))
              ((or (eq alignment layered-alignment.right) (eq side port-side.west))
               (setf (.x label-pos) (- (+ x (.x size)) (.x (get-size l-label)))))
              (t
               ;; Alignment is center
               (setf (.x label-pos) (+ x (/ (- (.x size) (.x (get-size l-label))) 2)))))
        (setf (.y label-pos) y)
        (add label-pos offset)
        (incf y (+ (.y (get-size l-label)) (.label-label-spacing this)))))))

(defun %lic-apply-placement-vertical (this offset)
  (let ((x (.x (.position this)))
        (y (.y (.position this)))
        (size (.size this)))
    (jdo (l-label (.l-labels this))
      (let ((label-pos (get-position l-label)))
        (setf (.x label-pos) x)
        ;; We always top-align, except for the northern side
        (if (eq (.side this) port-side.north)
            (setf (.y label-pos) (- (+ y (.y size)) (.y (get-size l-label))))
            (setf (.y label-pos) y))
        (add label-pos offset)
        (incf x (+ (.x (get-size l-label)) (.label-label-spacing this)))))))

(defjmethod get-size ((this self-hyper-loop-labels)) (.size this))
(defjmethod get-position ((this self-hyper-loop-labels)) (.position this))
(defjmethod get-side ((this self-hyper-loop-labels)) (.side this))
(defjmethod set-side ((this self-hyper-loop-labels) side) (setf (.side this) side))
(defjmethod get-alignment ((this self-hyper-loop-labels)) (.alignment this))
(defjmethod set-alignment ((this self-hyper-loop-labels) a) (setf (.alignment this) a))
(defjmethod get-alignment-reference-sl-port ((this self-hyper-loop-labels)) (.alignment-reference-sl-port this))
(defjmethod set-alignment-reference-sl-port ((this self-hyper-loop-labels) p)
  (setf (.alignment-reference-sl-port this) p))
