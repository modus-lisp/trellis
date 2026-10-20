;;;; graph-util.lisp -- org.eclipse.elk.alg.layered.graph.LGraphUtil

(in-package #:elk)

(defun %lg-float (x)
  "Java (float) cast of a double, back to double."
  (cond ((or (not (floatp x)) (nan-p x) (infinite-p x)) (jd x))
        ((> (abs x) most-positive-single-float)
         (if (> x 0) double.positive-infinity double.negative-infinity))
        (t (jd (coerce x 'single-float)))))

(defjstatic l-graph-util.to-node-array (nodes) (to-jvector nodes))
(defjstatic l-graph-util.to-edge-array (edges) (to-jvector edges))
(defjstatic l-graph-util.to-port-array (ports) (to-jvector ports))

;;; ------------------------------------------------------------ Node Resizing

(defjstatic l-graph-util.resize-node (node new-size move-ports move-labels)
  (with-java-floats
    (let* ((old-size (new-k-vector (get-size node)))
           (width-ratio (%lg-float (/ (.x new-size) (.x old-size))))
           (height-ratio (%lg-float (/ (.y new-size) (.y old-size))))
           (width-diff (%lg-float (- (.x new-size) (.x old-size))))
           (height-diff (%lg-float (- (.y new-size) (.y old-size)))))
      ;; Update port positions
      (when move-ports
        (let ((fixed-ports (eq (get-property node layered-options.port-constraints)
                               port-constraints.fixed-pos)))
          (jdo (port (get-ports node))
            (let ((pos (get-position port)))
              (enum-case (get-side port)
                (port-side.north
                 (unless fixed-ports (setf (.x pos) (* (.x pos) width-ratio))))
                (port-side.east
                 (setf (.x pos) (+ (.x pos) width-diff))
                 (unless fixed-ports (setf (.y pos) (* (.y pos) height-ratio))))
                (port-side.south
                 (unless fixed-ports (setf (.x pos) (* (.x pos) width-ratio)))
                 (setf (.y pos) (+ (.y pos) height-diff)))
                (port-side.west
                 (unless fixed-ports (setf (.y pos) (* (.y pos) height-ratio)))))))))
      ;; Update label positions
      (when move-labels
        (jdo (label (get-labels node))
          (let* ((lpos (get-position label))
                 (midx (+ (.x lpos) (/ (.x (get-size label)) 2)))
                 (midy (+ (.y lpos) (/ (.y (get-size label)) 2)))
                 (width-percent (/ midx (.x old-size)))
                 (height-percent (/ midy (.y old-size))))
            (when (>= (+ width-percent height-percent) 1)
              (cond ((and (> (- width-percent height-percent) 0) (>= midy 0))
                     ;; label is on the right
                     (setf (.x lpos) (+ (.x lpos) width-diff))
                     (setf (.y lpos) (+ (.y lpos) (* height-diff height-percent))))
                    ((and (< (- width-percent height-percent) 0) (>= midx 0))
                     ;; label is on the bottom
                     (setf (.x lpos) (+ (.x lpos) (* width-diff width-percent)))
                     (setf (.y lpos) (+ (.y lpos) height-diff))))))))
      ;; Set the new node size
      (setf (.x (get-size node)) (.x new-size)
            (.y (get-size node)) (.y new-size))
      ;; Set fixed size option for the node: now the size is assumed to stay as determined here
      (set-property node layered-options.node-size-constraints (size-constraint.fixed))
      nil)))

;;; ------------------------------------------------------------ Graph Offsetting

(defjstatic l-graph-util.offset-graphs (graphs offsetx offsety)
  (jdo (graph graphs)
    (l-graph-util.offset-graph graph offsetx offsety)))

(defjstatic l-graph-util.offset-graph (graph offsetx offsety)
  (let ((graph-offset (new-k-vector offsetx offsety)))
    (jdo (node (get-layerless-nodes graph))
      (add (get-position node) graph-offset)
      (jdo (port (get-ports node))
        (jdo (edge (get-outgoing-edges port))
          (offset (get-bend-points edge) graph-offset)
          (let ((junction-points (get-property edge layered-options.junction-points)))
            (when junction-points
              (offset junction-points graph-offset)))
          (jdo (label (get-labels edge))
            (add (get-position label) graph-offset)))))
    nil))

;;; ------------------------------------------------------------ Layer Things

(defjstatic l-graph-util.place-nodes-horizontally (layer xoffset)
  ;; determine maximal left and right margin
  (let ((max-left-margin 0.0) (max-right-margin 0.0))
    (jdo (node (get-nodes layer))
      (setf max-left-margin (math.max max-left-margin (.left (get-margin node))))
      (setf max-right-margin (math.max max-right-margin (.right (get-margin node)))))
    (jdo (node (get-nodes layer))
      (let* ((alignment (get-property node layered-options.alignment))
             (ratio
               (enum-case alignment
                 (alignment.left 0.0)
                 (alignment.right 1.0)
                 (alignment.center 0.5)
                 (t
                  ;; determine the number of input and output ports for the node
                  (let ((inports 0) (outports 0))
                    (jdo (port (get-ports node))
                      (unless (jl-is-empty (get-incoming-edges port)) (incf inports))
                      (unless (jl-is-empty (get-outgoing-edges port)) (incf outports)))
                    ;; calculate node placement based on the port numbers
                    (if (zerop (+ inports outports))
                        0.5
                        (/ (jd outports) (+ inports outports)))))))
             ;; align nodes to the layer's maximal margin
             (size (get-size layer))
             (node-size (.x (get-size node)))
             (xpos (* (- (.x size) node-size) ratio)))
        (cond ((> ratio 0.5) (decf xpos (* max-right-margin 2 (- ratio 0.5))))
              ((< ratio 0.5) (incf xpos (* max-left-margin 2 (- 0.5 ratio)))))
        ;; consider the node's individual margin
        (let ((left-margin (.left (get-margin node))))
          (when (< xpos left-margin) (setf xpos left-margin)))
        (let ((right-margin (.right (get-margin node))))
          (when (> xpos (- (.x size) right-margin node-size))
            (setf xpos (- (.x size) right-margin node-size))))
        (setf (.x (get-position node)) (+ xoffset xpos))))
    nil))

(defjstatic l-graph-util.find-max-non-dummy-node-width (layer respect-node-margins)
  (if (is-vertical (get-property (get-graph layer) layered-options.direction))
      0.0
      (let ((max-width 0.0))
        (jdo (node (get-nodes layer))
          (when (eq (get-type node) node-type.normal)
            (let ((width (.x (get-size node))))
              (when respect-node-margins
                (incf width (+ (.left (get-margin node)) (.right (get-margin node)))))
              (setf max-width (math.max max-width width)))))
        max-width)))

;;; ------------------------------------------------------------ Graph Properties

(defjstatic l-graph-util.compute-graph-properties (layered-graph)
  (let ((props (get-property layered-graph layered-internal-properties.graph-properties)))
    (unless (is-empty props)
      (clear props))
    (let ((direction (l-graph-util.get-direction layered-graph)))
      (jdo (node (get-layerless-nodes layered-graph))
        (cond ((get-property node layered-options.comment-box)
               (add props layered-graph-properties.comments))
              ((get-property node layered-options.hypernode)
               (add props layered-graph-properties.hypernodes)
               (add props layered-graph-properties.hyperedges))
              ((eq (get-type node) node-type.external-port)
               (add props layered-graph-properties.external-ports)))
        (let ((port-constraints (get-property node layered-options.port-constraints)))
          (cond ((eq port-constraints port-constraints.undefined)
                 ;; correct the port constraints value
                 (set-property node layered-options.port-constraints port-constraints.free))
                ((not (eq port-constraints port-constraints.free))
                 (add props layered-graph-properties.non-free-ports))))
        (jdo (port (get-ports node))
          (when (> (+ (length (get-incoming-edges port)) (length (get-outgoing-edges port))) 1)
            (add props layered-graph-properties.hyperedges))
          (let ((port-side (get-side port)))
            (enum-case direction
              ((direction.up direction.down)
               (when (or (eq port-side port-side.east) (eq port-side port-side.west))
                 (add props layered-graph-properties.north-south-ports)))
              (t
               (when (or (eq port-side port-side.north) (eq port-side port-side.south))
                 (add props layered-graph-properties.north-south-ports)))))
          (jdo (edge (get-outgoing-edges port))
            (when (eq (get-node (get-target edge)) node)
              (add props layered-graph-properties.self-loops))
            (jdo (label (get-labels edge))
              (enum-case (get-property label layered-options.edge-labels-placement)
                (edge-label-placement.center
                 (add props layered-graph-properties.center-labels))
                ((edge-label-placement.head edge-label-placement.tail)
                 (add props layered-graph-properties.end-labels))))))))
    nil))

;;; ------------------------------------------------------------ Handling of Ports

(defjstatic l-graph-util.create-port (node end-point type layered-graph)
  (let ((port nil)
        (direction (l-graph-util.get-direction layered-graph))
        (merge-ports (get-property layered-graph layered-options.merge-edges)))
    (if (and (or merge-ports (get-property node layered-options.hypernode))
             (not (is-side-fixed (get-property node layered-options.port-constraints))))
        ;; Hypernodes have one output port and one input port
        (let ((default-side (port-side.from-direction direction)))
          (setf port (l-graph-util.provide-collector-port
                      layered-graph node type
                      (if (eq type port-type.output) default-side (opposed default-side)))))
        (progn
          (setf port (new-l-port))
          (set-node port node)
          (if end-point
              (let ((pos (get-position port)))
                (setf (.x pos) (- (.x end-point) (.x (get-position node))))
                (setf (.y pos) (- (.y end-point) (.y (get-position node))))
                (bound pos 0 0 (.x (get-size node)) (.y (get-size node)))
                (set-side port (l-graph-util.calc-port-side port direction)))
              (let ((default-side (port-side.from-direction direction)))
                (set-side port (if (eq type port-type.output) default-side (opposed default-side)))))
          (let ((graph-properties (get-property layered-graph layered-internal-properties.graph-properties))
                (port-side (get-side port)))
            (enum-case direction
              ((direction.left direction.right)
               (when (or (eq port-side port-side.north) (eq port-side port-side.south))
                 (add graph-properties layered-graph-properties.north-south-ports)))
              ((direction.up direction.down)
               (when (or (eq port-side port-side.east) (eq port-side port-side.west))
                 (add graph-properties layered-graph-properties.north-south-ports)))))))
    port))

(defjstatic l-graph-util.calc-port-side (port direction)
  (with-java-floats
    (block nil
      (let* ((node (get-node port))
             ;; if the node has zero size, we cannot decide anything
             (node-width (.x (get-size node)))
             (node-height (.y (get-size node))))
        (when (and (<= node-width 0) (<= node-height 0))
          (return port-side.undefined))
        ;; check direction-dependent criterion
        (let ((xpos (.x (get-position port)))
              (ypos (.y (get-position port)))
              (width (.x (get-size port)))
              (height (.y (get-size port))))
          (enum-case direction
            ((direction.left direction.right)
             (cond ((< xpos 0) (return port-side.west))
                   ((> (+ xpos width) node-width) (return port-side.east))))
            ((direction.up direction.down)
             (cond ((< ypos 0) (return port-side.north))
                   ((> (+ ypos height) node-height) (return port-side.south)))))
          ;; check general criterion
          (let ((width-percent (/ (+ xpos (/ width 2)) node-width))
                (height-percent (/ (+ ypos (/ height 2)) node-height)))
            (cond ((and (<= (+ width-percent height-percent) 1)
                        (<= (- width-percent height-percent) 0))
                   ;; port is on the left
                   port-side.west)
                  ((and (>= (+ width-percent height-percent) 1)
                        (>= (- width-percent height-percent) 0))
                   ;; port is on the right
                   port-side.east)
                  ((< height-percent 0.5)
                   ;; port is on the top
                   port-side.north)
                  (t
                   ;; port is on the bottom
                   port-side.south))))))))

(defjstatic l-graph-util.calc-port-offset (port side)
  (let ((node (get-node port)))
    (enum-case side
      (port-side.north (- (+ (.y (get-position port)) (.y (get-size port)))))
      (port-side.east (- (.x (get-position port)) (.x (get-size node))))
      (port-side.south (- (.y (get-position port)) (.y (get-size node))))
      (port-side.west (- (+ (.x (get-position port)) (.x (get-size port)))))
      (t 0.0))))

(defjstatic l-graph-util.center-point (point boundary side)
  (enum-case side
    (port-side.north
     (setf (.x point) (/ (.x boundary) 2) (.y point) 0.0))
    (port-side.east
     (setf (.x point) (.x boundary) (.y point) (/ (.y boundary) 2)))
    (port-side.south
     (setf (.x point) (/ (.x boundary) 2) (.y point) (.y boundary)))
    (port-side.west
     (setf (.x point) 0.0 (.y point) (/ (.y boundary) 2))))
  nil)

(defjstatic l-graph-util.provide-collector-port (layered-graph node type side)
  (declare (ignore layered-graph))
  (let ((port nil))
    (block found
      (enum-case type
        (port-type.input
         (jdo (inport (get-ports node))
           (when (get-property inport layered-internal-properties.input-collect)
             (return-from found inport)))
         (setf port (new-l-port))
         (set-property port layered-internal-properties.input-collect t))
        (port-type.output
         (jdo (outport (get-ports node))
           (when (get-property outport layered-internal-properties.output-collect)
             (return-from found outport)))
         (setf port (new-l-port))
         (set-property port layered-internal-properties.output-collect t)))
      (when port
        (set-node port node)
        (set-side port side)
        (l-graph-util.center-point (get-position port) (get-size node) side))
      port)))

(defjstatic l-graph-util.initialize-port (port port-constraints direction anchor-pos)
  (with-java-floats
    (let ((port-side (get-side port)))
      (when (and (eq port-side port-side.undefined) (is-side-fixed port-constraints))
        ;; calculate the port side and offset from the port's current position
        (setf port-side (l-graph-util.calc-port-side port direction))
        (set-side port port-side)
        ;; if port coordinates are (0,0), we default to port offset 0 to make the common case
        ;; frustration-free
        (when (and (not (has-property port layered-options.port-border-offset))
                   (not (eq port-side port-side.undefined))
                   (or (/= (.x (get-position port)) 0) (/= (.y (get-position port)) 0)))
          (set-property port layered-options.port-border-offset
                        (l-graph-util.calc-port-offset port port-side))))
      ;; if the port constraints are set to fixed ratio, remember the current ratio
      (when (is-ratio-fixed port-constraints)
        (let ((ratio 0.0))
          (enum-case port-side
            ((port-side.north port-side.south)
             (let ((node-width (.x (get-size (get-node port)))))
               (when (> node-width 0)
                 (setf ratio (/ (.x (get-position port)) node-width)))))
            ((port-side.east port-side.west)
             (let ((node-height (.y (get-size (get-node port)))))
               (when (> node-height 0)
                 (setf ratio (/ (.y (get-position port)) node-height))))))
          (set-property port layered-internal-properties.port-ratio-or-position ratio)))
      (let ((port-size (get-size port))
            (port-anchor (get-anchor port)))
        (cond (anchor-pos
               ;; if the port anchor property is set, use it as anchor point
               (setf (.x port-anchor) (.x anchor-pos)
                     (.y port-anchor) (.y anchor-pos))
               ;; Since we have applied an explicit anchor, assume the user knows what they are
               ;; doing and fix it
               (set-explicitly-supplied-port-anchor port t))
              ((and (is-side-fixed port-constraints) (not (eq port-side port-side.undefined)))
               ;; set the anchor point according to the port side
               (enum-case port-side
                 (port-side.north
                  (setf (.x port-anchor) (/ (.x port-size) 2)))
                 (port-side.east
                  (setf (.x port-anchor) (.x port-size)
                        (.y port-anchor) (/ (.y port-size) 2)))
                 (port-side.south
                  (setf (.x port-anchor) (/ (.x port-size) 2)
                        (.y port-anchor) (.y port-size)))
                 (port-side.west
                  (setf (.y port-anchor) (/ (.y port-size) 2)))))
              (t
               ;; the port side will be decided later, so set the anchor to the center point
               (setf (.x port-anchor) (/ (.x port-size) 2)
                     (.y port-anchor) (/ (.y port-size) 2)))))
      nil)))

;;; ------------------------------------------------------------ External Ports

(defjstatic l-graph-util.create-external-port-dummy (property-holder port-constraints port-side net-flow
                                                     port-node-size port-position port-size
                                                     layout-direction layered-graph)
  (with-java-floats
    (let ((final-external-port-side port-side)
          ;; Create the dummy with one port
          (dummy (new-l-node layered-graph)))
      (set-type dummy node-type.external-port)
      (set-property dummy layered-internal-properties.ext-port-size port-size)
      (set-property dummy layered-options.port-constraints port-constraints.fixed-pos)
      (let ((port-border-offset (get-property property-holder layered-options.port-border-offset))
            (dummy-port (new-l-port)))
        (set-property dummy layered-options.port-border-offset port-border-offset)
        (set-node dummy-port dummy)
        ;; If the port constraints are free, we need to determine where to put the dummy (and its port)
        (unless (is-side-fixed port-constraints)
          ;; we need some layout direction here, use RIGHT as default in case it is undefined
          (if (>= net-flow 0)
              (setf final-external-port-side (port-side.from-direction layout-direction))
              (setf final-external-port-side (opposed (port-side.from-direction layout-direction))))
          (set-property property-holder layered-options.port-side final-external-port-side))
        ;; Retrieve the anchor point, possibly to be modified later
        (let ((anchor (new-k-vector))
              (explicit-anchor nil))
          (if (has-property property-holder layered-options.port-anchor)
              (progn (j-set anchor (get-property property-holder layered-options.port-anchor))
                     (setf explicit-anchor t))
              (j-set anchor (/ (.x port-size) 2) (/ (.y port-size) 2)))
          ;; With the port side at hand, set the necessary properties and place the dummy's port
          ;; at the dummy's center
          (enum-case final-external-port-side
            (port-side.west
             (set-property dummy layered-options.layering-layer-constraint layer-constraint.first-separate)
             (set-property dummy layered-internal-properties.edge-constraint edge-constraint.outgoing-only)
             (setf (.y (get-size dummy)) (.y port-size))
             (when (< port-border-offset 0)
               (setf (.x (get-size dummy)) (- port-border-offset)))
             (set-side dummy-port port-side.east)
             (unless explicit-anchor
               (setf (.x anchor) (.x port-size)))
             ;; The port anchors think that there is a difference between the port's left and right
             ;; border coordinates, which makes sense if the port has a non-zero width. The port dummy,
             ;; however, will have a width of zero. Thus, the anchor must be relative to -portWidth.
             ;; This fixes #546.
             (setf (.x anchor) (- (.x anchor) (.x port-size))))
            (port-side.east
             (set-property dummy layered-options.layering-layer-constraint layer-constraint.last-separate)
             (set-property dummy layered-internal-properties.edge-constraint edge-constraint.incoming-only)
             (setf (.y (get-size dummy)) (.y port-size))
             (when (< port-border-offset 0)
               (setf (.x (get-size dummy)) (- port-border-offset)))
             (set-side dummy-port port-side.west)
             (unless explicit-anchor
               (setf (.x anchor) 0.0)))
            (port-side.north
             (set-property dummy layered-internal-properties.in-layer-constraint in-layer-constraint.top)
             (setf (.x (get-size dummy)) (.x port-size))
             (when (< port-border-offset 0)
               (setf (.y (get-size dummy)) (- port-border-offset)))
             (set-side dummy-port port-side.south)
             (unless explicit-anchor
               (setf (.y anchor) (.y port-size)))
             ;; See comments in case WEST. This partly fixes #680.
             (setf (.y anchor) (- (.y anchor) (.y port-size))))
            (port-side.south
             (set-property dummy layered-internal-properties.in-layer-constraint in-layer-constraint.bottom)
             (setf (.x (get-size dummy)) (.x port-size))
             (when (< port-border-offset 0)
               (setf (.y (get-size dummy)) (- port-border-offset)))
             (set-side dummy-port port-side.north)
             (unless explicit-anchor
               (setf (.y anchor) 0.0)))
            ;; Should never happen!
            (t nil))
          ;; Finally apply the anchor by setting the dummy port position accordingly. Also, remember the
          ;; anchor on the dummy itself since the hierarchical port processors depend on that
          (j-set (get-position dummy-port) anchor)
          (set-property dummy layered-options.port-anchor anchor)
          (when (is-order-fixed port-constraints)
            ;; The order of ports is fixed in some way, so what we will have to do is to remember
            ;; information about it
            (let ((information-about-it 0.0))
              (if (and (eq port-constraints port-constraints.fixed-order)
                       (has-property property-holder layered-options.port-index))
                  ;; On the SOUTH and WEST sides, the index is in reverse to what we would later
                  ;; expect in the code, so we'll use the index * -1 there
                  (enum-case final-external-port-side
                    ((port-side.north port-side.east)
                     (setf information-about-it
                           (jd (get-property property-holder layered-options.port-index))))
                    ((port-side.south port-side.west)
                     (setf information-about-it
                           (* -1 (jd (get-property property-holder layered-options.port-index))))))
                  ;; Otherwise, we will just go with the position itself
                  (enum-case final-external-port-side
                    ((port-side.west port-side.east)
                     (setf information-about-it (.y port-position))
                     (when (is-ratio-fixed port-constraints)
                       (setf information-about-it (/ information-about-it (.y port-node-size)))))
                    ((port-side.north port-side.south)
                     (setf information-about-it (.x port-position))
                     (when (is-ratio-fixed port-constraints)
                       (setf information-about-it (/ information-about-it (.x port-node-size)))))))
              (set-property dummy layered-internal-properties.port-ratio-or-position information-about-it)))
          ;; Set the port side of the dummy
          (set-property dummy layered-internal-properties.ext-port-side final-external-port-side)
          dummy)))))

(defjstatic l-graph-util.get-external-port-position (graph port-dummy port-width port-height)
  (let ((port-position (new-k-vector (get-position port-dummy))))
    (incf (.x port-position) (/ (.x (get-size port-dummy)) 2.0))
    (incf (.y port-position) (/ (.y (get-size port-dummy)) 2.0))
    (let ((port-offset (get-property port-dummy layered-options.port-border-offset))
          ;; Get some properties of the graph
          (graph-size (get-size graph))
          (padding (get-padding graph))
          (graph-offset (get-offset graph))
          (dpos (get-position port-dummy)))
      ;; The exact coordinates depend on the port's side...
      (enum-case (get-property port-dummy layered-internal-properties.ext-port-side)
        (port-side.north
         (incf (.x port-position) (- (+ (.left padding) (.x graph-offset)) (/ port-width 2.0)))
         (setf (.y port-position) (- (- port-height) port-offset))
         (setf (.y dpos) (- (+ (.top padding) port-offset (.y graph-offset)))))
        (port-side.east
         (setf (.x port-position) (+ (.x graph-size) (.left padding) (.right padding) port-offset))
         (incf (.y port-position) (- (+ (.top padding) (.y graph-offset)) (/ port-height 2.0)))
         (setf (.x dpos) (- (+ (.x graph-size) (.right padding) port-offset) (.x graph-offset))))
        (port-side.south
         (incf (.x port-position) (- (+ (.left padding) (.x graph-offset)) (/ port-width 2.0)))
         (setf (.y port-position) (+ (.y graph-size) (.top padding) (.bottom padding) port-offset))
         (setf (.y dpos) (- (+ (.y graph-size) (.bottom padding) port-offset) (.y graph-offset))))
        (port-side.west
         (setf (.x port-position) (- (- port-width) port-offset))
         (incf (.y port-position) (- (+ (.top padding) (.y graph-offset)) (/ port-height 2.0)))
         (setf (.x dpos) (- (+ (.left padding) port-offset (.x graph-offset)))))))
    port-position))

;;; ------------------------------------------------------------ Compound Graphs

(defjstatic l-graph-util.is-descendant (child parent)
  (let* ((current child)
         (next (get-parent-node (get-graph current))))
    (loop while next
          do (setf current next)
             (when (eq current parent)
               (return-from l-graph-util.is-descendant/2 t))
             (setf next (get-parent-node (get-graph current))))
    nil))

(defjstatic l-graph-util.change-coord-system (point old-graph new-graph)
  (when (eq old-graph new-graph)
    ;; nothing has to be done
    (return-from l-graph-util.change-coord-system/3 nil))
  ;; transform to absolute coordinates
  (let ((graph old-graph) (node nil))
    (loop
      (add point (get-offset graph))
      (setf node (get-parent-node graph))
      (when node
        (let ((padding (get-padding graph)))
          (add point (.left padding) (.top padding))
          (add point (get-position node))
          (setf graph (get-graph node))))
      (unless node (return)))
    ;; transform to relative coordinates (to newGraph)
    (setf graph new-graph)
    (loop
      (sub point (get-offset graph))
      (setf node (get-parent-node graph))
      (when node
        (let ((padding (get-padding graph)))
          (sub point (.left padding) (.top padding))
          (sub point (get-position node))
          (setf graph (get-graph node))))
      (unless node (return)))
    nil))

;;; ------------------------------------------------------------ Other Stuff

(defjstatic l-graph-util.get-individual-or-inherited (node property)
  (let ((result nil))
    (when (has-property node core-options.spacing-individual)
      (let ((individual-spacings (get-property node core-options.spacing-individual)))
        (when (has-property individual-spacings property)
          (setf result (get-property individual-spacings property)))))
    ;; use the common value
    (when (and (null result) (get-graph node))
      (setf result (get-property (get-graph node) property)))
    result))

(defjstatic l-graph-util.get-direction (graph)
  (let ((direction (get-property graph layered-options.direction)))
    (if (eq direction direction.undefined)
        (let ((aspect-ratio (get-property graph layered-options.aspect-ratio)))
          (if (>= aspect-ratio 1)
              ;; Default due to default value of the ASPECT_RATIO option for the algorithm
              direction.right
              direction.down))
        direction)))

(defjstatic l-graph-util.get-minimal-model-order (graph)
  (let ((order integer.max-value))
    (jdo (node (get-layerless-nodes graph))
      (when (has-property node layered-internal-properties.model-order)
        (setf order (min order (get-property node layered-internal-properties.model-order)))))
    order))
