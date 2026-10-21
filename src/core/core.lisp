;;;; core.lisp -- org.eclipse.elk.core: AbstractLayoutProvider,
;;;; RecursiveGraphLayoutEngine, LayoutConfigurator.

(in-package #:elk)

;;; ---------------------------------------------------------------- AbstractLayoutProvider

(defjmethod initialize ((this abstract-layout-provider) parameter)
  parameter
  ;; do nothing - override in subclasses
  nil)

(defjmethod dispose ((this abstract-layout-provider))
  ;; do nothing - override in subclasses
  nil)

;;; ---------------------------------------------------------------- RecursiveGraphLayoutEngine

(defjmethod layout ((this recursive-graph-layout-engine) layout-graph progress-monitor)
  (layout this layout-graph nil progress-monitor))

(defjmethod layout ((this recursive-graph-layout-engine) layout-graph test-controller progress-monitor)
  (with-java-floats
    (let ((node-count (count-nodes-recursively this layout-graph t)))
      (begin progress-monitor "Recursive Graph Layout" node-count)
      (elk-util.apply-visitors layout-graph (vector (new-deprecated-layout-option-replacer)))
      (unless (has-property layout-graph core-options.resolved-algorithm)
        ;; Apply the default algorithm resolver to the graph in order to obtain algorithm meta data
        (elk-util.apply-visitors layout-graph (vector (new-layout-algorithm-resolver))))
      ;; Perform recursive layout of the whole substructure of the given node
      (layout-recursively this layout-graph test-controller progress-monitor)
      (done progress-monitor))))

(defun %core-children-empty-p (node)
  (is-empty (get-children node)))

(defjmethod layout-recursively ((this recursive-graph-layout-engine) layout-node test-controller
                                progress-monitor)
  (block nil
    (when (is-canceled progress-monitor)
      (return (new-jlist)))
    ;; Check if the node should be laid out at all
    (when (get-property layout-node core-options.no-layout)
      (return (new-jlist)))
    (let* (;; We have to process the node if it has children...
           (has-children (not (%core-children-empty-p layout-node)))
           ;; ...or if inside self loop processing is enabled and it actually has inside self loops
           (inside-self-loops (gather-inside-self-loops this layout-node))
           (has-inside-self-loops (not (is-empty inside-self-loops))))
      (unless (or has-children has-inside-self-loops)
        (return (new-jlist)))
      ;; Fetch the layout algorithm that should be used to compute a layout for its content
      (let ((algorithm-data (get-property layout-node core-options.resolved-algorithm)))
        (when (null algorithm-data)
          (error (new-unsupported-configuration-exception
                  (jstr "Resolved algorithm is not set;"
                        " apply a LayoutAlgorithmResolver before computing layout."))))
        (let ((supports-inside-self-loops (supports-feature algorithm-data graph-feature.inside-self-loops))
              (children-inside-self-loops (new-jlist))
              (node-count 0))
          ;; Persist the Hierarchy Handling in the nodes by querying the parent node
          (%rgle-evaluate-hierarchy-handling-inheritance layout-node)
          ;; If the node contains inside self loops, but no regular children and if the layout
          ;; algorithm doesn't actually support inside self loops, we cancel
          (when (and (not has-children) has-inside-self-loops (not supports-inside-self-loops))
            (return (new-jlist)))
          ;; If the layout provider supports hierarchy, it is expected to layout the node's compound
          ;; node children as well
          (if (and (eq (get-property layout-node core-options.hierarchy-handling)
                       hierarchy-handling.include-children)
                   (or (supports-feature algorithm-data graph-feature.compound)
                       (supports-feature algorithm-data graph-feature.clusters)))
              (progn
                ;; Topdown layout and hierarchy handling are incompatible
                (when (get-property layout-node core-options.topdown-layout)
                  (error (new-unsupported-configuration-exception
                          "Topdown layout cannot be used together with hierarchy handling.")))
                ;; The layout algorithm will compute a layout for multiple levels of hierarchy
                (setf node-count (%rgle-count-nodes-with-hierarchy layout-node))
                ;; Look for nodes that stop the hierarchy handling, evaluating the inheritance on the way
                (let ((node-queue (new-jlist)))
                  (jl-add-all node-queue (get-children layout-node))
                  (loop until (jl-is-empty node-queue)
                        do (let ((node (jl-poll-first node-queue)))
                             ;; Persist the Hierarchy Handling in every case.
                             (%rgle-evaluate-hierarchy-handling-inheritance node)
                             (let ((stop-hierarchy (eq (get-property node core-options.hierarchy-handling)
                                                       hierarchy-handling.separate-children)))
                               ;; Hierarchical layout is stopped by explicitly disabling or switching
                               ;; the algorithm. In that case, a separate recursive call is used.
                               (if (or stop-hierarchy
                                       (and (has-property node core-options.algorithm)
                                            (not (jequals algorithm-data
                                                          (get-property node core-options.resolved-algorithm)))))
                                   (let ((child-layout-self-loops
                                           (layout-recursively this node test-controller progress-monitor)))
                                     (jl-add-all children-inside-self-loops child-layout-self-loops)
                                     ;; Explicitly disable hierarchical layout for the child node.
                                     (set-property node core-options.hierarchy-handling
                                                   hierarchy-handling.separate-children)
                                     ;; Apply the LayoutOptions.SCALE_FACTOR if present
                                     (elk-util.apply-configured-node-scaling node))
                                   ;; Child should be included in current layout, possibly adding its own children
                                   (jl-add-all node-queue (get-children node))))))))
              (progn
                (setf node-count (size (get-children layout-node)))
                ;; If performing topdown layout, layout this node first, then compute scale factors
                ;; and layout children recursively
                (when (get-property layout-node core-options.topdown-layout)
                  (%rgle-topdown-layout this layout-node algorithm-data test-controller progress-monitor
                                        node-count))
                ;; Layout each compound node contained in this node separately
                (jdo (child (get-children layout-node))
                  (let ((child-layout-self-loops (layout-recursively this child test-controller progress-monitor)))
                    (jl-add-all children-inside-self-loops child-layout-self-loops)
                    ;; Apply the LayoutOptions.SCALE_FACTOR if present
                    (elk-util.apply-configured-node-scaling child)))))
          (when (is-canceled progress-monitor)
            (return (new-jlist)))
          ;; Before running layout on our node, we need to exclude any inside self loops of children
          ;; from being laid out again
          (jdo (self-loop children-inside-self-loops)
            (set-property self-loop core-options.no-layout t))
          (unless (get-property layout-node core-options.topdown-layout)
            (execute-algorithm this layout-node algorithm-data test-controller
                               (sub-task progress-monitor node-count)))
          ;; Post-process the inner self loops we collected
          (post-process-inside-self-loops this children-inside-self-loops)
          ;; Return our own inside self loops to be processed later
          (if (and has-inside-self-loops supports-inside-self-loops)
              inside-self-loops
              (new-jlist)))))))

(defun %rgle-topdown-layout (this layout-node algorithm-data test-controller progress-monitor node-count)
  "The 'if (layoutNode.getProperty(CoreOptions.TOPDOWN_LAYOUT))' block of layoutRecursively."
  (let ((topdown-layout-monitor (sub-task progress-monitor 1)))
    (begin topdown-layout-monitor "Topdown Layout" 1)
    ;; If we are currently in a hierarchical node and about to produce a child layout,
    ;; then we need to step through the children and set their sizes
    ;; only if their layout is performed by a topdown layout provider
    (when (null (get-property layout-node core-options.topdown-node-type))
      (error (new-unsupported-configuration-exception
              (jstr (get-identifier layout-node) " has not been assigned a top-down node type."))))
    (when (or (eq (get-property layout-node core-options.topdown-node-type) topdown-node-types.hierarchical-node)
              (eq (get-property layout-node core-options.topdown-node-type) topdown-node-types.root-node))
      (jdo (child-node (get-children layout-node))
        ;; check if child has children and whether it will be laid out with a topdown layout
        ;; provider if yes its size needs to be pre-computed before computing the layout
        (let ((local-algorithm-data (get-property child-node core-options.resolved-algorithm))
              (padding (get-property child-node core-options.padding)))
          (cond ((and (> (size (get-children child-node)) 0)
                      (typep (fetch (get-instance-pool local-algorithm-data)) 'i-topdown-layout-provider))
                 ;; topdownlayout providers should not be used on hierarchical nodes
                 (when (eq (get-property child-node core-options.topdown-node-type)
                           topdown-node-types.hierarchical-node)
                   (error (new-unsupported-configuration-exception
                           "Topdown Layout Providers should only be used on parallel nodes.")))
                 (let* ((topdown-layout-provider (fetch (get-instance-pool local-algorithm-data)))
                        (required-size (get-predicted-graph-size topdown-layout-provider child-node)))
                   (set-dimensions child-node (math.max (get-width child-node) (.x required-size))
                                   (math.max (get-height child-node) (.y required-size)))))
                ((and (get-property child-node core-options.topdown-size-approximator)
                      (get-children child-node)
                      (> (size (get-children child-node)) 0))
                 (let* ((approximator (get-property child-node core-options.topdown-size-approximator))
                        (size (get-size approximator child-node)))
                   (set-dimensions child-node
                                   (math.max (get-width child-node)
                                             (+ (.x size) (.left padding) (.right padding)))
                                   (math.max (get-height child-node)
                                             (+ (.y size) (.top padding) (.bottom padding))))))
                (t
                 ;; If no approximator is set, use the set sizes for atomic nodes and use the properties
                 ;; that have been set for nodes containing further children
                 (when (/= (size (get-children child-node)) 0)
                   (let ((size (new-k-vector
                                (get-property child-node core-options.topdown-hierarchical-node-width)
                                (/ (get-property child-node core-options.topdown-hierarchical-node-width)
                                   (get-property child-node core-options.topdown-hierarchical-node-aspect-ratio)))))
                     (set-dimensions child-node
                                     (math.max (get-width child-node)
                                               (+ (.x size) (.left padding) (.right padding)))
                                     (math.max (get-height child-node)
                                               (+ (.y size) (.top padding) (.bottom padding)))))))))))
    (let* ((padding (get-property layout-node core-options.padding))
           (child-area-available-width (- (get-width layout-node) (+ (.left padding) (.right padding))))
           (child-area-available-height (- (get-height layout-node) (+ (.top padding) (.bottom padding)))))
      (j-log topdown-layout-monitor (jstr "Available Child Area: (" child-area-available-width
                                        "|" child-area-available-height ")"))
      (set-property layout-node core-options.aspect-ratio
                    (/ (jd child-area-available-width) child-area-available-height))
      (execute-algorithm this layout-node algorithm-data test-controller (sub-task progress-monitor node-count))
      ;; root node needs its size to be set manually
      (when (eq (get-property layout-node core-options.topdown-node-type) topdown-node-types.root-node)
        (elk-util.compute-child-area-dimensions layout-node)
        (set-dimensions layout-node
                        (+ (.left padding) (get-property layout-node core-options.child-area-width)
                           (.right padding))
                        (+ (.top padding) (get-property layout-node core-options.child-area-height)
                           (.bottom padding))))
      (j-log topdown-layout-monitor (jstr "Executed layout algorithm: "
                                        (get-property layout-node core-options.algorithm)
                                        " on node " (get-identifier layout-node)))
      (when (eq (get-property layout-node core-options.topdown-node-type) topdown-node-types.hierarchical-node)
        (when (or (< child-area-available-width 0) (< child-area-available-height 0))
          ;; Hierarchical node width and/or aspect ratio of parent set to be smaller than paddings
          (error (new-unsupported-configuration-exception
                  (jstr "The size defined by the parent parallel node"
                        " is too small for the space provided by the paddings of the child hierarchical"
                        " node. " (get-identifier layout-node)))))
        ;; check whether child area has been set, and if it hasn't run the util function
        ;; to determine the size of the area
        (unless (or (has-property layout-node core-options.child-area-width)
                    (has-property layout-node core-options.child-area-height))
          (elk-util.compute-child-area-dimensions layout-node))
        (let* ((child-area-desired-width (get-property layout-node core-options.child-area-width))
               (child-area-desired-height (get-property layout-node core-options.child-area-height))
               ;; compute scaleFactor
               (scale-factor-x (/ (jd child-area-available-width) child-area-desired-width))
               (scale-factor-y (/ (jd child-area-available-height) child-area-desired-height))
               (scale-factor (math.min scale-factor-x
                                       (math.min scale-factor-y
                                                 (get-property layout-node core-options.topdown-scale-cap))))
               (content-alignment nil)
               (alignment-shift-x 0d0)
               (alignment-shift-y 0d0))
          (j-log topdown-layout-monitor (jstr "Desired Child Area: (" child-area-desired-width
                                            "|" child-area-desired-height ")"))
          (set-property layout-node core-options.topdown-scale-factor scale-factor)
          (j-log topdown-layout-monitor (jstr (get-identifier layout-node) " -- Local Scale Factor (X|Y): ("
                                            scale-factor-x "|" scale-factor-y ")"))
          ;; content alignment
          (setf content-alignment (get-property layout-node core-options.content-alignment))
          ;; horizontal alignment
          (when (< scale-factor scale-factor-x)
            (cond ((contains content-alignment content-alignment.h-center)
                   (setf alignment-shift-x (/ (- (/ child-area-available-width 2)
                                                 (/ (* child-area-desired-width scale-factor) 2))
                                              scale-factor)))
                  ((contains content-alignment content-alignment.h-right)
                   (setf alignment-shift-x (/ (- child-area-available-width
                                                 (* child-area-desired-width scale-factor))
                                              scale-factor)))))
          ;; vertical alignment
          (when (< scale-factor scale-factor-y)
            (cond ((contains content-alignment content-alignment.v-center)
                   (setf alignment-shift-y (/ (- (/ child-area-available-height 2)
                                                 (/ (* child-area-desired-height scale-factor) 2))
                                              scale-factor)))
                  ((contains content-alignment content-alignment.v-bottom)
                   (setf alignment-shift-y (/ (- child-area-available-height
                                                 (* child-area-desired-height scale-factor))
                                              scale-factor)))))
          (let ((x-shift (+ alignment-shift-x (- (/ (.left padding) scale-factor) (.left padding))))
                (y-shift (+ alignment-shift-y (- (/ (.top padding) scale-factor) (.top padding)))))
            (j-log topdown-layout-monitor (jstr "Shift: (" x-shift "|" y-shift ")"))
            ;; shift all nodes in layout
            (jdo (node (get-children layout-node))
              (set-x node (+ (get-x node) x-shift))
              (set-y node (+ (get-y node) y-shift)))
            ;; shift all edges
            (jdo (edge (get-contained-edges layout-node))
              (jdo (section (get-sections edge))
                (set-start-location section (+ (get-start-x section) x-shift) (+ (get-start-y section) y-shift))
                (set-end-location section (+ (get-end-x section) x-shift) (+ (get-end-y section) y-shift))
                (jdo (bend-point (get-bend-points section))
                  (j-set bend-point (+ (get-x bend-point) x-shift) (+ (get-y bend-point) y-shift))))
              ;; shift edge labels
              (jdo (label (get-labels edge))
                (set-location label (+ (get-x label) x-shift) (+ (get-y label) y-shift)))
              ;; shift junction points
              (let ((junction-points (get-property edge core-options.junction-points)))
                (jdo (junction-point junction-points)
                  (incf (.x junction-point) x-shift)
                  (incf (.y junction-point) y-shift))
                (set-property edge core-options.junction-points junction-points)))))))
    (done topdown-layout-monitor)))

(defjmethod execute-algorithm ((this recursive-graph-layout-engine) layout-node algorithm-data
                               test-controller progress-monitor)
  ;; Get an instance of the layout provider
  (let ((layout-provider (fetch (get-instance-pool algorithm-data)))
        (ok nil))
    ;; If we have a test controller and the layout algorithm supports test controllers, setup the test
    (when (and test-controller (targets test-controller algorithm-data))
      (install test-controller layout-provider))
    (unwind-protect
         (progn
           ;; Perform layout on the current hierarchy level
           (layout layout-provider layout-node progress-monitor)
           (release (get-instance-pool algorithm-data) layout-provider)
           (setf ok t))
      ;; The layout provider has failed - destroy it slowly and painfully
      (unless ok
        (dispose layout-provider))
      (when test-controller
        (uninstall test-controller)))))

(defjmethod count-nodes-recursively ((this recursive-graph-layout-engine) layout-node count-ancestors)
  ;; count the content of the given node
  (let ((count (size (get-children layout-node))))
    (jdo (child-node (get-children layout-node))
      (unless (%core-children-empty-p child-node)
        (incf count (count-nodes-recursively this child-node nil))))
    ;; count the ancestors path
    (when count-ancestors
      (let ((parent (get-parent layout-node)))
        (loop while parent
              do (incf count (size (get-children parent)))
                 (setf parent (get-parent parent)))))
    count))

;;; Hierarchy Handling

(defun %rgle-evaluate-hierarchy-handling-inheritance (layout-node)
  "Evaluates one level of inheritance for HIERARCHY_HANDLING.  A root set to inherit
(or not set at all) is set to SEPARATE_CHILDREN."
  (when (eq (get-property layout-node core-options.hierarchy-handling) hierarchy-handling.inherit)
    (if (null (get-parent layout-node))
        ;; Set root node to separate children handling
        (set-property layout-node core-options.hierarchy-handling hierarchy-handling.separate-children)
        ;; Set hierarchy handling to the value of the parent.  It is safe to assume that the
        ;; parent has been handled before and is not set to INHERIT anymore.
        (let ((parent-handling (get-property (get-parent layout-node) core-options.hierarchy-handling)))
          (set-property layout-node core-options.hierarchy-handling parent-handling)))))

(defun %rgle-count-nodes-with-hierarchy (parent-node)
  ;; Count the content of the given node
  (let ((count (size (get-children parent-node))))
    (jdo (child-node (get-children parent-node))
      (unless (eq (get-property child-node core-options.hierarchy-handling) hierarchy-handling.separate-children)
        (let ((parent-data (get-property parent-node core-options.resolved-algorithm))
              (child-data (get-property child-node core-options.resolved-algorithm)))
          ;; Only count nodes that don't abort the hierarchical layout
          (when (and (or (eq parent-data child-data) (and parent-data (jequals parent-data child-data)))
                     (not (%core-children-empty-p child-node)))
            (incf count (%rgle-count-nodes-with-hierarchy child-node))))))
    count))

;;; Inside Self Loops

(defjmethod gather-inside-self-loops ((this recursive-graph-layout-engine) node)
  (if (get-property node core-options.inside-self-loops-activate)
      (let ((inside-self-loops (new-jlist)))
        (jdo (edge (elk-graph-util.all-outgoing-edges node))
          ;; MIGRATE Adapt to hyperedges and make error-safe
          (when (is-selfloop edge)
            (when (get-property edge core-options.inside-self-loops-yo)
              (jl-add inside-self-loops edge))))
        inside-self-loops)
      (new-jlist)))

(defjmethod post-process-inside-self-loops ((this recursive-graph-layout-engine) inside-self-loops)
  (jdo (self-loop inside-self-loops)
    ;; MIGRATE Adapt to hyperedges and make error-safe
    (let* ((node (elk-graph-util.connectable-shape-to-node (j-get (get-sources self-loop) 0)))
           (x-offset (get-x node))
           (y-offset (get-y node))
           ;; Offset the edge coordinates by the node's position
           (section (j-get (get-sections self-loop) 0)))
      (set-start-location section (+ (get-start-x section) x-offset) (+ (get-start-y section) y-offset))
      (set-end-location section (+ (get-end-x section) x-offset) (+ (get-end-y section) y-offset))
      (jdo (bend (get-bend-points section))
        (j-set bend (+ (get-x bend) x-offset) (+ (get-y bend) y-offset)))
      ;; Offset junction points by the node position
      (offset (get-property self-loop core-options.junction-points) x-offset y-offset))))

;;; ---------------------------------------------------------------- LayoutConfigurator

(defproperty layout-configurator.add-layout-config "org.eclipse.elk.addLayoutConfig")

(defjconst layout-configurator.no-overwrite-holder
  (lambda (e p) (not (has-property e p))))

(defjconst layout-configurator.no-overwrite
  (lambda (e p) (not (has-property e p))))

(defjconst layout-configurator.option-target-filter
  (lambda (e property)
    (let ((option-data (get-option-data (layout-meta-data-service.get-instance) (get-id property))))
      (if option-data
          (let ((targets (get-targets option-data)))
            (cond ((typep e 'elk-node)
                   (if (not (is-hierarchical e))
                       (contains targets core-target.nodes)
                       (or (contains targets core-target.nodes)
                           (contains targets core-target.parents))))
                  ((typep e 'elk-edge) (contains targets core-target.edges))
                  ((typep e 'elk-port) (contains targets core-target.ports))
                  ((typep e 'elk-label) (contains targets core-target.labels))
                  (t t)))
          t))))

(defjmethod is-clear-layout ((this layout-configurator)) (.clear-layout this))

(defjmethod set-clear-layout ((this layout-configurator) do-clear-layout)
  (setf (.clear-layout this) do-clear-layout)
  this)

(defjmethod add-filter ((this layout-configurator) filter)
  (jl-add (.option-filters this) filter)
  this)

(defjmethod get-filters ((this layout-configurator)) (.option-filters this))

;; configure(ElkGraphElement) / configure(Class<? extends ElkGraphElement>):
;; a class is passed as its class-name symbol, e.g. (configure c 'elk-node).
(defjmethod configure ((this layout-configurator) element)
  (let ((map (if (symbolp element) (.class-option-map this) (.element-option-map this))))
    (or (gethash element map)
        (setf (gethash element map) (new-map-property-holder)))))

(defjmethod j-get-properties ((this layout-configurator) element)
  (values (gethash element (if (symbolp element) (.class-option-map this) (.element-option-map this)))))

(defun %lc-clear-properties (element)
  "element.getProperties().clear()"
  (if (typep element 'map-property-holder)
      (when (.property-map element) (clrhash (.property-map element)))
      (clear (j-get-properties element))))

(defjmethod visit ((this layout-configurator) element)
  (when (.clear-layout this)
    (%lc-clear-properties element))
  (let ((combined (%lc-find-class-options this element)))
    ;; implicitly overwrite options specified for a class with options specified for the specific element
    (copy-properties combined (j-get-properties this element))
    (apply-properties this element combined)))

(defjmethod apply-properties ((this layout-configurator) element properties)
  (when properties
    (let ((filters (get-filters this)))
      (jdo-map (key value (get-all-properties properties))
        (when (every (lambda (filter) (funcall filter element key)) filters)
          (when (cloneable-p value)
            (let ((clone (jclone value)))
              (when clone (setf value clone))))
          (set-property element key value))))))

(defun %lc-find-class-options (this element)
  (let ((combined (new-map-property-holder))
        (map (.class-option-map this)))
    (flet ((copy-from (class) (copy-properties combined (gethash class map))))
      ;; order is important as it reflects inheritance
      (block nil
        (when (typep element 'elk-graph-element) (copy-from 'elk-graph-element))
        (when (typep element 'elk-shape) (copy-from 'elk-shape))
        (when (typep element 'elk-label)
          (copy-from 'elk-label)
          ;; cannot be anything of the following, we can return
          (return))
        (when (typep element 'elk-connectable-shape) (copy-from 'elk-connectable-shape))
        (when (typep element 'elk-node)
          (copy-from 'elk-node)
          (return))
        (when (typep element 'elk-port)
          (copy-from 'elk-port)
          (return))
        (when (typep element 'elk-edge) (copy-from 'elk-edge))))
    combined))

(defjmethod override-with ((this layout-configurator) other)
  (flet ((merge-map (mine theirs)
           (jdo-map (k holder theirs)
             (let ((this-holder (gethash k mine)))
               (unless this-holder
                 (setf this-holder (new-map-property-holder))
                 (setf (gethash k mine) this-holder))
               (copy-properties this-holder holder)))))
    (merge-map (.element-option-map this) (.element-option-map other))
    (merge-map (.class-option-map this) (.class-option-map other)))
  (setf (.clear-layout this) (.clear-layout other))
  (jl-clear (.option-filters this))
  (jl-add-all (.option-filters this) (.option-filters other))
  this)
