;;;; org.eclipse.elk.alg.force -- ElkGraphImporter, ComponentsProcessor, ForceLayoutProvider

(in-package #:elk)

(defun %force-f32 (x)
  "Java (float) cast of a double, widened back to double."
  (jd (coerce x 'single-float)))

;;; ---------------------------------------------------------------- ElkGraphImporter

(defjmethod import-graph ((this force-elk-graph-importer) kgraph)
  (let ((fgraph (new-f-graph)))
    ;; copy the properties of the KGraph to the force graph
    (copy-properties fgraph kgraph)
    (set-property fgraph force-internal-properties.origin kgraph)
    ;; keep a list of created nodes in the force graph
    (let ((elem-map (new-jmap)))
      ;; transform everything
      (%force-transform-nodes kgraph fgraph elem-map)
      (%force-transform-edges kgraph fgraph elem-map))
    fgraph))

(defun %force-transform-nodes (parent-node fgraph elem-map)
  (let ((index 0))
    (jdo (knode (get-children parent-node))
      (let ((label ""))
        (unless (is-empty (get-labels knode))
          (setf label (get-text (j-get (get-labels knode) 0))))
        (let ((new-node (new-f-node label)))
          (copy-properties new-node knode)
          (set-property new-node force-internal-properties.origin knode)
          (setf (.id new-node) index)
          (incf index)
          (setf (.x (get-position new-node)) (+ (get-x knode) (/ (get-width knode) 2))
                (.y (get-position new-node)) (+ (get-y knode) (/ (get-height knode) 2))
                (.x (get-size new-node)) (jd (math.max (get-width knode) 1.0))
                (.y (get-size new-node)) (jd (math.max (get-height knode) 1.0)))
          (jl-add (get-nodes fgraph) new-node)
          (jm-put elem-map knode new-node)
          ;; port constraints cannot be undefined (the value is unused in ELK too)
          (let ((port-constraints (get-property knode force-options.port-constraints)))
            (when (eq port-constraints port-constraints.undefined)
              (setf port-constraints port-constraints.free))
            port-constraints))))))

(defun %force-transform-edges (parent-node fgraph elem-map)
  (jdo (knode (get-children parent-node))
    (jdo (kedge (elk-graph-util.all-outgoing-edges knode))
      ;; We don't support hyperedges
      (when (is-hyperedge kedge)
        (jthrow 'unsupported-graph-exception "Graph must not contain hyperedges."))
      ;; exclude edges that pass hierarchy bounds as well as self-loops
      (when (and (not (is-hierarchical kedge))
                 (not (eq knode (elk-graph-util.connectable-shape-to-node (j-get (get-targets kedge) 0)))))
        (let ((new-edge (new-f-edge)))
          (copy-properties new-edge kedge)
          (set-property new-edge force-internal-properties.origin kedge)
          (set-source new-edge (jm-get elem-map knode))
          (set-target new-edge (jm-get elem-map (elk-graph-util.connectable-shape-to-node
                                                 (j-get (get-targets kedge) 0))))
          (jl-add (get-edges fgraph) new-edge)
          ;; transform the edge's labels
          (jdo (klabel (get-labels kedge))
            (let ((new-label (new-f-label new-edge (get-text klabel))))
              (copy-properties new-label klabel)
              (set-property new-label force-internal-properties.origin klabel)
              (setf (.x (get-size new-label)) (jd (math.max (get-width klabel) 1.0))
                    (.y (get-size new-label)) (jd (math.max (get-height klabel) 1.0)))
              (refresh-position new-label)
              (jl-add (get-labels fgraph) new-label))))))))

(defjmethod apply-layout ((this force-elk-graph-importer) fgraph)
  (let ((kgraph (get-property fgraph force-internal-properties.origin))
        ;; calculate the offset from border spacing and node distribution
        (min-x-pos (jd integer.max-value))
        (min-y-pos (jd integer.max-value))
        (max-x-pos (jd integer.min-value))
        (max-y-pos (jd integer.min-value)))
    (flet ((extend (particle)
             (let ((pos (get-position particle)) (size (get-size particle)))
               (setf min-x-pos (math.min min-x-pos (- (.x pos) (/ (.x size) 2)))
                     min-y-pos (math.min min-y-pos (- (.y pos) (/ (.y size) 2)))
                     max-x-pos (math.max max-x-pos (+ (.x pos) (/ (.x size) 2)))
                     max-y-pos (math.max max-y-pos (+ (.y pos) (/ (.y size) 2)))))))
      (jdo (node (get-nodes fgraph)) (extend node))
      (jdo (bendpoint (get-bendpoints fgraph)) (extend bendpoint)))
    (let* ((padding (get-property kgraph force-options.padding))
           (offset (new-k-vector (- (get-left padding) min-x-pos) (- (get-top padding) min-y-pos))))
      ;; process the nodes
      (jdo (fnode (get-nodes fgraph))
        (let ((object (get-property fnode force-internal-properties.origin)))
          (when (typep object 'elk-node)
            (let ((node-pos (add (new-k-vector (get-position fnode)) offset)))
              (set-location object (- (.x node-pos) (/ (get-width object) 2))
                            (- (.y node-pos) (/ (get-height object) 2)))))))
      ;; process the edges
      (jdo (fedge (get-edges fgraph))
        (let* ((kedge (get-property fedge force-internal-properties.origin))
               (kedge-section (elk-graph-util.first-edge-section kedge t t))
               (start-location (new-k-vector (get-source-point fedge))))
          ;; Add bendpoints
          (add start-location offset)
          (set-start-location kedge-section (.x start-location) (.y start-location))
          (jdo (bp (get-bendpoints fedge))
            (let ((position (new-k-vector (get-position bp))))
              (add position offset)
              (elk-graph-util.create-bend-point kedge-section (.x position) (.y position))))
          (let ((end-location (new-k-vector (get-target-point fedge))))
            (add end-location offset)
            (set-end-location kedge-section (.x end-location) (.y end-location)))))
      ;; process the labels
      (jdo (flabel (get-labels fgraph))
        (let* ((klabel (get-property flabel force-internal-properties.origin))
               (label-pos (add (new-k-vector (get-position flabel)) offset)))
          (set-location klabel (.x label-pos) (.y label-pos))))
      ;; set up the parent node
      (let ((width (+ (- max-x-pos min-x-pos) (get-horizontal padding)))
            (height (+ (- max-y-pos min-y-pos) (get-vertical padding))))
        (unless (get-property kgraph core-options.node-size-fixed-graph-size)
          (elk-util.resize-node kgraph width height nil t))
        (set-property kgraph core-options.child-area-width (- width (get-horizontal padding)))
        (set-property kgraph core-options.child-area-height (- height (get-vertical padding)))))))

;;; ---------------------------------------------------------------- ComponentsProcessor

(defjmethod split ((this force-components-processor) graph)
  (let ((separate (get-property graph force-options.separate-connected-components)))
    ;; Java: separate == null || separate
    (if separate
        (let ((visited (make-array (length (get-nodes graph)) :initial-element nil))
              (incidence (%force-build-incidence-lists graph))
              ;; perform DFS starting on each node, collecting connected components
              (components (new-jlist)))
          (jdo (node (get-nodes graph))
            (let ((comp (%force-dfs node nil nil visited incidence)))
              (when comp
                (copy-properties comp graph)
                (jl-add components comp))))
          ;; redistribute identifier numbers to each component
          (when (> (length components) 1)
            (jdo (comp components)
              (let ((id 0))
                (jdo (node (get-nodes comp))
                  (setf (.id node) id)
                  (incf id)))))
          components)
        (jlist graph))))

(defun %force-build-incidence-lists (graph)
  (let* ((n (length (get-nodes graph)))
         (incidence (make-array n :initial-element nil)))
    ;; create incidence lists
    (jdo (node (get-nodes graph))
      (setf (aref incidence (.id node)) (new-jlist)))
    ;; add edges to incidence lists
    (jdo (edge (get-edges graph))
      (jl-add (aref incidence (.id (get-source edge))) edge)
      (jl-add (aref incidence (.id (get-target edge))) edge))
    incidence))

(defun %force-dfs (node last graph visited incidence)
  (unless (aref visited (.id node))
    (setf (aref visited (.id node)) t)
    (let ((component (or graph (new-f-graph))))
      (jl-add (get-nodes component) node)
      (jdo (edge (aref incidence (.id node)))
        ;; Do not handle again the edge we just arrived from
        (unless (or (eq (get-target edge) last) (eq (get-source edge) last))
          (unless (eq (get-source edge) node)
            (%force-dfs (get-source edge) node component visited incidence))
          (unless (eq (get-target edge) node)
            (%force-dfs (get-target edge) node component visited incidence))
          (jl-add (get-edges component) edge)
          (jl-add-all (get-labels component) (get-labels edge))))
      component)))

(defun %force-bb-size (graph)
  (sub (clone (get-property graph force-internal-properties.bb-lowright))
       (get-property graph force-internal-properties.bb-upleft)))

(defjmethod recombine ((this force-components-processor) components)
  (cond
    ((= (size components) 1) (j-get components 0))
    ((<= (size components) 0) (new-f-graph))
    (t
     ;; assign priorities and sizes
     (jdo (graph components)
       (let ((priority 0)
             (minx (jd integer.max-value)) (miny (jd integer.max-value))
             (maxx (jd integer.min-value)) (maxy (jd integer.min-value)))
         (jdo (node (get-nodes graph))
           (incf priority (get-property node force-options.priority))
           ;; careful: the (x,y) of an FNode refers to its center
           (let ((pos (get-position node)) (size (get-size node)))
             (setf minx (math.min minx (- (.x pos) (/ (.x size) 2)))
                   miny (math.min miny (- (.y pos) (/ (.y size) 2)))
                   maxx (math.max maxx (+ (.x pos) (/ (.x size) 2)))
                   maxy (math.max maxy (+ (.y pos) (/ (.y size) 2))))))
         (set-property graph force-options.priority priority)
         (set-property graph force-internal-properties.bb-upleft (new-k-vector minx miny))
         (set-property graph force-internal-properties.bb-lowright (new-k-vector maxx maxy))))
     ;; sort the components by their priority and size
     (jl-sort components
              (lambda (graph1 graph2)
                (let ((prio (- (get-property graph2 force-options.priority)
                               (get-property graph1 force-options.priority))))
                  (if (= prio 0)
                      (let ((size1 (%force-bb-size graph1))
                            (size2 (%force-bb-size graph2)))
                        (double.compare (* (.x size1) (.y size1)) (* (.x size2) (.y size2))))
                      prio))))
     (let ((result (new-f-graph))
           (max-row-width 0.0)
           (total-area 0.0))
       (copy-properties result (j-get components 0))
       ;; determine the maximal row width by the maximal box width and the total area
       (jdo (graph components)
         (let ((size (%force-bb-size graph)))
           (setf max-row-width (math.max max-row-width (.x size)))
           (incf total-area (* (.x size) (.y size)))))
       (setf max-row-width (math.max max-row-width
                                     (* (%force-f32 (math.sqrt total-area))
                                        (get-property result force-options.aspect-ratio))))
       (let* ((spacing (jd (get-property result force-options.spacing-node-node)))
              ;; place nodes iteratively into rows
              (xpos 0.0) (ypos 0.0) (highest-box 0.0) (broadest-row spacing))
         (jdo (graph components)
           (let ((size (%force-bb-size graph)))
             (when (> (+ xpos (.x size)) max-row-width)
               ;; place the graph into the next row
               (setf xpos 0.0)
               (incf ypos (+ highest-box spacing))
               (setf highest-box 0.0))
             (%force-move-graph result graph xpos ypos)
             (setf broadest-row (math.max broadest-row (+ xpos (.x size))))
             (setf highest-box (math.max highest-box (.y size)))
             (incf xpos (+ (.x size) spacing))))
         broadest-row)
       result))))

(defun %force-move-graph (dest-graph source-graph offsetx offsety)
  (let ((graph-offset (new-k-vector offsetx offsety)))
    (sub graph-offset (get-property source-graph force-internal-properties.bb-upleft))
    (jdo (node (get-nodes source-graph))
      (add (get-position node) graph-offset)
      (jl-add (get-nodes dest-graph) node))
    (jdo (edge (get-edges source-graph))
      (jdo (bendpoint (get-bendpoints edge))
        (add (get-position bendpoint) graph-offset))
      (jl-add (get-edges dest-graph) edge))
    (jdo (label (get-labels source-graph))
      (add (get-position label) graph-offset)
      (jl-add (get-labels dest-graph) label))))

;;; ---------------------------------------------------------------- ForceLayoutProvider

(defjmethod layout ((this force-layout-provider) elk-graph progress-monitor)
  (begin progress-monitor "ELK Force" 1)
  ;; if requested, compute nodes's dimensions, place node labels, ports, port labels, etc.
  (unless (get-property elk-graph force-options.omit-node-micro-layout)
    (execute (node-micro-layout.for-graph elk-graph)))
  ;; transform the input graph
  (let* ((graph-importer (new-force-elk-graph-importer))
         (fgraph (import-graph graph-importer elk-graph)))
    ;; set special properties for the layered graph
    (%force-set-options fgraph elk-graph)
    ;; update the force model depending on user selection
    (%force-update-model this (get-property fgraph force-options.model))
    ;; split the input graph into components
    (let ((components (split (.components-processor this) fgraph)))
      ;; perform the actual layout
      (jdo (comp components)
        (layout (.force-model this) comp
                (sub-task progress-monitor (%force-f32 (/ 1.0f0 (jsize components))))))
      ;; pack the components back into one graph
      (setf fgraph (recombine (.components-processor this) components)))
    ;; apply the layout results to the original graph
    (apply-layout graph-importer fgraph))
  (done progress-monitor))

(defun %force-set-options (fgraph parent)
  (declare (ignore parent))
  ;; set the random number generator based on the random seed option
  (let ((random-seed (get-property fgraph force-options.random-seed)))
    (if random-seed
        (if (= random-seed 0)
            (set-property fgraph force-internal-properties.random (new-random))
            (set-property fgraph force-internal-properties.random (new-random random-seed)))
        (set-property fgraph force-internal-properties.random (new-random 1)))))

(defun %force-update-model (this strategy)
  (enum-case strategy
    (force-model-strategy.eades
     (unless (typep (.force-model this) 'eades-model)
       (setf (.force-model this) (new-eades-model))))
    (force-model-strategy.fruchterman-reingold
     (unless (typep (.force-model this) 'fruchterman-reingold-model)
       (setf (.force-model this) (new-fruchterman-reingold-model))))))
