;;;; compaction-components.lisp -- org.eclipse.elk.alg.layered.compaction.components

(in-package #:elk)

;;; IExternalExtension default method
(defjmethod get-placeholder ((this i-external-extension))
  nil)

;;; ------------------------------------------------------------ helpers

(defun %lm-sides-key (sides)
  "A canonical EQUAL key for a Set<PortSide>: the sorted ordinals."
  (let ((acc '()))
    (jdo (s sides) (pushnew (ordinal s) acc))
    (sort acc #'<)))

(defun %lm-sides-equal (a b)
  "Set<PortSide>.equals"
  (equal (%lm-sides-key a) (%lm-sides-key b)))

(defun %lm-multimap-put (table key value)
  "Multimap.put on a hash table KEY -> jlist (insertion ordered)."
  (multiple-value-bind (l found) (gethash key table)
    (unless found
      (setf l (new-jlist) (gethash key table) l))
    (jl-add l value)))

(defun %lm-multimap-get (table key)
  (or (gethash key table) (new-jlist 1)))

;;; ------------------------------------------------------------ ComponentsToCGraphTransformer

(defjctor components-to-c-graph-transformer (spacing)
  (setf (.spacing this) spacing))

(defjmethod get-offset ((this components-to-c-graph-transformer) c)
  (sub (clone (gethash c (.old-position this)))
       (get-position (.rect (gethash c (.offsets this))))))

(defjmethod get-global-offset ((this components-to-c-graph-transformer))
  (.global-offset this))

(defjmethod get-graph-size ((this components-to-c-graph-transformer))
  (.graph-size this))

(defjmethod get-external-extensions ((this components-to-c-graph-transformer))
  (.external-extensions this))

(defjmethod get-external-placeholder ((this components-to-c-graph-transformer))
  (.external-placeholder this))

(defjmethod transform ((this components-to-c-graph-transformer) ccs)
  (setf (.c-graph this) (new-layered-c-graph (enum-set.all-of 'core-direction)))
  (let ((c-graph (.c-graph this)))
    (jdo (comp (get-components ccs))
      (let ((group (new-layered-c-group)))
        (jl-add (.c-groups c-graph) group)
        ;; convert the hull of the graph's elements without external edges
        (jdo (rect (get-hull comp))
          (let ((rect-node (new-c-rect-node this rect)))
            (set-lock this rect-node (get-external-extension-sides comp))
            (unless (jm-contains-key (.old-position this) comp)
              (setf (gethash comp (.old-position this)) (new-k-vector (.x rect) (.y rect))
                    (gethash comp (.offsets this)) rect-node))
            (jl-add (.c-nodes c-graph) rect-node)
            (add-c-node group rect-node)))
        ;; now prepare the rectangles for the external extensions
        ;; they can be added to the CGraph on demand later on
        (jdo (ee (get-external-extensions comp))
          (let ((rect-node (new-c-rect-node this (get-representor ee))))
            (setf (gethash ee (.external-extensions this)) (pair.of group rect-node))
            (set-lock this rect-node (get-external-extension-sides comp))
            ;; placeholder
            (when (get-placeholder ee)
              (let ((rect-placeholder (new-c-rect-node this (get-placeholder ee) 1d0)))
                (set-lock this rect-placeholder (get-external-extension-sides comp))
                (let ((dummy-group (new-layered-c-group)))
                  (add-c-node dummy-group rect-placeholder))
                (%lm-multimap-put (.external-placeholder this) (get-direction ee)
                                  (pair.of group rect-placeholder))))))))
    c-graph))

(defjmethod set-lock ((this components-to-c-graph-transformer) c-node port-sides)
  (let ((lock (.lock c-node)))
    (flet ((is (sides) (%lm-sides-equal port-sides sides)))
      (when (is-empty port-sides)
        (j-set lock t t t t))
      ;; regulars
      (when (is port-side.sides-north) (j-set lock t t t nil))
      (when (is port-side.sides-east) (j-set lock nil t t t))
      (when (is port-side.sides-south) (j-set lock t t nil t))
      (when (is port-side.sides-west) (j-set lock t nil t t))
      ;; corners
      (when (is port-side.sides-north-east) (j-set lock nil t t nil))
      (when (is port-side.sides-east-south) (j-set lock nil t nil t))
      (when (is port-side.sides-south-west) (j-set lock t nil nil t))
      (when (is port-side.sides-north-west) (j-set lock t nil t nil))
      ;; opposite
      (when (is port-side.sides-north-south) (j-set lock t t t t))
      (when (is port-side.sides-east-west) (j-set lock t t t t))
      ;; triumvirates
      (when (is port-side.sides-north-south) (j-set lock t t t t))
      (when (is port-side.sides-east-south-west) (j-set lock t t t t))
      (when (is port-side.sides-north-south-west) (j-set lock t t t t))
      (when (is port-side.sides-north-east-west) (j-set lock t t t t))
      ;; all of them!
      (when (is port-side.sides-north-east-south-west) (j-set lock t t t t)))))

(defun %lm-extend-bounds (top-left bottom-right c-node)
  (let ((h (.hitbox c-node)))
    (setf (.x top-left) (math.min (.x top-left) (.x h))
          (.y top-left) (math.min (.y top-left) (.y h))
          (.x bottom-right) (math.max (.x bottom-right) (+ (.x h) (.width h)))
          (.y bottom-right) (math.max (.y bottom-right) (+ (.y h) (.height h))))))

(defjmethod apply-layout ((this components-to-c-graph-transformer))
  (let ((c-graph (.c-graph this)))
    (jdo (n (.c-nodes c-graph))
      (apply-element-position n))
    ;; calculating new graph size and offset
    (let ((top-left (new-k-vector double.positive-infinity double.positive-infinity))
          (bottom-right (new-k-vector double.negative-infinity double.negative-infinity)))
      (jdo (c-node (.c-nodes c-graph))
        (%lm-extend-bounds top-left bottom-right c-node))
      ;; note that placeholdes "can move" during compaction and are _not_ fixed to the boundary
      ;; which allows to use them to determine the maximum position
      (jdo (pairs (.external-placeholder this))
        (jdo (placeholder pairs)
          (%lm-extend-bounds top-left bottom-right (get-second placeholder))))
      (setf (.global-offset this) (negate (clone top-left))
            (.graph-size this) (sub (clone bottom-right) top-left)))
    ;; resetting lists
    (jl-clear (.c-groups c-graph))
    (jl-clear (.c-nodes c-graph))))

;;; CRectNode (inner class: OUTER is the transformer)

(defjctor c-rect-node (outer rect)
  (this-ctor c-rect-node this outer rect nil))

(defjctor c-rect-node (outer rect spacing)
  (setf (.outer this) outer
        (.rect this) rect
        (.hitbox this) (new-elk-rectangle (.x rect) (.y rect) (.width rect) (.height rect))
        (.individual-spacing this) spacing))

(defjmethod get-horizontal-spacing ((this c-rect-node))
  (or (.individual-spacing this) (.spacing (.outer this))))

(defjmethod get-vertical-spacing ((this c-rect-node))
  (or (.individual-spacing this) (.spacing (.outer this))))

(defjmethod apply-element-position ((this c-rect-node))
  (setf (.x (.rect this)) (.x (.hitbox this))
        (.y (.rect this)) (.y (.hitbox this))))

(defjmethod get-element-position ((this c-rect-node))
  (.x (.rect this)))

(defjmethod to-string ((this c-rect-node))
  "")

(defjmethod get-horizontal-spacing ((this %lm-components-spacing-handler) c-node1 c-node2)
  (math.min (get-horizontal-spacing c-node1) (get-horizontal-spacing c-node2)))
(defjmethod get-vertical-spacing ((this %lm-components-spacing-handler) c-node1 c-node2)
  (math.min (get-vertical-spacing c-node1) (get-vertical-spacing c-node2)))

(defjconst components-to-c-graph-transformer.spacing-handler
  (new-%lm-components-spacing-handler))

;;; ------------------------------------------------------------ OneDimensionalComponentsCompaction

(defjconst one-dimensional-components-compaction.max-iteration 10)
(defjconst one-dimensional-components-compaction.epsilon 0.0001d0)
(defjconst one-dimensional-components-compaction.left-right
  (list core-direction.left core-direction.right))
(defjconst one-dimensional-components-compaction.up-down
  (list core-direction.up core-direction.down))

(defjstatic one-dimensional-components-compaction.init (ccs spacing)
  (let ((compaction (new-one-dimensional-components-compaction)))
    (setf (.transformer compaction) (new-components-to-c-graph-transformer spacing))
    (setf (.compaction-graph compaction) (transform (.transformer compaction) ccs))
    compaction))

(defjmethod compact ((this one-dimensional-components-compaction) monitor)
  (declare (ignore monitor))
  ;; separate vertical and horizontal extensions
  (let ((all-nodes (new-jlist)))
    (setf (.vertical-external-extensions this) (new-jlist)
          (.horizontal-external-extensions this) (new-jlist))
    (jdo-map (ee pair (get-external-extensions (.transformer this)))
      (jl-add all-nodes (get-second pair))
      (if (is-horizontal (get-direction ee))
          (jl-add (.horizontal-external-extensions this) pair)
          (jl-add (.vertical-external-extensions this) pair)))
    ;; add the external edge representations prior to the creation
    ;; of the one dimensional compactor.
    ;; it is necessary for all group offsets to be calculated properly
    (add-external-edge-representations this (.horizontal-external-extensions this))
    (add-external-edge-representations this (.vertical-external-extensions this))
    ;; create our favorite compactor instance
    (setf (.compactor this) (new-layered-one-dimensional-compactor (.compaction-graph this)))
    (set-spacings-handler (.compactor this) components-to-c-graph-transformer.spacing-handler)
    (remove-external-edge-representations this (.horizontal-external-extensions this))
    (remove-external-edge-representations this (.vertical-external-extensions this))
    (jl-add-all all-nodes (.c-nodes (.c-graph (.compactor this))))
    ;; calculating new graph size and offset
    (setf (.top-left this) (new-k-vector double.positive-infinity double.positive-infinity)
          (.bottom-right this) (new-k-vector double.negative-infinity double.negative-infinity))
    ;; determine the current drawing's dimensions
    (jdo (c-node all-nodes)
      (%lm-extend-bounds (.top-left this) (.bottom-right this) c-node))
    ;; we "lock" nodes based on outgoing constraints, i.e. if there's a
    ;; node in the "opposite" shadow we do not move it away
    ;; remember do execute an initial unlocked compaction
    ;; in #compact() first, though
    (set-locking-strategy (.compactor this)
                          (lambda (pair) (/= (.out-degree (.c-group (get-first pair))) 0)))
    ;; now execute compaction until no improvement is made or we hit the maximum number of iterations
    (let ((run 0) (delta 0d0))
      (loop
        (setf delta (compact this run))
        (incf run)
        (unless (and (or (< run 2) (> delta one-dimensional-components-compaction.epsilon))
                     (< run one-dimensional-components-compaction.max-iteration))
          (return))))
    ;; "align" the compaction top left
    (set-locking-strategy (.compactor this)
                          (lambda (pair)
                            (or (j-get (.lock (get-first pair)) (get-second pair))
                                (and (/= (.out-degree (.c-group (get-first pair))) 0)
                                     (j-get (.lock (get-first pair)) (get-second pair))))))
    (compact this 0)
    ;; finish it!
    ;; make sure that the external edge representations have been removed from the
    ;; compaction graph at this point for correct graph size calculation
    (finish (.compactor this))
    ;; finally apply the determined positions
    (apply-layout (.transformer this))))

(defjmethod compact ((this one-dimensional-components-compaction) (run integer))
  (let ((delta 0d0)
        (compactor (.compactor this))
        (groups (.c-groups (.compaction-graph this))))
    ;; reset all groups' deltas
    (jdo (g groups)
      (setf (.delta g) 0d0 (.delta-normalized g) 0d0))
    ;; #1 We want to perform horizontal compaction first.
    ;;    For this we have to add the vertical external
    ;;    edges to the hulls to prevent edge node overlaps
    (add-placeholders this dir.horz)
    (add-external-edge-representations this (.vertical-external-extensions this))
    (calculate-group-offsets compactor)
    ;; there are new nodes in the compaction graph, we have to update the constraints
    (force-constraints-recalculation compactor)
    (let ((direction core-direction.left))
      ;; compact horizontally
      (compact (apply-locking-strategy (change-direction
       (compact (apply-locking-strategy (change-direction
        (compact (change-direction compactor direction))
        (opposite direction))))
       direction))))
    ;; very important to transform back to LEFT
    ;; because the hitboxes representing external edges
    ;; that will be added in a second have not been transformed yet
    (change-direction compactor core-direction.left)
    ;; remove the vertical external edges again
    (remove-external-edge-representations this (.vertical-external-extensions this))
    (remove-placeholders this dir.horz)
    ;; .. and offset the horizontal external edges (that will be added next)
    ;; according to the just finished horizontal compaction
    (update-external-extension-dimensions this dir.horz)
    (update-placeholders this dir.vert)
    ;; #2 We want to perform vertical compaction.
    ;; now add them
    (add-placeholders this dir.vert)
    (add-external-edge-representations this (.horizontal-external-extensions this))
    (calculate-group-offsets compactor)
    ;; remember delta from horizontal compaction
    (jdo (g groups)
      (incf delta (math.abs (.delta-normalized g))))
    ;; reset all groups' deltas
    (jdo (g groups)
      (setf (.delta g) 0d0 (.delta-normalized g) 0d0))
    (let ((direction core-direction.up))
      ;; compact vertically
      (compact (apply-locking-strategy (change-direction
       (compact (apply-locking-strategy (change-direction
        (compact (force-constraints-recalculation (change-direction compactor direction)))
        (opposite direction))))
       direction))))
    ;; transform back to LEFT
    (change-direction compactor core-direction.left)
    ;; remove the horizontal external edges
    (remove-external-edge-representations this (.horizontal-external-extensions this))
    (remove-placeholders this dir.vert)
    ;; ... offset the vertical external edges
    ;; (which have been excluded during the last compaction)
    (update-external-extension-dimensions this dir.vert)
    (update-placeholders this dir.horz)
    (force-constraints-recalculation compactor)
    ;; ... and the delta from vertical compaction
    (jdo (g groups)
      (incf delta (math.abs (.delta-normalized g))))
    delta))

(defjmethod get-graph-size ((this one-dimensional-components-compaction))
  (get-graph-size (.transformer this)))

(defjmethod get-offset ((this one-dimensional-components-compaction) c)
  (let ((individual (get-offset (.transformer this) c)))
    (add (negate individual) (get-global-offset (.transformer this)))))

(defjmethod add-external-edge-representations ((this one-dimensional-components-compaction) ees)
  (jdo (p ees)
    (jl-add (.c-nodes (.compaction-graph this)) (get-second p))
    (add-c-node (get-first p) (get-second p))))

(defjmethod remove-external-edge-representations ((this one-dimensional-components-compaction) ees)
  (jdo (p ees)
    (%lm-set-remove (.c-nodes (.compaction-graph this)) (get-second p))
    (remove-c-node (get-first p) (get-second p))))

(defun %lm-dirs (dir)
  (if (eq dir dir.vert)
      one-dimensional-components-compaction.up-down
      one-dimensional-components-compaction.left-right))

(defjmethod add-placeholders ((this one-dimensional-components-compaction) dir)
  (dolist (d (%lm-dirs dir))
    (jdo (pair (%lm-multimap-get (get-external-placeholder (.transformer this)) d))
      (jl-add (.c-nodes (.compaction-graph this)) (get-second pair))
      (jl-add (.c-groups (.compaction-graph this)) (.c-group (get-second pair))))))

(defjmethod remove-placeholders ((this one-dimensional-components-compaction) dir)
  (dolist (d (%lm-dirs dir))
    (jdo (pair (%lm-multimap-get (get-external-placeholder (.transformer this)) d))
      (%lm-set-remove (.c-nodes (.compaction-graph this)) (get-second pair))
      (%lm-set-remove (.c-groups (.compaction-graph this)) (.c-group (get-second pair))))))

(defjmethod update-placeholders ((this one-dimensional-components-compaction) dir)
  (dolist (d (%lm-dirs dir))
    (jdo (pair (%lm-multimap-get (get-external-placeholder (.transformer this)) d))
      (let* ((c-node (get-second pair))
             (parent-component-group (get-first pair))
             ;; deltaNormalized is negative if a group was moved to the left,
             ;;  positive if the group was moved to the right
             ;; the size of each component stays the same!
             (adelta (.delta-normalized parent-component-group)))
        (enum-case d
          ((core-direction.left core-direction.right)
           (incf (.y (.hitbox c-node)) adelta))
          ((core-direction.up core-direction.down)
           ;; y sticks to the top of the diagram
           (incf (.x (.hitbox c-node)) adelta)))))))

(defjmethod update-external-extension-dimensions ((this one-dimensional-components-compaction) dir)
  (jdo-map (ee pair (get-external-extensions (.transformer this)))
    (let ((ed (get-direction ee)))
      (when (if (eq dir dir.vert)
                (or (eq ed core-direction.up) (eq ed core-direction.down))
                (or (eq ed core-direction.left) (eq ed core-direction.right)))
        (let* ((c-node (get-second pair))
               ;; the node may not be associated with the group atm, thus cNode.group may be null
               (group (get-first pair))
               (adelta (.delta-normalized group))
               (h (.hitbox c-node)))
          (enum-case ed
            (core-direction.left
             ;; x sticks to the left of the diagram
             (setf (.x h) (.x (.top-left this))
                   (.width h) (math.max 1 (+ (.width h) adelta))))
            (core-direction.right
             (setf (.x h) (+ (.x h) adelta)
                   (.width h) (math.max 1 (- (.width h) adelta))))
            (core-direction.up
             ;; y sticks to the top of the diagram
             (setf (.y h) (.y (.top-left this))
                   (.height h) (math.max 1 (+ (.height h) adelta))))
            (core-direction.down
             (setf (.y h) (+ (.y h) adelta)
                   (.height h) (math.max 1 (- (.height h) adelta))))))))))
