;;;; components.lisp -- org.eclipse.elk.alg.layered.components
;;;;
;;;; Placement of connected components (AbstractGraphPlacer and its row, group
;;;; and model-order subclasses), ComponentsProcessor (splitting a graph into
;;;; components and combining them again) and ComponentsCompactor (the
;;;; compaction of placed components via the compaction.components package).

(in-package #:elk)
;; Call macros used before their definition in this file.
(define-jcall add-lgraph-element-bounds :instance)
(define-jcall place-group-components :instance)
(define-jcall portside-to-direction :instance)

;;; ------------------------------------------------------------ Set<PortSide> multimaps
;;
;; ArrayListMultimap / HashMultimap keyed by Set<PortSide> are EQUAL hash tables
;; keyed by the sorted ordinals of the set (see %lm-sides-key).  Each entry is
;; (original-set . jlist-of-values).  Key and value iteration is insertion order;
;; Java's HashMap order over EnumSet keys (identity hashes) is not reproducible
;; anyway.

(defun %cgp-put (table sides value)
  (let* ((key (%lm-sides-key sides))
         (entry (or (gethash key table)
                    (setf (gethash key table) (cons sides (new-jlist))))))
    (jl-add (cdr entry) value)
    value))

(defun %cgp-get (table sides)
  "Multimap.get: the live value list for SIDES, or a fresh empty list."
  (let ((entry (gethash (%lm-sides-key sides) table)))
    (if entry (cdr entry) (new-jlist))))

(defun %cgp-nonempty-p (table sides)
  (plusp (length (%cgp-get table sides))))

(defun %cgp-keys (table)
  (let ((out (new-jlist)))
    (maphash (lambda (k entry) (declare (ignore k)) (jl-add out (car entry))) table)
    out))

(defun %cgp-values (table)
  (let ((out (new-jlist)))
    (maphash (lambda (k entry)
               (declare (ignore k))
               (jdo (v (cdr entry)) (jl-add out v)))
             table)
    out))

(defmacro %cgp-table (&rest pairs)
  "A Multimap literal: each (key value) pair is a Multimap.put, in order."
  `(let ((%cgp-tbl (make-hash-table :test 'equal)))
     ,@(loop for (k v) in pairs collect `(%cgp-put %cgp-tbl ,k ,v))
     %cgp-tbl))

;;; ------------------------------------------------------------ AbstractGraphPlacer

(defjgeneric combine 3)

(defjmethod move-graphs ((this abstract-graph-placer) dest-graph source-graphs offsetx offsety)
  (jdo (source-graph source-graphs)
    (move-graph this dest-graph source-graph offsetx offsety)))

(defjmethod move-graph ((this abstract-graph-placer) dest-graph source-graph offsetx offsety)
  (let ((graph-offset (add (get-offset source-graph) offsetx offsety)))
    (jdo (node (get-layerless-nodes source-graph))
      (add (get-position node) graph-offset)
      (jdo (port (get-ports node))
        (jdo (edge (get-outgoing-edges port))
          (offset (get-bend-points edge) graph-offset)
          (let ((junction-points (get-property edge layered-options.junction-points)))
            (when junction-points
              (offset junction-points graph-offset)))
          (jdo (label (get-labels edge))
            (add (get-position label) graph-offset))))
      (jl-add (get-layerless-nodes dest-graph) node)
      (set-graph node dest-graph))))

(defjmethod offset-graphs ((this abstract-graph-placer) graphs offsetx offsety)
  (jdo (graph graphs)
    (offset-graph this graph offsetx offsety)))

(defjmethod offset-graph ((this abstract-graph-placer) graph offsetx offsety)
  (let ((graph-offset (new-k-vector/2 offsetx offsety)))
    (jdo (node (get-layerless-nodes graph))
      (add (get-position node) graph-offset)
      (jdo (port (get-ports node))
        (jdo (edge (get-outgoing-edges port))
          (offset (get-bend-points edge) graph-offset)
          (let ((junction-points (get-property edge layered-options.junction-points)))
            (when junction-points
              (offset junction-points graph-offset)))
          (jdo (label (get-labels edge))
            (add (get-position label) graph-offset)))))))

;;; ------------------------------------------------------------ ComponentGroup

;; CONSTRAINTS: Multimap<Set<PortSide>, Set<PortSide>> (generated from ComponentGroup.java)
(defparameter component-group.constraints
  (%cgp-table
   (port-side.sides-none port-side.sides-north-east-south-west)
   (port-side.sides-west port-side.sides-north-east-south-west)
   (port-side.sides-west port-side.sides-north-south-west)
   (port-side.sides-east port-side.sides-north-east-south)
   (port-side.sides-east port-side.sides-north-east-south-west)
   (port-side.sides-north port-side.sides-north-east-south-west)
   (port-side.sides-north port-side.sides-north-east-west)
   (port-side.sides-south port-side.sides-east-south-west)
   (port-side.sides-south port-side.sides-north-east-south-west)
   (port-side.sides-north-south port-side.sides-east-west)
   (port-side.sides-north-south port-side.sides-north-east-south-west)
   (port-side.sides-north-south port-side.sides-north-east-west)
   (port-side.sides-north-south port-side.sides-east-south-west)
   (port-side.sides-east-west port-side.sides-north-south)
   (port-side.sides-east-west port-side.sides-north-south-west)
   (port-side.sides-east-west port-side.sides-north-east-south)
   (port-side.sides-east-west port-side.sides-north-east-south-west)
   (port-side.sides-north-west port-side.sides-north-west)
   (port-side.sides-north-west port-side.sides-north-east-west)
   (port-side.sides-north-west port-side.sides-north-south-west)
   (port-side.sides-north-east port-side.sides-north-east)
   (port-side.sides-north-east port-side.sides-north-east-west)
   (port-side.sides-north-east port-side.sides-north-east-south)
   (port-side.sides-south-west port-side.sides-south-west)
   (port-side.sides-south-west port-side.sides-east-south-west)
   (port-side.sides-south-west port-side.sides-north-south-west)
   (port-side.sides-east-south port-side.sides-east-south)
   (port-side.sides-east-south port-side.sides-east-south-west)
   (port-side.sides-east-south port-side.sides-north-east-south)
   (port-side.sides-north-east-west port-side.sides-north)
   (port-side.sides-north-east-west port-side.sides-north-south)
   (port-side.sides-north-east-west port-side.sides-north-west)
   (port-side.sides-north-east-west port-side.sides-north-east)
   (port-side.sides-north-east-west port-side.sides-north-east-south-west)
   (port-side.sides-north-east-west port-side.sides-north-east-west)
   (port-side.sides-north-east-west port-side.sides-north-south-west)
   (port-side.sides-north-east-west port-side.sides-north-east-south)
   (port-side.sides-east-south-west port-side.sides-south)
   (port-side.sides-east-south-west port-side.sides-north-south)
   (port-side.sides-east-south-west port-side.sides-south-west)
   (port-side.sides-east-south-west port-side.sides-east-south)
   (port-side.sides-east-south-west port-side.sides-east-south-west)
   (port-side.sides-east-south-west port-side.sides-north-south-west)
   (port-side.sides-east-south-west port-side.sides-north-east-south)
   (port-side.sides-east-south-west port-side.sides-north-east-south-west)
   (port-side.sides-north-south-west port-side.sides-west)
   (port-side.sides-north-south-west port-side.sides-east-west)
   (port-side.sides-north-south-west port-side.sides-north-west)
   (port-side.sides-north-south-west port-side.sides-south-west)
   (port-side.sides-north-south-west port-side.sides-north-east-west)
   (port-side.sides-north-south-west port-side.sides-east-south-west)
   (port-side.sides-north-south-west port-side.sides-north-south-west)
   (port-side.sides-north-south-west port-side.sides-north-east-south-west)
   (port-side.sides-north-east-south port-side.sides-east)
   (port-side.sides-north-east-south port-side.sides-east-west)
   (port-side.sides-north-east-south port-side.sides-north-east)
   (port-side.sides-north-east-south port-side.sides-east-south)
   (port-side.sides-north-east-south port-side.sides-north-east-west)
   (port-side.sides-north-east-south port-side.sides-east-south-west)
   (port-side.sides-north-east-south port-side.sides-north-east-south)
   (port-side.sides-north-east-south port-side.sides-north-east-south-west)
   (port-side.sides-north-east-south-west port-side.sides-none)
   (port-side.sides-north-east-south-west port-side.sides-west)
   (port-side.sides-north-east-south-west port-side.sides-east)
   (port-side.sides-north-east-south-west port-side.sides-north)
   (port-side.sides-north-east-south-west port-side.sides-south)
   (port-side.sides-north-east-south-west port-side.sides-north-south)
   (port-side.sides-north-east-south-west port-side.sides-east-west)
   (port-side.sides-north-east-south-west port-side.sides-north-east-west)
   (port-side.sides-north-east-south-west port-side.sides-east-south-west)
   (port-side.sides-north-east-south-west port-side.sides-north-south-west)
   (port-side.sides-north-east-south-west port-side.sides-north-east-south)
   (port-side.sides-north-east-south-west port-side.sides-north-east-south-west)
   ))

(defjctor component-group (component)
  (add this component))

;; Component Management

(defjmethod add ((this component-group) component)
  (if (can-add this component)
      (progn
        (%cgp-put (.components this)
                  (get-property component layered-internal-properties.ext-port-connections)
                  component)
        t)
      nil))

(defjmethod can-add ((this component-group) component)
  ;; Check if we have a component with incompatible external port sides
  (let* ((candidate-sides (get-property component layered-internal-properties.ext-port-connections))
         (constraints (%cgp-get component-group.constraints candidate-sides)))
    (not (some (lambda (constraint) (%cgp-nonempty-p (.components this) constraint))
               constraints))))

(defjmethod get-port-sides ((this component-group))
  (%cgp-keys (.components this)))

(defjmethod get-components ((this component-group))
  (%cgp-values (.components this)))

(defjmethod get-components ((this component-group) connections)
  (%cgp-get (.components this) connections))

;;; ------------------------------------------------------------ ModelOrderComponentGroup

;; MODEL_ORDER_CONSTRAINTS (generated from ModelOrderComponentGroup.java)
(defparameter model-order-component-group.model-order-constraints
  (%cgp-table
   (port-side.sides-north port-side.sides-none)
   (port-side.sides-west port-side.sides-none)
   (port-side.sides-north-east port-side.sides-none)
   (port-side.sides-north-west port-side.sides-none)
   (port-side.sides-north-south-west port-side.sides-none)
   (port-side.sides-north-east-west port-side.sides-none)
   (port-side.sides-north-west port-side.sides-north)
   (port-side.sides-none port-side.sides-east)
   (port-side.sides-north port-side.sides-east)
   (port-side.sides-west port-side.sides-east)
   (port-side.sides-north-east port-side.sides-east)
   (port-side.sides-north-south port-side.sides-east)
   (port-side.sides-north-west port-side.sides-east)
   (port-side.sides-north-south-west port-side.sides-east)
   (port-side.sides-north-east-west port-side.sides-east)
   (port-side.sides-east-west port-side.sides-east)
   (port-side.sides-none port-side.sides-south)
   (port-side.sides-north port-side.sides-south)
   (port-side.sides-east port-side.sides-south)
   (port-side.sides-west port-side.sides-south)
   (port-side.sides-north-east port-side.sides-south)
   (port-side.sides-north-south port-side.sides-south)
   (port-side.sides-north-west port-side.sides-south)
   (port-side.sides-east-west port-side.sides-south)
   (port-side.sides-south-west port-side.sides-south)
   (port-side.sides-north-south-west port-side.sides-south)
   (port-side.sides-north-east-south port-side.sides-south)
   (port-side.sides-north-east-west port-side.sides-south)
   (port-side.sides-north port-side.sides-west)
   (port-side.sides-north-east port-side.sides-west)
   (port-side.sides-north-west port-side.sides-west)
   (port-side.sides-north-east-west port-side.sides-west)
   (port-side.sides-north port-side.sides-north-east)
   (port-side.sides-west port-side.sides-north-east)
   (port-side.sides-north-west port-side.sides-north-east)
   (port-side.sides-north-east port-side.sides-north-east)
   (port-side.sides-north-south-west port-side.sides-north-east)
   (port-side.sides-none port-side.sides-east-south)
   (port-side.sides-north port-side.sides-east-south)
   (port-side.sides-east port-side.sides-east-south)
   (port-side.sides-south port-side.sides-east-south)
   (port-side.sides-west port-side.sides-east-south)
   (port-side.sides-north-east port-side.sides-east-south)
   (port-side.sides-north-south port-side.sides-east-south)
   (port-side.sides-north-west port-side.sides-east-south)
   (port-side.sides-south-west port-side.sides-east-south)
   (port-side.sides-east-west port-side.sides-east-south)
   (port-side.sides-north-east-west port-side.sides-east-south)
   (port-side.sides-north-south-west port-side.sides-east-south)
   (port-side.sides-north-east-south-west port-side.sides-east-south)
   (port-side.sides-none port-side.sides-south-west)
   (port-side.sides-north port-side.sides-south-west)
   (port-side.sides-east port-side.sides-south-west)
   (port-side.sides-west port-side.sides-south-west)
   (port-side.sides-north-east port-side.sides-south-west)
   (port-side.sides-north-south port-side.sides-south-west)
   (port-side.sides-north-west port-side.sides-south-west)
   (port-side.sides-east-west port-side.sides-south-west)
   (port-side.sides-north-east-west port-side.sides-south-west)
   (port-side.sides-north-east-south port-side.sides-south-west)
   (port-side.sides-north-east-south-west port-side.sides-south-west)
   (port-side.sides-north port-side.sides-east-west)
   (port-side.sides-west port-side.sides-east-west)
   (port-side.sides-north-east port-side.sides-east-west)
   (port-side.sides-north-west port-side.sides-east-west)
   (port-side.sides-south-west port-side.sides-east-west)
   (port-side.sides-north-east-west port-side.sides-east-west)
   (port-side.sides-north-south-west port-side.sides-east-west)
   (port-side.sides-none port-side.sides-east-south-west)
   (port-side.sides-north port-side.sides-east-south-west)
   (port-side.sides-east port-side.sides-east-south-west)
   (port-side.sides-west port-side.sides-east-south-west)
   (port-side.sides-north-east port-side.sides-east-south-west)
   (port-side.sides-north-south port-side.sides-east-south-west)
   (port-side.sides-north-west port-side.sides-east-south-west)
   (port-side.sides-east-west port-side.sides-east-south-west)
   (port-side.sides-north-east-west port-side.sides-east-south-west)
   (port-side.sides-north port-side.sides-north-south-west)
   (port-side.sides-east port-side.sides-north-south-west)
   (port-side.sides-south port-side.sides-north-south-west)
   (port-side.sides-north-east port-side.sides-north-south-west)
   (port-side.sides-none port-side.sides-north-east-south)
   (port-side.sides-north port-side.sides-north-east-south)
   (port-side.sides-south port-side.sides-north-east-south)
   (port-side.sides-west port-side.sides-north-east-south)
   (port-side.sides-north-east port-side.sides-north-east-south)
   (port-side.sides-north-south port-side.sides-north-east-south)
   (port-side.sides-north-west port-side.sides-north-east-south)
   (port-side.sides-north-west port-side.sides-north-east-south-west)
   (port-side.sides-north-east port-side.sides-north-east-south-west)
   (port-side.sides-east-west port-side.sides-none)
   (port-side.sides-east-west port-side.sides-west)
   (port-side.sides-east-west port-side.sides-east)
   (port-side.sides-north-south port-side.sides-none)
   (port-side.sides-north-south port-side.sides-north)
   (port-side.sides-north-south port-side.sides-south)
   ))

(defjctor model-order-component-group (component)
  (add this component)
  (jl-add (.component-order this) component))

(defjmethod add ((this model-order-component-group) component)
  (if (can-add this component)
      (progn
        (%cgp-put (.components this)
                  (get-property component layered-internal-properties.ext-port-connections)
                  component)
        (jl-add (.component-order this) component)
        t)
      nil))

(defjmethod can-add ((this model-order-component-group) component)
  (let* ((candidate-sides (get-property component layered-internal-properties.ext-port-connections))
         (constraints (%cgp-get component-group.constraints candidate-sides))
         (model-order-constraints
           (%cgp-get model-order-component-group.model-order-constraints candidate-sides)))
    (and (not (some (lambda (constraint) (%cgp-nonempty-p (.components this) constraint))
                    constraints))
         (not (some (lambda (constraint) (%cgp-nonempty-p (.components this) constraint))
                    model-order-constraints)))))

(defjmethod get-component-order ((this model-order-component-group))
  (.component-order this))

;;; ------------------------------------------------------------ SimpleRowGraphPlacer

(defun %cgp-float-sqrt (x)
  "Java (float) Math.sqrt(x), widened back to double."
  (float (float (sqrt x) 1f0) 1d0))

(defjmethod combine ((this simple-row-graph-placer) components target)
  (cond
    ((= (length components) 1)
     (let ((source (jl-get components 0)))
       (unless (eq source target)
         (jl-clear (get-layerless-nodes target))
         (move-graph this target source 0d0 0d0)
         (copy-properties target source)
         (copy (get-padding target) (get-padding source))
         (setf (.x (get-size target)) (.x (get-size source))
               (.y (get-size target)) (.y (get-size source))))
       (return-from combine nil)))
    ((zerop (length components))
     (jl-clear (get-layerless-nodes target))
     (setf (.x (get-size target)) 0d0
           (.y (get-size target)) 0d0)
     (return-from combine nil)))
  ;; Sort components
  (sort-components this components target)
  (let ((first-component (jl-get components 0)))
    (jl-clear (get-layerless-nodes target))
    (copy-properties target first-component)
    ;; determine the maximal row width by the maximal box width and the total area
    (let ((max-row-width 0d0)
          (total-area 0d0))
      (jdo (graph components)
        (let ((size (get-size graph)))
          (setf max-row-width (math.max max-row-width (.x size)))
          (incf total-area (* (.x size) (.y size)))))
      (setf max-row-width
            (math.max max-row-width
                      (* (%cgp-float-sqrt total-area)
                         (get-property target layered-options.aspect-ratio))))
      (let ((component-spacing (get-property target layered-options.spacing-component-component)))
        (place-components this components target max-row-width component-spacing)
        ;; if compaction is desired, do so!
        (when (get-property first-component layered-options.compaction-connected-components)
          (let ((compactor (new-components-compactor/0)))
            (compact compactor components (get-size target) component-spacing)
            ;; the compaction algorithm places components absolutely,
            ;; therefore we have to use the final drawing's offset
            (jdo (h components)
              (add (reset (get-offset h)) (get-offset compactor)))
            ;; set the new graph size
            (add (reset (get-size target)) (get-graph-size compactor))))))
    ;; finally move the components to the combined graph
    (move-graphs this target components 0d0 0d0)))

(defjmethod sort-components ((this simple-row-graph-placer) components target)
  (when (eq (get-property target layered-options.consider-model-order-components)
            component-ordering-strategy.none)
    ;; assign priorities
    (jdo (graph components)
      (let ((priority 0))
        (jdo (node (get-layerless-nodes graph))
          (incf priority (get-property node layered-options.priority)))
        (setf (.id graph) priority)))
    ;; sort the components by their priority and size.
    (jl-sort components
             (lambda (graph1 graph2)
               (let ((prio (- (.id graph2) (.id graph1))))
                 (if (zerop prio)
                     (let ((size1 (* (.x (get-size graph1)) (.y (get-size graph1))))
                           (size2 (* (.x (get-size graph2)) (.y (get-size graph2)))))
                       (double.compare size1 size2))
                     prio))))))

(defjmethod place-components ((this simple-row-graph-placer) components target max-row-width
                              component-spacing)
  ;; place nodes iteratively into rows
  (let ((xpos 0d0) (ypos 0d0) (highest-box 0d0) (broadest-row component-spacing))
    (jdo (graph components)
      (let ((size (get-size graph)))
        (when (> (+ xpos (.x size)) max-row-width)
          ;; place the graph into the next row
          (setf xpos 0d0)
          (incf ypos (+ highest-box component-spacing))
          (setf highest-box 0d0))
        (let ((offset (get-offset graph)))
          (offset-graph this graph (+ xpos (.x offset)) (+ ypos (.y offset)))
          (reset offset))
        (setf broadest-row (math.max broadest-row (+ xpos (.x size))))
        (setf highest-box (math.max highest-box (.y size)))
        (incf xpos (+ (.x size) component-spacing))))
    (setf (.x (get-size target)) broadest-row
          (.y (get-size target)) (+ ypos highest-box))))

;;; ------------------------------------------------------------ ModelOrderRowGraphPlacer

(defjmethod sort-components ((this model-order-row-graph-placer) components target)
  ;; Does nothing since components are already sorted by model order
  nil)

(defjmethod place-components ((this model-order-row-graph-placer) components target
                              max-row-width component-spacing)
  ;; Place nodes iteratively into rows while considering the model order.
  (let ((xpos 0d0) (ypos 0d0) (highest-box 0d0) (broadest-row component-spacing)
        (last-component nil)
        (start-x-of-row 0d0))
    (jdo (graph components)
      (let ((size (get-size graph)))
        (when (or (and (> (+ xpos (.x size)) max-row-width)
                       (not (contains (get-property graph layered-internal-properties.ext-port-connections)
                                      port-side.north)))
                  (and last-component
                       (contains (get-property last-component layered-internal-properties.ext-port-connections)
                                 port-side.east))
                  (contains (get-property graph layered-internal-properties.ext-port-connections)
                            port-side.west))
          ;; Components with NORTH connection are allowed to violate the width constrain.
          ;; Place the graph into the next row.
          ;; Previous EAST ports require a new row.
          ;; A WEST port requires a new row.
          (setf xpos start-x-of-row)
          (incf ypos (+ highest-box component-spacing))
          (setf highest-box 0d0))
        (let ((offset (get-offset graph)))
          ;; North ports should be placed such that they don't intersect with prior components.
          (when (contains (get-property graph layered-internal-properties.ext-port-connections)
                          port-side.north)
            (setf xpos (+ broadest-row component-spacing)))
          (offset-graph this graph (+ xpos (.x offset)) (+ ypos (.y offset)))
          (setf broadest-row (math.max broadest-row (+ xpos (.x size))))
          ;; South ports block of everything below them.
          (when (contains (get-property graph layered-internal-properties.ext-port-connections)
                          port-side.south)
            (setf start-x-of-row (math.max start-x-of-row (+ xpos (.x size) component-spacing))))
          (reset offset))
        (setf highest-box (math.max highest-box (.y size)))
        (incf xpos (+ (.x size) component-spacing))
        (setf last-component graph)))
    (setf (.x (get-size target)) broadest-row
          (.y (get-size target)) (+ ypos highest-box))))

;;; ------------------------------------------------------------ ComponentGroupGraphPlacer

(defjmethod combine ((this component-group-graph-placer) components target)
  (combine-component-groups this components target nil))

(defjmethod combine ((this component-group-model-order-graph-placer) components target)
  (combine-component-groups this components target t))

(defun combine-component-groups (placer components target model-order-p)
  "ComponentGroupGraphPlacer.combine, and ComponentGroupModelOrderGraphPlacer.combine
when MODEL-ORDER-P (the two differ in group placement and the resulting size only)."
  (block combine-component-groups
  (setf (.component-groups placer) (new-jlist))
  (jl-clear (get-layerless-nodes target))
  ;; Check if there are any components to be placed
  (when (zerop (length components))
    (setf (.x (get-size target)) 0d0
          (.y (get-size target)) 0d0)
    (return-from combine-component-groups nil))
  ;; Set the graph properties
  (let ((first-component (jl-get components 0)))
    (copy-properties target first-component)
    ;; Construct component groups
    (jdo (component components)
      (add-component placer component))
    (let* ((component-spacing (get-property first-component layered-options.spacing-component-component))
           (groups (.component-groups placer)))
      (if (not model-order-p)
          (let ((offset (new-k-vector/0)))
            (jdo (group groups)
              ;; Place the components
              (let ((group-size (place-components-of-group placer group component-spacing)))
                (offset-graphs placer (get-components group) (.x offset) (.y offset))
                ;; Compute the new offset
                (incf (.x offset) (.x group-size))
                (incf (.y offset) (.y group-size))))
            ;; Set the graph's new size (the component group sizes include additional spacing
            ;; on the right and bottom sides which we need to subtract at this point)
            (setf (.x (get-size target)) (- (.x offset) component-spacing)
                  (.y (get-size target)) (- (.y offset) component-spacing)))
          (combine-model-order-groups placer components target first-component
                                      component-spacing groups))
      (finish-component-combination placer components target first-component
                                    component-spacing groups)))))

(defun combine-model-order-groups (placer components target first-component
                                   component-spacing groups)
  (declare (ignore components first-component))
  (let ((space-blocked-by-south-edges (new-k-vector/0))
        (space-blocked-by-components (new-k-vector/0))
        (offset (new-k-vector/0))
        (max-size (new-k-vector/0)))
    ;; In horizontal mode the next component is placed below the last one.
    ;; If it has a connection to NORTH, it has to be placed at an x-coordinate to not hit the other components.
    ;; Otherwise will be placed as far WEST as possible without potentially hitting SOUTH edges.
    (jdo (group groups)
      (let ((direction (get-property target core-options.direction)))
        ;; Place the components
        (cond
          ((is-horizontal direction)
           (setf (.x offset) (.x space-blocked-by-south-edges))
           (when (some (lambda (side) (contains side port-side.north)) (get-port-sides group))
             (setf (.x offset) (.x space-blocked-by-components))))
          ((is-vertical direction)
           (setf (.y offset) (.y space-blocked-by-south-edges))
           (when (some (lambda (side) (contains side port-side.west)) (get-port-sides group))
             (setf (.y offset) (.y space-blocked-by-components))))))
      (let ((group-size (place-components-of-group placer group component-spacing))
            (direction (get-property target core-options.direction)))
        (offset-graphs placer (get-components group) (.x offset) (.y offset))
        (cond
          ((is-horizontal direction)
           (setf (.x space-blocked-by-components) (+ (.x offset) (.x group-size)))
           (setf (.x max-size) (math.max (.x max-size) (.x space-blocked-by-components)))
           (when (some (lambda (side) (contains side port-side.south)) (get-port-sides group))
             (setf (.x space-blocked-by-south-edges) (+ (.x offset) (.x group-size))))
           (setf (.y space-blocked-by-components) (+ (.y offset) (.y group-size)))
           (setf (.y offset) (.y space-blocked-by-components))
           (setf (.y max-size) (math.max (.y max-size) (.y offset))))
          ((is-vertical direction)
           (setf (.y space-blocked-by-components) (+ (.y offset) (.y group-size)))
           (setf (.y max-size) (math.max (.y max-size) (.y space-blocked-by-components)))
           (when (some (lambda (side) (contains side port-side.east)) (get-port-sides group))
             (setf (.y space-blocked-by-south-edges) (+ (.y offset) (.y group-size))))
           (setf (.x space-blocked-by-components) (+ (.x offset) (.x group-size)))
           (setf (.x offset) (.x space-blocked-by-components))
           (setf (.x max-size) (math.max (.x max-size) (.x offset)))))))
    ;; Set the graph's new size (the component group sizes include additional spacing
    ;; on the right and bottom sides which we need to subtract at this point)
    (setf (.x (get-size target)) (- (.x max-size) component-spacing)
          (.y (get-size target)) (- (.y max-size) component-spacing))))

(defun finish-component-combination (placer components target first-component
                                     component-spacing groups)
  ;; if compaction is desired, do so!
  (when (and (get-property first-component layered-options.compaction-connected-components)
             (eq (get-property first-component layered-options.edge-routing) edge-routing.orthogonal))
    ;; apply graph offsets (which we reset later on)
    ;; since the compaction works in a common coordinate system
    (jdo (h components)
      (let ((o (get-offset h)))
        (offset-graph placer h (.x o) (.y o))))
    (let ((compactor (new-components-compactor/0)))
      (compact compactor components (get-size target) component-spacing)
      ;; the compaction algorithm places components absolutely,
      ;; therefore we have to use the final drawing's offset
      (jdo (h components)
        (add (reset (get-offset h)) (get-offset compactor)))
      ;; set the new (compacted) graph size
      (add (reset (get-size target)) (get-graph-size compactor))))
  ;; finally move the components to the combined graph
  (jdo (group groups)
    (move-graphs placer target (get-components group) 0d0 0d0)))

(defun place-components-of-group (placer group spacing)
  (place-group-components placer group spacing))

(defjmethod add-component ((this component-group-graph-placer) component)
  ;; Check if one of the existing component groups has some place left
  (jdo (group (.component-groups this))
    (when (add group component)
      (return-from add-component nil)))
  ;; Create a new component group for the component
  (jl-add (.component-groups this) (new-component-group/1 component))
  nil)

(defjmethod add-component ((this component-group-model-order-graph-placer) component)
  ;; Check if one of the existing component groups has some place left
  (when (> (length (.component-groups this)) 0)
    (let ((group (jl-get (.component-groups this) (1- (length (.component-groups this))))))
      (when (add group component)
        (return-from add-component nil))))
  ;; Create a new component group for the component
  (jl-add (.component-groups this) (new-model-order-component-group/1 component))
  nil)

(defjmethod place-group-components ((this component-group-graph-placer) group spacing)
  ;; Determine the spacing between two components
  ;; Place the different sector components and remember the amount of space their placement uses.
  ;; In this phase, we pretend that no other components are in the component group.
  (let* ((size-c (place-components-in-rows this (get-components group port-side.sides-none) spacing))
         (size-n (place-components-horizontally this (get-components group port-side.sides-north) spacing))
         (size-s (place-components-horizontally this (get-components group port-side.sides-south) spacing))
         (size-w (place-components-vertically this (get-components group port-side.sides-west) spacing))
         (size-e (place-components-vertically this (get-components group port-side.sides-east) spacing))
         (size-nw (place-components-horizontally this (get-components group port-side.sides-north-west) spacing))
         (size-ne (place-components-horizontally this (get-components group port-side.sides-north-east) spacing))
         (size-sw (place-components-horizontally this (get-components group port-side.sides-south-west) spacing))
         (size-se (place-components-horizontally this (get-components group port-side.sides-east-south) spacing))
         (size-we (place-components-vertically this (get-components group port-side.sides-east-west) spacing))
         (size-ns (place-components-horizontally this (get-components group port-side.sides-north-south) spacing))
         (size-nwe (place-components-horizontally this (get-components group port-side.sides-north-east-west) spacing))
         (size-swe (place-components-horizontally this (get-components group port-side.sides-east-south-west) spacing))
         (size-wns (place-components-vertically this (get-components group port-side.sides-north-south-west) spacing))
         (size-ens (place-components-vertically this (get-components group port-side.sides-north-east-south) spacing))
         (size-nesw (place-components-horizontally this (get-components group port-side.sides-north-east-south-west) spacing)))
    ;; Find the maximum height of the three rows and the maximum width of the three columns the
    ;; component group is divided into (we're adding a fourth row for WE components and a fourth
    ;; column for NS components to make the placement easier later)
    (let* ((col-left-width (elk-math.maxd (list (.x size-nw) (.x size-w) (.x size-sw) (.x size-wns))))
           (col-mid-width (elk-math.maxd (list (.x size-n) (.x size-c) (.x size-s) (.x size-nesw))))
           (col-ns-width (.x size-ns))
           (col-right-width (elk-math.maxd (list (.x size-ne) (.x size-e) (.x size-se) (.x size-ens))))
           (row-top-height (elk-math.maxd (list (.y size-nw) (.y size-n) (.y size-ne) (.y size-nwe))))
           (row-mid-height (elk-math.maxd (list (.y size-w) (.y size-c) (.y size-e) (.y size-nesw))))
           (row-we-height (.y size-we))
           (row-bottom-height (elk-math.maxd (list (.y size-sw) (.y size-s) (.y size-se) (.y size-swe)))))
      ;; With the individual placements computed, we now move the components to their final place,
      ;; taking the size of other component placements into account (the NW, NWE, and WNS components
      ;; stay at coordinates (0,0) and thus don't need to be moved around)
      (flet ((off (sides dx dy)
               (offset-graphs this (get-components group sides) dx dy)))
        (off port-side.sides-none (+ col-left-width col-ns-width) (+ row-top-height row-we-height))
        (off port-side.sides-north-east-south-west (+ col-left-width col-ns-width) (+ row-top-height row-we-height))
        (off port-side.sides-north (+ col-left-width col-ns-width) 0d0)
        (off port-side.sides-south (+ col-left-width col-ns-width) (+ row-top-height row-we-height row-mid-height))
        (off port-side.sides-west 0d0 (+ row-top-height row-we-height))
        (off port-side.sides-east (+ col-left-width col-ns-width col-mid-width) (+ row-top-height row-we-height))
        (off port-side.sides-north-east (+ col-left-width col-ns-width col-mid-width) 0d0)
        (off port-side.sides-south-west 0d0 (+ row-top-height row-we-height row-mid-height))
        (off port-side.sides-east-south (+ col-left-width col-ns-width col-mid-width) (+ row-top-height row-we-height row-mid-height))
        (off port-side.sides-east-west 0d0 row-top-height)
        (off port-side.sides-north-south col-left-width 0d0)
        (off port-side.sides-east-south-west 0d0 (+ row-top-height row-we-height row-mid-height))
        (off port-side.sides-north-east-south (+ col-left-width col-ns-width col-mid-width) 0d0))
      ;; Compute this component group's size
      (let ((component-size (new-k-vector/0)))
        (setf (.x component-size)
              (elk-math.maxd (list (+ col-left-width col-mid-width col-ns-width col-right-width)
                                   (.x size-we) (.x size-nwe) (.x size-swe))))
        (setf (.y component-size)
              (elk-math.maxd (list (+ row-top-height row-mid-height row-we-height row-bottom-height)
                                   (.y size-ns) (.y size-wns) (.y size-ens))))
        component-size))))


(defjmethod place-components-horizontally ((this component-group-graph-placer) components spacing)
  (let ((size (new-k-vector/0)))
    ;; Iterate over the components and place them
    (jdo (component components)
      (offset-graph this component (.x size) 0d0)
      (incf (.x size) (+ (.x (get-size component)) spacing))
      (setf (.y size) (math.max (.y size) (.y (get-size component)))))
    ;; Add vertical spacing, if necessary
    (when (> (.y size) 0d0)
      (incf (.y size) spacing))
    size))

(defjmethod place-components-vertically ((this component-group-graph-placer) components spacing)
  (let ((size (new-k-vector/0)))
    ;; Iterate over the components and place them
    (jdo (component components)
      (offset-graph this component 0d0 (.y size))
      (incf (.y size) (+ (.y (get-size component)) spacing))
      (setf (.x size) (math.max (.x size) (.x (get-size component)))))
    ;; Add horizontal spacing, if necessary
    (when (> (.x size) 0d0)
      (incf (.x size) spacing))
    size))

(defjmethod place-components-in-rows ((this component-group-graph-placer) components spacing)
  ;; This code is basically taken from the SimpleRowGraphPlacer.
  ;; Check if there actually are any components
  (when (zerop (length components))
    (return-from place-components-in-rows (new-k-vector/0)))
  ;; Determine the maximal row width by the maximal box width and the total area
  (let ((max-row-width 0d0)
        (total-area 0d0))
    (jdo (component components)
      (let ((component-size (get-size component)))
        (setf max-row-width (math.max max-row-width (.x component-size)))
        (incf total-area (* (.x component-size) (.y component-size)))))
    (setf max-row-width
          (math.max max-row-width
                    (* (%cgp-float-sqrt total-area)
                       (get-property (jl-get components 0) layered-options.aspect-ratio))))
    ;; Place nodes iteratively into rows
    (let ((xpos 0d0) (ypos 0d0) (highest-box 0d0) (broadest-row spacing))
      (jdo (graph components)
        (let ((size (get-size graph)))
          (when (> (+ xpos (.x size)) max-row-width)
            ;; Place the graph into the next row
            (setf xpos 0d0)
            (incf ypos (+ highest-box spacing))
            (setf highest-box 0d0))
          (offset-graph this graph xpos ypos)
          (setf broadest-row (math.max broadest-row (+ xpos (.x size))))
          (setf highest-box (math.max highest-box (.y size)))
          (incf xpos (+ (.x size) spacing))))
      (new-k-vector/2 (+ broadest-row spacing) (+ ypos highest-box spacing)))))

;;; ------------------------------------------------------------ ComponentsProcessor

(defjmethod split ((this layered-components-processor) lgraph)
  (let ((result nil))
    ;; Default to the simple graph placer
    (setf (.graph-placer this) (.simple-row-graph-placer this))
    ;; Whether separate components processing is requested
    (let* ((separate (get-property lgraph layered-options.separate-connected-components))
           ;; Whether the graph contains external ports
           (ext-ports (contains (get-property lgraph layered-internal-properties.graph-properties)
                                layered-graph-properties.external-ports))
           ;; The graph's external port constraints
           (ext-port-constraints (get-property lgraph layered-options.port-constraints))
           (compatible-port-constraints (not (is-order-fixed ext-port-constraints))))
      ;; The graph may only be separated
      ;;  1. Separation was requested.
      ;;  2. If the graph contains external ports, port constraints are set to either
      ;;     FREE or FIXED_SIDES.
      (if (and separate (or compatible-port-constraints (not ext-ports)))
          (progn
            ;; Set id of all nodes to 0
            (jdo (node (get-layerless-nodes lgraph))
              (setf (.id node) 0))
            ;; Perform DFS starting on each node, collecting connected components
            (setf result (new-jlist))
            (jdo (node (get-layerless-nodes lgraph))
              (let ((component-data (dfs this node nil)))
                (when component-data
                  (let ((new-graph (new-l-graph/0)))
                    (copy-properties new-graph lgraph)
                    (set-property new-graph layered-internal-properties.ext-port-connections
                                  (cdr component-data))
                    (copy (get-padding new-graph) (get-padding lgraph))
                    ;; If a minimum size was set on the original graph, setting it on the seperated graphs as well
                    ;; might enlarge them although their combined area might not actually have fallen below the
                    ;; minimum size; thus, remove the minimum size
                    (set-property new-graph layered-options.node-size-minimum nil)
                    (jdo (n (car component-data))
                      (jl-add (get-layerless-nodes new-graph) n)
                      (set-graph n new-graph))
                    (jl-add result new-graph)))))
            (when ext-ports
              ;; With external port connections, we want to use the more complex components
              ;; placement algorithm
              (let ((strategy (get-property lgraph layered-options.consider-model-order-components)))
                (setf (.graph-placer this)
                      (cond ((eq strategy component-ordering-strategy.group-model-order)
                             (.component-group-model-order-graph-placer this))
                            ((eq strategy component-ordering-strategy.model-order)
                             (.model-order-row-graph-placer this))
                            (t (.component-group-graph-placer this)))))))
          (setf result (jlist lgraph))))
    ;; If model order should be preserved the connected components should be ordered by their elements.
    ;; The component with the node with the smallest order should be first.
    (unless (eq (get-property lgraph layered-options.consider-model-order-components)
                component-ordering-strategy.none)
      (jl-sort result
               (lambda (g1 g2)
                 (integer.compare (l-graph-util.get-minimal-model-order g1)
                                  (l-graph-util.get-minimal-model-order g2)))))
    result))

(defjmethod dfs ((this layered-components-processor) node data)
  (if (eql (.id node) 0)
      (progn
        ;; Mark the node as visited
        (setf (.id node) 1)
        ;; Check if we already have a list of nodes for the connected component
        (let ((mutable-data data))
          (when (null mutable-data)
            (setf mutable-data (cons (new-jlist) (enum-set.none-of 'port-side))))
          ;; Add this node to the component
          (jl-add (car mutable-data) node)
          ;; Check if this node is an external port dummy and, if so, add its side
          (when (eq (get-type node) node-type.external-port)
            (add (cdr mutable-data)
                 (get-property node layered-internal-properties.ext-port-side)))
          ;; DFS
          (jdo (port1 (get-ports node))
            (jdo (port2 (get-connected-ports port1))
              (dfs this (get-node port2) mutable-data)))
          mutable-data))
      ;; The node was already visited
      nil))

(defjmethod combine ((this layered-components-processor) components target)
  (combine (.graph-placer this) components target))

;;; ------------------------------------------------------------ ComponentsCompactor

;; Hullpoints.add(Point), add(KVector), add(ElkRectangle): the Java overloads, by
;; argument type.
(defjmethod add ((this hullpoints) e)
  (typecase e
    (layered-point
     (%hp-add-point this e)
     t)
    (k-vector
     (%hp-add-point this (layered-point.from e))
     t)
    (elk-rectangle
     (let ((rv t))
       (setf rv (and (add this (get-position e)) rv))
       (setf rv (and (add this (add (get-position e) (.width e) 0d0)) rv))
       (setf rv (and (add this (add (get-position e) 0d0 (.height e))) rv))
       (setf rv (and (add this (add (get-position e) (.width e) (.height e))) rv))
       rv))
    (t (error "Hullpoints.add: unexpected argument ~A" e))))

(defun %hp-add-point (hp e)
  (let ((tl (.top-left hp)) (br (.bottom-right hp)))
    (setf (.x tl) (math.min (.x tl) (.x e))
          (.y tl) (math.min (.y tl) (.y e))
          (.x br) (math.max (.x br) (.x e))
          (.y br) (math.max (.y br) (.y e))))
  (jl-add (.items hp) e)
  t)

(defjmethod compact ((this components-compactor) graphs original-graphs-size spacing)
  (declare (ignore original-graphs-size))
  ;; determine the extreme points of the current diagram,
  ;; we will reuse this 'frame' to cut external extensions at appropriate lengths
  (setf (.graph-top-left this) (new-k-vector/2 double.positive-infinity double.positive-infinity)
        (.graph-bottom-right this) (new-k-vector/2 double.negative-infinity double.negative-infinity))
  (jdo (graph graphs)
    (jdo (node (get-layerless-nodes graph))
      (let ((pos (get-position node))
            (margin (get-margin node))
            (size (get-size node)))
        (setf (.x (.graph-top-left this)) (math.min (.x (.graph-top-left this)) (- (.x pos) (.left margin))))
        (setf (.y (.graph-top-left this)) (math.min (.y (.graph-top-left this)) (- (.y pos) (.top margin))))
        (setf (.x (.graph-bottom-right this))
              (math.max (.x (.graph-bottom-right this)) (+ (.x pos) (.x size) (.right margin))))
        (setf (.y (.graph-bottom-right this))
              (math.max (.y (.graph-bottom-right this)) (+ (.y pos) (.y size) (.bottom margin)))))))
  ;; from the lgraphs, create connected components
  (let ((ccs (new-internal-connected-components/0)))
    (jdo (graph graphs)
      (let ((c (transform-l-graph this graph)))
        (jl-add (.components ccs) c)
        (setf (.contains-regular-nodes c)
              (or (.contains-regular-nodes c)
                  (not (is-empty (get-external-extension-sides c)))))))
    ;; for every component we create an element in the compactor
    (setf (.compactor this) (one-dimensional-components-compaction.init ccs spacing))
    ;; execute compaction
    (compact (.compactor this) (new-basic-progress-monitor/0))
    (setf (.yet-another-offset this) (new-k-vector/0))
    (setf (.compacted-graph-size this) (get-graph-size (.compactor this)))
    ;; apply the positions
    (jdo (cc (.components ccs))
      ;; retrieve the common offset for the currently handled connected component
      (let ((offset (get-offset (.compactor this) cc)))
        ;; move it
        (l-graph-util.offset-graph (.graph cc) (.x offset) (.y offset))
        ;; adjust positions of external ports
        (jdo (n (get-nodes cc))
          (when (eq (get-type n) node-type.external-port)
            (let ((new-pos (get-external-port-position this (get-position n)
                                                      (get-property n layered-internal-properties.ext-port-side))))
              (add (reset (get-position n)) new-pos))))))
    ;; external edges contribute to the graph's size ... however, only certain segments do.
    (jdo (cc (.components ccs))
      (jdo (e (get-external-edges cc))
        (let ((vc (new-k-vector-chain/1 (get-bend-points e)))
              (last nil))
          (add vc 0 (get-absolute-anchor (get-source e)))
          (add vc (get-absolute-anchor (get-target e)))
          (jdo (v vc)
            (if (null last)
                (setf last v)
                (progn
                  (cond
                    ((%cc-fuzzy-equals (.x last) (.x v))
                     (setf (.x (.yet-another-offset this))
                           (math.min (.x (.yet-another-offset this)) (.x last)))
                     (setf (.x (.compacted-graph-size this))
                           (math.max (.x (.compacted-graph-size this)) (.x last))))
                    ((%cc-fuzzy-equals (.y last) (.y v))
                     (setf (.y (.yet-another-offset this))
                           (math.min (.y (.yet-another-offset this)) (.y last)))
                     (setf (.y (.compacted-graph-size this))
                           (math.max (.y (.compacted-graph-size this)) (.y last)))))
                  (setf last v)))))))
    (negate (.yet-another-offset this))
    (add (.compacted-graph-size this) (.yet-another-offset this))
    nil))

(defconstant +cc-epsilon+ 0.0001d0)

(defun %cc-fuzzy-equals (a b)
  (%lm-fuzzy-equals a b +cc-epsilon+))

(defjmethod get-offset ((this components-compactor))
  (.yet-another-offset this))

(defjmethod get-graph-size ((this components-compactor))
  (.compacted-graph-size this))

(defjmethod transform-l-graph ((this components-compactor) graph)
  (let ((component (new-internal-component/1 graph)))
    (unless (.contains-regular-nodes component)
      (create-dummy-node this graph))
    ;; #1 convert the nodes to hull points
    (let ((hull-points (component-hull-points this graph))
          ;; #2 convert external edges,
          ;;    while doing this, add further hull points contributed by inner segments
          ;;    and remember extreme points of external segments
          (external-extensions (new-jmap))
          (outer-segments (new-outer-segments/0)))
      (jdo (node (get-layerless-nodes graph))
        (jdo (edge (get-outgoing-edges node))
          (when (is-external-edge this edge)
            (let* ((iee (transform-l-edge this edge hull-points outer-segments))
                   (dir (get-direction iee))
                   (table external-extensions))
              (let ((entry (or (gethash dir table)
                               (setf (gethash dir table) (new-jlist)))))
                (unless (jl-contains entry (get-representative iee))
                  (jl-add entry (get-representative iee))))))))
      ;; #3 create the common external extensions for this component
      ;;    (there can be 4 per component, one in each direction)
      (let ((extensions (new-jlist)))
        (jdo (ps (get-external-extension-sides component))
          (let* ((min (aref (.min outer-segments) (ordinal ps)))
                 (max (aref (.max outer-segments) (ordinal ps)))
                 (extent (aref (.extent outer-segments) (ordinal ps)))
                 (extension nil)
                 (placeholder nil)
                 (tl (.top-left hull-points))
                 (br (.bottom-right hull-points))
                 (gtl (.graph-top-left this))
                 (gbr (.graph-bottom-right this)))
            (cond
              ((eq ps port-side.west)
               (setf extension (new-elk-rectangle/4 (.x gtl) min (- (.x tl) (.x gtl)) (- max min)))
               (setf placeholder (new-elk-rectangle/4 (.x gtl) min extent (- max min)))
               (add hull-points (get-top-right extension))
               (add hull-points (get-bottom-right extension)))
              ((eq ps port-side.east)
               (setf extension (new-elk-rectangle/4 (.x br) min (- (.x gbr) (.x br)) (- max min)))
               (setf placeholder (new-elk-rectangle/4 (- (.x gbr) extent) min extent (- max min)))
               (add hull-points (get-top-left extension))
               (add hull-points (get-bottom-left extension)))
              ((eq ps port-side.north)
               (setf extension (new-elk-rectangle/4 min (.y gtl) (- max min) (- (.y tl) (.y gtl))))
               (setf placeholder (new-elk-rectangle/4 min (.y gtl) (- max min) extent))
               (add hull-points (get-bottom-left extension))
               (add hull-points (get-bottom-right extension)))
              ((eq ps port-side.south)
               (setf extension (new-elk-rectangle/4 min (.y br) (- max min) (- (.y gbr) (.y br))))
               (setf placeholder (new-elk-rectangle/4 min (- (.y gbr) extent) (- max min) extent))
               (add hull-points (get-top-left extension))
               (add hull-points (get-top-right extension))))
            ;; instantiate the external extension object
            (when extension
              (let ((iuee (new-internal-union-external-extension/0)))
                (setf (.side iuee) ps
                      (.extension iuee) extension
                      (.placeholder iuee) placeholder)
                (let ((edges (new-jlist)))
                  (jdo (e (let ((l (gethash (portside-to-direction this ps) external-extensions)))
                            (or l (new-jlist))))
                    (unless (jl-contains edges e)
                      (jl-add edges e)))
                  (setf (.edges iuee) edges))
                (jl-add extensions iuee)))))
        ;; #4 calculate the hull for the component
        (jdo (ee extensions)
          (jl-add (.external-extensions component) ee))
        (setf (.rectilinear-convex-hull component)
              (split-into-rectangles (layered-rectilinear-convex-hull.of hull-points)))))
    component))

(defjmethod create-dummy-node ((this components-compactor) graph)
  ;; this can only be the case if there's a single external port
  ;;  to handle it properly, we give it a tiny little dummy rectangle
  (let* ((ext-port-dummy (jl-get (get-layerless-nodes graph) 0))
         ;; create a small dummy node for the external port dummy
         (dummy (new-l-node/1 graph)))
    (jl-add (get-layerless-nodes graph) dummy)
    ;; reassemble the size of the external dummy but assure
    ;;  that no dimension is zero. Otherwise we may end up
    ;;  with dimension-less hull rectangles which causes
    ;;  the scanline to fail
    (setf (.x (get-size dummy)) (math.max 1d0 (.x (get-size ext-port-dummy)))
          (.y (get-size dummy)) (math.max 1d0 (.y (get-size ext-port-dummy))))
    ;; set the position such that it is on the proper side
    ;; with respect to the dummy port
    (setf (.x (get-position dummy)) (.x (get-position ext-port-dummy))
          (.y (get-position dummy)) (.y (get-position ext-port-dummy)))
    (let ((side (get-property ext-port-dummy layered-internal-properties.ext-port-side)))
      (cond ((eq side port-side.west) (incf (.x (get-position dummy)) 2d0))
            ((eq side port-side.north) (incf (.y (get-position dummy)) 2d0))
            ((eq side port-side.east) (decf (.x (get-position dummy)) 2d0))
            ((eq side port-side.south) (decf (.y (get-position dummy)) 2d0))))
    ;; give the dummy a port and create an edge for it
    (let* ((dummy-port (new-l-port/0))
           (dummy-edge (new-l-edge/0))
           (ext-port-dummy-port (jl-get (get-ports ext-port-dummy) 0)))
      (set-node dummy-port dummy)
      (set-source dummy-edge ext-port-dummy-port)
      (set-target dummy-edge dummy-port)
      ;; position the dummy port such that the edge will be straight
      (add (reset (get-position dummy-port)) (get-position ext-port-dummy-port))
      (add (reset (get-anchor dummy-port)) (get-anchor ext-port-dummy-port)))
    dummy))

(defjmethod component-hull-points ((this components-compactor) graph)
  (let ((pts (new-hullpoints/0)))
    (jdo (n (get-layerless-nodes graph))
      (unless (eq (get-type n) node-type.external-port)
        ;; Note that labels of nodes are already part of a node's margins
        ;; the same is true for ports and their labels
        (add-lgraph-element-bounds this pts n (new-k-vector/0))
        ;; add bend points of the edges
        (jdo (edge (get-outgoing-edges n))
          (unless (is-external-edge this edge)
            (jdo (bp (get-bend-points edge))
              (add pts (new-layered-point/2 (.x bp) (.y bp))))))))
    pts))

(defjmethod add-lgraph-element-bounds ((this components-compactor) pts element offset)
  ;; extract the relevant margins object.
  (let ((margins (typecase element
                   (l-node (get-margin element))
                   (l-port (get-margin element))
                   (t (new-l-margin/0))))
        (pos (get-position element))
        (size (get-size element)))
    (add pts (new-layered-point/2 (+ (- (.x pos) (.left margins)) (.x offset))
                                  (+ (- (.y pos) (.top margins)) (.y offset))))
    (add pts (new-layered-point/2 (+ (- (.x pos) (.left margins)) (.x offset))
                                  (+ (.y pos) (.y size) (.bottom margins) (.y offset))))
    (add pts (new-layered-point/2 (+ (.x pos) (.x size) (.right margins) (.x offset))
                                  (+ (- (.y pos) (.top margins)) (.y offset))))
    (add pts (new-layered-point/2 (+ (.x pos) (.x size) (.right margins) (.x offset))
                                  (+ (.y pos) (.y size) (.bottom margins) (.y offset))))))

(defjmethod is-external-edge ((this components-compactor) edge)
  (or (eq (get-type (get-node (get-source edge))) node-type.external-port)
      (eq (get-type (get-node (get-target edge))) node-type.external-port)))

(defjmethod transform-l-edge ((this components-compactor) external-edge hull-points outer-segments)
  (let* ((external-extension (new-internal-external-extension/1 external-edge))
         ;; #1 convert the edge's path into a set of segments
         (segments (edge-to-segments this external-edge external-extension))
         ;; #2 all 'inner' segments contribute to the hull (consider the edge's thickness)
         (thickness (math.max (jd (get-property external-edge layered-options.edge-thickness)) 1d0)))
    (jdo (segment (.inner-segments segments))
      (let ((rect (segment-to-rectangle this (car segment) (cdr segment) thickness)))
        (add hull-points rect)))
    ;; #3 the 'outer' segment, being the segment that actually connects to the external port,
    ;;    contributes to the 'union external segment' that we create
    ;;    for one direction of the component
    (let* ((side (.external-port-side external-extension))
           (outer-segment-rect (segment-to-rectangle this (car (.outer-segment segments))
                                                     (cdr (.outer-segment segments)) thickness))
           (idx (ordinal side)))
      (if (or (eq side port-side.west) (eq side port-side.east))
          (progn
            (setf (aref (.min outer-segments) idx)
                  (math.min (aref (.min outer-segments) idx) (.y outer-segment-rect)))
            (setf (aref (.max outer-segments) idx)
                  (math.max (aref (.max outer-segments) idx)
                            (+ (.y outer-segment-rect) (.height outer-segment-rect)))))
          (progn
            (setf (aref (.min outer-segments) idx)
                  (math.min (aref (.min outer-segments) idx) (.x outer-segment-rect)))
            (setf (aref (.max outer-segments) idx)
                  (math.max (aref (.max outer-segments) idx)
                            (+ (.x outer-segment-rect) (.width outer-segment-rect))))))
      ;; extent
      (let* ((extent double.negative-infinity)
             (margins (get-margin (get-node (.external-port external-extension)))))
        (cond ((eq side port-side.west) (setf extent (.right margins)))
              ((eq side port-side.east) (setf extent (.left margins)))
              ((eq side port-side.north) (setf extent (.bottom margins)))
              ((eq side port-side.south) (setf extent (.top margins))))
        (setf (aref (.extent outer-segments) idx)
              (math.max (aref (.extent outer-segments) idx) extent))))
    external-extension))

(defjmethod segment-to-rectangle ((this components-compactor) p1 p2 extent)
  (new-elk-rectangle/4 (- (math.min (.x p1) (.x p2)) (/ extent 2d0))
                       (- (math.min (.y p1) (.y p2)) (/ extent 2d0))
                       (+ (abs (- (.x p1) (.x p2))) extent)
                       (+ (abs (- (.y p1) (.y p2))) extent)))

(defjmethod edge-to-segments ((this components-compactor) edge external-extension)
  (let* ((external-port (.external-port external-extension))
         (external-port-side (.external-port-side external-extension))
         ;; extract the correct segment that is to be used for the external extension
         (p1 (get-absolute-anchor (get-source edge)))
         (p2 (get-absolute-anchor (get-target edge))))
    ;; external ports of components may not be routed until the very border
    ;; of the diagram yet, hence we adjust their position here
    (if (eq external-port (get-source edge))
        (setf p1 (get-external-port-position this p1 external-port-side)
              p2 (get-port-position-on-margin this (get-target edge)))
        (setf p1 (get-port-position-on-margin this (get-source edge))
              p2 (get-external-port-position this p2 external-port-side)))
    (let ((points (new-k-vector-chain/1 (get-bend-points edge)))
          (outer-segment-is-first (eq (get-source edge) external-port))
          (segments (new-segments/0)))
      (add-first points p1)
      (add-last points p2)
      ;; for easy processing, make it a list of segments
      (dotimes (i (1- (size points)))
        (let ((segment (cons (j-get points i) (j-get points (1+ i)))))
          (if (or (and outer-segment-is-first (= i 0))
                  (and (not outer-segment-is-first) (= i (- (size points) 2))))
              (setf (.outer-segment segments) segment)
              (jl-add (.inner-segments segments) segment))))
      segments)))

(defjmethod get-external-port-position ((this components-compactor) pos ps)
  (cond
    ((eq ps port-side.north)
     (new-k-vector/2 (.x pos) (math.min (.y (.graph-top-left this)) (.y pos))))
    ((eq ps port-side.east)
     (new-k-vector/2 (math.max (.x (.graph-bottom-right this)) (.x pos)) (.y pos)))
    ((eq ps port-side.south)
     (new-k-vector/2 (.x pos) (math.max (.y (.graph-bottom-right this)) (.y pos))))
    ((eq ps port-side.west)
     (new-k-vector/2 (math.min (.x pos) (.x (.graph-top-left this))) (.y pos)))
    (t (clone pos))))

(defjmethod get-port-position-on-margin ((this components-compactor) port)
  (let ((pos (clone (get-absolute-anchor port)))
        (margins (get-margin (get-node port)))
        (side (get-side port)))
    (cond
      ((eq side port-side.north) (decf (.y pos) (.top margins)))
      ((eq side port-side.east) (incf (.x pos) (.right margins)))
      ((eq side port-side.south) (incf (.y pos) (.bottom margins)))
      ((eq side port-side.west) (decf (.x pos) (.left margins))))
    pos))

(defjmethod portside-to-direction ((this components-compactor) side)
  (cond
    ((eq side port-side.north) core-direction.up)
    ((eq side port-side.west) core-direction.left)
    ((eq side port-side.east) core-direction.right)
    ((eq side port-side.south) core-direction.down)
    (t core-direction.undefined)))

;;; ------------------------------------------------------------ inner classes

(defjmethod get-components ((this internal-connected-components))
  (.components this))

(defjmethod is-contains-external-extensions ((this internal-connected-components))
  (.contains-external-ports this))

(defjctor internal-component (graph)
  (setf (.graph this) graph)
  (setf (.contains-regular-nodes this) nil)
  (jdo (n (get-layerless-nodes graph))
    (when (eq (get-type n) node-type.normal)
      (setf (.contains-regular-nodes this) t))))

(defjmethod get-external-extension-sides ((this internal-component))
  (get-property (.graph this) layered-internal-properties.ext-port-connections))

(defjmethod get-hull ((this internal-component))
  (.rectilinear-convex-hull this))

(defjmethod get-external-extensions ((this internal-component))
  (.external-extensions this))

(defjmethod get-nodes ((this internal-component))
  (get-layerless-nodes (.graph this)))

(defjmethod get-external-edges ((this internal-component))
  (let ((edges (new-jlist)))
    (jdo (ee (.external-extensions this))
      (jdo (e (get-representative ee))
        (jl-add edges e)))
    edges))

(defjmethod get-direction ((this internal-union-external-extension))
  (portside-to-direction (new-components-compactor/0) (.side this)))

(defjmethod get-representative ((this internal-union-external-extension))
  (.edges this))

(defjmethod get-representor ((this internal-union-external-extension))
  (.extension this))

(defjmethod get-parent ((this internal-union-external-extension))
  (error "UnsupportedOperationException: InternalUnionExternalExtension.getParent"))

(defjmethod get-placeholder ((this internal-union-external-extension))
  (.placeholder this))

(defjctor internal-external-extension (edge)
  (setf (.edge this) edge)
  (cond
    ((eq (get-type (get-node (get-source edge))) node-type.external-port)
     (setf (.external-port this) (get-source edge))
     (setf (.external-port-side this)
           (get-property (get-node (get-source edge)) layered-internal-properties.ext-port-side)))
    ((eq (get-type (get-node (get-target edge))) node-type.external-port)
     (setf (.external-port this) (get-target edge))
     (setf (.external-port-side this)
           (get-property (get-node (get-target edge)) layered-internal-properties.ext-port-side)))
    (t (error "Edge ~A is not an external edge." edge))))

(defjmethod get-direction ((this internal-external-extension))
  (portside-to-direction (new-components-compactor/0) (.external-port-side this)))

(defjmethod get-representative ((this internal-external-extension))
  (.edge this))

(defjmethod get-representor ((this internal-external-extension))
  (.external-extension this))

(defjmethod get-parent ((this internal-external-extension))
  (.parent this))
