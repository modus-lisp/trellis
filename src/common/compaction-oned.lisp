;;;; compaction-oned.lisp -- org.eclipse.elk.alg.common.compaction.oned methods
;;;; CGraph, CGroup(+Builder), CNode(+Builder), CompareFuzzy, Quadruplet,
;;;; LongestPathCompaction, QuadraticConstraintCalculation, OneDimensionalCompactor.
;;;; (ScanlineConstraintCalculator and its handler are in compaction-scanline.lisp.)

(in-package #:elk)

;;; ------------------------------------------------------------ ISpacingsHandler

(defjmethod get-horizontal-spacing ((this common-default-spacings-handler) c1 c2)
  (declare (ignore c1 c2))
  0)

(defjmethod get-vertical-spacing ((this common-default-spacings-handler) c1 c2)
  (declare (ignore c1 c2))
  0)

(defparameter common-i-spacings-handler.default-spacing-handler
  (new-common-default-spacings-handler))

;;; ------------------------------------------------------------ CGraph

(defjctor common-c-graph (supported-directions)
  (setf (.supported-directions this) supported-directions))

(defjmethod supports ((this common-c-graph) direction)
  (js-contains-or-member (.supported-directions this) direction))

(defun js-contains-or-member (coll x)
  (if (jset-p coll) (js-contains coll x) (and (position x coll) t)))

;;; ------------------------------------------------------------ CGroup

(defjstatic common-c-group.of ()
  (new-common-c-group-builder))

(defun %cg-add-cnode (group c-node)
  "LinkedHashSet.add plus the owner bookkeeping of CGroup.addCNode."
  (when (common-c-node.c-group-of c-node)
    (jthrow 'runtime-exception "CNode belongs to another CGroup."))
  (unless (jl-contains (.c-nodes group) c-node)
    (jl-add (.c-nodes group) c-node))
  (setf (.c-group c-node) group)
  (unless (.reference group)
    (setf (.reference group) c-node)))

(defun common-c-node.c-group-of (c-node) (.c-group c-node))

(defjmethod add-cnode ((this common-c-group) c-node)
  (%cg-add-cnode this c-node))

(defjmethod remove-cnode ((this common-c-group) c-node)
  (let ((removed (jl-contains (.c-nodes this) c-node)))
    (when removed
      (jl-remove (.c-nodes this) c-node)
      (setf (.c-group c-node) nil))
    removed))

;;; CGroupBuilder

(defjmethod nodes-list ((this common-c-group-builder) nodes)
  (jdo (n nodes)
    (%cg-add-cnode (.group this) n))
  this)

(defjmethod nodes ((this common-c-group-builder) nodes)
  ;; Java's nodes(CNode...): NODES is a collection of CNodes, or one CNode.  (A
  ;; &rest parameter would count as an extra arity in defjmethod's name, so the
  ;; collection is passed as one argument.)
  (nodes-list this (if (and (vectorp nodes) (not (stringp nodes))) nodes (list nodes))))

(defjmethod reference ((this common-c-group-builder) reference-node)
  (setf (.reference (.group this)) reference-node)
  this)

(defjmethod master ((this common-c-group-builder) master)
  (setf (.master (.group this)) master)
  this)

(defjmethod create ((this common-c-group-builder) graph)
  (jl-add (.c-groups graph) (.group this))
  (.group this))

;;; ------------------------------------------------------------ CNode

(defjmethod to-string ((this common-c-node))
  (if (.to-string-delegate this)
      (funcall (.to-string-delegate this) this)
      (format nil "~A" (.origin this))))

(defjstatic common-c-node.of ()
  (new-c-node-builder))

(defjmethod id ((this c-node-builder) id)
  (setf (.id (.node this)) id)
  this)

(defjmethod origin ((this c-node-builder) origin)
  (setf (.origin (.node this)) origin)
  this)

(defjmethod j-type ((this c-node-builder) type)
  (setf (.type (.node this)) type)
  this)

(defjmethod hitbox ((this c-node-builder) hitbox)
  (setf (.hitbox (.node this)) hitbox)
  this)

(defjmethod to-string-delegate ((this c-node-builder) delegate)
  (setf (.to-string-delegate (.node this)) delegate)
  this)

(defjmethod group-with ((this c-node-builder) parent)
  (setf (.group-parent this) parent)
  this)

(defjmethod create ((this c-node-builder) graph)
  (let ((node (.node this))
        (group-parent (.group-parent this)))
    (jl-add (.c-nodes graph) node)
    (when group-parent
      (if (null (.c-group group-parent))
          (create (nodes-list (common-c-group.of) (jlist group-parent node)) graph)
          (%cg-add-cnode (.c-group group-parent) node)))
    node))

;;; ------------------------------------------------------------ Quadruplet

(defjctor common-quadruplet (l r u d)
  (j-set this l r u d))

(defjmethod j-set ((this common-quadruplet) l r u d)
  (setf (.left this) l
        (.right this) r
        (.up this) u
        (.down this) d))

(defjmethod apply-or ((this common-quadruplet) other)
  (setf (.left this) (or (.left this) (.left other))
        (.right this) (or (.right this) (.right other))
        (.up this) (or (.up this) (.up other))
        (.down this) (or (.down this) (.down other))))

(defjmethod j-set ((this common-quadruplet) value direction)
  (enum-case direction
    (core-direction.left (setf (.left this) value))
    (core-direction.right (setf (.right this) value))
    (core-direction.up (setf (.up this) value))
    (core-direction.down (setf (.down this) value))
    (t nil))
  nil)

(defjmethod j-get ((this common-quadruplet) direction)
  (enum-case direction
    (core-direction.left (.left this))
    (core-direction.right (.right this))
    (core-direction.up (.up this))
    (core-direction.down (.down this))
    (t nil)))

;;; ------------------------------------------------------------ LongestPathCompaction

(defjmethod compact ((this common-longest-path-compaction) compactor)
  (let ((min-start-pos double.positive-infinity))
    (jdo (c-node (.c-nodes (.c-graph compactor)))
      (setf min-start-pos
            (math.min min-start-pos
                      (+ (.x (.hitbox (.reference (.c-group c-node)))) (.x (.c-group-offset c-node))))))
    (let ((sinks (new-jlist)))
      (jdo (group (.c-groups (.c-graph compactor)))
        (setf (.start-pos group) min-start-pos)
        (when (zerop (.out-degree group))
          (jl-add sinks group)))
      (loop until (jl-is-empty sinks)
            do (let* ((group (jl-poll-first sinks))
                      (diff (.x (.hitbox (.reference group)))))
                 (jdo (node (.c-nodes group))
                   (let ((suggested-x (+ (.start-pos group) (.x (.c-group-offset node)))))
                     (if (or (not (is-locked compactor group (.direction compactor)))
                             (< (.x (.hitbox node)) suggested-x))
                         (setf (.start-pos node) suggested-x)
                         (setf (.start-pos node) (.x (.hitbox node))))))
                 (decf diff (.start-pos (.reference group)))
                 (incf (.delta group) diff)
                 (if (or (eq (.direction compactor) core-direction.right)
                         (eq (.direction compactor) core-direction.down))
                     (incf (.delta-normalized group) diff)
                     (decf (.delta-normalized group) diff))
                 (jdo (node (.c-nodes group))
                   (jdo (inc-node (.constraints node))
                     (let ((spacing (if (is-horizontal (.direction compactor))
                                        (get-horizontal-spacing (.spacings-handler compactor) node inc-node)
                                        (get-vertical-spacing (.spacings-handler compactor) node inc-node)))
                           (inc-group (.c-group inc-node)))
                       (setf (.start-pos inc-group)
                             (math.max (.start-pos inc-group)
                                       (- (+ (.start-pos node) (.width (.hitbox node)) spacing)
                                          (.x (.c-group-offset inc-node)))))
                       (when (is-locked compactor inc-node (.direction compactor))
                         (setf (.start-pos inc-group)
                               (math.max (.start-pos inc-group)
                                         (- (.x (.hitbox inc-node)) (.x (.c-group-offset inc-node))))))
                       (decf (.out-degree inc-group))
                       (when (zerop (.out-degree inc-group))
                         (jl-add sinks inc-group))))))))
    (jdo (c-node (.c-nodes (.c-graph compactor)))
      (setf (.x (.hitbox c-node)) (.start-pos c-node)))))

;;; ------------------------------------------------------------ QuadraticConstraintCalculation

(defjmethod calculate-constraints ((this common-quadratic-constraint-calculation) compactor)
  (let ((nodes (.c-nodes (.c-graph compactor))))
    (jdo (c nodes) (jl-clear (.constraints c)))
    (jdo (c1 nodes)
      (jdo (c2 nodes)
        (unless (or (eq c1 c2)
                    (and (.c-group c1) (eq (.c-group c1) (.c-group c2))))
          (let ((spacing (if (is-horizontal (.direction compactor))
                             (get-vertical-spacing (.spacings-handler compactor) c1 c2)
                             (get-horizontal-spacing (.spacings-handler compactor) c1 c2))))
            (when (and (or (> (.x (.hitbox c2)) (.x (.hitbox c1)))
                           (and (= (.x (.hitbox c1)) (.x (.hitbox c2)))
                                (< (.width (.hitbox c1)) (.width (.hitbox c2)))))
                       (common-compare-fuzzy.gt (+ (.y (.hitbox c2)) (.height (.hitbox c2)) spacing)
                                                (.y (.hitbox c1)))
                       (common-compare-fuzzy.lt (.y (.hitbox c2))
                                                (+ (.y (.hitbox c1)) (.height (.hitbox c1)) spacing)))
              (jl-add (.constraints c1) c2))))))))

;;; ------------------------------------------------------------ OneDimensionalCompactor

(defparameter common-longest-path-compaction.default (new-common-longest-path-compaction))
(defparameter common-scanline-constraint-calculation.default
  (new-common-scanline-constraint-calculator))
(defparameter common-one-dimensional-compactor.longest-path-compaction common-longest-path-compaction.default)
(defparameter common-one-dimensional-compactor.quadratic-constraints
  (new-common-quadratic-constraint-calculation))
(defparameter common-one-dimensional-compactor.scanline-constraints
  common-scanline-constraint-calculation.default)

(defjctor common-one-dimensional-compactor (c-graph)
  (setf (.compaction-algorithm this) common-longest-path-compaction.default
        (.constraint-algorithm this) common-scanline-constraint-calculation.default
        (.c-graph this) c-graph
        (.spacings-handler this) common-i-spacings-handler.default-spacing-handler
        (.direction this) core-direction.undefined
        (.finished this) nil)
  (%ocomp-calculate-group-offsets this)
  (jdo (n (.c-nodes c-graph))
    (when (null (.c-group n))
      (create (nodes-list (common-c-group.of) (jlist n)) c-graph)))
  (jdo (n (.c-nodes c-graph))
    (setf (.hitbox-pre-compaction n) (new-elk-rectangle (.hitbox n)))))


(defjmethod set-spacings-handler ((this common-one-dimensional-compactor) handler)
  (setf (.spacings-handler this) handler)
  this)

(defjmethod set-compaction-algorithm ((this common-one-dimensional-compactor) compactor)
  (setf (.compaction-algorithm this) compactor)
  this)

(defjmethod set-constraint-algorithm ((this common-one-dimensional-compactor) the-constraint-algorithm)
  (setf (.constraint-algorithm this) the-constraint-algorithm)
  this)

(defjmethod compact ((this common-one-dimensional-compactor))
  (when (.finished this)
    (jthrow 'illegal-state-exception "The OneDimensionalCompactor instance has been finished already."))
  (when (eq (.direction this) core-direction.undefined)
    (%ocomp-change-direction this core-direction.left))
  (jdo (g (.c-groups (.c-graph this)))
    (setf (.out-degree g) (.out-degree-real g)))
  (jdo (n (.c-nodes (.c-graph this)))
    (setf (.start-pos n) double.negative-infinity))
  (compact (.compaction-algorithm this) this)
  this)

(defjmethod finish ((this common-one-dimensional-compactor))
  (%ocomp-change-direction this core-direction.left)
  (setf (.finished this) t)
  this)

(defjmethod change-direction ((this common-one-dimensional-compactor) dir)
  (when (.finished this)
    (jthrow 'illegal-state-exception "The OneDimensionalCompactor instance has been finished already."))
  (%ocomp-change-direction this dir))

(defun %ocomp-change-direction (this dir)
  (unless (js-contains-or-member (.supported-directions (.c-graph this)) dir)
    (jthrow 'runtime-exception
            (format nil "The direction ~A is not supported by the CGraph instance." (name dir))))
  (when (eq dir (.direction this))
    (return-from %ocomp-change-direction this))
  (let ((old (.direction this)))
    (setf (.direction this) dir)
    (flet ((m () (%ocomp-mirror this))
           (tr () (%ocomp-transpose this))
           (calc () (%ocomp-calculate-constraints this))
           (rev () (%ocomp-reverse-constraints this)))
      (enum-case old
        (core-direction.undefined
         (enum-case dir
           (core-direction.left (calc))
           (core-direction.right (m) (calc))
           (core-direction.up (tr) (calc))
           (core-direction.down (tr) (m) (calc))
           (t nil)))
        (core-direction.left
         (enum-case dir
           (core-direction.right (m) (rev))
           (core-direction.up (tr) (calc))
           (core-direction.down (tr) (m) (calc))
           (t nil)))
        (core-direction.right
         (enum-case dir
           (core-direction.left (m) (rev))
           (core-direction.up (m) (tr) (calc))
           (core-direction.down (m) (tr) (m) (calc))
           (t nil)))
        (core-direction.up
         (enum-case dir
           (core-direction.left (tr) (calc))
           (core-direction.right (tr) (m) (calc))
           (core-direction.down (m) (rev))
           (t nil)))
        (core-direction.down
         (enum-case dir
           (core-direction.left (m) (tr) (calc))
           (core-direction.right (m) (tr) (m) (calc))
           (core-direction.up (m) (rev))
           (t nil)))
        (t nil))))
  this)

(defjmethod is-locked ((this common-one-dimensional-compactor) x dir)
  (if (typep x 'common-c-group)
      (jdo (n (.c-nodes x))
        (when (is-locked this n dir)
          (return-from is-locked/2 t)))
      (let ((fun (.lock-fun this)))
        (when fun
          (return-from is-locked/2 (is-locked fun x dir)))))
  nil)

(defjmethod set-lock-function ((this common-one-dimensional-compactor) fun)
  (setf (.lock-fun this) fun)
  this)

(defjmethod force-constraints-recalculation ((this common-one-dimensional-compactor))
  (%ocomp-calculate-constraints this)
  this)

(defjmethod calculate-group-offsets ((this common-one-dimensional-compactor))
  (%ocomp-calculate-group-offsets this)
  this)

(defun %ocomp-calculate-group-offsets (this)
  (jdo (group (.c-groups (.c-graph this)))
    (setf (.reference group) nil)
    (jdo (n (.c-nodes group))
      (reset (.c-group-offset n))
      (when (or (null (.reference group))
                (< (.x (.hitbox n)) (.x (.hitbox (.reference group)))))
        (setf (.reference group) n)))
    (jdo (n (.c-nodes group))
      (setf (.x (.c-group-offset n)) (- (.x (.hitbox n)) (.x (.hitbox (.reference group))))
            (.y (.c-group-offset n)) (- (.y (.hitbox n)) (.y (.hitbox (.reference group)))))))
  this)

(defun %ocomp-mirror (this)
  (jdo (c-node (.c-nodes (.c-graph this)))
    (setf (.x (.hitbox c-node))
          (- (- (.x (.hitbox c-node))) (.width (.hitbox c-node)))))
  (%ocomp-calculate-group-offsets this))

(defun %ocomp-transpose (this)
  (jdo (c-node (.c-nodes (.c-graph this)))
    (let ((h (.hitbox c-node)) (o (.c-group-offset c-node)))
      (rotatef (.x h) (.y h))
      (rotatef (.width h) (.height h))
      (rotatef (.x o) (.y o))))
  (%ocomp-calculate-group-offsets this))

(defun %ocomp-calculate-constraints (this)
  (jdo (c-node (.c-nodes (.c-graph this)))
    (jl-clear (.constraints c-node)))
  (let ((cstrs (if (is-horizontal (.direction this))
                   (.predefined-horizontal-constraints (.c-graph this))
                   (.predefined-vertical-constraints (.c-graph this)))))
    (jdo (p cstrs)
      (if (or (eq (.direction this) core-direction.left) (eq (.direction this) core-direction.up))
          (jl-add (.constraints (get-first p)) (get-second p))
          (jl-add (.constraints (get-second p)) (get-first p)))))
  (calculate-constraints (.constraint-algorithm this) this)
  (%ocomp-calculate-constraints-for-cgroups this))

(defun %ocomp-calculate-constraints-for-cgroups (this)
  (jdo (group (.c-groups (.c-graph this)))
    (setf (.out-degree group) 0
          (.out-degree-real group) 0)
    (js-clear (.incoming-constraints group)))
  (jdo (group (.c-groups (.c-graph this)))
    (jdo (c-node (.c-nodes group))
      (jdo (inc (.constraints c-node))
        (unless (eq (.c-group inc) group)
          (js-add (.incoming-constraints group) inc)
          (incf (.out-degree (.c-group inc)))
          (incf (.out-degree-real (.c-group inc))))))))

(defun %ocomp-reverse-constraints (this)
  (let ((inc-map (new-jmap)))
    (jdo (c-node (.c-nodes (.c-graph this)))
      (setf (gethash c-node inc-map) (new-jlist)))
    (jdo (c-node (.c-nodes (.c-graph this)))
      (setf (.start-pos c-node) double.negative-infinity)
      (jdo (inc (.constraints c-node))
        (jl-add (gethash inc inc-map) c-node)))
    (jdo (c-node (.c-nodes (.c-graph this)))
      (jl-clear (.constraints c-node))
      (setf (.constraints c-node) (gethash c-node inc-map))))
  (%ocomp-calculate-constraints-for-cgroups this))
