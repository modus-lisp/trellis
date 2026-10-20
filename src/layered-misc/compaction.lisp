;;;; compaction.lisp -- org.eclipse.elk.alg.layered.compaction.{oned,oned.algs,recthull}

(in-package #:elk)

;;; ------------------------------------------------------------ helpers

(defun %lm-fuzzy-equals (a b tol)
  "Guava DoubleMath.fuzzyEquals."
  (or (and (not (nan-p a)) (not (nan-p b)) (<= (abs (- a b)) tol))
      (and (not (nan-p a)) (= a b))
      (and (nan-p a) (nan-p b))))

(defun %lm-fuzzy-compare (a b tol)
  "Guava DoubleMath.fuzzyCompare."
  (cond ((%lm-fuzzy-equals a b tol) 0)
        ((< a b) -1)
        ((> a b) 1)
        (t (boolean.compare (nan-p a) (nan-p b)))))

(defun %lm-set-add (list x)
  "LinkedHashSet/HashSet add on a jlist (identity)."
  (unless (find x list :test #'eq) (vector-push-extend x list) t))

(defun %lm-set-remove (list x)
  (let ((i (position x list :test #'eq)))
    (when i (jl-remove-at list i) t)))

;;; ------------------------------------------------------------ CGraph

(defjctor layered-c-graph (supported-directions)
  (setf (.supported-directions this) supported-directions))

(defjmethod supports ((this layered-c-graph) direction)
  (contains (.supported-directions this) direction))

;;; ------------------------------------------------------------ CGroup

(defjctor layered-c-group ()
  nil)

(defjctor layered-c-group (input-c-nodes)
  ;; CGroup(CNode... inputCNodes): varargs as one sequence
  (setf (.c-nodes this) (new-jlist)
        (.incoming-constraints this) (new-jlist)
        (.out-degree this) 0)
  (jdo (c-node input-c-nodes)
    (when (null (.reference this))
      (setf (.reference this) c-node))
    (add-c-node this c-node)))

(defjmethod add-c-node ((this layered-c-group) c-node)
  (%lm-set-add (.c-nodes this) c-node)
  (when (.c-group c-node)
    (jthrow 'runtime-exception "CNode belongs to another CGroup."))
  (setf (.c-group c-node) this))

(defjmethod remove-c-node ((this layered-c-group) c-node)
  (let ((removed (%lm-set-remove (.c-nodes this) c-node)))
    (when removed
      (setf (.c-group c-node) nil))
    removed))

;;; ------------------------------------------------------------ CNode

(defjmethod get-position ((this layered-c-node))
  (.x (.hitbox this)))

(defjmethod apply-position ((this layered-c-node))
  (setf (.x (.hitbox this)) (.start-pos this)))

;;; ------------------------------------------------------------ CompareFuzzy

(defjconst layered-compare-fuzzy.tolerance 0.0001d0)

(defjstatic layered-compare-fuzzy.eq (d1 d2)
  (%lm-fuzzy-equals d1 d2 layered-compare-fuzzy.tolerance))
(defjstatic layered-compare-fuzzy.gt (d1 d2)
  (> (%lm-fuzzy-compare d1 d2 layered-compare-fuzzy.tolerance) 0))
(defjstatic layered-compare-fuzzy.lt (d1 d2)
  (< (%lm-fuzzy-compare d1 d2 layered-compare-fuzzy.tolerance) 0))
(defjstatic layered-compare-fuzzy.ge (d1 d2)
  (>= (%lm-fuzzy-compare d1 d2 layered-compare-fuzzy.tolerance) 0))
(defjstatic layered-compare-fuzzy.le (d1 d2)
  (<= (%lm-fuzzy-compare d1 d2 layered-compare-fuzzy.tolerance) 0))

;;; ------------------------------------------------------------ ISpacingsHandler

(defjmethod get-horizontal-spacing ((this %lm-default-spacing-handler) c-node1 c-node2)
  (math.max (get-horizontal-spacing c-node1) (get-horizontal-spacing c-node2)))
(defjmethod get-vertical-spacing ((this %lm-default-spacing-handler) c-node1 c-node2)
  (math.max (get-vertical-spacing c-node1) (get-vertical-spacing c-node2)))

(defjconst layered-i-spacings-handler.default-spacing-handler
  (new-%lm-default-spacing-handler))

;;; ------------------------------------------------------------ Quadruplet

(defjctor layered-quadruplet ()
  (j-set this nil nil nil nil))

(defjctor layered-quadruplet (l r u d)
  (j-set this l r u d))

(defjmethod j-set ((this layered-quadruplet) l r u d)
  (setf (.left this) l (.right this) r (.up this) u (.down this) d))

(defjmethod j-set ((this layered-quadruplet) value direction)
  (enum-case direction
    (core-direction.left (setf (.left this) value))
    (core-direction.right (setf (.right this) value))
    (core-direction.up (setf (.up this) value))
    (core-direction.down (setf (.down this) value))
    (t nil)))

(defjmethod j-get ((this layered-quadruplet) direction)
  (enum-case direction
    (core-direction.left (.left this))
    (core-direction.right (.right this))
    (core-direction.up (.up this))
    (core-direction.down (.down this))
    (t nil)))

;;; ------------------------------------------------------------ LongestPathCompaction

(defjmethod compact ((this layered-longest-path-compaction) compactor)
  ;; calculating the left-most position of any element
  ;; this will be our starting point for the compaction
  (let ((min-start-pos double.positive-infinity)
        (c-graph (.c-graph compactor)))
    (jdo (c-node (.c-nodes c-graph))
      (setf min-start-pos
            (math.min min-start-pos
                      (+ (.x (.hitbox (.reference (.c-group c-node)))) (.x (.c-group-offset c-node))))))
    ;; finding the sinks of the constraint graph
    (let ((sinks (new-jlist)))
      (jdo (group (.c-groups c-graph))
        (setf (.start-pos group) min-start-pos)
        (when (= (.out-degree group) 0)
          (jl-add sinks group)))
      ;; process sinks until every node in the constraint graph was handled
      (loop until (jl-is-empty sinks)
            do (let* ((group (jl-poll-first sinks))
                      ;; record the movement of this group during the current compaction
                      (diff (.x (.hitbox (.reference group)))))
                 ;; #1 final positions for this group's nodes
                 (jdo (node (.c-nodes group))
                   ;; CNodes can be locked in place to avoid pulling clusters apart
                   (let ((suggested-x (+ (.start-pos group) (.x (.c-group-offset node)))))
                     (if (or (.reposition (.c-group node))
                             ;; does the "fixed" position violate the constraints?
                             (< (get-position node) suggested-x))
                         (setf (.start-pos node) suggested-x)
                         ;; leave the node where it was!
                         (setf (.start-pos node) (.x (.hitbox node))))))
                 (decf diff (.start-pos (.reference group)))
                 (incf (.delta group) diff)
                 (if (or (eq (.direction compactor) core-direction.right)
                         (eq (.direction compactor) core-direction.down))
                     (incf (.delta-normalized group) diff)
                     (decf (.delta-normalized group) diff))
                 ;; #2 propagate start positions to constrained groups
                 (jdo (node (.c-nodes group))
                   (jdo (inc-node (.constraints node))
                     ;; determine the required spacing
                     (let ((spacing (if (is-horizontal (.direction compactor))
                                        (get-horizontal-spacing (.spacings-handler compactor) node inc-node)
                                        (get-vertical-spacing (.spacings-handler compactor) node inc-node))))
                       (setf (.start-pos (.c-group inc-node))
                             (math.max (.start-pos (.c-group inc-node))
                                       (- (+ (.start-pos node) (.width (.hitbox node)) spacing)
                                          ;; respect the other group's node's offset
                                          (.x (.c-group-offset inc-node)))))
                       ;; whether the node's current position should be preserved
                       (unless (.reposition inc-node)
                         (setf (.start-pos (.c-group inc-node))
                               (math.max (.start-pos (.c-group inc-node))
                                         (- (get-position inc-node) (.x (.c-group-offset inc-node))))))
                       (decf (.out-degree (.c-group inc-node)))
                       (when (= (.out-degree (.c-group inc-node)) 0)
                         (jl-add sinks (.c-group inc-node)))))))))
    ;; #3 setting hitbox positions to new starting positions
    (jdo (c-node (.c-nodes c-graph))
      (apply-position c-node))))

;;; ------------------------------------------------------------ QuadraticConstraintCalculation

(defjmethod calculate-constraints ((this layered-quadratic-constraint-calculation) compactor)
  (let ((nodes (.c-nodes (.c-graph compactor))))
    ;; resetting constraints
    (jdo (c-node nodes)
      (jl-clear (.constraints c-node)))
    ;; inferring constraints from hitbox intersections
    (jdo (c-node1 nodes)
      (jdo (c-node2 nodes)
        (block continue
          ;; no self constraints
          (when (eq c-node1 c-node2) (return-from continue))
          ;; no constraints between nodes of the same group
          (when (and (.c-group c-node1) (eq (.c-group c-node1) (.c-group c-node2)))
            (return-from continue))
          (let* ((spacing (if (is-horizontal (.direction compactor))
                              (get-vertical-spacing (.spacings-handler compactor) c-node1 c-node2)
                              (get-horizontal-spacing (.spacings-handler compactor) c-node1 c-node2)))
                 (h1 (.hitbox c-node1))
                 (h2 (.hitbox c-node2)))
            ;; add constraint if cNode2 is to the right of cNode1 and could collide if moved
            ;; horizontally; exclude parentNodes because they don't constrain their north/south segments
            (when (and (not (eq c-node1 (.parent-node c-node2)))
                       ;; '>' avoids simultaneous constraints A->B and B->A
                       (or (> (.x h2) (.x h1))
                           (and (= (.x h1) (.x h2)) (< (.width h1) (.width h2))))
                       (layered-compare-fuzzy.gt (+ (.y h2) (.height h2) spacing) (.y h1))
                       (layered-compare-fuzzy.lt (.y h2) (+ (.y h1) (.height h1) spacing)))
              (jl-add (.constraints c-node1) c-node2))))))))

;;; ------------------------------------------------------------ ScanlineConstraintCalculator

(defjconst layered-scanline-constraint-calculator.epsilon 0.5d0)

(defjctor layered-timestamp (node low)
  (setf (.node this) node (.low this) low))

(defun %lm-timestamp-y (p)
  (let ((y (.y (.hitbox (.node p)))))
    (if (.low p) y (+ y (.height (.hitbox (.node p)))))))

(defun %lm-constraints-scanline-comparator (p1 p2)
  ;; chose the right coordinate, compare them ...
  (let ((cmp (double.compare (%lm-timestamp-y p1) (%lm-timestamp-y p2))))
    ;; ... and if they are equal, sort low->high
    (if (= cmp 0)
        (cond ((and (not (.low p1)) (.low p2)) -1)
              ((and (not (.low p2)) (.low p1)) 1)
              (t cmp))
        cmp)))

(defjctor layered-scanline-constraint-calculator ()
  (setf (.constraints-scanline-comparator this) #'%lm-constraints-scanline-comparator
        (.constraints-scanline-handler this)
        (let ((h (new-layered-constraints-scanline-handler)))
          (setf (.outer h) this)
          h)))

(defjmethod calculate-constraints ((this layered-scanline-constraint-calculator) the-compactor)
  (setf (.compactor this) the-compactor)
  ;; consider all nodes, do not add spacing
  (sweep this (lambda (n) (declare (ignore n)) t) (lambda (n) (declare (ignore n)) 0d0)))

(defjmethod sweep ((this layered-scanline-constraint-calculator) filter-fun spacing-fun)
  (blow-up-hitboxes this filter-fun spacing-fun)
  ;; add all nodes twice (once for the lower, once for the upper border)
  (let ((points (new-jlist)))
    (jdo (n (.c-nodes (.c-graph (.compactor this))))
      (when (funcall filter-fun n)
        (jl-add points (new-layered-timestamp n t))
        (jl-add points (new-layered-timestamp n nil))))
    ;; reset internal state
    (reset (.constraints-scanline-handler this))
    ;; execute the scanline
    (layered-scanline.execute points (.constraints-scanline-comparator this)
                              (.constraints-scanline-handler this)))
  (normalize-hitboxes this filter-fun spacing-fun))

(defun %lm-spacing-up-ok (compactor n)
  (let ((dir (.direction compactor)))
    (and (not (and (is-horizontal dir) (.up (.spacing-ignore n))))
         (not (and (is-vertical dir) (.left (.spacing-ignore n)))))))

(defun %lm-spacing-down-ok (compactor n)
  (let ((dir (.direction compactor)))
    (and (not (and (is-horizontal dir) (.down (.spacing-ignore n))))
         (not (and (is-vertical dir) (.right (.spacing-ignore n)))))))

(defjmethod blow-up-hitboxes ((this layered-scanline-constraint-calculator) filter spacing-fun)
  (let ((eps layered-scanline-constraint-calculator.epsilon)
        (compactor (.compactor this)))
    (jdo (n (.c-nodes (.c-graph compactor)))
      (when (funcall filter n)
        (let ((spacing (funcall spacing-fun n)))
          (when (> spacing 0)
            (when (%lm-spacing-up-ok compactor n)
              (decf (.y (.hitbox n)) (math.max 0 (- (/ spacing 2) eps))))
            (when (%lm-spacing-down-ok compactor n)
              (incf (.height (.hitbox n)) (math.max 0 (- spacing (* 2 eps)))))))))))

(defjmethod normalize-hitboxes ((this layered-scanline-constraint-calculator) filter spacing-fun)
  (let ((eps layered-scanline-constraint-calculator.epsilon)
        (compactor (.compactor this)))
    (jdo (n (.c-nodes (.c-graph compactor)))
      (when (funcall filter n)
        (let ((spacing (funcall spacing-fun n)))
          (when (> spacing 0)
            (when (%lm-spacing-up-ok compactor n)
              (incf (.y (.hitbox n)) (math.max 0 (- (/ spacing 2) eps))))
            (when (%lm-spacing-down-ok compactor n)
              (decf (.height (.hitbox n)) (- spacing (* 2 eps))))))))))

;;; ConstraintsScanlineHandler.  The TreeSet<CNode> ordered by hitbox center is
;;; a sorted jlist; like a TreeSet, elements comparing 0 are "equal": add
;;; ignores them and remove removes whichever element compares 0.

(defun %lm-center-cmp (c1 c2)
  (double.compare (+ (.x (.hitbox c1)) (/ (.width (.hitbox c1)) 2))
                  (+ (.x (.hitbox c2)) (/ (.width (.hitbox c2)) 2))))

(defun %lm-ts-search (set x)
  "Index of the first element >= X, and whether it compares equal."
  (let ((lo 0) (hi (length set)))
    (loop while (< lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (< (%lm-center-cmp (aref set mid) x) 0)
                   (setf lo (1+ mid))
                   (setf hi mid))))
    (values lo (and (< lo (length set)) (= 0 (%lm-center-cmp (aref set lo) x))))))

(defun %lm-ts-add (set x)
  (multiple-value-bind (i found) (%lm-ts-search set x)
    (unless found (jl-add-at set i x) t)))

(defun %lm-ts-remove (set x)
  (multiple-value-bind (i found) (%lm-ts-search set x)
    (when found (jl-remove-at set i) t)))

(defun %lm-ts-lower (set x)
  (let ((i (%lm-ts-search set x)))
    (if (> i 0) (aref set (1- i)) nil)))

(defun %lm-ts-higher (set x)
  (multiple-value-bind (i found) (%lm-ts-search set x)
    (let ((j (if found (1+ i) i)))
      (if (< j (length set)) (aref set j) nil))))

(defjmethod reset ((this layered-constraints-scanline-handler))
  (let ((nodes (.c-nodes (.c-graph (.compactor (.outer this))))))
    (jl-clear (.intervals this))
    (setf (.cand this) (make-array (length nodes) :initial-element nil))
    (let ((index 0))
      (jdo (n nodes)
        (setf (.id n) index)
        (incf index)))))

(defjmethod handle ((this layered-constraints-scanline-handler) p)
  (if (.low p)
      (insert this p)
      (j-delete this p)))

(defjmethod insert ((this layered-constraints-scanline-handler) p)
  (let ((intervals (.intervals this)) (cand (.cand this)))
    (%lm-ts-add intervals (.node p))
    (setf (aref cand (.id (.node p))) (%lm-ts-lower intervals (.node p)))
    (let ((right (%lm-ts-higher intervals (.node p))))
      (when right
        (setf (aref cand (.id right)) (.node p))))))

;; delete(Timestamp)
(defjmethod j-delete ((this layered-constraints-scanline-handler) p)
  (let* ((intervals (.intervals this)) (cand (.cand this))
         (node (.node p))
         (left (%lm-ts-lower intervals node)))
    (when (and left (eq left (aref cand (.id node))))
      ;; different groups?
      (when (and (.c-group left) (not (eq (.c-group left) (.c-group node))))
        (jl-add (.constraints left) node)))
    (let ((right (%lm-ts-higher intervals node)))
      (when (and right (eq (aref cand (.id right)) node))
        ;; different groups?
        (when (and (.c-group right) (not (eq (.c-group right) (.c-group node))))
          (jl-add (.constraints node) right))))
    ;; we are done with you!
    (%lm-ts-remove intervals node)))

;;; ------------------------------------------------------------ OneDimensionalCompactor

(defjconst layered-one-dimensional-compactor.longest-path-compaction
  (new-layered-longest-path-compaction))
(defjconst layered-one-dimensional-compactor.scanline-constraints
  (new-layered-scanline-constraint-calculator))
(defjconst layered-one-dimensional-compactor.quadratic-constraints
  (new-layered-quadratic-constraint-calculation))

(defjctor layered-one-dimensional-compactor (c-graph)
  (setf (.compaction-algorithm this) layered-one-dimensional-compactor.longest-path-compaction
        (.constraint-algorithm this) layered-one-dimensional-compactor.scanline-constraints
        (.spacings-handler this) layered-i-spacings-handler.default-spacing-handler
        (.direction this) core-direction.undefined)
  (setf (.c-graph this) c-graph)
  ;; the default locking strategy locks CNodes if they are not constrained
  (set-locking-strategy this (lambda (pair) (not (= (.out-degree (.c-group (get-first pair))) 0))))
  ;; for any pre-specified groups, deduce the offset of the elements
  (calculate-group-offsets this)
  ;; the compaction operates solely on CGroups,
  ;; thus, we wrap any plain CNodes into a CGroup
  (jdo (n (.c-nodes c-graph))
    (when (null (.c-group n))
      (jl-add (.c-groups c-graph) (new-layered-c-group (list n))))))

(defjmethod set-spacings-handler ((this layered-one-dimensional-compactor) handler)
  (setf (.spacings-handler this) handler)
  this)

(defjmethod set-compaction-algorithm ((this layered-one-dimensional-compactor) compactor)
  (setf (.compaction-algorithm this) compactor)
  this)

(defjmethod set-constraint-algorithm ((this layered-one-dimensional-compactor) the-constraint-algorithm)
  (setf (.constraint-algorithm this) the-constraint-algorithm)
  this)

(defun %lm-check-not-finished (this)
  (when (.finished this)
    (jthrow 'illegal-state-exception
            "The OneDimensionalCompactor instance has been finished already.")))

(defjmethod compact ((this layered-one-dimensional-compactor))
  (%lm-check-not-finished this)
  ;; if no direction was specified the direction defaults to LEFT
  (when (eq (.direction this) core-direction.undefined)
    (change-direction this core-direction.left))
  ;; prepare compaction by setting initial outDegree value for groups
  (jdo (g (.c-groups (.c-graph this)))
    (setf (.out-degree g) 0))
  (jdo (n (.c-nodes (.c-graph this)))
    ;; reset any previously calculated start positions
    (setf (.start-pos n) double.negative-infinity)
    (jdo (inc-n (.constraints n))
      (incf (.out-degree (.c-group inc-n)))))
  ;; perform the actual compaction
  (let ((alg (.compaction-algorithm this)))
    (if (functionp alg) (funcall alg this) (compact alg this)))
  ;; remove all locks
  (jdo (node (.c-nodes (.c-graph this)))
    (setf (.reposition node) t))
  this)

(defjmethod finish ((this layered-one-dimensional-compactor))
  (change-direction this core-direction.left)
  (setf (.finished this) t)
  this)

(defjmethod change-direction ((this layered-one-dimensional-compactor) dir)
  (%lm-check-not-finished this)
  ;; prohibits vertical compaction if the graph has edges, because vertical and horizontal
  ;; segments would interchange
  (unless (supports (.c-graph this) dir)
    (jthrow 'runtime-exception "The direction ~A is not supported by the CGraph instance." (name dir)))
  ;; short-circuit if nothing todo
  (when (eq dir (.direction this))
    (return-from change-direction/1 this))
  (let ((old-direction (.direction this)))
    (setf (.direction this) dir)
    ;; executes required transformations and calculates constraints
    ;; if the graph is compacted in opposing directions the constraints are reversed instead of
    ;; being recalculated and CNodes are locked
    (macrolet ((m () `(mirror-hitboxes this))
               (tr () `(transpose-hitboxes this))
               (cc () `(calculate-constraints this))
               (rc () `(reverse-constraints this)))
      (enum-case old-direction
        (core-direction.undefined
         (enum-case dir
           (core-direction.left (cc))
           (core-direction.right (m) (cc))
           (core-direction.up (tr) (cc))
           (core-direction.down (tr) (m) (cc))))
        (core-direction.left
         (enum-case dir
           (core-direction.right (m) (rc))
           (core-direction.up (tr) (cc))
           (core-direction.down (tr) (m) (cc))))
        (core-direction.right
         (enum-case dir
           (core-direction.left (m) (rc))
           (core-direction.up (m) (tr) (cc))
           (core-direction.down (m) (tr) (m) (cc))))
        (core-direction.up
         (enum-case dir
           (core-direction.left (tr) (cc))
           (core-direction.right (tr) (m) (cc))
           (core-direction.down (m) (rc))))
        (core-direction.down
         (enum-case dir
           (core-direction.left (m) (tr) (cc))
           (core-direction.right (m) (tr) (m) (cc))
           (core-direction.up (m) (rc)))))))
  this)

(defjmethod set-locking-strategy ((this layered-one-dimensional-compactor) strategy)
  (setf (.locking-strategy this) strategy)
  this)

(defjmethod apply-locking-strategy ((this layered-one-dimensional-compactor))
  (apply-locking-strategy this (.direction this))
  this)

(defjmethod apply-locking-strategy ((this layered-one-dimensional-compactor) dir)
  (jdo (c-group (.c-groups (.c-graph this)))
    (setf (.reposition c-group) t))
  (jdo (c-node (.c-nodes (.c-graph this)))
    (setf (.reposition c-node) (and (funcall (.locking-strategy this) (pair.of c-node dir)) t))
    (setf (.reposition (.c-group c-node))
          (and (.reposition (.c-group c-node))
               (funcall (.locking-strategy this) (pair.of c-node dir))
               t)))
  this)

(defjmethod force-constraints-recalculation ((this layered-one-dimensional-compactor))
  (calculate-constraints this)
  this)

(defjmethod calculate-group-offsets ((this layered-one-dimensional-compactor))
  ;; for any pre-specified groups, deduce the offset of the elements
  (jdo (group (.c-groups (.c-graph this)))
    (setf (.reference group) nil)
    ;; find left-most element
    (jdo (n (.c-nodes group))
      (reset (.c-group-offset n))
      (when (or (null (.reference group))
                (< (.x (.hitbox n)) (.x (.hitbox (.reference group)))))
        (setf (.reference group) n)))
    ;; calculate offsets
    (jdo (n (.c-nodes group))
      (setf (.x (.c-group-offset n)) (- (.x (.hitbox n)) (.x (.hitbox (.reference group))))
            (.y (.c-group-offset n)) (- (.y (.hitbox n)) (.y (.hitbox (.reference group)))))))
  this)

(defjmethod mirror-hitboxes ((this layered-one-dimensional-compactor))
  (jdo (c-node (.c-nodes (.c-graph this)))
    (let ((h (.hitbox c-node)))
      (setf (.x h) (- (- (.x h)) (.width h))))
    ;; mirroring the offsets inside CGroups
    (when (.parent-node c-node)
      (setf (.x (.c-group-offset c-node))
            (+ (- (.x (.c-group-offset c-node))) (.width (.hitbox (.parent-node c-node)))))))
  (calculate-group-offsets this))

(defjmethod transpose-hitboxes ((this layered-one-dimensional-compactor))
  (jdo (c-node (.c-nodes (.c-graph this)))
    (let ((h (.hitbox c-node)) (o (.c-group-offset c-node)))
      (rotatef (.x h) (.y h))
      (rotatef (.width h) (.height h))
      (rotatef (.x o) (.y o))))
  (calculate-group-offsets this))

(defjmethod calculate-constraints ((this layered-one-dimensional-compactor))
  ;; resetting constraints
  (jdo (c-node (.c-nodes (.c-graph this)))
    (jl-clear (.constraints c-node)))
  (let ((alg (.constraint-algorithm this)))
    (if (functionp alg) (funcall alg this) (calculate-constraints alg this)))
  ;; update the "external" constraints of the groups
  (calculate-constraints-for-c-groups this))

(defjmethod calculate-constraints-for-c-groups ((this layered-one-dimensional-compactor))
  (jdo (group (.c-groups (.c-graph this)))
    (setf (.out-degree group) 0)
    (jl-clear (.incoming-constraints group)))
  (jdo (group (.c-groups (.c-graph this)))
    (jdo (c-node (.c-nodes group))
      (jdo (inc (.constraints c-node))
        (unless (eq (.c-group inc) group)
          ;; adding constraints to each CGroup, that refer to CNodes outside of the CGroup
          (%lm-set-add (.incoming-constraints group) inc)
          ;; setting the outDegree of CGroups
          (incf (.out-degree (.c-group inc))))))))

(defjmethod reverse-constraints ((this layered-one-dimensional-compactor))
  ;; maps CNodes to temporary lists of incoming constraints
  (let ((inc-map (new-jmap)))
    (jdo (c-node (.c-nodes (.c-graph this)))
      (setf (gethash c-node inc-map) (new-jlist)))
    ;; resetting fields of CNodes and reversing constraints
    (jdo (c-node (.c-nodes (.c-graph this)))
      (setf (.start-pos c-node) double.negative-infinity)
      (jdo (inc (.constraints c-node))
        (jl-add (gethash inc inc-map) c-node)))
    ;; write back
    (jdo (c-node (.c-nodes (.c-graph this)))
      (jl-clear (.constraints c-node))
      (setf (.constraints c-node) (gethash c-node inc-map))))
  ;; resetting constraints for CGroups
  (calculate-constraints-for-c-groups this))

;; drawHitboxes is debug output: a no-op
(defjmethod draw-hitboxes ((this layered-one-dimensional-compactor) monitor name)
  (declare (ignore monitor name))
  this)

;;; ------------------------------------------------------------ recthull.Point

(defjctor layered-point (x y)
  (setf (.x this) x (.y this) y))

(defjctor layered-point (x y quadrant)
  (this-ctor layered-point this x y)
  (setf (.quadrant this) quadrant))

(defjstatic layered-point.from (v)
  (new-layered-point (.x v) (.y v)))

(defjmethod to-string ((this layered-point))
  (jstr "(" (.x this) ", " (.y this) (if (.convex this) "cx" "")
        (if (.quadrant this) (name (.quadrant this)) nil) ")"))

(defmethod print-object ((p layered-point) s)
  (print-unreadable-object (p s) (write-string (to-string p) s)))

(defjmethod equals ((this layered-point) obj)
  (and obj (typep obj 'layered-point)
       (eql (.x this) (.x obj)) (eql (.y this) (.y obj))))

;;; Quadrant (enum methods)

(defjmethod is-upper ((this layered-quadrant))
  (or (eq this layered-quadrant.q1) (eq this layered-quadrant.q2)))

(defjmethod is-left ((this layered-quadrant))
  (or (eq this layered-quadrant.q1) (eq this layered-quadrant.q4)))

(defjstatic layered-point.is-both-left-or-both-right (q1 q2)
  (or (and (eq q1 layered-quadrant.q1) (eq q2 layered-quadrant.q4))
      (and (eq q1 layered-quadrant.q4) (eq q2 layered-quadrant.q1))
      (and (eq q1 layered-quadrant.q3) (eq q2 layered-quadrant.q2))
      (and (eq q1 layered-quadrant.q2) (eq q2 layered-quadrant.q3))))

(defjstatic layered-point.is-one-left-one-right (q1 q2)
  (or (and (eq q1 layered-quadrant.q1) (eq q2 layered-quadrant.q2))
      (and (eq q1 layered-quadrant.q1) (eq q2 layered-quadrant.q3))
      (and (eq q1 layered-quadrant.q4) (eq q2 layered-quadrant.q3))
      (and (eq q1 layered-quadrant.q4) (eq q2 layered-quadrant.q2))))

;; Quadrant.isBothLeftOrBothRight spelled after the enum, too
(defjstatic layered-quadrant.is-both-left-or-both-right (q1 q2)
  (layered-point.is-both-left-or-both-right q1 q2))
(defjstatic layered-quadrant.is-one-left-one-right (q1 q2)
  (layered-point.is-one-left-one-right q1 q2))

;;; ------------------------------------------------------------ recthull.Scanline

(defjctor layered-scanline (points comparator event-handlers)
  (setf (.comparator this) comparator
        (.points this) points
        (.event-handlers this) (jlist-from event-handlers)))

(defjstatic layered-scanline.execute (points comparator event-handlers)
  ;; execute(points, comparator, EventHandler) or (..., Iterable<EventHandler>)
  (let ((handlers (if (or (typep event-handlers 'layered-event-handler) (functionp event-handlers))
                      (list event-handlers)
                      event-handlers)))
    ;; copy the points! we will resort them!
    (j-go (new-layered-scanline (jlist-from points) comparator handlers))))

(defjmethod j-go ((this layered-scanline))
  ;; sort
  (jl-sort (.points this) (.comparator this))
  ;; now move scanline
  (jdo (p (.points this))
    (jdo (h (.event-handlers this))
      (if (functionp h) (funcall h p) (handle h p)))))

;;; ------------------------------------------------------------ recthull.RectilinearConvexHull

(defun %lm-right-high-first (p1 p2)
  (if (= (.x p1) (.x p2)) (double.compare (.y p2) (.y p1)) (double.compare (.x p1) (.x p2))))
(defun %lm-right-low-first (p1 p2)
  (if (= (.x p1) (.x p2)) (double.compare (.y p1) (.y p2)) (double.compare (.x p1) (.x p2))))
(defun %lm-left-high-first (p1 p2)
  (if (= (.x p1) (.x p2)) (double.compare (.y p2) (.y p1)) (double.compare (.x p2) (.x p1))))
(defun %lm-left-low-first (p1 p2)
  (if (= (.x p1) (.x p2)) (double.compare (.y p1) (.y p2)) (double.compare (.x p2) (.x p1))))

(defun %lm-right-special-order (p1 p2)
  (if (= (.x p1) (.x p2))
      (block nil
        ;; same quadrant or both either left or right
        (when (or (eq (.quadrant p1) (.quadrant p2))
                  (layered-point.is-both-left-or-both-right (.quadrant p1) (.quadrant p2)))
          ;; for left quadrants concave before convex,
          ;; for right quadrants convex before concave
          (let ((val (if (is-left (.quadrant p1)) 1 -1)))
            (cond ((and (.convex p1) (not (.convex p2))) (return val))
                  ((and (not (.convex p1)) (.convex p2)) (return (- val))))))
        (integer.compare (ordinal (.quadrant p1)) (ordinal (.quadrant p2))))
      (double.compare (.x p1) (.x p2))))

(defjstatic layered-rectilinear-convex-hull.of (points)
  (let ((rch (new-layered-rectilinear-convex-hull)))
    (jdo (p points)
      ;; equality important since the two extremal points can be equal
      (when (or (null (.x-max1 rch)) (>= (.x p) (.x (.x-max1 rch))))
        (setf (.x-max2 rch) (.x-max1 rch) (.x-max1 rch) p))
      (when (or (null (.x-min1 rch)) (<= (.x p) (.x (.x-min1 rch))))
        (setf (.x-min2 rch) (.x-min1 rch) (.x-min1 rch) p))
      (when (or (null (.y-max1 rch)) (>= (.y p) (.y (.y-max1 rch))))
        (setf (.y-max2 rch) (.y-max1 rch) (.y-max1 rch) p))
      (when (or (null (.y-min1 rch)) (<= (.y p) (.y (.y-min1 rch))))
        (setf (.y-min2 rch) (.y-min1 rch) (.y-min1 rch) p)))
    ;; q1 | q2
    ;; ---------
    ;; q4 | q3
    (let ((q1 (new-layered-maximal-elements-event-handler layered-quadrant.q1))
          (q4 (new-layered-maximal-elements-event-handler layered-quadrant.q4))
          (q2 (new-layered-maximal-elements-event-handler layered-quadrant.q2))
          (q3 (new-layered-maximal-elements-event-handler layered-quadrant.q3)))
      (layered-scanline.execute points #'%lm-right-low-first q1)
      (layered-scanline.execute points #'%lm-right-high-first q4)
      (layered-scanline.execute points #'%lm-left-low-first q2)
      (layered-scanline.execute points #'%lm-left-high-first q3)
      ;; the scanline algorithm detected all convex corners,
      ;; now we add the concave corners to the lists
      (layered-rectilinear-convex-hull.add-concave-corners (.points q1) layered-quadrant.q1)
      (layered-rectilinear-convex-hull.add-concave-corners (.points q2) layered-quadrant.q2)
      (layered-rectilinear-convex-hull.add-concave-corners (.points q3) layered-quadrant.q3)
      (layered-rectilinear-convex-hull.add-concave-corners (.points q4) layered-quadrant.q4)
      ;; ... and add everything to the hull list (in clockwise order, q1..q4)
      (let ((hull (get-hull rch)))
        (jl-clear hull)
        (jl-add-all hull (.points q1))
        (jl-add-all hull (reverse (.points q2)))
        (jl-add-all hull (.points q3))
        (jl-add-all hull (reverse (.points q4)))))
    rch))

(defjmethod get-hull ((this layered-rectilinear-convex-hull))
  (.hull this))

(defjmethod split-into-rectangles ((this layered-rectilinear-convex-hull))
  (let ((handler (new-layered-rectangle-event-handler this)))
    (layered-scanline.execute (.hull this) #'%lm-right-special-order handler)
    ;; append the lastly queued rectangle
    (when (.queued handler)
      (jl-add (.rects handler) (.queued handler)))
    (.rects handler)))

(defjstatic layered-rectilinear-convex-hull.add-concave-corners (pts q)
  (when (zerop (length pts)) (jthrow 'no-such-element-exception))
  (let ((last (aref pts 0))
        (i 1))
    (loop while (< i (length pts))
          do (let* ((next (aref pts i))
                    (p (new-layered-point (.x next) (.y last) q)))
               (jl-add-at pts i p)
               (setf (.convex p) nil)
               (incf i 2)
               (setf last next)))))

(defjctor layered-maximal-elements-event-handler (quadrant)
  (setf (.quadrant this) quadrant)
  (enum-case quadrant
    ((layered-quadrant.q1 layered-quadrant.q2)
     (setf (.compare this) (reversed-comparator #'double.compare)
           (.maximal-y this) double.positive-infinity))
    ((layered-quadrant.q3 layered-quadrant.q4)
     (setf (.compare this) #'double.compare
           (.maximal-y this) double.negative-infinity))))

(defjmethod handle ((this layered-maximal-elements-event-handler) p)
  (when (> (funcall (.compare this) (.y p) (.maximal-y this)) 0)
    (jl-add (.points this) (new-layered-point (.x p) (.y p) (.quadrant this)))
    (setf (.maximal-y this) (.y p))))

(defjctor layered-rectangle-event-handler (outer)
  (setf (.outer this) outer
        (.last-x this) (math.min (.x (.x-min1 outer)) (.x (.x-min2 outer)))))

(defjmethod handle ((this layered-rectangle-event-handler) p)
  (when (and (.queued this)
             (or (/= (.x p) (.x (.queued-pnt this)))
                 (layered-point.is-one-left-one-right (.quadrant (.queued-pnt this)) (.quadrant p))))
    (jl-add (.rects this) (.queued this))
    (setf (.last-x this) (+ (.x (.queued this)) (.width (.queued this)))
          (.queued this) nil
          (.queued-pnt this) nil))
  (if (is-upper (.quadrant p))
      (setf (.min-y this) p)
      (setf (.max-y this) p))
  (let ((q (.quadrant p)))
    (when (or (and (eq q layered-quadrant.q1) (not (.convex p)))
              (and (eq q layered-quadrant.q2) (.convex p))
              (and (eq q layered-quadrant.q3) (.convex p))
              (and (eq q layered-quadrant.q4) (not (.convex p))))
      ;; queue a possible rect
      (when (and (.min-y this) (.max-y this))
        (let ((r (new-elk-rectangle (.last-x this) (.y (.min-y this))
                                    (- (.x p) (.last-x this))
                                    (- (.y (.max-y this)) (.y (.min-y this))))))
          (setf (.queued this) r
                (.queued-pnt this) p))))))
