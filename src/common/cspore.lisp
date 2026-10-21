;;;; cspore.lisp -- org.eclipse.elk.alg.common.spore methods
;;;; The SVG debug output of ScanlineOverlapCheck / DepthFirstCompaction is a
;;;; no-op here (debug output is not ported); the SVGImage parameters are kept
;;;; in the signatures and ignored.

(in-package #:elk)

;;; ------------------------------------------------------------ InternalProperties
;;; FUZZINESS (0.0001) is defined in root.lisp, which loads first.

(defproperty common-internal-properties.debug-svg "debugSVG" nil)
(defproperty common-internal-properties.overlaps-existed "overlapsExisted" t)

;;; DoubleMath.fuzzyCompare(a, b, FUZZINESS)
(defun %spore-fuzzy-compare (a b)
  (%cm-fuzzy-compare a b common-internal-properties.fuzziness))

;;; ------------------------------------------------------------ Node

(defjctor node (v r)
  (setf (.original-vertex this) v
        (.vertex this) (clone v)
        (.rect this) (new-elk-rectangle r)))

(defjmethod translate ((this node) v)
  (add (.vertex this) v)
  (incf (.x (.rect this)) (.x v))
  (incf (.y (.rect this)) (.y v)))

(defjmethod set-center-position ((this node) p)
  (translate this (sub (clone p) (.vertex this))))

(defjmethod underlap ((this node) other)
  (let* ((rect (.rect this))
         (orect (.rect other))
         (horizontal-center-distance
           (math.abs (- (.x (get-center rect)) (.x (get-center orect)))))
         (vertical-center-distance
           (math.abs (- (.y (get-center rect)) (.y (get-center orect)))))
         (horizontal-underlap 0d0)
         (vertical-underlap 0d0)
         (h-scale 1d0)
         (v-scale 1d0))
    (when (> horizontal-center-distance (+ (/ (.width rect) 2.0d0) (/ (.width orect) 2.0d0)))
      (setf horizontal-underlap
            (math.min (math.abs (- (.x rect) (+ (.x orect) (.width orect))))
                      (math.abs (- (+ (.x rect) (.width rect)) (.x orect)))))
      (setf h-scale (- 1 (/ horizontal-underlap horizontal-center-distance))))
    (when (> vertical-center-distance (+ (/ (.height rect) 2.0d0) (/ (.height orect) 2.0d0)))
      (setf vertical-underlap
            (math.min (math.abs (- (.y rect) (+ (.y orect) (.height orect))))
                      (math.abs (- (+ (.y rect) (.height rect)) (.y orect)))))
      (setf v-scale (- 1 (/ vertical-underlap vertical-center-distance))))
    (let ((scale (math.min h-scale v-scale)))
      (* (- 1 scale)
         (math.sqrt (+ (* horizontal-center-distance horizontal-center-distance)
                       (* vertical-center-distance vertical-center-distance)))))))

(defjmethod distance ((this node) other v)
  (let ((result double.positive-infinity))
    (jdo (e1 (utils.get-rect-edges (.rect this)))
      (jdo (e2 (utils.get-rect-edges (.rect other)))
        (let ((d (elk-math.distance (.u e1) (.v e1) (.u e2) (.v e2) v)))
          (setf result (math.min result d)))))
    result))

(defjmethod touches ((this node) other)
  (let ((r (.rect this)) (o (.rect other)))
    (and (<= (%spore-fuzzy-compare (.x r) (+ (.x o) (.width o))) 0)
         (<= (%spore-fuzzy-compare (.x o) (+ (.x r) (.width r))) 0)
         (<= (%spore-fuzzy-compare (.y r) (+ (.y o) (.height o))) 0)
         (<= (%spore-fuzzy-compare (.y o) (+ (.y r) (.height r))) 0))))

;;; ------------------------------------------------------------ DepthFirstCompaction

(defvar *depth-first-compaction-root* nil)
(defvar *depth-first-compaction-orthogonal* nil)

(defjstatic depth-first-compaction.compact (tree orthogonal debug-output-file)
  (declare (ignore debug-output-file))
  (setf *depth-first-compaction-orthogonal* orthogonal
        *depth-first-compaction-root* tree)
  (depth-first-compaction.compact-tree tree))

(defjstatic depth-first-compaction.compact (tree orthogonal)
  (depth-first-compaction.compact tree orthogonal nil))

(defjstatic depth-first-compaction.compact-tree (tree)
  (jdo (c (.children tree))
    (depth-first-compaction.compact-tree c))
  (jdo (child (.children tree))
    (let* ((compaction-vector (sub (clone (.vertex (.node tree))) (.vertex (.node child))))
           (min-underlap 0d0))
      (if *depth-first-compaction-orthogonal*
          (let ((rt (.rect (.node tree)))
                (rc (.rect (.node child))))
            (if (>= (math.abs (.x compaction-vector)) (math.abs (.y compaction-vector)))
                (progn
                  (setf (.y compaction-vector) 0d0)
                  (when (and (> (+ (.y rc) (.height rc)) (.y rt))
                             (< (.y rc) (+ (.y rt) (.height rt))))
                    (scale-to-length compaction-vector
                                     (math.max (- (.x rt) (+ (.x rc) (.width rc)))
                                               (- (.x rc) (+ (.x rt) (.width rt)))))))
                (progn
                  (setf (.x compaction-vector) 0d0)
                  (when (and (> (+ (.x rc) (.width rc)) (.x rt))
                             (< (.x rc) (+ (.x rt) (.width rt))))
                    (scale-to-length compaction-vector
                                     (math.max (- (.y rt) (+ (.y rc) (.height rc)))
                                               (- (.y rc) (+ (.y rt) (.height rt)))))))))
          (scale-to-length compaction-vector (underlap (.node tree) (.node child))))
      (setf min-underlap (j-length compaction-vector))
      (setf min-underlap (%dfc-get-min-underlap *depth-first-compaction-root* child
                                                min-underlap compaction-vector))
      (scale-to-length compaction-vector min-underlap)
      (%dfc-translate-subtree child compaction-vector))))

(defun %dfc-get-min-underlap (tree child current-min-underlap compaction-vector)
  (let ((min-underlap (math.min current-min-underlap
                                (%dfc-min-underlap-with-subtree (.node tree) child
                                                                current-min-underlap compaction-vector))))
    (jdo (c (.children tree))
      (unless (eq c child)
        (setf min-underlap
              (math.min min-underlap
                        (%dfc-get-min-underlap c child min-underlap compaction-vector)))))
    min-underlap))

(defun %dfc-min-underlap-with-subtree (r tree current-min-underlap compaction-vector)
  (let ((min-underlap current-min-underlap))
    (block scan
      (jdo (child (.children tree))
        (let* ((c-node (.node child))
               (c (.rect c-node))
               (rr (.rect r)))
          (if (touches r c-node)
              (when (or (and (= (%spore-fuzzy-compare (.x c) (+ (.x rr) (.width rr))) 0)
                             (< (.x compaction-vector) 0))
                        (and (= (%spore-fuzzy-compare (+ (.x c) (.width c)) (.x rr)) 0)
                             (> (.x compaction-vector) 0))
                        (and (= (%spore-fuzzy-compare (.y c) (+ (.y rr) (.height rr))) 0)
                             (< (.y compaction-vector) 0))
                        (and (= (%spore-fuzzy-compare (+ (.y c) (.height c)) (.y rr)) 0)
                             (> (.y compaction-vector) 0)))
                (setf min-underlap 0d0)
                (return-from scan))
              (setf min-underlap (math.min min-underlap (distance r c-node compaction-vector))))
          (setf min-underlap
                (math.min min-underlap
                          (%dfc-min-underlap-with-subtree r child min-underlap compaction-vector))))))
    min-underlap))

(defun %dfc-translate-subtree (tree compaction-vector)
  (translate (.node tree) compaction-vector)
  (jdo (c (.children tree))
    (%dfc-translate-subtree c compaction-vector)))

;;; TreeSet helpers: a sorted adjustable vector driven by a comparator.  add
;;; rejects a comparator-equal element (TreeSet.add returns false); remove takes
;;; out the comparator-equal element.  Iteration order is ascending.

(defun %sp-tree-add (set node cmp)
  (let ((lo 0) (hi (length set)))
    (loop while (< lo hi)
          do (let* ((mid (floor (+ lo hi) 2))
                    (c (funcall cmp (aref set mid) node)))
               (cond ((zerop c) (return-from %sp-tree-add nil))
                     ((< c 0) (setf lo (1+ mid)))
                     (t (setf hi mid)))))
    (jl-add-at set lo node)
    t))

(defun %sp-tree-remove (set node cmp)
  (let ((lo 0) (hi (length set)))
    (loop while (< lo hi)
          do (let* ((mid (floor (+ lo hi) 2))
                    (c (funcall cmp (aref set mid) node)))
               (cond ((zerop c) (jl-remove-at set mid) (return-from %sp-tree-remove t))
                     ((< c 0) (setf lo (1+ mid)))
                     (t (setf hi mid)))))
    nil))

;;; ------------------------------------------------------------ ScanlineOverlapCheck

(defjctor scanline-overlap-check (oh dbg)
  ;; SVGImage debug output is not ported; DBG is accepted and ignored.
  dbg
  (setf (.overlap-handler this) oh
        (.overlaps-scanline-comparator this)
        (lambda (p1 p2)
          (let ((y1 (.y (.rect (.node p1))))
                (y2 (.y (.rect (.node p2)))))
            (unless (.low p1)
              (incf y1 (.height (.rect (.node p1)))))
            (unless (.low p2)
              (incf y2 (.height (.rect (.node p2)))))
            (let ((cmp (double.compare y1 y2)))
              (when (zerop cmp)
                (cond ((and (not (.low p1)) (.low p2)) (setf cmp -1))
                      ((and (not (.low p2)) (.low p1)) (setf cmp 1))))
              cmp)))))

(defjmethod sweep ((this scanline-overlap-check) nodes)
  (let ((points (new-jlist)))
    (jdo (n nodes)
      (jl-add points (new-common-spore-timestamp n t))
      (jl-add points (new-common-spore-timestamp n nil)))
    (let ((handler (new-overlaps-scanline-handler)))
      (setf (.outer handler) this)
      (init handler)
      (common-scanline.execute points (.overlaps-scanline-comparator this) handler))))

(defjctor common-spore-timestamp (node low)
  (setf (.node this) node
        (.low this) low))

(defun %sp-interval-compare (n1 n2)
  (let ((cmp (double.compare (.x (.rect n1)) (.x (.rect n2)))))
    (unless (zerop cmp) (return-from %sp-interval-compare cmp))
    (setf cmp (double.compare (.x (.original-vertex n1)) (.x (.original-vertex n2))))
    (unless (zerop cmp) (return-from %sp-interval-compare cmp))
    (double.compare (.y (.original-vertex n1)) (.y (.original-vertex n2)))))

(defjmethod init ((this overlaps-scanline-handler))
  (jl-clear (.intervals-sort-left this)))

(defjmethod handle ((this overlaps-scanline-handler) p)
  (if (.low p)
      (%osh-insert this p)
      (%osh-delete this p)))

(defun %osh-insert (this p)
  (unless (%sp-tree-add (.intervals-sort-left this) (.node p) #'%sp-interval-compare)
    (jthrow 'illegal-state-exception "Invalid hitboxes for scanline overlap calculation."))
  (let ((overlaps-found nil)
        (outer (.outer this)))
    (block scan
      (jdo (other (.intervals-sort-left this))
        (if (%sosc-overlap (.node p) other)
            (progn
              (handle (.overlap-handler outer) (.node p) other)
              (setf overlaps-found t))
            (when overlaps-found
              (return-from scan)))))))

(defun %osh-delete (this p)
  (%sp-tree-remove (.intervals-sort-left this) (.node p) #'%sp-interval-compare))

(defun %sosc-overlap (n1 n2)
  (if (or (null n1) (null n2) (eq n1 n2))
      nil
      (and (< (%spore-fuzzy-compare (.x (.rect n1)) (+ (.x (.rect n2)) (.width (.rect n2)))) 0)
           (< (%spore-fuzzy-compare (.x (.rect n2)) (+ (.x (.rect n1)) (.width (.rect n1)))) 0))))
