;;;; compaction-scanline.lisp -- Scanline (compaction/Scanline.java), DebugUtil
;;;; (no-op: debug output is not ported), and the oned ScanlineConstraintCalculator.

(in-package #:elk)

;;; ------------------------------------------------------------ CompareFuzzy

(defjconst common-compare-fuzzy.tolerance 0.0001d0)

(defjstatic common-compare-fuzzy.eq (d1 d2)
  (%cm-fuzzy-equals d1 d2 common-compare-fuzzy.tolerance))

(defjstatic common-compare-fuzzy.gt (d1 d2)
  (> (%cm-fuzzy-compare d1 d2 common-compare-fuzzy.tolerance) 0))

(defjstatic common-compare-fuzzy.lt (d1 d2)
  (< (%cm-fuzzy-compare d1 d2 common-compare-fuzzy.tolerance) 0))

(defjstatic common-compare-fuzzy.ge (d1 d2)
  (>= (%cm-fuzzy-compare d1 d2 common-compare-fuzzy.tolerance) 0))

(defjstatic common-compare-fuzzy.le (d1 d2)
  (<= (%cm-fuzzy-compare d1 d2 common-compare-fuzzy.tolerance) 0))


;;; ------------------------------------------------------------ Scanline

(defun %scanline-go (points comparator handlers)
  (jl-sort points comparator)
  (jdo (p points)
    (jdo (h handlers)
      (handle h p))))

(defjstatic common-scanline.execute (points comparator handlers)
  ;; Java has two overloads (Iterable<EventHandler> and a single EventHandler);
  ;; both arrive here.  A collection of handlers is a jlist or a CL list.
  (let ((copy (jlist-from points))
        (hs (if (or (listp handlers) (jlist-p handlers))
                (jlist-from handlers)
                (jlist handlers))))
    (%scanline-go copy comparator hs)))

;;; ------------------------------------------------------------ DebugUtil

(defjstatic common-debug-util.draw-hitboxes (c-graph file-name)
  (declare (ignore c-graph file-name))
  nil)

;;; ------------------------------------------------------------ oned Timestamp

(defjctor common-oned-timestamp (node low)
  (setf (.node this) node
        (.low this) low))

;;; ------------------------------------------------------------ ScanlineConstraintCalculator

;; TreeSet<CNode> helpers.  The intervals set is ordered by a comparator and
;; never holds two comparator-equal elements (add fails then), so it is a sorted
;; vector and every query on a member is an index step.
(defun %sc-tree-index (set node cmp)
  "Index of the element comparator-equal to NODE, or NIL."
  (let ((lo 0) (hi (length set)))
    (loop while (< lo hi)
          do (let* ((mid (floor (+ lo hi) 2))
                    (c (funcall cmp (aref set mid) node)))
               (cond ((zerop c) (return-from %sc-tree-index mid))
                     ((< c 0) (setf lo (1+ mid)))
                     (t (setf hi mid)))))
    nil))

(defun %sc-tree-add (set node cmp)
  (let ((lo 0) (hi (length set)))
    (loop while (< lo hi)
          do (let* ((mid (floor (+ lo hi) 2))
                    (c (funcall cmp (aref set mid) node)))
               (cond ((zerop c) (return-from %sc-tree-add nil))
                     ((< c 0) (setf lo (1+ mid)))
                     (t (setf hi mid)))))
    (jl-add-at set lo node)
    t))

(defun %sc-tree-remove (set node cmp)
  (let ((i (%sc-tree-index set node cmp)))
    (when i (jl-remove-at set i) t)))

(defun %sc-center-x-compare (c1 c2)
  (double.compare (+ (.x (.hitbox c1)) (/ (.width (.hitbox c1)) 2))
                  (+ (.x (.hitbox c2)) (/ (.width (.hitbox c2)) 2))))

(defun %sc-lower (set node cmp)
  "TreeSet.lower of a member: its predecessor, or NIL."
  (let ((i (%sc-tree-index set node cmp)))
    (and i (> i 0) (aref set (1- i)))))

(defun %sc-higher (set node cmp)
  (let ((i (%sc-tree-index set node cmp)))
    (and i (< (1+ i) (length set)) (aref set (1+ i)))))

(defjmethod calculate-constraints ((this common-scanline-constraint-calculator) the-compactor)
  (setf (.compactor this) the-compactor)
  (sweep this (lambda (n) (declare (ignore n)) t)))

(defjmethod sweep ((this common-scanline-constraint-calculator) filter-fun)
  (let ((points (new-jlist)))
    (jdo (n (.c-nodes (.c-graph (.compactor this))))
      (when (funcall filter-fun n)
        (jl-add points (new-common-oned-timestamp n t))
        (jl-add points (new-common-oned-timestamp n nil))))
    (%cco-reset (.constraints-scanline-handler this) (.compactor this))
    (common-scanline.execute points (.constraints-scanline-comparator this)
                             (.constraints-scanline-handler this))))

(defun %sc-scanline-compare (p1 p2)
  (let ((y1 (.y (.hitbox (.node p1))))
        (y2 (.y (.hitbox (.node p2)))))
    (unless (.low p1)
      (incf y1 (.height (.hitbox (.node p1)))))
    (unless (.low p2)
      (incf y2 (.height (.hitbox (.node p2)))))
    (let ((cmp (double.compare y1 y2)))
      (when (zerop cmp)
        (cond ((and (not (.low p1)) (.low p2)) (return-from %sc-scanline-compare -1))
              ((and (not (.low p2)) (.low p1)) (return-from %sc-scanline-compare 1))))
      cmp)))

(defun %cco-reset (handler compactor)
  "ConstraintsScanlineHandler.reset()"
  (setf (.outer handler) compactor)
  (jl-clear (.intervals handler))
  (setf (.cand handler) (make-array (length (.c-nodes (.c-graph compactor))) :initial-element nil))
  (let ((index 0))
    (jdo (n (.c-nodes (.c-graph compactor)))
      (setf (.id n) index)
      (incf index))))

(defun %cco-overlap (n1 n2)
  (cond ((or (null n1) (null n2) (eq n1 n2)) nil)
        (t (and (common-compare-fuzzy.le (.x (.hitbox n1)) (+ (.x (.hitbox n2)) (.width (.hitbox n2))))
                (common-compare-fuzzy.le (.x (.hitbox n2)) (+ (.x (.hitbox n1)) (.width (.hitbox n1))))))))

(defjmethod handle ((this common-constraints-scanline-handler) p)
  (if (.low p)
      (%cco-insert this p)
      (%cco-delete this p)))

(defun %cco-insert (this p)
  (let ((intervals (.intervals this)))
    (unless (%sc-tree-add intervals (.node p) #'%sc-center-x-compare)
      (jthrow 'illegal-state-exception "Invalid hitboxes for scanline constraint calculation."))
    (when (or (%cco-overlap (.node p) (%cco-floor intervals (.node p)))
              (%cco-overlap (.node p) (%cco-ceiling intervals (.node p))))
      (format *error-output* "~A has overlap.~%" (to-string (.node p))))
    (setf (aref (.cand this) (.id (.node p)))
          (%sc-lower intervals (.node p) #'%sc-center-x-compare))
    (let ((right (%sc-higher intervals (.node p) #'%sc-center-x-compare)))
      (when right
        (setf (aref (.cand this) (.id right)) (.node p))))))

(defun %cco-floor (intervals node)
  "TreeSet.floor of a member: the member itself."
  (let ((i (%sc-tree-index intervals node #'%sc-center-x-compare)))
    (and i (aref intervals i))))

(defun %cco-ceiling (intervals node)
  "TreeSet.ceiling of a member: the member itself."
  (let ((i (%sc-tree-index intervals node #'%sc-center-x-compare)))
    (and i (aref intervals i))))

(defun %cco-delete (this p)
  (let* ((intervals (.intervals this))
         (node (.node p))
         (left (%sc-lower intervals node #'%sc-center-x-compare)))
    (when (and left (eq left (aref (.cand this) (.id node))))
      (when (and (.c-group left) (not (eq (.c-group left) (.c-group node))))
        (jl-add (.constraints left) node)))
    (let ((right (%sc-higher intervals node #'%sc-center-x-compare)))
      (when (and right (eq (aref (.cand this) (.id right)) node))
        (when (and (.c-group right) (not (eq (.c-group right) (.c-group node))))
          (jl-add (.constraints node) right))))
    (%sc-tree-remove intervals node #'%sc-center-x-compare)))
