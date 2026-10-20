;;;; counting.lisp -- org.eclipse.elk.alg.layered.p3order.counting

(in-package #:elk)

(defun %p3-reversed (list)
  "Guava Lists.reverse, as a fresh jlist (callers only read it)."
  (let ((v (new-jlist (max 1 (length list)))))
    (loop for i from (1- (length list)) downto 0 do (vector-push-extend (aref list i) v))
    v))

(defun %p3-int-array (n) (make-array n :initial-element 0))
(defun %p3-bool-array (n) (make-array n :initial-element nil))
(defun %p3-float-array (n) (make-array n :element-type 'single-float :initial-element 0f0))

;;; ---------------------------------------------------------------- IInitializable

(defjstatic i-initializable.init (initializables order)
  (loop for l from 0 below (length order) do
    (jdo (i initializables) (init-at-layer-level i l order))
    (loop for n from 0 below (length (aref order l)) do
      (jdo (i initializables) (init-at-node-level i l n order))
      (let ((ports (get-ports (aref (aref order l) n))))
        (loop for p from 0 below (length ports) do
          (jdo (i initializables) (init-at-port-level i l n p order))
          (let ((port (aref ports p)) (e 0))
            (jdo (edge (get-connected-edges port))
              (jdo (i initializables)
                (init-at-edge-level i l n p e edge order)
                (incf e))))))))
  (jdo (i initializables) (init-after-traversal i)))

(defjmethod init-at-layer-level ((this i-initializable) l node-order)
  (declare (ignore l node-order)) nil)
(defjmethod init-at-node-level ((this i-initializable) l n node-order)
  (declare (ignore l n node-order)) nil)
(defjmethod init-at-port-level ((this i-initializable) l n p node-order)
  (declare (ignore l n p node-order)) nil)
(defjmethod init-at-edge-level ((this i-initializable) l n p e edge node-order)
  (declare (ignore l n p e edge node-order)) nil)
(defjmethod init-after-traversal ((this i-initializable)) nil)

;;; ---------------------------------------------------------------- CrossMinUtil

(defjstatic cross-min-util.in-north-south-east-west-order (node side)
  (enum-case side
    ((port-side.east port-side.north) (get-port-side-view node side))
    ((port-side.south port-side.west) (%p3-reversed (get-port-side-view node side)))
    (t (new-jlist))))

;;; ---------------------------------------------------------------- BinaryIndexedTree

(defjctor binary-indexed-tree (max-num)
  (setf (.max-num this) max-num
        (.binary-sums this) (%p3-int-array (1+ max-num))
        (.nums-per-index this) (%p3-int-array max-num)
        (.size this) 0))

(defjmethod add ((this binary-indexed-tree) index)
  (incf (.size this))
  (incf (aref (.nums-per-index this) index))
  (let ((i (1+ index)) (sums (.binary-sums this)))
    (loop while (< i (length sums))
          do (incf (aref sums i))
             (incf i (logand i (- i))))))

(defjmethod rank ((this binary-indexed-tree) index)
  (let ((i index) (sum 0) (sums (.binary-sums this)))
    (loop while (> i 0)
          do (incf sum (aref sums i))
             (decf i (logand i (- i))))
    sum))

(defmethod size/0 ((this binary-indexed-tree)) (.size this))

(defjmethod remove-all ((this binary-indexed-tree) index)
  (let ((num-entries (aref (.nums-per-index this) index)))
    (unless (zerop num-entries)
      (setf (aref (.nums-per-index this) index) 0)
      (decf (.size this) num-entries)
      (let ((i (1+ index)) (sums (.binary-sums this)))
        (loop while (< i (length sums))
              do (decf (aref sums i) num-entries)
                 (incf i (logand i (- i))))))))

(defmethod clear/0 ((this binary-indexed-tree))
  (setf (.binary-sums this) (%p3-int-array (1+ (.max-num this)))
        (.nums-per-index this) (%p3-int-array (.max-num this))
        (.size this) 0))

(defmethod is-empty/0 ((this binary-indexed-tree)) (zerop (.size this)))

;;; ---------------------------------------------------------------- CrossingsCounter

(defjctor crossings-counter (port-positions)
  (setf (.port-positions this) port-positions
        (.ends this) (new-jlist)))

(defun %cc-position-of (this port) (aref (.port-positions this) (.id port)))
(defun %cc-other-end-of (edge from-port)
  (if (eq from-port (get-source edge)) (get-target edge) (get-source edge)))
(defun %cc-is-in-layer (edge)
  (eq (get-layer (get-node (get-source edge))) (get-layer (get-node (get-target edge)))))
(defun %cc-is-port-self-loop (edge) (eq (get-source edge) (get-target edge)))

(defjmethod count-crossings-between-layers ((this crossings-counter) left-layer-nodes right-layer-nodes)
  (let ((ports (%cc-init-port-positions-counter-clockwise this left-layer-nodes right-layer-nodes)))
    (setf (.index-tree this) (new-binary-indexed-tree (length ports)))
    (%cc-count-crossings-on-ports this ports)))

(defjmethod count-in-layer-crossings-on-side ((this crossings-counter) nodes side)
  (let ((ports (init-port-positions-for-in-layer-crossings this nodes side)))
    (%cc-count-in-layer-crossings-on-ports this ports)))

(defjmethod count-north-south-port-crossings-in-layer ((this crossings-counter) layer)
  (let ((ports (%cc-init-positions-for-north-south-counting this layer)))
    (setf (.index-tree this) (new-binary-indexed-tree (length ports)))
    (%cc-count-north-south-crossings-on-ports this ports)))

(defun %cc-sort-by-position (this ports)
  (jl-sort ports (lambda (a b) (integer.compare (%cc-position-of this a) (%cc-position-of this b)))))

(defjmethod count-crossings-between-ports-in-both-orders ((this crossings-counter) upper-port lower-port)
  (let* ((ports (%cc-connected-ports-sorted-by-position this upper-port lower-port))
         (upper-lower (%cc-count-crossings-on-ports this ports)))
    ;; Since we might add end positions of ports which are not in the ports
    ;; list, we need to explicitly clear the index tree.
    (clear (.index-tree this))
    (switch-ports this upper-port lower-port)
    (%cc-sort-by-position this ports)
    (let ((lower-upper (%cc-count-crossings-on-ports this ports)))
      (clear (.index-tree this))
      (switch-ports this lower-port upper-port)
      (pair.of upper-lower lower-upper))))

(defjmethod count-in-layer-crossings-between-nodes-in-both-orders ((this crossings-counter) upper-node lower-node side)
  (let* ((ports (%cc-connected-in-layer-ports-sorted-by-position this upper-node lower-node side))
         (upper-lower (%cc-count-in-layer-crossings-on-ports this ports)))
    (switch-nodes this upper-node lower-node side)
    (clear (.index-tree this))
    (%cc-sort-by-position this ports)
    (let ((lower-upper (%cc-count-in-layer-crossings-on-ports this ports)))
      (switch-nodes this lower-node upper-node side)
      (clear (.index-tree this))
      (pair.of upper-lower lower-upper))))

(defjmethod init-for-counting-between ((this crossings-counter) left-layer-nodes right-layer-nodes)
  (let ((ports (%cc-init-port-positions-counter-clockwise this left-layer-nodes right-layer-nodes)))
    (setf (.index-tree this) (new-binary-indexed-tree (length ports)))))

(defjmethod init-port-positions-for-in-layer-crossings ((this crossings-counter) nodes side)
  (let ((ports (new-jlist)))
    (%cc-init-positions this nodes ports side t t)
    (setf (.index-tree this) (new-binary-indexed-tree (length ports)))
    ports))

(defjmethod switch-ports ((this crossings-counter) top-port bottom-port)
  (let* ((pp (.port-positions this))
         (top-pos (aref pp (.id top-port))))
    (setf (aref pp (.id top-port)) (aref pp (.id bottom-port))
          (aref pp (.id bottom-port)) top-pos)))

(defjmethod switch-nodes ((this crossings-counter) was-upper-node was-lower-node side)
  (let ((pp (.port-positions this)) (card (.node-cardinalities this)))
    (jdo (port (cross-min-util.in-north-south-east-west-order was-upper-node side))
      (setf (aref pp (.id port)) (+ (%cc-position-of this port) (aref card (.id was-lower-node)))))
    (jdo (port (cross-min-util.in-north-south-east-west-order was-lower-node side))
      (setf (aref pp (.id port)) (- (%cc-position-of this port) (aref card (.id was-upper-node)))))))

;;; TreeSet ordered by position: positions are distinct per port, so a set
;;; keyed on position (first wins) sorted ascending is equivalent.
(defun %cc-tree-set-add (this set port)
  (let ((pos (%cc-position-of this port)))
    (unless (find pos set :key (lambda (p) (%cc-position-of this p)))
      (vector-push-extend port set))))

(defun %cc-connected-in-layer-ports-sorted-by-position (this upper-node lower-node side)
  (let ((ports (new-jlist)))
    (dolist (node (list upper-node lower-node))
      (jdo (port (cross-min-util.in-north-south-east-west-order node side))
        (jdo (edge (get-connected-edges port))
          (unless (is-self-loop edge)
            (%cc-tree-set-add this ports port)
            (when (%cc-is-in-layer edge)
              (%cc-tree-set-add this ports (%cc-other-end-of edge port)))))))
    (%cc-sort-by-position this ports)
    ports))

(defun %cc-connected-ports-sorted-by-position (this upper-port lower-port)
  (let ((ports (new-jlist)))
    (dolist (port (list upper-port lower-port))
      (%cc-tree-set-add this ports port)
      (jdo (edge (get-connected-edges port))
        (unless (%cc-is-port-self-loop edge)
          (%cc-tree-set-add this ports (%cc-other-end-of edge port)))))
    (%cc-sort-by-position this ports)
    ports))

(defun %cc-drain-ends (this)
  (let ((ends (.ends this)) (tree (.index-tree this)))
    ;; ArrayDeque push/pop = LIFO at the head; we push/pop at the tail.
    (loop while (plusp (length ends))
          do (add tree (vector-pop ends)))))

(defun %cc-count-crossings-on-ports (this ports)
  (let ((crossings 0) (tree (.index-tree this)))
    (jdo (port ports)
      (remove-all tree (%cc-position-of this port))
      (jdo (edge (get-connected-edges port))
        (let ((end-position (%cc-position-of this (%cc-other-end-of edge port))))
          (when (> end-position (%cc-position-of this port))
            (incf crossings (rank tree end-position))
            (vector-push-extend end-position (.ends this)))))
      (%cc-drain-ends this))
    crossings))

(defun %cc-count-in-layer-crossings-on-ports (this ports)
  (let ((crossings 0) (tree (.index-tree this)))
    (jdo (port ports)
      (remove-all tree (%cc-position-of this port))
      (let ((num-between-layer-edges 0))
        (jdo (edge (get-connected-edges port))
          (if (%cc-is-in-layer edge)
              (let ((end-position (%cc-position-of this (%cc-other-end-of edge port))))
                (when (> end-position (%cc-position-of this port))
                  (incf crossings (rank tree end-position))
                  (vector-push-extend end-position (.ends this))))
              (incf num-between-layer-edges)))
        (incf crossings (* (size tree) num-between-layer-edges)))
      (%cc-drain-ends this))
    crossings))

(defun %cc-count-north-south-crossings-on-ports (this ports)
  (let ((crossings 0) (tree (.index-tree this)) (targets-and-degrees (new-jlist)))
    (jdo (port ports)
      (remove-all tree (%cc-position-of this port))
      (jl-clear targets-and-degrees)
      (enum-case (get-type (get-node port))
        (node-type.normal
         (let ((dummy (get-property port layered-internal-properties.port-dummy)))
           (jdo (p (get-ports dummy))
             (jl-add targets-and-degrees (cons p (get-degree p))))))
        (node-type.long-edge
         (let ((p (find-if (lambda (p) (not (eq p port))) (get-ports (get-node port)))))
           (when p (jl-add targets-and-degrees (cons p (get-degree p))))))
        (node-type.north-south-port
         (let ((dummy-port (get-property port layered-internal-properties.origin)))
           (jl-add targets-and-degrees (cons dummy-port (get-degree port))))))
      (jdo (td targets-and-degrees)
        (let ((end-position (%cc-position-of this (car td))))
          (when (> end-position (%cc-position-of this port))
            (incf crossings (* (rank tree end-position) (cdr td)))
            (vector-push-extend end-position (.ends this)))))
      (%cc-drain-ends this))
    crossings))

(defun %cc-get-ports (node side top-down)
  (if (eq side port-side.east)
      (if top-down (get-port-side-view node side) (%p3-reversed (get-port-side-view node side)))
      (if top-down (%p3-reversed (get-port-side-view node side)) (get-port-side-view node side))))

(defun %cc-init-positions (this nodes ports side top-down get-cardinalities)
  (let ((num-ports (length ports)) (pp (.port-positions this)))
    (when get-cardinalities
      (setf (.node-cardinalities this) (%p3-int-array (length nodes))))
    (loop for i = (if top-down 0 (1- (length nodes))) then (+ i (if top-down 1 -1))
          while (if top-down (< i (length nodes)) (>= i 0))
          do (let* ((node (aref nodes i))
                    (node-ports (%cc-get-ports node side top-down)))
               (when get-cardinalities
                 (setf (aref (.node-cardinalities this) (.id node)) (length node-ports)))
               (jdo (port node-ports)
                 (setf (aref pp (.id port)) num-ports)
                 (incf num-ports))
               (jl-add-all ports node-ports)))))

(defun %cc-init-port-positions-counter-clockwise (this left-layer-nodes right-layer-nodes)
  (let ((ports (new-jlist)))
    (%cc-init-positions this left-layer-nodes ports port-side.east t nil)
    (%cc-init-positions this right-layer-nodes ports port-side.west nil nil)
    ports))

(defun %cc-is-layout-unit-changed (last-unit node)
  (if (or (null last-unit) (eq last-unit node)
          (not (has-property node layered-internal-properties.in-layer-layout-unit)))
      nil
      (not (eq (get-property node layered-internal-properties.in-layer-layout-unit) last-unit))))

(defun %cc-empty-stack (this stack ports side start-index)
  (let ((index start-index))
    (loop while (plusp (length stack))
          do (let* ((dummy (vector-pop stack))
                    (p (jl-get (get-port-side-view dummy side) 0)))
               (setf (aref (.port-positions this) (.id p)) index)
               (incf index)
               (jl-add ports p)))
    index))

(defun %cc-init-positions-for-north-south-counting (this nodes)
  ;; INDEXING_SIDE = WEST, STACK_SIDE = EAST.  The deque is a stack: push and
  ;; pop at the same end (we use the tail).
  (let ((ports (new-jlist)) (stack (new-jlist)) (last-layout-unit nil) (index 0)
        (pp (.port-positions this)))
    (flet ((index-port (p) (setf (aref pp (.id p)) index) (incf index) (jl-add ports p))
           (ns-ports-with-incident-edges (node side)
             (remove-if-not (lambda (p) (has-property p layered-internal-properties.port-dummy))
                            (coerce (get-port-side-view node side) 'list))))
      (loop for i from 0 below (length nodes) do
        (let ((current (aref nodes i)))
          (when (%cc-is-layout-unit-changed last-layout-unit current)
            (setf index (%cc-empty-stack this stack ports port-side.east index)))
          (when (has-property current layered-internal-properties.in-layer-layout-unit)
            (setf last-layout-unit (get-property current layered-internal-properties.in-layer-layout-unit)))
          (enum-case (get-type current)
            (node-type.normal
             (dolist (p (ns-ports-with-incident-edges current port-side.north)) (index-port p))
             (setf index (%cc-empty-stack this stack ports port-side.east index))
             (dolist (p (ns-ports-with-incident-edges current port-side.south)) (index-port p)))
            (node-type.north-south-port
             (unless (jl-is-empty (get-port-side-view current port-side.west))
               (index-port (jl-get (get-port-side-view current port-side.west) 0)))
             (unless (jl-is-empty (get-port-side-view current port-side.east))
               (vector-push-extend current stack)))
            (node-type.long-edge
             (jdo (p (get-port-side-view current port-side.west)) (index-port p))
             ;; one stack entry per EAST port
             (dotimes (k (jsize (get-port-side-view current port-side.east)))
               (vector-push-extend current stack))))))
      (%cc-empty-stack this stack ports port-side.east index)
      ports)))

;;; ---------------------------------------------------------------- AllCrossingsCounter

(defjctor all-crossings-counter (graph)
  (setf (.in-layer-edge-counts this) (%p3-int-array (length graph))
        (.has-north-south-ports this) (%p3-bool-array (length graph))
        (.has-hyper-edges-east-of-index this) (%p3-bool-array (length graph))
        (.n-ports this) 0))

(defjmethod count-all-crossings ((this all-crossings-counter) current-order)
  (if (zerop (length current-order))
      0
      (let* ((cc (.crossing-counter this))
             (crossings (+ (count-in-layer-crossings-on-side cc (aref current-order 0) port-side.west)
                           (count-in-layer-crossings-on-side
                            cc (aref current-order (1- (length current-order))) port-side.east))))
        (loop for layer-index from 0 below (length current-order)
              do (incf crossings (%acc-count-crossings-at this layer-index current-order)))
        crossings)))

(defun %acc-count-crossings-at (this layer-index current-order)
  (let ((total 0) (left-layer (aref current-order layer-index)) (cc (.crossing-counter this)))
    (when (< layer-index (1- (length current-order)))
      (let ((right-layer (aref current-order (1+ layer-index))))
        (if (aref (.has-hyper-edges-east-of-index this) layer-index)
            (setf total (+ (count-crossings (.hyperedge-crossings-counter this) left-layer right-layer)
                           (count-in-layer-crossings-on-side cc left-layer port-side.east)
                           (count-in-layer-crossings-on-side cc right-layer port-side.west)))
            (setf total (count-crossings-between-layers cc left-layer right-layer)))))
    (when (aref (.has-north-south-ports this) layer-index)
      (incf total (count-north-south-port-crossings-in-layer cc left-layer)))
    total))

(defjmethod init-at-node-level ((this all-crossings-counter) l n node-order)
  (let ((node (aref (aref node-order l) n)))
    (when (eq (get-type node) node-type.north-south-port)
      (setf (aref (.has-north-south-ports this) l) t))))

(defjmethod init-at-port-level ((this all-crossings-counter) l n p node-order)
  (let ((port (jl-get (get-ports (aref (aref node-order l) n)) p)))
    (setf (.id port) (.n-ports this))
    (incf (.n-ports this))
    (when (> (+ (length (get-outgoing-edges port)) (length (get-incoming-edges port))) 1)
      (cond ((eq (get-side port) port-side.east)
             (setf (aref (.has-hyper-edges-east-of-index this) l) t))
            ((and (eq (get-side port) port-side.west) (> l 0))
             (setf (aref (.has-hyper-edges-east-of-index this) (1- l)) t))))))

(defjmethod init-at-edge-level ((this all-crossings-counter) l n p e edge node-order)
  (declare (ignore e))
  (let ((port (jl-get (get-ports (aref (aref node-order l) n)) p)))
    (when (and (eq (get-source edge) port)
               (eq (get-layer (get-node (get-source edge))) (get-layer (get-node (get-target edge)))))
      (incf (aref (.in-layer-edge-counts this) l)))))

(defjmethod init-after-traversal ((this all-crossings-counter))
  (let ((port-pos (%p3-int-array (.n-ports this))))
    (setf (.hyperedge-crossings-counter this)
          (new-hyperedge-crossings-counter (.in-layer-edge-counts this) (.has-north-south-ports this) port-pos)
          (.crossing-counter this) (new-crossings-counter port-pos))))

;;; ---------------------------------------------------------------- HyperedgeCrossingsCounter

;;; The constructor's first two arguments are not used by the Java class either
;;; (only portPos is kept).
(defjctor hyperedge-crossings-counter (in-layer-edge-count has-north-south-ports port-pos)
  (progn in-layer-edge-count has-north-south-ports)
  (setf (.port-pos this) port-pos))

(defjmethod compare-to ((this hyperedge) other)
  (cond ((< (.upper-left this) (.upper-left other)) -1)
        ((> (.upper-left this) (.upper-left other)) 1)
        ((< (.upper-right this) (.upper-right other)) -1)
        ((> (.upper-right this) (.upper-right other)) 1)
        (t (- (.hash this) (.hash other)))))

(defjctor hyperedge-corner (hyperedge position opposite-position type)
  (setf (.hyperedge this) hyperedge
        (.position this) position
        (.opposite-position this) opposite-position
        (.type this) type))

(defjmethod compare-to ((this hyperedge-corner) other)
  (cond ((< (.position this) (.position other)) -1)
        ((> (.position this) (.position other)) 1)
        ((< (.opposite-position this) (.opposite-position other)) -1)
        ((> (.opposite-position this) (.opposite-position other)) 1)
        ((not (eq (.hyperedge this) (.hyperedge other)))
         (- (.hash (.hyperedge this)) (.hash (.hyperedge other))))
        ((and (eq (.type this) layered-type.upper) (eq (.type other) layered-type.lower)) -1)
        ((and (eq (.type this) layered-type.lower) (eq (.type other) layered-type.upper)) 1)
        (t 0)))

(defun %hcc-sort-array (array)
  "Arrays.sort on Comparable objects (stable merge sort)."
  (let ((sorted (stable-sort (copy-seq array) (lambda (a b) (< (compare-to a b) 0)))))
    (replace array sorted)))

(defun %hcc-assign-source-positions (this left-layer)
  "Ports of the left layer with cross-layer outgoing edges get consecutive
positions.  Returns the source count."
  (let ((port-pos (.port-pos this)) (source-count 0))
    (jdo (node left-layer)
      (jdo (port (get-ports node))
        (let ((port-edges 0))
          (jdo (edge (get-outgoing-edges port))
            (unless (eq (get-layer node) (get-layer (get-node (get-target edge))))
              (incf port-edges)))
          (when (> port-edges 0)
            (setf (aref port-pos (.id port)) source-count)
            (incf source-count)))))
    source-count))

(defun %hcc-count-north-input-ports (node ports)
  "Leading NORTH ports with an incoming cross-layer edge."
  (let ((count 0))
    (block north
      (jdo (port ports)
        (if (eq (get-side port) port-side.north)
            (block inner
              (jdo (edge (get-incoming-edges port))
                (unless (eq (get-layer node) (get-layer (get-node (get-source edge))))
                  (incf count)
                  (return-from inner))))
            (return-from north))))
    count))

(defun %hcc-assign-target-positions (this right-layer)
  "Ports of the right layer with cross-layer incoming edges get positions
north - west - south - east.  Returns the target count."
  (let ((port-pos (.port-pos this)) (target-count 0))
    (jdo (node right-layer)
      (let* ((ports (get-ports node))
             (north-input-ports (%hcc-count-north-input-ports node ports))
             (other-input-ports 0))
        ;; Java walks the list backwards with a ListIterator.
        (loop for i from (1- (length ports)) downto 0 do
          (let ((port (aref ports i)) (port-edges 0))
            (jdo (edge (get-incoming-edges port))
              (unless (eq (get-layer node) (get-layer (get-node (get-source edge))))
                (incf port-edges)))
            (when (> port-edges 0)
              (if (eq (get-side port) port-side.north)
                  (progn (setf (aref port-pos (.id port)) target-count)
                         (incf target-count))
                  (progn (setf (aref port-pos (.id port))
                               (+ target-count north-input-ports other-input-ports))
                         (incf other-input-ports))))))
        (incf target-count other-input-ports)))
    target-count))

(defun %hcc-merge-edge (port2hyperedge hyperedge-set edge source-port target-port)
  "Adds EDGE to the hyperedge it belongs to, merging two hyperedges if needed."
  (let ((source-he (jm-get port2hyperedge source-port))
        (target-he (jm-get port2hyperedge target-port)))
    (cond ((and (null source-he) (null target-he))
           (let ((he (new-hyperedge)))
             (jl-add hyperedge-set he)
             (jl-add (.edges he) edge)
             (jl-add (.ports he) source-port)
             (jm-put port2hyperedge source-port he)
             (jl-add (.ports he) target-port)
             (jm-put port2hyperedge target-port he)))
          ((null source-he)
           (jl-add (.edges target-he) edge)
           (jl-add (.ports target-he) source-port)
           (jm-put port2hyperedge source-port target-he))
          ((null target-he)
           (jl-add (.edges source-he) edge)
           (jl-add (.ports source-he) target-port)
           (jm-put port2hyperedge target-port source-he))
          ((eq source-he target-he)
           (jl-add (.edges source-he) edge))
          (t
           (jl-add (.edges source-he) edge)
           (jdo (p (.ports target-he)) (jm-put port2hyperedge p source-he))
           (jl-add-all (.edges source-he) (.edges target-he))
           (jl-add-all (.ports source-he) (.ports target-he))
           (jl-remove hyperedge-set target-he)))))

(defun %hcc-gather-hyperedges (left-layer)
  "The LinkedHashSet of hyperedges, in insertion order."
  (let ((port2hyperedge (new-jmap)) (hyperedge-set (new-jlist)))
    (jdo (node left-layer)
      (jdo (source-port (get-ports node))
        (jdo (edge (get-outgoing-edges source-port))
          (let ((target-port (get-target edge)))
            (unless (eq (get-layer node) (get-layer (get-node target-port)))
              (%hcc-merge-edge port2hyperedge hyperedge-set edge source-port target-port))))))
    hyperedge-set))

(defun %hcc-set-extents (this hyperedges left-layer-ref right-layer-ref source-count target-count)
  "Each hyperedge's upper/lower positions on the left and right layers."
  (let ((port-pos (.port-pos this)))
    (loop for he across hyperedges do
      (setf (.upper-left he) source-count
            (.upper-right he) target-count)
      (jdo (port (.ports he))
        (let ((pos (aref port-pos (.id port))))
          (cond ((eq (get-layer (get-node port)) left-layer-ref)
                 (when (< pos (.upper-left he)) (setf (.upper-left he) pos))
                 (when (> pos (.lower-left he)) (setf (.lower-left he) pos)))
                ((eq (get-layer (get-node port)) right-layer-ref)
                 (when (< pos (.upper-right he)) (setf (.upper-right he) pos))
                 (when (> pos (.lower-right he)) (setf (.lower-right he) pos)))))))))

(defun %hcc-straight-crossings (hyperedges target-count)
  "Crossings between the right-hand ends of the hyperedges (accumulator tree).
HYPEREDGES must already be sorted."
  (let* ((n (length hyperedges))
         (south-sequence (%p3-int-array n))
         (compress-deltas (%p3-int-array (1+ target-count)))
         (crossings 0))
    (loop for i from 0 below n do
      (setf (aref south-sequence i) (.upper-right (aref hyperedges i)))
      (setf (aref compress-deltas (aref south-sequence i)) 1))
    (let ((delta 0))
      (loop for i from 0 below (length compress-deltas) do
        (if (= (aref compress-deltas i) 1)
            (setf (aref compress-deltas i) delta)
            (decf delta))))
    (let ((q 0))
      (loop for i from 0 below n do
        (incf (aref south-sequence i) (aref compress-deltas (aref south-sequence i)))
        (setf q (max q (1+ (aref south-sequence i)))))
      (let ((first-index 1))
        (loop while (< first-index q) do (setf first-index (* first-index 2)))
        (let ((tree (%p3-int-array (1- (* 2 first-index)))))
          (decf first-index)
          (loop for k from 0 below n do
            (let ((index (+ (aref south-sequence k) first-index)))
              (incf (aref tree index))
              (loop while (> index 0) do
                (when (> (imod index 2) 0)
                  (incf crossings (aref tree (1+ index))))
                (setf index (idiv (1- index) 2))
                (incf (aref tree index))))))))
    crossings))

(defun %hcc-corner-crossings (hyperedges upper-fn lower-fn)
  "One sweep over the sorted corners of HYPEREDGES.  UPPER-FN and LOWER-FN
select the two positions of a hyperedge on the side being swept."
  (let* ((n (length hyperedges))
         (corners (make-array (* 2 n)))
         (open 0)
         (crossings 0))
    (loop for i from 0 below n do
      (let ((he (aref hyperedges i)))
        (setf (aref corners (* 2 i))
              (new-hyperedge-corner he (funcall upper-fn he) (funcall lower-fn he) layered-type.upper)
              (aref corners (1+ (* 2 i)))
              (new-hyperedge-corner he (funcall lower-fn he) (funcall upper-fn he) layered-type.lower))))
    (%hcc-sort-array corners)
    (loop for corner across corners do
      (enum-case (.type corner)
        (layered-type.upper (incf open))
        (layered-type.lower (decf open) (incf crossings open))))
    crossings))

(defjmethod count-crossings ((this hyperedge-crossings-counter) left-layer right-layer)
  (let* ((source-count (%hcc-assign-source-positions this left-layer))
         (target-count (%hcc-assign-target-positions this right-layer))
         (hyperedges (coerce (%hcc-gather-hyperedges left-layer) 'simple-vector)))
    (%hcc-set-extents this hyperedges
                      (get-layer (aref left-layer 0)) (get-layer (aref right-layer 0))
                      source-count target-count)
    (%hcc-sort-array hyperedges)
    (+ (%hcc-straight-crossings hyperedges target-count)
       (%hcc-corner-crossings hyperedges #'.upper-left #'.lower-left)
       (%hcc-corner-crossings hyperedges #'.upper-right #'.lower-right))))
