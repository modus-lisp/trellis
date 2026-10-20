;;;; loops-ordering.lisp -- org.eclipse.elk.alg.layered.intermediate.loops.ordering

(in-package #:elk)

;;; ------------------------------------------------------------ PortSideAssigner

(defjmethod assign-port-sides ((this port-side-assigner) sl-holder)
  (enum-case (get-property (get-l-node sl-holder) layered-options.edge-routing-self-loop-distribution)
    (self-loop-distribution-strategy.north (%lic-assign-to-north-side sl-holder))
    (self-loop-distribution-strategy.north-south (%lic-assign-to-north-or-south-side sl-holder))
    (self-loop-distribution-strategy.equally (%lic-assign-to-all-sides sl-holder))))

(defun %lic-hidden-sl-ports (sl-loop)
  (remove-if-not (lambda (p) (is-hidden p)) (to-jvector (get-sl-ports sl-loop))))

(defun %lic-assign-to-north-side (sl-holder)
  (jdo (sl-loop (get-sl-hyper-loops sl-holder))
    (loop for sl-port across (%lic-hidden-sl-ports sl-loop)
          do (set-side (get-l-port sl-port) port-side.north))))

(defun %lic-assign-to-north-or-south-side (sl-holder)
  ;; Self loops can consist of more than two ports. Instead of simply assigning an equal number of self loops to
  ;; both sides, we'd rather end up with a similar number of ports on both sides (greedy).
  (let ((north-ports 0) (south-ports 0))
    (jdo (sl-loop (get-sl-hyper-loops sl-holder))
      (let ((hidden (%lic-hidden-sl-ports sl-loop))
            (new-side nil))
        (cond ((<= north-ports south-ports)
               (setf new-side port-side.north)
               (incf north-ports (length hidden)))
              ((< south-ports north-ports)
               (setf new-side port-side.south)
               (incf south-ports (length hidden))))
        (loop for sl-port across hidden
              do (set-side (get-l-port sl-port) new-side))))))

;; Target enum fields
(defjmethod first-side ((this layered-target))
  (enum-case this
    (layered-target.north port-side.north)
    (layered-target.south port-side.south)
    (layered-target.east port-side.east)
    (layered-target.west port-side.west)
    (layered-target.north-west-corner port-side.west)
    (layered-target.north-east-corner port-side.north)
    (layered-target.south-west-corner port-side.south)
    (layered-target.south-east-corner port-side.east)))

(defjmethod second-side ((this layered-target))
  (enum-case this
    (layered-target.north port-side.north)
    (layered-target.south port-side.south)
    (layered-target.east port-side.east)
    (layered-target.west port-side.west)
    (layered-target.north-west-corner port-side.north)
    (layered-target.north-east-corner port-side.east)
    (layered-target.south-west-corner port-side.west)
    (layered-target.south-east-corner port-side.south)))

(defjmethod is-corner-target ((this layered-target))
  (not (eq (first-side this) (second-side this))))

(defun %lic-assign-to-all-sides (sl-holder)
  ;; Sorted descendingly by the number of involved ports
  (let ((sorted (jlist-from (get-sl-hyper-loops sl-holder))))
    (jl-sort sorted (lambda (a b) (integer.compare (length (get-sl-ports b)) (length (get-sl-ports a)))))
    (let ((targets (layered-target.values))
          (curr-loop 0))
      (jdo (sl-loop sorted)
        (%lic-assign-to-target sl-loop (aref targets (mod curr-loop (length targets))))
        (incf curr-loop)))))

(defun %lic-assign-to-target (sl-loop target)
  (let ((sl-ports (get-sl-ports sl-loop)))
    ;; For corner targets, sort the port's original list by net flow
    (when (is-corner-target target)
      (jl-sort sl-ports (lambda (p1 p2) (integer.compare (get-net-flow (get-l-port p1))
                                                         (get-net-flow (get-l-port p2))))))
    (let ((second-half-start (idiv (length sl-ports) 2)))
      (loop for i from 0 below second-half-start
            do (let ((p (aref sl-ports i)))
                 (when (is-hidden p) (set-side (get-l-port p) (first-side target)))))
      (loop for i from second-half-start below (length sl-ports)
            do (let ((p (aref sl-ports i)))
                 (when (is-hidden p) (set-side (get-l-port p) (second-side target))))))))

;;; ------------------------------------------------------------ PortRestorer

(defjmethod restore-ports ((this port-restorer) sl-holder monitor)
  (declare (ignore monitor))
  (%lic-init-target-areas this)
  (setf (.sl-loops-by-type this) (%lic-gather-self-loops-by-type sl-holder))
  (%lic-process-one-side-loops this (get-property (get-l-node sl-holder)
                                                  layered-options.edge-routing-self-loop-ordering))
  (%lic-process-two-side-corner-loops this)
  (%lic-process-three-side-loops this)
  (%lic-process-four-side-loops this)
  (%lic-process-two-side-opposing-loops this)
  ;; Actually go ahead and add the ports to the real port list
  (%lic-restore-ports this sl-holder)
  ;; We're not hiding any ports anymore
  (loop for side-row across (.target-areas this)
        do (loop for area across side-row
                 do (jdo (sl-port area) (set-hidden sl-port nil))))
  (set-ports-hidden sl-holder nil)
  (setf (.sl-loops-by-type this) nil))

(defun %lic-init-target-areas (this)
  ;; ArrayTable<PortSide, PortSideArea, List>: vector by side ordinal of vectors by area ordinal
  (let* ((sides (length (port-side.values)))
         (areas (length (port-side-area.values)))
         (table (make-array sides)))
    (dotimes (s sides)
      (let ((row (make-array areas)))
        (dotimes (a areas) (setf (aref row a) (new-jlist)))
        (setf (aref table s) row)))
    (setf (.target-areas this) table)))

(defun %lic-target-area (this side area)
  (aref (aref (.target-areas this) (ordinal side)) (ordinal area)))

(defun %lic-gather-self-loops-by-type (sl-holder)
  (let ((loops (new-jmap)))
    (jdo (sl-loop (get-sl-hyper-loops sl-holder))
      (let ((type (get-self-loop-type sl-loop)))
        (jl-add (or (gethash type loops) (setf (gethash type loops) (new-jlist))) sl-loop)))
    loops))

(defun %lic-loops-of-type (this type)
  "ListMultimap.get: a live (possibly freshly created, empty) list."
  (let ((m (.sl-loops-by-type this)))
    (or (gethash type m) (setf (gethash type m) (new-jlist)))))

(defun %lic-process-one-side-loops (this ordering)
  (when (eq ordering self-loop-ordering-strategy.reverse-stacked)
    (jl-reverse (%lic-loops-of-type this self-loop-type.one-side)))
  (jdo (sl-loop (%lic-loops-of-type this self-loop-type.one-side))
    (let ((side (get-side (get-l-port (aref (get-sl-ports sl-loop) 0))))
          (sorted (jlist-from (get-sl-ports sl-loop))))
      (jl-sort sorted (lambda (a b) (integer.compare (get-sl-net-flow a) (get-sl-net-flow b))))
      (enum-case ordering
        (self-loop-ordering-strategy.sequenced
         (%lic-add-ports-to-target-area this sorted side port-side-area.middle add-mode.append))
        ((self-loop-ordering-strategy.reverse-stacked self-loop-ordering-strategy.stacked)
         (let ((split (%lic-compute-port-list-split-index sorted)))
           (%lic-add-ports-to-target-area this (jl-sub-list sorted 0 split) side
                                          port-side-area.middle add-mode.prepend)
           (%lic-add-ports-to-target-area this (jl-sub-list sorted split (length sorted)) side
                                          port-side-area.middle add-mode.append)))))))

(defun %lic-compute-port-list-split-index (sorted)
  (let ((n (length sorted))
        (positive 0))
    ;; Find index of the first port with a positive net flow
    (loop while (and (< positive n) (not (> (get-sl-net-flow (aref sorted positive)) 0)))
          do (incf positive))
    (when (and (> positive 0) (< positive (- n 1)))
      (return-from %lic-compute-port-list-split-index positive))
    ;; (sic) the "non-negative" search uses > 0 in ELK as well
    (let ((non-negative 0))
      (loop while (and (< non-negative n) (not (> (get-sl-net-flow (aref sorted non-negative)) 0)))
            do (incf non-negative))
      (when (and (> non-negative 0) (< positive (- n 1)))
        (return-from %lic-compute-port-list-split-index non-negative)))
    (idiv n 2)))

(defun %lic-process-two-side-corner-loops (this)
  (jdo (sl-loop (%lic-loops-of-type this self-loop-type.two-sides-corner))
    (let ((sides (port-restorer.sorted-two-side-loop-port-sides sl-loop)))
      (%lic-add-loop-to-target-area this sl-loop (aref sides 0) port-side-area.end add-mode.prepend)
      (%lic-add-loop-to-target-area this sl-loop (aref sides 1) port-side-area.start add-mode.append))))

(defun %lic-process-two-side-opposing-loops (this)
  (jdo (sl-loop (%lic-loops-of-type this self-loop-type.two-sides-opposing))
    (let ((sides (port-restorer.sorted-two-side-loop-port-sides sl-loop)))
      (%lic-add-loop-to-target-area this sl-loop (aref sides 0) port-side-area.end add-mode.prepend)
      (%lic-add-loop-to-target-area this sl-loop (aref sides 1) port-side-area.start add-mode.append))))

(defjstatic port-restorer.sorted-two-side-loop-port-sides (sl-loop)
  ;; Sort the port sides such that they follow a clockwise order
  (let ((sides (coerce (enum-set-items (%lic-sl-port-sides sl-loop)) 'vector)))
    ;; enum set items are already in ordinal order (Arrays.sort)
    (when (and (eq (aref sides 0) port-side.north) (eq (aref sides 1) port-side.west))
      (setf (aref sides 0) port-side.west
            (aref sides 1) port-side.north))
    sides))

(defun %lic-determine-loop-constellation (sl-loop)
  (let ((port-sides (%lic-sl-port-sides sl-loop)))
    (cond ((not (contains port-sides port-side.north)) (vector port-side.east port-side.south port-side.west))
          ((not (contains port-sides port-side.east)) (vector port-side.south port-side.west port-side.north))
          ((not (contains port-sides port-side.south)) (vector port-side.west port-side.north port-side.east))
          ((not (contains port-sides port-side.west)) (vector port-side.north port-side.east port-side.south))
          (t nil))))

(defun %lic-process-three-side-loops (this)
  (jdo (sl-loop (%lic-loops-of-type this self-loop-type.three-sides))
    (let ((sides (%lic-determine-loop-constellation sl-loop)))
      (%lic-add-loop-to-target-area this sl-loop (aref sides 0) port-side-area.end add-mode.prepend)
      (%lic-add-loop-to-target-area this sl-loop (aref sides 1) port-side-area.middle add-mode.append)
      (%lic-add-loop-to-target-area this sl-loop (aref sides 2) port-side-area.start add-mode.append))))

(defun %lic-process-four-side-loops (this)
  (jdo (sl-loop (%lic-loops-of-type this self-loop-type.four-sides))
    (jdo (side (%lic-sl-port-sides sl-loop))
      (%lic-add-loop-to-target-area this sl-loop side port-side-area.middle add-mode.append))))

(defun %lic-add-loop-to-target-area (this sl-loop port-side area add-mode)
  (%lic-add-ports-to-target-area this (get-sl-ports-by-side sl-loop port-side) port-side area add-mode))

(defun %lic-add-ports-to-target-area (this sl-ports port-side area add-mode)
  (let ((hidden (jlist-from (remove-if-not (lambda (p) (is-hidden p)) (to-jvector sl-ports)))))
    (jl-reverse hidden)
    (let ((target-area (%lic-target-area this port-side area)))
      (if (eq add-mode add-mode.prepend)
          (jl-add-all-at target-area 0 hidden)
          (jl-add-all target-area hidden)))))

(defun %lic-restore-ports (this sl-holder)
  (let* ((l-node (get-l-node sl-holder))
         (old-port-list (jlist-from (get-ports l-node)))
         (next 0)
         (new-port-list (get-ports l-node)))
    (jl-clear new-port-list)
    (flet ((add-area (side area)
             (jdo (sl-port (%lic-target-area this side area))
               (set-node (get-l-port sl-port) l-node)))
           (add-that (pred)
             (setf next (%lic-add-all-that old-port-list next pred new-port-list))))
      (add-area port-side.north port-side-area.start)
      (add-that (lambda (p) (and (eq (get-side p) port-side.north)
                                 (%lic-is-north-south-port-with-west-or-west-east-connections p))))
      (add-area port-side.north port-side-area.middle)
      (add-that (lambda (p) (eq (get-side p) port-side.north)))
      (add-area port-side.north port-side-area.end)
      (add-area port-side.east port-side-area.start)
      (add-area port-side.east port-side-area.middle)
      (add-that (lambda (p) (eq (get-side p) port-side.east)))
      (add-area port-side.east port-side-area.end)
      (add-area port-side.south port-side-area.start)
      (add-that (lambda (p) (and (eq (get-side p) port-side.south)
                                 (%lic-is-north-south-port-with-east-connections p))))
      (add-area port-side.south port-side-area.middle)
      (add-that (lambda (p) (eq (get-side p) port-side.south)))
      (add-area port-side.south port-side-area.end)
      (add-area port-side.west port-side-area.start)
      (add-that (lambda (p) (eq (get-side p) port-side.west)))
      (add-area port-side.west port-side-area.middle)
      (add-area port-side.west port-side-area.end))))

(defun %lic-add-all-that (l-ports from-index condition target)
  (loop for i from from-index below (length l-ports)
        do (let ((l-port (aref l-ports i)))
             (if (funcall condition l-port)
                 (jl-add target l-port)
                 (return-from %lic-add-all-that i))))
  (length l-ports))

(defun %lic-is-north-south-port-with-west-or-west-east-connections (l-port)
  (let* ((connections (%lic-north-south-port-connection-sides l-port))
         (east (contains connections port-side.east))
         (west (contains connections port-side.west)))
    (or west (and west east))))

(defun %lic-is-north-south-port-with-east-connections (l-port)
  (contains (%lic-north-south-port-connection-sides l-port) port-side.east))

(defun %lic-north-south-port-connection-sides (l-port)
  (let ((sides (enum-set.none-of 'port-side))
        (port-dummy (get-property l-port layered-internal-properties.port-dummy)))
    (when port-dummy
      (jdo (dummy-port (get-ports port-dummy))
        (when (eq (get-property dummy-port layered-internal-properties.origin) l-port)
          (unless (is-empty (get-connected-edges dummy-port))
            (add sides (get-side dummy-port))))))
    sides))
