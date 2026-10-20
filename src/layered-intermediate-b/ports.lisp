;;;; ports.lisp -- PortSideProcessor and PortListSorter.

(in-package #:elk)

;;; ------------------------------------------------ PortSideProcessor

(defjmethod process ((this port-side-processor) layered-graph monitor)
  (begin monitor "Port side processing" 1)
  ;; IF USED BEFORE PHASE 1
  ;; Iterate through the unlayered nodes of the graph
  (jdo (node (get-layerless-nodes layered-graph))
    (process this node))
  ;; IF USED BEFORE PHASE 3
  ;; Iterate through the nodes of all layers
  (jdo (layer (get-layers layered-graph))
    (jdo (node (get-nodes layer))
      (process this node)))
  (done monitor))

(defjmethod process ((this port-side-processor) node)
  (if (is-side-fixed (get-property node layered-options.port-constraints))
      ;; Check whether there are ports with undefined side
      (jdo (port (get-ports node))
        (when (eq (get-side port) port-side.undefined)
          (port-side-processor.set-port-side port)))
      (progn
        ;; Distribute all ports and change constraints to fixed side
        (jdo (port (get-ports node))
          (port-side-processor.set-port-side port))
        (set-property node layered-options.port-constraints port-constraints.fixed-side))))

(defjstatic port-side-processor.set-port-side (port)
  (let ((port-dummy (get-property port layered-internal-properties.port-dummy)))
    (cond (port-dummy
           (set-side port (get-property port-dummy layered-internal-properties.ext-port-side)))
          ((< (get-net-flow port) 0)
           (set-side port port-side.east))
          (t
           (set-side port port-side.west)))))

;;; ------------------------------------------------ PortListSorter

(defjconst port-list-sorter.in-edges (lambda (p) (get-incoming-edges p)))
(defjconst port-list-sorter.out-edges (lambda (p) (get-outgoing-edges p)))

(defjconst port-list-sorter.cmp-port-side
  (lambda (p1 p2)
    ;; Sort by side first (if the comparison ends here, the ports were on different sides;
    ;; otherwise, the ports must be on the same side)
    (let ((ordinal-difference (- (ordinal (get-side p1)) (ordinal (get-side p2)))))
      (if (/= ordinal-difference 0)
          ordinal-difference
          0))))

(defjstatic port-list-sorter.real-degree (p edges-fun)
  (let ((real-degree 0))
    (jdo (e (funcall edges-fun p))
      (unless (get-property e layered-internal-properties.reversed)
        (incf real-degree)))
    real-degree))

(defjconst port-list-sorter.cmp-port-degree-east-west
  (lambda (p1 p2)
    (let ((ordinal-difference (- (ordinal (get-side p1)) (ordinal (get-side p2)))))
      (if (/= ordinal-difference 0)
          0 ; ports on different sides -- not our job
          (enum-case (get-side p1)
            (port-side.east
             (- (port-list-sorter.real-degree p2 port-list-sorter.out-edges)
                (port-list-sorter.real-degree p1 port-list-sorter.out-edges)))
            (port-side.west
             (- (port-list-sorter.real-degree p1 port-list-sorter.in-edges)
                (port-list-sorter.real-degree p2 port-list-sorter.in-edges)))
            (t 0))))))

(defjconst port-list-sorter.cmp-fixed-order-and-fixed-pos
  (lambda (p1 p2)
    (block cmp
      (let* ((port-constraints (get-property (get-node p1) layered-options.port-constraints))
             (ordinal-difference (- (ordinal (get-side p1)) (ordinal (get-side p2)))))
        ;; Do nothing if not at least the port order is fixed or the ports are on different sides
        (when (or (/= ordinal-difference 0) (not (is-order-fixed port-constraints)))
          (return-from cmp 0))
        ;; If the ports are on the same side and the node has FIXED_ORDER port constraints (that is,
        ;; the coordinates of the ports don't necessarily make sense), we check if the port index
        ;; has been explicitly set
        (when (eq port-constraints port-constraints.fixed-order)
          ;; In case of equal sides, sort by port index property
          (let ((index1 (get-property p1 layered-options.port-index))
                (index2 (get-property p2 layered-options.port-index)))
            (when (and index1 index2)
              (let ((index-difference (- index1 index2)))
                (unless (zerop index-difference)
                  (return-from cmp index-difference))))))
        ;; In case of equal index (or FIXED_POS), sort by position
        (enum-case (get-side p1)
          (port-side.north
           ;; Compare x coordinates
           (double.compare (.x (get-position p1)) (.x (get-position p2))))
          (port-side.east
           ;; Compare y coordinates
           (double.compare (.y (get-position p1)) (.y (get-position p2))))
          (port-side.south
           ;; Compare x coordinates in reversed order
           (double.compare (.x (get-position p2)) (.x (get-position p1))))
          (port-side.west
           ;; Compare y coordinates in reversed order
           (double.compare (.y (get-position p2)) (.y (get-position p1))))
          (t
           ;; Port sides should not be undefined
           (jthrow 'illegal-state-exception "Port side is undefined")))))))

(defjconst port-list-sorter.cmp-combined
  (then-comparing port-list-sorter.cmp-port-side port-list-sorter.cmp-fixed-order-and-fixed-pos))

(defjmethod process ((this port-list-sorter) layered-graph monitor)
  (begin monitor "Port order processing" 1)
  (let ((pss (get-property layered-graph layered-options.port-sorting-strategy)))
    ;; Iterate through the nodes of all layers
    (jdo (layer (get-layers layered-graph))
      (jdo (node (get-nodes layer))
        (let ((port-constraints (get-property node layered-options.port-constraints))
              (ports (get-ports node)))
          ;; We need to sort the port list according to port constraints
          (cond ((is-order-fixed port-constraints)
                 (jl-sort ports port-list-sorter.cmp-combined))
                ((is-side-fixed port-constraints)
                 ;; by default try to preserve the input order
                 (jl-sort ports port-list-sorter.cmp-port-side)
                 (reverse-west-and-south-side this ports)
                 (when (eq pss port-sorting-strategy.port-degree)
                   (jl-sort ports port-list-sorter.cmp-port-degree-east-west))))
          (cache-port-sides node)))))
  (done monitor))

(defjmethod reverse-west-and-south-side ((this port-list-sorter) ports)
  (when (<= (length ports) 1)
    (return-from reverse-west-and-south-side/1 nil))
  (let ((south-indices (find-port-side-range this ports port-side.south)))
    (j-reverse this ports (car south-indices) (cdr south-indices)))
  (let ((west-indices (find-port-side-range this ports port-side.west)))
    (j-reverse this ports (car west-indices) (cdr west-indices))))

(defjmethod find-port-side-range ((this port-list-sorter) ports side)
  "Pair<Integer, Integer> as a cons (first . second)."
  (when (jl-is-empty ports)
    (return-from find-port-side-range/2 (cons 0 0)))
  (let* ((current-side (get-side (jl-get ports 0)))
         (low-idx 0)
         (lb (ordinal side))
         (hb (1+ (ordinal side))))
    (loop while (and (< low-idx (1- (length ports))) (< (ordinal current-side) lb))
          do (incf low-idx)
             (setf current-side (get-side (jl-get ports low-idx))))
    (let ((high-idx low-idx))
      (loop while (and (< high-idx (1- (length ports))) (< (ordinal current-side) hb))
            do (incf high-idx)
               ;; sic: the Java code re-reads the port at lowIdx here
               (setf current-side (get-side (jl-get ports low-idx))))
      (cons low-idx high-idx))))

(defjmethod j-reverse ((this port-list-sorter) ports low-idx high-idx)
  (when (<= high-idx (+ low-idx 2))
    (return-from j-reverse/3 nil))
  (let ((n (floor (- high-idx low-idx) 2)))
    (dotimes (i n)
      (let ((tmp (jl-get ports (+ low-idx i))))
        (jl-set ports (+ low-idx i) (jl-get ports (- high-idx i 1)))
        (jl-set ports (- high-idx i 1) tmp)))))
