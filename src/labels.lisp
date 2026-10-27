;;;; labels.lisp -- node micro layout: sizes, labels and ports on one node.
;;;;
;;;; Every algorithm needs the same things done to a node before and after it
;;;; places it: the node grown to fit what nodeSize.constraints asks for, its
;;;; labels placed as nodeLabels.placement says, its ports put on their sides,
;;;; and the margins all of that adds around the node measured, so neighbours
;;;; keep clear of it.

(in-package #:trellis)

;;; ---- port sides ------------------------------------------------------------

(defun port-side (port)
  "The side PORT is on: its port.side option, else the side its position is
   nearest to, else :undefined."
  (let ((s (option port :port-side)))
    (if (member s '(:north :east :south :west))
        s
        (if (shape-positioned port) (infer-port-side port) :undefined))))

(defun infer-port-side (port)
  (let* ((n (port-node port))
         (w (max 0d0 (node-width n))) (h (max 0d0 (node-height n)))
         (cx (+ (port-x port) (/ (port-width port) 2)))
         (cy (+ (port-y port) (/ (port-height port) 2))))
    (if (and (zerop w) (zerop h))
        :east
        (let ((dn (abs cy)) (ds (abs (- h cy))) (dw (abs cx)) (de (abs (- w cx))))
          (let ((m (min dn ds dw de)))
            (cond ((= m dw) :west) ((= m de) :east) ((= m dn) :north) (t :south)))))))

(defvar *ports-placed* nil
  "Compound nodes whose own layout has put their ports in place; the level
   above must leave those ports where they are.")

(defun port-constraints (node)
  (if (and *ports-placed* (gethash node *ports-placed*))
      :fixed-pos
      (opt-enum node :port-constraints :undefined
                '(:undefined :free :fixed-side :fixed-order :fixed-ratio :fixed-pos))))

(defun fixed-port-positions-p (node)
  (member (port-constraints node) '(:fixed-pos :fixed-ratio)))

(defun port-border-offset (port)
  (opt-num port :port-border-offset 0d0))

(defun put-port-on-side (port side pos)
  "Place PORT on SIDE of its node, its centre at POS along that side."
  (let* ((n (port-node port)) (w (max 0d0 (node-width n))) (h (max 0d0 (node-height n)))
         (pw (port-width port)) (ph (port-height port)) (off (port-border-offset port)))
    (ecase side
      (:north (setf (port-x port) (- pos (/ pw 2)) (port-y port) (- (- ph) off)))
      (:south (setf (port-x port) (- pos (/ pw 2)) (port-y port) (+ h off)))
      (:west (setf (port-x port) (- (- pw) off) (port-y port) (- pos (/ ph 2))))
      (:east (setf (port-x port) (+ w off) (port-y port) (- pos (/ ph 2)))))))

(defun distribute-ports (node side ports)
  "Spread PORTS, in order, along SIDE of NODE (clockwise order on north/east,
   left-to-right and top-to-bottom as given)."
  (let* ((len (if (member side '(:north :south)) (max 0d0 (node-width node))
                  (max 0d0 (node-height node))))
         (k (length ports)))
    (loop for p in ports for i from 1
          do (put-port-on-side p side (* len (/ i (d (1+ k))))))))

(defun ports-by-side (node)
  (let ((sides (list (cons :north '()) (cons :east '()) (cons :south '()) (cons :west '()))))
    (dolist (p (node-ports node))
      (let ((s (port-side p)))
        (push p (cdr (assoc (if (eq s :undefined) :east s) sides)))))
    (dolist (s sides sides) (setf (cdr s) (nreverse (cdr s))))))

(defun sort-ports-along-side (side ports)
  (sort (copy-list ports) #'<
        :key (lambda (p) (if (member side '(:north :south)) (port-x p) (port-y p)))))

;;; ---- node sizes ------------------------------------------------------------

(defun size-constraints (node)
  (opt-set node :node-size-constraints '()))

(defun label-placement (label)
  "The placement set that applies to LABEL: its own, else its owner's."
  (let ((own (option label :node-labels-placement :unset)))
    (if (listp own)
        own
        (let ((owner (label-owner label)))
          (if (node-p owner) (opt-set owner :node-labels-placement '()) '())))))

(defun node-labels-padding (node)
  (opt-pad node :node-labels-padding (pad 5d0 5d0 5d0 5d0)))

(defun inside-label-p (label)
  (let ((pl (label-placement label)))
    (and pl (not (member :outside pl)))))

(defun label-cell (placement)
  "(h . v) cell of a placement set, each :begin/:center/:end."
  (cons (cond ((member :h-left placement) :begin)
              ((member :h-right placement) :end)
              (t :center))
        (cond ((member :v-top placement) :begin)
              ((member :v-bottom placement) :end)
              (t :center))))

(defun apply-node-size (node &key (label-spacing 0d0) (port-spacing 10d0))
  "Grow NODE to satisfy its nodeSize.constraints."
  (let* ((cons (size-constraints node))
         (w (max 0d0 (node-width node))) (h (max 0d0 (node-height node))))
    (when cons
      (let ((minw 0d0) (minh 0d0))
        (when (member :minimum-size cons)
          (let ((m (option node :node-size-minimum)))
            (when (consp m) (setf minw (sane (px m)) minh (sane (py m)))))
          (when (member :default-minimum-size (opt-set node :node-size-options '()))
            (setf minw (max minw 20d0) minh (max minh 20d0))))
        (when (member :node-labels cons)
          (let ((pad (node-labels-padding node)) (cells (make-hash-table :test 'equal)))
            (dolist (l (node-labels node))
              (when (inside-label-p l)
                (push l (gethash (label-cell (label-placement l)) cells))))
            (let ((cols (make-hash-table)) (rows (make-hash-table)))
              (maphash (lambda (cell ls)
                         (let ((cw (reduce #'max ls :key #'label-width :initial-value 0d0))
                               (ch (+ (reduce #'+ ls :key #'label-height :initial-value 0d0)
                                      (* label-spacing (max 0 (1- (length ls)))))))
                           (setf (gethash (car cell) cols) (max cw (gethash (car cell) cols 0d0))
                                 (gethash (cdr cell) rows) (max ch (gethash (cdr cell) rows 0d0)))))
                       cells)
              (let ((sw (loop for v being the hash-values of cols sum v))
                    (sh (loop for v being the hash-values of rows sum v)))
                (when (plusp (hash-table-count cells))
                  (setf minw (max minw (+ sw (pad-left pad) (pad-right pad)))
                        minh (max minh (+ sh (pad-top pad) (pad-bottom pad)))))))))
        (when (and (member :ports cons) (node-ports node))
          (loop for (side . ps) in (ports-by-side node)
                for need = (+ (* port-spacing (1+ (length ps)))
                              (reduce #'+ ps :key (if (member side '(:north :south))
                                                      #'port-width #'port-height)
                                             :initial-value 0d0))
                when ps
                  do (if (member side '(:north :south))
                         (setf minw (max minw need))
                         (setf minh (max minh need)))))
        (setf w (max w minw) h (max h minh))))
    (setf (node-width node) w (node-height node) h)
    node))

;;; ---- node labels -----------------------------------------------------------

(defun place-node-labels (node &key (label-node 5d0) (label-label 0d0))
  "Position NODE's labels from their placement sets.  Labels with no placement
   keep their positions."
  (let ((cells (make-hash-table :test 'equal))
        (w (max 0d0 (node-width node))) (h (max 0d0 (node-height node)))
        (pad (node-labels-padding node)))
    (dolist (l (node-labels node))
      (let ((pl (label-placement l)))
        (when pl
          (push l (gethash (list (member :outside pl) (label-cell pl)) cells)))))
    (maphash
     (lambda (key ls)
       (destructuring-bind (outside (hc . vc)) key
         (let* ((ls (reverse ls))
                (stack-h (+ (reduce #'+ ls :key #'label-height :initial-value 0d0)
                            (* label-label (max 0 (1- (length ls))))))
                (y0 (if outside
                        (ecase vc
                          (:begin (- (+ stack-h label-node)))
                          (:end (+ h label-node))
                          (:center (/ (- h stack-h) 2)))
                        (ecase vc
                          (:begin (pad-top pad))
                          (:end (- h (pad-bottom pad) stack-h))
                          (:center (/ (- h stack-h) 2))))))
           (dolist (l ls)
             (let ((lw (label-width l)))
               (setf (label-x l)
                     (if outside
                         (if (eq vc :center)
                             (ecase hc
                               (:begin (- (+ lw label-node)))
                               (:end (+ w label-node))
                               (:center (/ (- w lw) 2)))
                             (ecase hc
                               (:begin 0d0)
                               (:end (- w lw))
                               (:center (/ (- w lw) 2))))
                         (ecase hc
                           (:begin (pad-left pad))
                           (:end (- w (pad-right pad) lw))
                           (:center (/ (- w lw) 2))))
                     (label-y l) y0)
               (incf y0 (+ (label-height l) label-label)))))))
     cells)))

;;; ---- port labels -----------------------------------------------------------

(defun place-port-labels (node &key (spacing 1d0))
  "Put each port's labels beside it: outside the node by default, inside when
   portLabels.placement asks."
  (let ((inside (member :inside (opt-set node :port-labels-placement '(:outside)))))
    (dolist (p (node-ports node))
      (let ((side (port-side* p)) (pw (port-width p)) (ph (port-height p)))
        (dolist (l (port-labels p))
          (let ((lw (label-width l)) (lh (label-height l)))
            (multiple-value-bind (x y)
                (ecase side
                  (:north (if inside (values (+ pw spacing) (+ ph spacing))
                              (values (+ pw spacing) (- (+ lh spacing)))))
                  (:south (if inside (values (+ pw spacing) (- (+ lh spacing)))
                              (values (+ pw spacing) (+ ph spacing))))
                  (:west (if inside (values (+ pw spacing) (- (+ lh spacing)))
                             (values (- (+ lw spacing)) (- (+ lh spacing)))))
                  (:east (if inside (values (- (+ lw spacing)) (- (+ lh spacing)))
                             (values (+ pw spacing) (- (+ lh spacing))))))
              (setf (label-x l) x (label-y l) y))))))))

(defun port-side* (port)
  "PORT's side, inferred from its position when not declared."
  (let ((s (port-side port)))
    (if (eq s :undefined) (infer-port-side port) s)))

;;; ---- margins ---------------------------------------------------------------

(defun node-extent (node)
  "The rectangle, relative to NODE's origin, that NODE and everything drawn with
   it -- outside labels, ports, port labels -- covers."
  (let ((r (rect 0d0 0d0 (max 0d0 (node-width node)) (max 0d0 (node-height node)))))
    (dolist (l (node-labels node))
      (setf r (rect-union r (rect (label-x l) (label-y l)
                                  (max 0d0 (label-width l)) (max 0d0 (label-height l))))))
    (dolist (p (node-ports node))
      (setf r (rect-union r (rect (port-x p) (port-y p)
                                  (max 0d0 (port-width p)) (max 0d0 (port-height p)))))
      (dolist (l (port-labels p))
        (setf r (rect-union r (rect (+ (port-x p) (label-x l)) (+ (port-y p) (label-y l))
                                    (max 0d0 (label-width l)) (max 0d0 (label-height l)))))))
    r))

(defun node-margins (node)
  "How far NODE's extent reaches beyond its box on each side, as a PAD."
  (let ((r (node-extent node)))
    (pad (max 0d0 (- (rect-y r)))
         (max 0d0 (- (rect-x r)))
         (max 0d0 (- (rect-bottom r) (max 0d0 (node-height node))))
         (max 0d0 (- (rect-right r) (max 0d0 (node-width node)))))))

(defun micro-layout (node parent &key (place-ports t))
  "Size NODE and place its labels and ports, reading spacings from PARENT, the
   graph NODE is laid out in."
  (let ((label-label (opt-num parent :spacing-label-label 0d0 :min 0))
        (label-node (opt-num parent :spacing-label-node 5d0 :min 0))
        (port-port (opt-num parent :spacing-port-port 10d0 :min 0)))
    (apply-node-size node :label-spacing label-label :port-spacing port-port)
    (place-node-labels node :label-node label-node :label-label label-label)
    (when (and place-ports (not (fixed-port-positions-p node)))
      (loop for (side . ps) in (ports-by-side node)
            when ps do (distribute-ports node side (sort-ports-along-side side ps))))
    (place-port-labels node :spacing (opt-num parent :spacing-label-port-horizontal 1d0 :min 0))
    node))
