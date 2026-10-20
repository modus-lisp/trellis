;;;; internal.lisp -- org.eclipse.elk.alg.common.nodespacing.internal

(in-package #:elk)

;;; ------------------------------------------------------------ the port context multimap
;;; NodeContext.portContexts is a TreeMultimap<PortSide, PortContext>: keys in
;;; port side ordinal order, values sorted by NodeContext.comparePortContexts.
;;; We keep one sorted jlist per port side ordinal.

(defun new-ns-port-contexts ()
  (let ((v (make-array (length (port-side.values)))))
    (dotimes (i (length v) v) (setf (aref v i) (new-jlist)))))

(defun ns-port-contexts-put (node-context side port-context)
  (let* ((l (aref (.port-contexts node-context) (ordinal side)))
         (pos (or (position-if (lambda (other)
                                 (> (node-context.compare-port-contexts other port-context) 0))
                               l)
                  (length l))))
    ;; a TreeMultimap value set ignores an element comparing equal to an existing one
    (unless (and (> pos 0)
                 (zerop (node-context.compare-port-contexts (aref l (1- pos)) port-context)))
      (jl-add-at l pos port-context))))

(defun ns-port-contexts-get (node-context side)
  "portContexts.get(side): the sorted (live) collection for SIDE."
  (aref (.port-contexts node-context) (ordinal side)))

(defun ns-port-contexts-values (node-context)
  "portContexts.values(): all port contexts, ordered by side then by comparator."
  (let ((r (new-jlist)))
    (loop for l across (.port-contexts node-context) do (jl-add-all r l))
    r))

(defun ns-enum-map-values (map enum)
  "EnumMap.values(): the values of MAP in ordinal order of the ENUM keys."
  (let ((r (new-jlist)))
    (loop for k across (enum-values-of enum)
          do (multiple-value-bind (v found) (gethash k map)
               (when found (jl-add r v))))
    r))

;;; ------------------------------------------------------------ NodeContext

(defjctor node-context (parent-graph node)
  parent-graph
  (setf (.node this) node)
  (setf (.node-size this) (new-k-vector (get-size node)))
  (setf (.topdown-layout this) (get-property node core-options.topdown-layout))
  (setf (.treat-as-compound-node this)
        (or (is-compound-node node) (get-property node core-options.inside-self-loops-activate)))
  (setf (.size-constraints this) (get-property node core-options.node-size-constraints))
  (setf (.size-options this) (get-property node core-options.node-size-options))
  (setf (.port-constraints this) (get-property node core-options.port-constraints))
  (setf (.port-labels-placement this) (get-property node core-options.port-labels-placement))
  (unless (port-label-placement.is-valid (.port-labels-placement this))
    (error 'unsupported-configuration-exception
           :message (format nil "Invalid port label placement: ~A" (.port-labels-placement this))))
  (setf (.port-labels-treat-as-group this) (get-property node core-options.port-labels-treat-as-group))
  (setf (.node-label-placement this) (get-property node core-options.node-labels-placement))
  (unless (node-label-placement.is-valid (.node-label-placement this))
    (error 'unsupported-configuration-exception
           :message (format nil "Invalid node label placement: ~A" (.node-label-placement this))))
  (setf (.node-labels-padding this)
        (individual-spacings.get-individual-or-inherited node core-options.node-labels-padding))
  (setf (.node-label-spacing this)
        (individual-spacings.get-individual-or-inherited node core-options.spacing-label-node))
  (setf (.label-label-spacing this)
        (individual-spacings.get-individual-or-inherited node core-options.spacing-label-label))
  (setf (.port-port-spacing this)
        (individual-spacings.get-individual-or-inherited node core-options.spacing-port-port))
  (setf (.port-label-spacing-horizontal this)
        (individual-spacings.get-individual-or-inherited node core-options.spacing-label-port-horizontal))
  (setf (.port-label-spacing-vertical this)
        (individual-spacings.get-individual-or-inherited node core-options.spacing-label-port-vertical))
  (setf (.surrounding-port-margins this)
        (individual-spacings.get-individual-or-inherited node core-options.spacing-ports-surrounding))
  (setf (.label-cell-spacing this) (* 2 (.label-label-spacing this)))
  (let ((symmetry (not (contains (.size-options this) size-options.asymmetrical))))
    (setf (.node-container this) (new-strip-container-cell strip.vertical symmetry 0.0))
    (setf (.node-container-middle-row this) (new-strip-container-cell strip.horizontal symmetry 0.0))
    (set-cell (.node-container this) container-area.center (.node-container-middle-row this))))

(defjmethod apply-node-size ((this node-context))
  (set-size (.node this) (.node-size this)))

(defjmethod get-port-alignment ((this node-context) port-side)
  (let ((alignment nil)
        (node (.node this)))
    (flet ((try (prop) (when (has-property node prop) (setf alignment (get-property node prop)))))
      (enum-case port-side
        (port-side.north (try core-options.port-alignment-north))
        (port-side.south (try core-options.port-alignment-south))
        (port-side.east (try core-options.port-alignment-east))
        (port-side.west (try core-options.port-alignment-west))))
    (or alignment (get-property node core-options.port-alignment-default))))

(defjstatic node-context.compare-port-sides (s1 s2)
  (integer.compare (ordinal s1) (ordinal s2)))

(defjstatic node-context.compare-port-contexts (pc1 pc2)
  (let ((c (node-context.compare-port-sides (get-side (.port pc1)) (get-side (.port pc2)))))
    (if (/= c 0)
        c
        (enum-case (get-side (.port pc1))
          ((port-side.north port-side.east)
           (integer.compare (get-volatile-id (.port pc1)) (get-volatile-id (.port pc2))))
          ((port-side.south port-side.west)
           (integer.compare (get-volatile-id (.port pc2)) (get-volatile-id (.port pc1))))
          (t 0)))))

;;; ------------------------------------------------------------ PortContext

(defjctor port-context (parent-node-context port)
  (setf (.parent-node-context this) parent-node-context
        (.port this) port
        (.port-position this) (new-k-vector (get-position port)))
  (let* ((placement (.port-labels-placement parent-node-context))
         (next-to-port (contains placement port-label-placement.next-to-port-if-possible)))
    (setf (.labels-next-to-port this)
          (cond ((contains placement port-label-placement.inside)
                 (if (.treat-as-compound-node parent-node-context)
                     (and next-to-port (not (has-compound-connections port)))
                     t))
                ((contains placement port-label-placement.outside)
                 (if next-to-port
                     (not (or (plusp (jsize (get-incoming-edges port)))
                              (plusp (jsize (get-outgoing-edges port)))))
                     nil))
                (t nil)))))

(defjmethod apply-port-position ((this port-context))
  (set-position (.port this) (.port-position this)))

;;; ------------------------------------------------------------ NodeLabelLocation

(defparameter *ns-node-label-location-data*
  ;; constant -> (horizontal-alignment vertical-alignment row column . assigned-placement-lists)
  (flet ((d (h v r c &rest sets) (list* h v r c sets)))
    (let ((o node-label-placement.outside) (i node-label-placement.inside)
          (vt node-label-placement.v-top) (vc node-label-placement.v-center) (vb node-label-placement.v-bottom)
          (hl node-label-placement.h-left) (hc node-label-placement.h-center) (hr node-label-placement.h-right)
          (hp node-label-placement.h-priority)
          (left horizontal-label-alignment.left) (hcen horizontal-label-alignment.center)
          (right horizontal-label-alignment.right)
          (top vertical-label-alignment.top) (vcen vertical-label-alignment.center)
          (bottom vertical-label-alignment.bottom)
          (b container-area.begin) (c container-area.center) (e container-area.end))
      (list
       (cons node-label-location.out-t-l (d left bottom b b (list o vt hl)))
       (cons node-label-location.out-t-c (d hcen bottom b c (list o vt hc) (list o vt hc hp)))
       (cons node-label-location.out-t-r (d right bottom b e (list o vt hr)))
       (cons node-label-location.out-b-l (d left top e b (list o vb hl)))
       (cons node-label-location.out-b-c (d hcen top e c (list o vb hc) (list o vb hc hp)))
       (cons node-label-location.out-b-r (d right top e e (list o vb hr)))
       (cons node-label-location.out-l-t (d right top b b (list o hl vt hp)))
       (cons node-label-location.out-l-c (d right vcen c b (list o hl vc) (list o hl vc hp)))
       (cons node-label-location.out-l-b (d right bottom e b (list o hl vb hp)))
       (cons node-label-location.out-r-t (d left top b e (list o hr vt hp)))
       (cons node-label-location.out-r-c (d left vcen c e (list o hr vc) (list o hr vc hp)))
       (cons node-label-location.out-r-b (d left bottom e e (list o hr vb hp)))
       (cons node-label-location.in-t-l (d left top b b (list i vt hl) (list i vt hl hp)))
       (cons node-label-location.in-t-c (d hcen top b c (list i vt hc) (list i vt hc hp)))
       (cons node-label-location.in-t-r (d right top b e (list i vt hr) (list i vt hr hp)))
       (cons node-label-location.in-c-l (d left vcen c b (list i vc hl) (list i vc hl hp)))
       (cons node-label-location.in-c-c (d hcen vcen c c (list i vc hc) (list i vc hc hp)))
       (cons node-label-location.in-c-r (d right vcen c e (list i vc hr) (list i vc hr hp)))
       (cons node-label-location.in-b-l (d left bottom e b (list i vb hl) (list i vb hl hp)))
       (cons node-label-location.in-b-c (d hcen bottom e c (list i vb hc) (list i vb hc hp)))
       (cons node-label-location.in-b-r (d right bottom e e (list i vb hr) (list i vb hr hp)))
       (cons node-label-location.undefined (d nil nil nil nil))))))

(defun %ns-nll-data (loc) (cdr (assoc loc *ns-node-label-location-data*)))

(defun %ns-placement-items (set)
  (cond ((null set) nil)
        ((enum-set-p set) (enum-set-items set))
        (t (to-cl-list set))))

(defjstatic node-label-location.from-node-label-placement (label-placement)
  ;; Set.equals between the given placement and each location's assigned placements
  (let ((items (%ns-placement-items label-placement)))
    (jdo (location (node-label-location.values) node-label-location.undefined)
      (when (some (lambda (assigned)
                    (and (subsetp assigned items) (subsetp items assigned)))
                  (nthcdr 4 (%ns-nll-data location)))
        (return-from node-label-location.from-node-label-placement/1 location)))))

(defjmethod get-horizontal-alignment ((this node-label-location)) (first (%ns-nll-data this)))
(defjmethod get-vertical-alignment ((this node-label-location)) (second (%ns-nll-data this)))
(defjmethod get-container-row ((this node-label-location)) (third (%ns-nll-data this)))
(defjmethod get-container-column ((this node-label-location)) (fourth (%ns-nll-data this)))

(defjmethod is-inside-location ((this node-label-location))
  (enum-case this
    ((node-label-location.in-t-l node-label-location.in-t-c node-label-location.in-t-r
      node-label-location.in-c-l node-label-location.in-c-c node-label-location.in-c-r
      node-label-location.in-b-l node-label-location.in-b-c node-label-location.in-b-r) t)
    (t nil)))

(defjmethod get-outside-side ((this node-label-location))
  (enum-case this
    ((node-label-location.out-t-l node-label-location.out-t-c node-label-location.out-t-r) port-side.north)
    ((node-label-location.out-b-l node-label-location.out-b-c node-label-location.out-b-r) port-side.south)
    ((node-label-location.out-l-t node-label-location.out-l-c node-label-location.out-l-b) port-side.west)
    ((node-label-location.out-r-t node-label-location.out-r-c node-label-location.out-r-b) port-side.east)
    (t port-side.undefined)))
