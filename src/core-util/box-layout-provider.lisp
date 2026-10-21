;;;; box-layout-provider.lisp -- org.eclipse.elk.core.util.BoxLayoutProvider
;;;; (and its nested Group class).  Faithful port: the simple mode, the
;;;; grouping modes (DEC / MIXED / INC), the expand-nodes pass and the
;;;; sort order are the Java code.  Java's private methods are defjmethods
;;;; on BOX-LAYOUT-PROVIDER; ListIterator index bookkeeping is written as
;;;; explicit indices.

(in-package #:elk)

(defjconst box-layout-provider.def-aspect-ratio 1.3d0)

;;; Java `float` fields are narrowed to single precision before they reach
;;; the double-precision placement code.
(defun %bl-float (x)
  (coerce (coerce x 'single-float) 'double-float))

(defun %bl-list-p (x)
  (or (listp x) (and (vectorp x) (not (stringp x)))))

;;; ------------------------------------------------------------ Group

;;; Group(ElkNode) and Group(Iterable<Group>) share one constructor arity;
;;; the argument decides which Java constructor runs.
(defjctor group (x)
  (cond ((%bl-list-p x)
         (setf (.groups this) (jlist-from x)
               (.bottom this) (new-jlist)
               (.right this) (new-jlist)
               (.size this) (new-k-vector)))
        (t
         (setf (.node this) x)
         (set-location x 0d0 0d0))))

(defjmethod area ((this group))
  (* (get-width this) (get-height this)))

(defjmethod get-width ((this group))
  (if (.node this)
      (get-width (.node this))
      (.x (.size this))))

(defjmethod get-height ((this group))
  (if (.node this)
      (get-height (.node this))
      (.y (.size this))))

(defjmethod set-width ((this group) w)
  (if (.node this)
      (set-width (.node this) w)
      ;; increase width of the rightmost nodes
      (let ((delta (- w (get-width this))))
        (jdo (g (.right this))
          (set-width g (+ (get-width g) delta)))))
  nil)

(defjmethod set-height ((this group) h)
  (if (.node this)
      (set-height (.node this) h)
      ;; increase bottom most groups
      (let ((delta (- h (get-height this))))
        (jdo (g (.bottom this))
          (set-height g (+ (get-height g) delta)))))
  nil)

(defjmethod translate ((this group) x y)
  (if (.node this)
      (progn
        (set-x (.node this) (+ (get-x (.node this)) x))
        (set-y (.node this) (+ (get-y (.node this)) y)))
      (jdo (g (.groups this))
        (translate g x y)))
  nil)

(defjmethod translate-inner-nodes ((this group) x y)
  (if (.node this)
      (elk-util.translate (.node this) x y)
      (jdo (g (.groups this))
        (translate-inner-nodes g x y)))
  nil)

;;; ------------------------------------------------------------ BoxLayoutProvider

(defjmethod layout ((this box-layout-provider) layout-node progress-monitor)
  (begin progress-monitor "Box layout" 2)
  (let ((obj-spacing (%bl-float (get-property layout-node box-layouter-options.spacing-node-node)))
        (padding (get-property layout-node box-layouter-options.padding))
        (expand-nodes (get-property layout-node box-layouter-options.expand-nodes))
        (interactive (get-property layout-node box-layouter-options.interactive)))
    (if (eq (get-property layout-node box-layouter-options.box-packing-mode) packing-mode.simple)
        (place-boxes this layout-node obj-spacing padding expand-nodes interactive)
        ;; any of the groups
        (place-boxes-grouping this layout-node obj-spacing padding expand-nodes)))
  (done progress-monitor)
  nil)

;;; ------------------------------------------------------------ Simple mode

(defjmethod place-boxes ((this box-layout-provider) parent-node obj-spacing padding
                         expand-nodes interactive)
  (let* ((sorted-boxes (j-sort this parent-node interactive))
         ;; Work on a copy of the minimum size to avoid changing the property's value
         (min-size (elk-util.effective-min-size-constraint-for parent-node))
         (aspect-ratio (get-property parent-node box-layouter-options.aspect-ratio)))
    (when (or (null aspect-ratio) (<= aspect-ratio 0))
      (setf aspect-ratio box-layout-provider.def-aspect-ratio))
    (let ((parent-size (place-boxes this sorted-boxes obj-spacing padding
                                    (.x min-size) (.y min-size) expand-nodes aspect-ratio)))
      (elk-util.resize-node parent-node (.x parent-size) (.y parent-size) nil t)))
  nil)

(defun %bl-compare-boxes (child1 child2 interactive)
  "The comparator of BoxLayoutProvider.sort(): priority, then position or area."
  (let ((prio1 (or (get-property child1 box-layouter-options.priority) 0))
        (prio2 (or (get-property child2 box-layouter-options.priority) 0)))
    (cond ((> prio1 prio2) -1)
          ((< prio1 prio2) 1)
          (t
           ;; boxes have same priority - compare their position or size
           (let ((c 0))
             (when interactive
               (setf c (double.compare (get-y child1) (get-y child2)))
               (when (/= c 0) (return-from %bl-compare-boxes c))
               (setf c (double.compare (get-x child1) (get-x child2)))
               (when (/= c 0) (return-from %bl-compare-boxes c)))
             (double.compare (* (get-width child1) (get-height child1))
                             (* (get-width child2) (get-height child2))))))))

(defjmethod j-sort ((this box-layout-provider) parent-node interactive)
  "Nodes with higher priority first; then by position (interactive) and area."
  (let ((sorted-boxes (jlist-from (get-children parent-node))))
    (jl-sort sorted-boxes (lambda (c1 c2) (%bl-compare-boxes c1 c2 interactive)))
    sorted-boxes))

(defjmethod place-boxes ((this box-layout-provider) sorted-boxes min-spacing padding
                         min-total-width min-total-height expand-nodes aspect-ratio)
  "Places the boxes in order; returns the bounding box as a KVector."
  ;; determine the maximal row width by the maximal box width and the total area
  (let ((max-row-width 0d0)
        (total-area 0d0)
        (n (length sorted-boxes)))
    (jdo (box sorted-boxes)
      (elk-util.resize-node box)
      (setf max-row-width (math.max max-row-width (get-width box)))
      (incf total-area (* (get-width box) (get-height box))))
    ;; calculate std deviation (see the Java rationale: x*n*stddev is added to the area)
    (let* ((mean (/ total-area (float n 1d0)))
           (stddev (area-std-dev this sorted-boxes mean)))
      (incf total-area (* (float n 1d0) 1d0 stddev))
      ;; add top and bottom padding to total area
      (incf total-area (* (math.sqrt total-area) (+ (get-bottom padding) (get-top padding))))
      ;; add left and right padding
      (incf total-area (* (math.sqrt total-area) (get-right padding)))
      ;; calculate the required row width w to achieve the desired aspect ratio
      (setf max-row-width (+ (math.max max-row-width (math.sqrt (* total-area aspect-ratio)))
                             (get-left padding))))
    ;; place nodes iteratively into rows
    (let ((xpos (get-left padding))
          (ypos (get-top padding))
          (highest-box 0d0)
          (broadest-row (get-horizontal padding))
          (row-indices (jlist 0))
          (row-heights (new-jlist)))
      (dotimes (i n)
        (let* ((box (jl-get sorted-boxes i))
               (width (get-width box))
               (height (get-height box)))
          (when (> (+ xpos width) max-row-width)
            ;; place box into the next row
            (when expand-nodes
              (jl-add row-heights highest-box)
              (jl-add row-indices i))
            (setf xpos (get-left padding))
            (incf ypos (+ highest-box min-spacing))
            (setf highest-box 0d0
                  broadest-row (math.max broadest-row (+ (get-horizontal padding) width))))
          (set-location box xpos ypos)
          (setf broadest-row (math.max broadest-row (+ xpos width (get-right padding))))
          (setf highest-box (math.max highest-box height))
          (incf xpos (+ width min-spacing))))
      (setf broadest-row (math.max broadest-row min-total-width))
      (let ((total-height (+ ypos highest-box (get-bottom padding))))
        (when (< total-height min-total-height)
          (incf highest-box (- min-total-height total-height))
          (setf total-height min-total-height))
        ;; expand nodes if required
        (when expand-nodes
          (setf xpos (get-left padding))
          (jl-add row-indices n)
          (let* ((row-index-pos 1)
                 (next-row-index (jl-get row-indices 0))
                 (row-height-pos 0)
                 (row-height 0d0))
            (jl-add row-heights highest-box)
            (dotimes (i n)
              (when (= i next-row-index)
                (setf xpos (get-left padding))
                (setf row-height (jl-get row-heights row-height-pos))
                (incf row-height-pos)
                (setf next-row-index (jl-get row-indices row-index-pos))
                (incf row-index-pos))
              (let* ((box (jl-get sorted-boxes i))
                     (old-height (get-height box)))
                (set-height box row-height)
                (let ((new-height row-height))
                  (when (= (1+ i) next-row-index)
                    (let* ((new-width (- broadest-row xpos (get-right padding)))
                           (old-width (get-width box)))
                      (set-width box new-width)
                      ;; Content alignment
                      (elk-util.translate box (new-k-vector new-width new-height)
                                          (new-k-vector old-width old-height))))
                  (incf xpos (+ (get-width box) min-spacing))))))
          nil)
        ;; return parent size
        (new-k-vector broadest-row total-height)))))

(defjmethod area-std-dev ((this box-layout-provider) boxes mean)
  (let ((variance 0d0))
    (jdo (box boxes)
      (incf variance (math.pow (- (* (get-width box) (get-height box)) mean) 2d0)))
    (math.sqrt (/ variance (float (1- (length boxes)) 1d0)))))

;;; ------------------------------------------------------------ Grouping modes

(defjmethod place-boxes-grouping ((this box-layout-provider) parent-node obj-spacing padding
                                  expand-nodes)
  ;; Work on a copy of the minimum size to avoid changing the property's value
  (let* ((min-size (new-k-vector (get-property parent-node box-layouter-options.node-size-minimum)))
         (aspect-ratio (get-property parent-node box-layouter-options.aspect-ratio)))
    (setf (.x min-size) (math.max (- (.x min-size) (get-left padding) (get-right padding)) 0d0))
    (setf (.y min-size) (math.max (- (.y min-size) (get-top padding) (get-bottom padding)) 0d0))
    (when (or (null aspect-ratio) (<= aspect-ratio 0))
      (setf aspect-ratio box-layout-provider.def-aspect-ratio))
    ;; wrap boxes in groups
    (let ((groups (new-jlist)))
      (jdo (nd (get-children parent-node))
        (jl-add groups (new-group nd)))
      ;; pack according to the mode (sorting is done within every method)
      (let* ((mode (get-property parent-node box-layouter-options.box-packing-mode))
             ;; no border spacing for the grouped nodes!
             (to-be-placed
               (cond ((eq mode packing-mode.group-inc)
                      (merge-and-place-inc this groups obj-spacing (.x min-size) (.y min-size)
                                           expand-nodes aspect-ratio))
                     ((eq mode packing-mode.group-dec)
                      (merge-and-place-dec this groups obj-spacing (.x min-size) (.y min-size)
                                           expand-nodes aspect-ratio))
                     (t ; GROUP_MIXED
                      (merge-and-place-mixed this groups obj-spacing (.x min-size) (.y min-size)
                                             expand-nodes aspect-ratio))))
             (final-group (new-group to-be-placed))
             ;; do final packing of the 'out-most' boxes
             (parent-size (place-inner-boxes this final-group obj-spacing padding
                                             (.x min-size) (.y min-size) expand-nodes aspect-ratio)))
        ;; adjust parent size
        (elk-util.resize-node parent-node (.x parent-size) (.y parent-size) nil t))))
  nil)

(defjmethod place-inner-boxes ((this box-layout-provider) group min-spacing padding
                               min-total-width min-total-height expand-nodes aspect-ratio)
  (let ((max-row-width 0d0)
        (total-area 0d0)
        (members (.groups group)))
    ;; determine the maximal row width by the maximal box width and the total area
    (jdo (box members)
      (when (.node box)
        (elk-util.resize-node (.node box)))
      (setf max-row-width (math.max max-row-width (get-width box)))
      (incf total-area (* (get-width box) (get-height box))))
    ;; calculate std deviation (GROUP_DEC worked better with sdInfluence 2 per the Java note)
    (let* ((n (length members))
           (mean (/ total-area (float n 1d0)))
           (stddev (area-std-dev2 this members mean))
           (sd-influence 1d0))
      (incf total-area (* (float n 1d0) sd-influence stddev))
      ;; calculate the required row width w to achieve the desired aspect ratio
      (setf max-row-width (+ (math.max max-row-width (math.sqrt (* total-area aspect-ratio)))
                             (get-left padding))))
    ;; place nodes iteratively into rows
    (let ((xpos (get-left padding))
          (ypos (get-top padding))
          (highest-box 0d0)
          (broadest-row (get-horizontal padding))
          (row-indices (jlist 0))
          (row-heights (new-jlist))
          (last nil)
          (bottoms (new-jlist)))
      (dotimes (i (length members))
        (let* ((box (jl-get members i))
               (width (get-width box))
               (height (get-height box)))
          (when (> (+ xpos width) max-row-width)
            ;; place box into the next row
            (when expand-nodes
              (jl-add row-heights highest-box)
              (jl-add row-indices i)
              (jl-add (.right group) last)
              (jl-clear bottoms))
            (setf xpos (get-left padding))
            (incf ypos (+ highest-box min-spacing))
            (setf highest-box 0d0
                  broadest-row (math.max broadest-row (+ (get-horizontal padding) width))))
          (jl-add bottoms box)
          (translate box xpos ypos)
          (setf broadest-row (math.max broadest-row (+ xpos width (get-right padding))))
          (setf highest-box (math.max highest-box height))
          (incf xpos (+ width min-spacing))
          (setf last box)))
      (jdo (b bottoms)
        (jl-add (.bottom group) b))
      (jl-add (.right group) (jl-get bottoms (1- (length bottoms))))
      (setf broadest-row (math.max broadest-row min-total-width))
      (let ((total-height (+ ypos highest-box (get-bottom padding))))
        (when (< total-height min-total-height)
          (incf highest-box (- min-total-height total-height))
          (setf total-height min-total-height))
        ;; expand nodes if required
        (when expand-nodes
          (setf xpos (get-left padding))
          (jl-add row-indices (length members))
          (let* ((row-index-pos 1)
                 (next-row-index (jl-get row-indices 0))
                 (row-height-pos 0)
                 (row-height 0d0))
            (jl-add row-heights highest-box)
            (dotimes (i (length members))
              (when (= i next-row-index)
                (setf xpos (get-left padding))
                (setf row-height (jl-get row-heights row-height-pos))
                (incf row-height-pos)
                (setf next-row-index (jl-get row-indices row-index-pos))
                (incf row-index-pos))
              (let ((box (jl-get members i)))
                (set-height box row-height)
                (when (= (1+ i) next-row-index)
                  (let* ((new-width (- broadest-row xpos (get-right padding)))
                         (old-width (get-width box)))
                    (set-width box new-width)
                    (translate-inner-nodes box (/ (- new-width old-width) 2) 0d0)))
                (incf xpos (+ (get-width box) min-spacing))))))
        ;; return parent size
        (new-k-vector broadest-row total-height)))))

(defjmethod area-std-dev2 ((this box-layout-provider) boxes mean)
  (let ((variance 0d0))
    (jdo (box boxes)
      (incf variance (math.pow (- (area box) mean) 2d0)))
    (math.sqrt (/ variance (float (1- (length boxes)) 1d0)))))

;;; Now the packing modes

(defjmethod merge-and-place-dec ((this box-layout-provider) groups obj-spacing
                                 min-width min-height expand-nodes aspect-ratio)
  (declare (ignorable aspect-ratio))
  ;; sort in decreasing area; the code below relies on this
  (jl-sort groups (lambda (g1 g2) (- (double.compare (area g1) (area g2)))))
  (let ((box-queue (jlist-from groups))
        (to-be-placed (new-jlist))
        (maybe-group (new-jlist))
        ;; current 'large' node and the combined area of 'smaller' nodes
        (box-to-beat nil)
        (collected-area 0d0))
    (loop while (not (jl-is-empty box-queue))
          do (let ((box (jl-poll-first box-queue)))
               (if (or (null box-to-beat)
                       ;; prevent 'box' from getting its own 'innerGroup'
                       (< (/ (area box-to-beat) 2d0) (area box)))
                   (progn
                     (setf box-to-beat box)
                     (jl-add to-be-placed box))
                   (progn
                     (incf collected-area (area box))
                     (jl-add maybe-group box)
                     (when (and (> (length maybe-group) 1)
                                (or (> collected-area (/ (area box-to-beat) 2d0))
                                    ;; make sure the remaining nodes in the queue get grouped
                                    (jl-is-empty box-queue)))
                       ;; combine
                       (let* ((inner-group (new-group maybe-group))
                              (inner-aspect-ratio (/ (get-width box-to-beat) (get-height box-to-beat)))
                              (group-size (place-inner-boxes this inner-group obj-spacing
                                                             (new-elk-padding)
                                                             min-width min-height expand-nodes
                                                             inner-aspect-ratio)))
                         (reset (.size inner-group))
                         (add (.size inner-group) group-size)
                         (setf box-to-beat inner-group)
                         (jl-add to-be-placed inner-group)
                         (setf collected-area 0d0)
                         (jl-clear maybe-group)))))))
    ;; add remaining boxes
    (jl-add-all to-be-placed maybe-group)
    to-be-placed))

(defjmethod merge-and-place-mixed ((this box-layout-provider) groups obj-spacing
                                   min-width min-height expand-nodes aspect-ratio)
  (declare (ignorable aspect-ratio))
  (let ((cum-area-array (make-array (length groups) :element-type 'double-float
                                                    :initial-element 0d0))
        (pq (new-jpq (lambda (g1 g2) (double.compare (area g1) (area g2)))))
        (to-be-placed (new-jlist))
        (index 0))
    (jdo (g groups)
      (add pq g))
    (loop while (not (is-empty pq))
          do (let ((box (peek pq)))   ; only take a look, we might not want to remove it
               ;; we need at least two boxes in the 'toBePlaced' list that can be merged
               (if (and (> index 1) (> (/ (area box) 2d0) (aref cum-area-array 0)))
                   (let ((any-index 0))
                     ;; find the largest index; we can use at most 'toBePlaced.size()' groups
                     (loop while (and (< any-index (- (length to-be-placed) 1))
                                      (> (/ (area box) 2d0) (aref cum-area-array any-index)))
                           do (incf any-index))
                     ;; place the selected groups
                     (let* ((select (jl-sub-list to-be-placed 0 (1+ any-index)))
                            (inner-group (new-group select))
                            (inner-aspect-ratio (/ (get-width box) (get-height box)))
                            (group-size (place-inner-boxes this inner-group obj-spacing
                                                           (new-elk-padding)
                                                           min-width min-height expand-nodes
                                                           inner-aspect-ratio))
                            (remain (jl-sub-list to-be-placed (1+ any-index)
                                                 (length to-be-placed))))
                       (reset (.size inner-group))
                       (add (.size inner-group) group-size)
                       ;; now start over
                       (add pq inner-group)
                       (jdo (g remain)
                         (add pq g))
                       (jl-clear to-be-placed)
                       (setf index 0)
                       (fill cum-area-array 0d0)))
                   (progn
                     ;; ok, now remove the group from the queue
                     (poll pq)
                     (when (> index 0)
                       (setf (aref cum-area-array index) (aref cum-area-array (1- index))))
                     (incf (aref cum-area-array index) (area box))
                     (incf index)
                     (jl-add to-be-placed box)))))
    to-be-placed))

(defjmethod merge-and-place-inc ((this box-layout-provider) groups obj-spacing
                                 min-width min-height expand-nodes aspect-ratio)
  (declare (ignorable aspect-ratio))
  ;; sort increasingly
  (jl-sort groups (lambda (g1 g2) (double.compare (area g1) (area g2))))
  (let ((to-be-placed (new-jlist))
        (common-area 0d0))
    (jdo (g groups)
      (if (and (not (jl-is-empty to-be-placed))
               (> (area g) (* common-area 2)))
          (let* ((merged (new-group to-be-placed))
                 (inner-aspect-ratio (/ (get-width g) (get-height g)))
                 ;; place the nodes in the group
                 (group-size (place-inner-boxes this merged obj-spacing (new-elk-padding)
                                                min-width min-height expand-nodes
                                                inner-aspect-ratio)))
            (reset (.size merged))
            (add (.size merged) group-size)
            ;; reset, then add the merged group and the current group to the active list
            (jl-clear to-be-placed)
            (setf common-area 0d0)
            (jl-add to-be-placed merged)
            (jl-add to-be-placed g)
            (setf common-area (+ (area merged) (area g))))
          (progn
            ;; add the group to the active set
            (jl-add to-be-placed g)
            (incf common-area (area g)))))
    to-be-placed))
