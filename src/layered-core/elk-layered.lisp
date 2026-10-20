;;;; elk-layered.lisp -- org.eclipse.elk.alg.layered.ElkLayered
;;;;
;;;; The main driver: configure, split into components, run the processors,
;;;; combine, resize.  Also hierarchical (INCLUDE_CHILDREN) layout and the
;;;; white-box test hooks (TestController notifications, step-wise execution).

(in-package #:elk)

;;; ------------------------------------------------------------ regular layout

(defjmethod do-layout ((this elk-layered) lgraph monitor)
  (let ((the-monitor monitor))
    (when (null the-monitor)
      (setf the-monitor (with-max-hierarchy-levels (new-basic-progress-monitor) 0)))
    (begin the-monitor "Layered layout" 1)
    ;; update the modules depending on user options
    (prepare-graph-for-layout (.graph-configurator this) lgraph)
    ;; split the input graph into components and perform layout on them
    (let ((components (split (.components-processor this) lgraph)))
      (if (= (jsize components) 1)
          (%elk-layered-layout this (j-get components 0) the-monitor)
          (let ((comp-work (float (/ 1f0 (jsize components)) 1d0)))
            (jdo (comp components)
              ;; Java tests the caller's MONITOR here (NPE if it was null)
              (when (is-canceled the-monitor)
                (return-from do-layout/2 nil))
              (%elk-layered-layout this comp (sub-task the-monitor comp-work)))))
      (combine (.components-processor this) components lgraph))
    ;; resize the resulting graph according to minimal size constraints and such
    (%elk-layered-resize-graph lgraph)
    (done the-monitor)))

;;; ------------------------------------------------------------ compound graph layout

(defjmethod do-compound-layout ((this elk-layered) lgraph monitor)
  (let ((the-monitor monitor))
    (when (null the-monitor)
      (setf the-monitor (with-max-hierarchy-levels (new-basic-progress-monitor) 0)))
    (begin the-monitor "Layered layout" 2)
    ;; split cross-hierarchy edges
    (%elk-layered-notify-processor-ready this lgraph (.compound-graph-preprocessor this))
    (process (.compound-graph-preprocessor this) lgraph (sub-task the-monitor 1))
    (%elk-layered-notify-processor-finished this lgraph (.compound-graph-preprocessor this))
    (%elk-layered-hierarchical-layout this lgraph (sub-task the-monitor 1))
    ;; combine split cross-hierarchy edges
    (%elk-layered-notify-processor-ready this lgraph (.compound-graph-postprocessor this))
    (process (.compound-graph-postprocessor this) lgraph (sub-task the-monitor 1))
    (%elk-layered-notify-processor-finished this lgraph (.compound-graph-postprocessor this))
    (done the-monitor)))

;; A (graph, iterator over its processors) pair.
(defstruct (%elk-layered-gal (:constructor %make-elk-layered-gal (graph processors)))
  graph processors (index 0))

(defun %gal-has-next (g)
  (< (%elk-layered-gal-index g) (jsize (%elk-layered-gal-processors g))))

(defun %gal-next (g)
  (prog1 (j-get (%elk-layered-gal-processors g) (%elk-layered-gal-index g))
    (incf (%elk-layered-gal-index g))))

(defun %elk-layered-hierarchical-layout (this lgraph monitor)
  ;; reversed breadth first search: the graphs in the lowest hierarchy come first
  (let ((graphs (%elk-layered-collect-all-graphs-bottom-up lgraph))
        (work 0)
        (graphs-and-algorithms (new-jlist)))
    ;; hierarchical processors must not break the control flow (#228)
    (%elk-layered-review-and-correct-hierarchical-processors lgraph graphs)
    (dolist (g graphs)
      (prepare-graph-for-layout (.graph-configurator this) g)
      (let ((processors (get-property g layered-internal-properties.processors)))
        (incf work (jsize processors))
        (jl-add graphs-and-algorithms (%make-elk-layered-gal g processors))))
    (begin monitor "Recursive hierarchical layout" work)
    ;; when the root graph has finished layout, the layout is complete
    (let ((slot-index 0)
          (root-processors (jl-last graphs-and-algorithms)))
      (loop while (%gal-has-next root-processors) do
        ;; layout from bottom up
        (jdo (graph-and-algorithm graphs-and-algorithms)
          (let ((graph (%elk-layered-gal-graph graph-and-algorithm)))
            (loop while (%gal-has-next graph-and-algorithm) do
              (let ((processor (%gal-next graph-and-algorithm)))
                (cond ((not (typep processor 'i-hierarchy-aware-layout-processor))
                       (%elk-layered-notify-processor-ready this graph processor)
                       (process processor graph (sub-task monitor 1))
                       (%elk-layered-notify-processor-finished this graph processor)
                       (incf slot-index))
                      ((%elk-layered-root-p graph)
                       ;; a processor on the full hierarchy must be executed on the root
                       (%elk-layered-notify-processor-ready this graph processor)
                       (process processor graph (sub-task monitor 1))
                       (%elk-layered-notify-processor-finished this graph processor)
                       (incf slot-index)
                       ;; continue with the graph at the bottom of the hierarchy
                       (return))
                      (t
                       ;; skip and pause until the root graph has processed
                       (return))))))))
      slot-index)
    (done monitor)))

(defun %elk-layered-collect-all-graphs-bottom-up (root)
  "Java returns an ArrayDeque filled by push(); iterating it yields the graphs
in reverse push order, i.e. this CL list's order.  The root comes last."
  (let ((collected-graphs (list root))
        (continue-searching (list root)))
    (loop while continue-searching do
      (let ((next-graph (pop continue-searching)))
        (jdo (node (get-layerless-nodes next-graph))
          (when (%elk-layered-has-nested-graph node)
            (let ((nested-graph (get-nested-graph node)))
              (push nested-graph collected-graphs)
              (push nested-graph continue-searching))))))
    collected-graphs))

(defun %elk-layered-review-and-correct-hierarchical-processors (root graphs)
  ;; crossing minimization: only layer sweep is hierarchical
  (let ((parent-cms (get-property root layered-options.crossing-minimization-strategy)))
    (dolist (child graphs)
      (let ((child-cms (get-property child layered-options.crossing-minimization-strategy)))
        (unless (eq child-cms parent-cms)
          (jthrow 'unsupported-graph-exception
                  "The hierarchy aware processor ~A in child node ~A is only allowed if the root node specifies the same hierarchical processor."
                  (name child-cms) child)))))
  ;; greedy switch: copy the behaviour of the root to all children
  (let ((root-type (get-property root layered-options.crossing-minimization-greedy-switch-hierarchical-type)))
    (dolist (g graphs)
      (set-property g layered-options.crossing-minimization-greedy-switch-hierarchical-type root-type))))

(defun %elk-layered-root-p (graph)
  (null (get-parent-node graph)))

(defun %elk-layered-has-nested-graph (node)
  (not (null (get-nested-graph node))))

;;; ------------------------------------------------------------ layout testing

(defjmethod get-graphs ((this test-execution-state)) (.graphs this))
(defjmethod get-step ((this test-execution-state)) (.step this))

(defjmethod prepare-layout-test ((this elk-layered) lgraph)
  (let ((state (new-test-execution-state)))
    (prepare-graph-for-layout (.graph-configurator this) lgraph)
    (setf (.graphs state) (split (.components-processor this) lgraph))
    state))

(defjmethod is-layout-test-finished ((this elk-layered) state)
  (let* ((graph (j-get (.graphs state) 0))
         (algorithm (get-property graph layered-internal-properties.processors)))
    (and algorithm (>= (.step state) (jsize algorithm)))))

(defun %elk-layered-processor-class-p (processor phase)
  "processor.getClass().equals(phase); PHASE is a class name symbol or a class."
  (if (symbolp phase)
      (eq (class-name (class-of processor)) phase)
      (eq (class-of processor) phase)))

(defjmethod run-layout-test-until ((this elk-layered) phase inclusive state)
  (let* ((algorithm (get-property (j-get (.graphs state) 0) layered-internal-properties.processors))
         (phase-exists nil)
         (phase-index (.step state))
         (n (jsize algorithm)))
    ;; check if the given phase exists in the current algorithm configuration
    (loop for i from (.step state) below n
          while (not phase-exists)
          do (if (%elk-layered-processor-class-p (j-get algorithm i) phase)
                 (progn (setf phase-exists t)
                        (when inclusive (incf phase-index)))
                 (incf phase-index)))
    (unless phase-exists
      (format *error-output* "Given processor ~A not part of the remaining algorithm.~%" phase))
    ;; perform the layout up to and including that phase
    (loop while (< (.step state) phase-index) do
      (%elk-layered-layout-test this (.graphs state) (j-get algorithm (.step state)))
      (incf (.step state)))))

(defjmethod run-layout-test-until ((this elk-layered) phase state)
  (run-layout-test-until this phase t state))

(defjmethod run-layout-test-step ((this elk-layered) state)
  (when (is-layout-test-finished this state)
    (jthrow 'illegal-state-exception "Current layout test run has finished."))
  (let ((algorithm (get-property (j-get (.graphs state) 0) layered-internal-properties.processors)))
    (%elk-layered-layout-test this (.graphs state) (j-get algorithm (.step state)))
    (incf (.step state))))

(defjmethod get-layout-test-configuration ((this elk-layered) state)
  (get-property (j-get (.graphs state) 0) layered-internal-properties.processors))

(defjmethod set-test-controller ((this elk-layered) test-controller)
  (setf (.test-controller this) test-controller))

(defun %elk-layered-notify-processor-ready (this lgraph processor)
  (let ((tc (.test-controller this)))
    (when tc
      (if (%elk-layered-root-p lgraph)
          (notify-root-processor-ready tc lgraph processor)
          (notify-processor-ready tc lgraph processor)))))

(defun %elk-layered-notify-processor-finished (this lgraph processor)
  (let ((tc (.test-controller this)))
    (when tc
      (if (%elk-layered-root-p lgraph)
          (notify-root-processor-finished tc lgraph processor)
          (notify-processor-finished tc lgraph processor)))))

;;; ------------------------------------------------------------ actual layout

(defun %elk-layered-layout (this lgraph monitor)
  (let ((monitor-was-already-running (is-running monitor)))
    (unless monitor-was-already-running
      (begin monitor "Component Layout" 1))
    (let* ((algorithm (get-property lgraph layered-internal-properties.processors))
           (monitor-progress (float (/ 1f0 (jsize algorithm)) 1d0)))
      (when (is-logging-enabled monitor)
        (j-log monitor (jstr "ELK Layered uses the following " (jsize algorithm) " modules:"))
        (let ((slot 0))
          (jdo (processor algorithm)
            (j-log monitor (format nil "   Slot ~2,'0D: ~(~A~)" slot (class-name (class-of processor))))
            (incf slot))))
      ;; invoke each layout processor
      (jdo (processor algorithm)
        (when (is-canceled monitor)
          (return-from %elk-layered-layout nil))
        (%elk-layered-notify-processor-ready this lgraph processor)
        (process processor lgraph (sub-task monitor monitor-progress))
        (%elk-layered-notify-processor-finished this lgraph processor)))
    ;; move all nodes away from the layers (two loops, as in Java)
    (jdo (layer (get-layers lgraph))
      (jl-add-all (get-layerless-nodes lgraph) (get-nodes layer))
      (jl-clear (get-nodes layer)))
    (jdo (node (get-layerless-nodes lgraph))
      (set-layer node nil))
    (jl-clear (get-layers lgraph))
    (unless monitor-was-already-running
      (done monitor))))

(defun %elk-layered-layout-test (this lgraphs processor)
  (jdo (lgraph lgraphs)
    (%elk-layered-notify-processor-ready this lgraph processor)
    (process processor lgraph (new-basic-progress-monitor))
    (%elk-layered-notify-processor-finished this lgraph processor)))

;;; ------------------------------------------------------------ graph postprocessing

(defun %elk-layered-resize-graph (lgraph)
  (let* ((size-constraint (get-property lgraph layered-options.node-size-constraints))
         (size-options (get-property lgraph layered-options.node-size-options))
         (calculated-size (get-actual-size lgraph))
         (adjusted-size (new-k-vector calculated-size)))
    (when (contains size-constraint size-constraint.minimum-size)
      (let ((min-size (get-property lgraph layered-options.node-size-minimum)))
        ;; if minimum width or height are not set, maybe default to default values
        (when (contains size-options size-options.default-minimum-size)
          (when (<= (.x min-size) 0)
            (setf (.x min-size) elk-util.default-min-width))
          (when (<= (.y min-size) 0)
            (setf (.y min-size) elk-util.default-min-height)))
        ;; apply new size including border spacing
        (setf (.x adjusted-size) (math.max (.x calculated-size) (.x min-size))
              (.y adjusted-size) (math.max (.y calculated-size) (.y min-size)))))
    (unless (get-property lgraph layered-options.node-size-fixed-graph-size)
      (%elk-layered-resize-graph-no-really-i-mean-it lgraph calculated-size adjusted-size))))

(defun %elk-layered-resize-graph-no-really-i-mean-it (lgraph old-size new-size)
  (let ((content-alignment (get-property lgraph layered-options.content-alignment)))
    ;; horizontal alignment
    (when (> (.x new-size) (.x old-size))
      (cond ((contains content-alignment content-alignment.h-center)
             (incf (.x (get-offset lgraph)) (/ (- (.x new-size) (.x old-size)) 2.0)))
            ((contains content-alignment content-alignment.h-right)
             (incf (.x (get-offset lgraph)) (- (.x new-size) (.x old-size))))))
    ;; vertical alignment
    (when (> (.y new-size) (.y old-size))
      (cond ((contains content-alignment content-alignment.v-center)
             (incf (.y (get-offset lgraph)) (/ (- (.y new-size) (.y old-size)) 2.0)))
            ((contains content-alignment content-alignment.v-bottom)
             (incf (.y (get-offset lgraph)) (- (.y new-size) (.y old-size)))))))
  ;; correct the position of eastern and southern hierarchical ports
  (when (and (contains (get-property lgraph layered-internal-properties.graph-properties)
                       layered-graph-properties.external-ports)
             (or (> (.x new-size) (.x old-size)) (> (.y new-size) (.y old-size))))
    (jdo (node (get-layerless-nodes lgraph))
      (when (eq (get-type node) node-type.external-port)
        (let ((ext-port-side (get-property node layered-internal-properties.ext-port-side)))
          (cond ((eq ext-port-side port-side.east)
                 (incf (.x (get-position node)) (- (.x new-size) (.x old-size))))
                ((eq ext-port-side port-side.south)
                 (incf (.y (get-position node)) (- (.y new-size) (.y old-size)))))))))
  ;; actually apply the new size
  (let ((l-padding (get-padding lgraph)))
    (setf (.x (get-size lgraph)) (- (.x new-size) (.left l-padding) (.right l-padding))
          (.y (get-size lgraph)) (- (.y new-size) (.top l-padding) (.bottom l-padding)))))
