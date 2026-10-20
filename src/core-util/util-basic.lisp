;;;; util-basic.lisp -- the small value classes and infrastructure of
;;;; org.eclipse.elk.core.util: progress monitors, Pair/Triple/Quadruple/Maybe,
;;;; exclusive bounds, factories, instance pools, logged graphs.

(in-package #:elk)

(defun %cu-float (x)
  "Java (float) cast of a number, as a double again."
  (if (and (floatp x) (or (nan-p x) (infinite-p x)))
      x
      (float (coerce x 'single-float) 1d0)))

;;; ------------------------------------------------------------ IElkCancelIndicator / IElkProgressMonitor

(defjconst i-elk-progress-monitor.unknown-work -1.0)

(defjgeneric is-canceled 0)
(defjgeneric begin 2)
(defjgeneric worked 1)
(defjgeneric done 0)
(defjgeneric is-running 0)
(defjgeneric get-task-name 0)
(defjgeneric sub-task 1)
(defjgeneric get-sub-monitors 0)
(defjgeneric get-parent-monitor 0)
(defjgeneric is-logging-enabled 0)
(defjgeneric is-log-persistence-enabled 0)
(defjgeneric j-log 1)
(defjgeneric get-logs 0)
(defjgeneric log-graph 2)
(defjgeneric log-graph 3)
(defjgeneric get-logged-graphs 0)
(defjgeneric get-debug-folder 0)
(defjgeneric is-execution-time-measured 0)
(defjgeneric get-execution-time 0)

;;; ------------------------------------------------------------ BasicProgressMonitor

(defjconst basic-progress-monitor.root-debug-folder-name "logs")

(defjctor basic-progress-monitor (themax-levels)
  (with-max-hierarchy-levels this themax-levels))

(defjmethod with-max-hierarchy-levels ((this basic-progress-monitor) levels)
  (setf (.max-levels this) (if (< levels 0) -1 levels))
  this)

(defjmethod with-logging ((this basic-progress-monitor) enabled)
  (setf (.record-logs this) enabled)
  (if enabled
      (setf (.log-messages this) (new-jlist) (.log-graphs this) (new-jlist))
      (setf (.log-messages this) nil (.log-graphs this) nil))
  this)

(defjmethod with-log-persistence ((this basic-progress-monitor) enabled)
  (setf (.persist-logs this) enabled)
  this)

(defjmethod with-execution-time-measurement ((this basic-progress-monitor) enabled)
  (setf (.record-execution-time this) enabled)
  this)

(defjmethod begin ((this basic-progress-monitor) name thetotal-work)
  (cond ((.closed this) (jthrow 'illegal-state-exception "The task is already done."))
        ((.task-name this) nil)
        (t
         (when (null name) (jthrow 'null-pointer-exception))
         (setf (.task-name this) name
               (.total-work this) (%cu-float thetotal-work))
         (do-begin this name thetotal-work (null (.parent-monitor this)) (.max-levels this))
         (when (.record-execution-time this)
           (setf (.start-time this) (get-internal-real-time)))
         t)))

(defjmethod do-begin ((this basic-progress-monitor) name new-total-work top-instance max-hierarchy-levels)
  (declare (ignore name new-total-work top-instance max-hierarchy-levels))
  nil)

(defjmethod worked ((this basic-progress-monitor) work)
  (when (and (> work 0) (not (.closed this)))
    (%cu-internal-worked this work)))

(defun %cu-internal-worked (this work)
  (when (and (> (.total-work this) 0) (< (.completed-work this) (.total-work this)))
    (setf (.completed-work this) (%cu-float (+ (.completed-work this) work)))
    (do-worked this (.completed-work this) (.total-work this) (null (.parent-monitor this)))
    (let ((parent (.parent-monitor this)))
      (when (and parent (> (.current-child-work parent) 0) (/= (.max-levels this) 0))
        (%cu-internal-worked parent (%cu-float (* (/ work (.total-work this))
                                                  (.current-child-work parent))))))))

(defjmethod do-worked ((this basic-progress-monitor) thecompleted-work thetotal-work top-instance)
  (declare (ignore thecompleted-work thetotal-work top-instance))
  nil)

(defjmethod done ((this basic-progress-monitor))
  (when (null (.task-name this))
    (jthrow 'illegal-state-exception "The task has not begun yet."))
  (unless (.closed this)
    (when (.record-execution-time this)
      (setf (.total-time this)
            (jd (/ (- (get-internal-real-time) (.start-time this))
                   internal-time-units-per-second))))
    (when (< (.completed-work this) (.total-work this))
      (%cu-internal-worked this (- (.total-work this) (.completed-work this))))
    (do-done this (null (.parent-monitor this)) (.max-levels this))
    (setf (.closed this) t))
  nil)

(defjmethod do-done ((this basic-progress-monitor) top-instance max-hierarchy-levels)
  (declare (ignore top-instance max-hierarchy-levels))
  nil)

(defjmethod is-running ((this basic-progress-monitor))
  (and (.task-name this) (not (.closed this))))

(defjmethod is-canceled ((this basic-progress-monitor)) nil)

(defjmethod get-task-name ((this basic-progress-monitor)) (.task-name this))

(defjmethod sub-task ((this basic-progress-monitor) work)
  (unless (.closed this)
    (let ((sub (do-sub-task this work (.max-levels this))))
      (jl-add (.children this) sub)
      (setf (.parent-monitor sub) this
            (.current-child-work this) (%cu-float work))
      sub)))

(defjmethod do-sub-task ((this basic-progress-monitor) work max-hierarchy-levels)
  (declare (ignore work))
  (let ((levels (if (> max-hierarchy-levels 0) (1- max-hierarchy-levels) max-hierarchy-levels)))
    (with-execution-time-measurement
        (with-log-persistence
            (with-logging (with-max-hierarchy-levels (new-basic-progress-monitor) levels)
              (.record-logs this))
          (.persist-logs this))
      (.record-execution-time this))))

(defjmethod get-sub-monitors ((this basic-progress-monitor)) (.children this))
(defjmethod get-parent-monitor ((this basic-progress-monitor)) (.parent-monitor this))
(defjmethod is-logging-enabled ((this basic-progress-monitor)) (.record-logs this))
(defjmethod is-log-persistence-enabled ((this basic-progress-monitor)) (.persist-logs this))

(defjmethod j-log ((this basic-progress-monitor) object)
  ;; Messages are recorded; persisting them to disk is debug output (a no-op).
  (when (and (.record-logs this) object)
    (jl-add (.log-messages this) (if (stringp object) object (jstr object)))))

(defjmethod get-logs ((this basic-progress-monitor)) (.log-messages this))

(defjmethod log-graph ((this basic-progress-monitor) graph tag)
  (when (and (.record-logs this) graph)
    (log-graph this graph tag core-util-type.elk)))

(defjmethod log-graph ((this basic-progress-monitor) object tag graph-type)
  (when (and (.record-logs this) object graph-type)
    (jl-add (.log-graphs this) (new-logged-graph object tag graph-type))))

(defjmethod get-logged-graphs ((this basic-progress-monitor)) (.log-graphs this))

(defjmethod get-debug-folder ((this basic-progress-monitor))
  ;; Debug folders are debug output; never created.
  (.debug-folder this))

(defjmethod is-execution-time-measured ((this basic-progress-monitor)) (.record-execution-time this))
(defjmethod get-execution-time ((this basic-progress-monitor)) (.total-time this))

;;; ------------------------------------------------------------ NullElkProgressMonitor

(defjmethod is-canceled ((this null-elk-progress-monitor)) nil)
(defjmethod begin ((this null-elk-progress-monitor) name total-work) (declare (ignore name total-work)) nil)
(defjmethod worked ((this null-elk-progress-monitor) work) (declare (ignore work)) nil)
(defjmethod done ((this null-elk-progress-monitor)) nil)
(defjmethod is-running ((this null-elk-progress-monitor)) nil)
(defjmethod get-task-name ((this null-elk-progress-monitor)) nil)
(defjmethod sub-task ((this null-elk-progress-monitor) work) (declare (ignore work)) this)
(defjmethod get-sub-monitors ((this null-elk-progress-monitor)) nil)
(defjmethod get-parent-monitor ((this null-elk-progress-monitor)) nil)
(defjmethod is-logging-enabled ((this null-elk-progress-monitor)) nil)
(defjmethod is-log-persistence-enabled ((this null-elk-progress-monitor)) nil)
(defjmethod j-log ((this null-elk-progress-monitor) object) (declare (ignore object)) nil)
(defjmethod get-logs ((this null-elk-progress-monitor)) nil)
(defjmethod log-graph ((this null-elk-progress-monitor) graph tag) (declare (ignore graph tag)) nil)
(defjmethod log-graph ((this null-elk-progress-monitor) graph tag type) (declare (ignore graph tag type)) nil)
(defjmethod get-logged-graphs ((this null-elk-progress-monitor)) nil)
(defjmethod get-debug-folder ((this null-elk-progress-monitor)) nil)
(defjmethod is-execution-time-measured ((this null-elk-progress-monitor)) nil)
(defjmethod get-execution-time ((this null-elk-progress-monitor)) 0.0)

;;; ------------------------------------------------------------ LoggedGraph

(defjmethod get-file-extension ((this core-util-type))
  (enum-case this
    (core-util-type.elk "elkg")
    (core-util-type.json "json")
    (core-util-type.dot "dot")
    (t "svg")))

(defjmethod is-type-compatible ((this core-util-type) o)
  (if (eq this core-util-type.elk)
      (typep o 'elk-node)
      (stringp o)))

(defjmethod check-type-compatibility ((this core-util-type) o)
  (unless (is-type-compatible this o)
    (jthrow 'class-cast-exception "Type ~A incompatible for ~A" (type-of o) (name this))))

(defjctor logged-graph (graph tag graph-type)
  (check-type-compatibility graph-type graph)
  (setf (.graph this) graph (.tag this) tag (.graph-type this) graph-type))

(defjmethod get-graph ((this logged-graph)) (.graph this))
(defjmethod get-graph-type ((this logged-graph)) (.graph-type this))
(defjmethod get-tag ((this logged-graph)) (.tag this))
(defjmethod serialize ((this logged-graph))
  ;; Serialising an ELK graph to .elkg is debug output.
  (if (eq (.graph-type this) core-util-type.elk)
      "Unexpected problem serializing ELK Graph."
      (jstr (.graph this))))

;;; ------------------------------------------------------------ AbstractRandomListAccessor

(defjgeneric provide-default 0)

(defjmethod get-list-item ((this abstract-random-list-accessor) index)
  (when (< index 0) (jthrow 'index-out-of-bounds-exception "Invalid index: ~A" index))
  (%cu-ensure-list-size this (1+ index))
  (jl-get (.list this) index))

(defjmethod set-list-item ((this abstract-random-list-accessor) index value)
  (when (< index 0) (jthrow 'index-out-of-bounds-exception "Invalid index: ~A" index))
  (if (< index (length (.list this)))
      (jl-set (.list this) index value)
      (progn (%cu-ensure-list-size this index)
             (jl-add (.list this) value)))
  nil)

(defjmethod get-list-size ((this abstract-random-list-accessor)) (length (.list this)))
(defjmethod clear-list ((this abstract-random-list-accessor)) (jl-clear (.list this)))

(defun %cu-ensure-list-size (this size)
  (loop for i from (length (.list this)) below size
        do (jl-add (.list this) (provide-default this))))

;;; ------------------------------------------------------------ factories, InstancePool

(defjgeneric create 0)
(defjgeneric destroy 1)

(defun %cu-instantiate (clazz)
  "clazz.newInstance(): CLAZZ is a class name symbol (or a class)."
  (let* ((name (if (symbolp clazz) clazz (class-name clazz)))
         (ctor (intern (format nil "NEW-~A/0" (symbol-name name)) :elk)))
    (if (fboundp ctor) (funcall ctor) (make-instance name))))

(defjctor algorithm-factory (theclazz) (this-ctor algorithm-factory this theclazz nil))
(defjctor algorithm-factory (theclazz theparameter)
  (setf (.clazz this) theclazz (.parameter this) theparameter))

(defjmethod create ((this algorithm-factory))
  (let ((algorithm (%cu-instantiate (.clazz this))))
    (initialize algorithm (.parameter this))
    algorithm))

(defjmethod destroy ((this algorithm-factory) obj) (dispose obj))

(defjctor default-factory (theclazz) (setf (.clazz this) theclazz))
(defjmethod create ((this default-factory)) (%cu-instantiate (.clazz this)))
(defjmethod destroy ((this default-factory) obj) (declare (ignore obj)) nil)

(defjconst instance-pool.infinite -1)

(defjctor instance-pool (thefactory) (this-ctor instance-pool this thefactory -1))
(defjctor instance-pool (thefactory thelimit)
  (setf (.factory this) thefactory (.limit this) thelimit))

(defjmethod fetch ((this instance-pool))
  (if (zerop (length (.instances this)))
      (create (.factory this))
      (jl-remove-first (.instances this))))

(defjmethod release ((this instance-pool) obj)
  (if (or (< (.limit this) 0) (< (length (.instances this)) (.limit this)))
      (jl-add (.instances this) obj)
      (destroy (.factory this) obj))
  nil)

(defjmethod clear ((this instance-pool))
  (jdo (obj (jl-copy (.instances this)))
    (destroy (.factory this) obj))
  (jl-clear (.instances this)))

;;; ------------------------------------------------------------ Maybe

(defjstatic maybe.create () (new-maybe))
(defjctor maybe (theobject) (setf (.object this) theobject))

(defjmethod equals ((this maybe) obj)
  (and (typep obj 'maybe)
       (if (null (.object this)) (null (.object obj)) (jequals (.object this) (.object obj)))))

(defjmethod hash-code ((this maybe)) (if (.object this) (sxhash (.object this)) 0))

(defjmethod to-string ((this maybe))
  (if (null (.object this)) "maybe(null)" (jstr "maybe(" (%cu-to-string (.object this)) ")")))

(defjmethod j-set ((this maybe) theobject) (setf (.object this) theobject) nil)
(defjmethod j-get ((this maybe)) (.object this))
(defjmethod clear ((this maybe)) (setf (.object this) nil))
(defjmethod is-empty ((this maybe)) (null (.object this)))
(defmethod jiterable-items ((m maybe)) (if (.object m) (list (.object m)) '()))

(defun %cu-to-string (x)
  "Object.toString() for anything."
  (cond ((stringp x) x)
        ((and (typep x 'jobject) (compute-applicable-methods #'to-string/0 (list x)))
         (to-string/0 x))
        ((typep x 'jenum) (name x))
        (t (jstr x))))

;;; ------------------------------------------------------------ Pair

(defjstatic pair.create () (new-pair))
(defjstatic pair.of (first second) (new-pair first second))
(defjstatic pair.from-map (map)
  (let ((list (new-jlist)))
    (jdo-map (k v map) (jl-add list (new-pair k v)))
    list))

(defjctor pair (thefirst thesecond)
  (setf (.first this) thefirst (.second this) thesecond))

(defjmethod compare ((this first-comparator) o1 o2) (jcompare (.first o1) (.first o2)))
(defjmethod compare ((this second-comparator) o1 o2) (jcompare (.second o1) (.second o2)))

(defjmethod equals ((this pair) obj)
  (and (typep obj 'pair)
       (if (null (.first this)) (null (.first obj)) (jequals (.first this) (.first obj)))
       (if (null (.second this)) (null (.second obj)) (jequals (.second this) (.second obj)))))

(defjmethod hash-code ((this pair))
  (logxor (if (.first this) (sxhash (.first this)) 0)
          (ash (if (.second this) (sxhash (.second this)) 0) 1)))

(defjmethod to-string ((this pair))
  (let ((f (.first this)) (s (.second this)))
    (jstr "pair(" (if f (%cu-to-string f) "null") "," (if s (%cu-to-string s) "null") ")")))

(defjmethod set-first ((this pair) thefirst) (setf (.first this) thefirst) nil)
(defjmethod get-first ((this pair)) (.first this))
(defjmethod set-second ((this pair) thesecond) (setf (.second this) thesecond) nil)
(defjmethod get-second ((this pair)) (.second this))
(defjmethod clear ((this pair)) (setf (.first this) nil (.second this) nil))
(defmethod jiterable-items ((p pair))
  (append (and (.first p) (list (.first p))) (and (.second p) (list (.second p)))))

;;; ------------------------------------------------------------ Triple, Quadruple

(defjctor core-util-triple (f s tt)
  (setf (.first this) f (.second this) s (.third this) tt))
(defjmethod get-first ((this core-util-triple)) (.first this))
(defjmethod get-second ((this core-util-triple)) (.second this))
(defjmethod get-third ((this core-util-triple)) (.third this))
(defjmethod to-string ((this core-util-triple))
  (jstr "(" (.first this) ", " (.second this) ", " (.third this) ")"))

(defjctor quadruple (a b c d)
  (setf (.first this) a (.second this) b (.third this) c (.fourth this) d))
(defjmethod get-first ((this quadruple)) (.first this))
(defjmethod get-second ((this quadruple)) (.second this))
(defjmethod get-third ((this quadruple)) (.third this))
(defjmethod get-fourth ((this quadruple)) (.fourth this))
(defjmethod to-string ((this quadruple))
  (jstr "(" (.first this) ", " (.second this) ", " (.third this) ", " (.fourth this) ")"))

;;; ------------------------------------------------------------ WrappedException

(defun new-wrapped-exception/1 (cause)
  (make-condition 'wrapped-exception :cause cause :message (and cause (princ-to-string cause))))
(defun new-wrapped-exception/2 (message cause)
  (make-condition 'wrapped-exception :cause cause :message message))
(define-jcall new-wrapped-exception :static)

;;; ------------------------------------------------------------ ExclusiveBounds

(defjstatic exclusive-bounds.greater-than (exclusive-lower-bound)
  (new-exclusive-lower-bound exclusive-lower-bound))
(defjstatic exclusive-bounds.less-than (exclusive-upper-bound)
  (new-exclusive-upper-bound exclusive-upper-bound))

(defjctor exclusive-lower-bound (b) (setf (.exclusive-lower-bound this) (jd b)))
(defjctor exclusive-upper-bound (b) (setf (.exclusive-upper-bound this) (jd b)))

(defjmethod compare-to ((this exclusive-lower-bound) x)
  (if (< (.exclusive-lower-bound this) x) -1 1))
(defjmethod compare-to ((this exclusive-upper-bound) x)
  (if (> (.exclusive-upper-bound this) x) 1 -1))

(defjmethod equals ((this exclusive-lower-bound) obj)
  (and (typep obj 'exclusive-lower-bound)
       (= (.exclusive-lower-bound this) (.exclusive-lower-bound obj))))
(defjmethod equals ((this exclusive-upper-bound) obj)
  (and (typep obj 'exclusive-upper-bound)
       (= (.exclusive-upper-bound this) (.exclusive-upper-bound obj))))

(defjmethod hash-code ((this exclusive-lower-bound)) (sxhash (.exclusive-lower-bound this)))
(defjmethod hash-code ((this exclusive-upper-bound)) (sxhash (.exclusive-upper-bound this)))

(defjmethod to-string ((this exclusive-lower-bound))
  (jstr (.exclusive-lower-bound this) " (exclusive)"))
(defjmethod to-string ((this exclusive-upper-bound))
  (jstr (.exclusive-upper-bound this) " (exclusive)"))

(defmethod print-object ((b exclusive-lower-bound) s) (write-string (to-string/0 b) s))
(defmethod print-object ((b exclusive-upper-bound) s) (write-string (to-string/0 b) s))
