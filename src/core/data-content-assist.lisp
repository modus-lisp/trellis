;;;; data-content-assist.lisp -- org.eclipse.elk.core.data.LayoutDataContentAssist:
;;;; the proposals an editor offers for layout option ids, algorithm ids and
;;;; option values.  Pure logic over the metadata service; no IDE types.

(in-package #:elk)

(defjclass layout-data-content-assist () ())

(defun %lda-svc () (layout-meta-data-service.get-instance/1 nil))

;; Proposal.of(String, String) and Proposal.of(String, T): told apart by the second argument.
(defjstatic proposal.of (text second)
  (if (or (null second) (stringp second))
      (make-instance 'proposal :proposal text :label second :data nil)
      (make-instance 'proposal :proposal text :label nil :data second)))

(defjstatic proposal.of (text label data)
  (make-instance 'proposal :proposal text :label label :data data))

(defun %lda-blank-p (s)
  (or (null s) (zerop (length s))))

(defun %lda-split-dots (s)
  "String.split(\"\\\\.\"): the pieces, with trailing empty pieces removed."
  (if (not (find #\. s))
      (list s)
      (let ((pieces (loop with start = 0
                          for pos = (position #\. s :start start)
                          collect (subseq s start pos)
                          while pos
                          do (setf start (1+ pos)))))
        (loop while (and pieces (string= (car (last pieces)) ""))
              do (setf pieces (butlast pieces)))
        pieces)))

(defun %lda-join (pieces)
  "Joiner.on('.').join(pieces)"
  (format nil "~{~A~^.~}" pieces))

(defun %lda-starts-with-p (s prefix)
  (and (>= (length s) (length prefix))
       (string= prefix s :end2 (length prefix))))

(defun %lda-list-starts-with (strings prefix)
  "The Java startsWith over a list of strings: PREFIX matches a run of STRINGS starting at some index."
  (or (null prefix)
      (loop for i from 0 below (1+ (- (length strings) (length prefix)))
            thereis (let ((matches t) (j 0))
                      (loop while (and (< j (length prefix)) matches)
                            do (setf matches (%lda-starts-with-p (nth (+ i j) strings) (nth j prefix)))
                               (incf j))
                      matches))))

(defun %lda-matches-prefix (data checker prefix)
  "Optional<Proposal> for DATA: the shortest dotted suffix of its id that CHECKER resolves back to DATA
and that matches PREFIX (by name, or by the leading id elements)."
  (let* ((matches-name (and (not (%lda-blank-p prefix))
                            (search (string-downcase prefix) (string-downcase (get-name data)))
                            t))
         (id-split (%lda-split-dots (get-id data)))
         (prefix-split (if matches-name nil (%lda-split-dots prefix)))
         (start (1- (length id-split))))
    (when (typep data 'layout-option-data)
      (let ((group (or (get-group data) "")))
        (unless (%lda-blank-p group) (decf start))
        (decf start (count #\. group))))
    (loop for i from start downto 0
          do (let* ((suffix-elements (nthcdr i id-split))
                    (suffix (%lda-join suffix-elements)))
               (when (and (funcall checker suffix)
                          (or matches-name (%lda-list-starts-with suffix-elements prefix-split)))
                 (return (proposal.of suffix data)))))))

(defun %lda-option-proposals (element algorithm-data targets prefix)
  "The option proposals for ELEMENT (NIL: any element) among options that TARGETS admits."
  (let ((svc (%lda-svc))
        (result (new-jlist)))
    (jdo (option (get-option-data svc))
      (let ((option-targets (get-targets option)))
        (when (and (or (is-empty option-targets)
                       (some (lambda (target) (contains option-targets target)) targets))
                   (or (null algorithm-data)
                       (knows-option algorithm-data option)
                       (jequals option core-options.algorithm))
                   (or (null element) (not (has-property element option)))
                   (not (eq (get-visibility option) visibility.hidden)))
          (let ((proposal (%lda-matches-prefix
                           option
                           (lambda (s) (get-option-data-by-suffix svc s))
                           prefix)))
            (when proposal (jl-add result proposal))))))
    result))

(defun %lda-relevant-node (element)
  (cond ((typep element 'elk-node)
         (or (get-parent element) element))
        ((typep element 'elk-edge)
         (get-containing-node element))
        ((typep element 'elk-port)
         (let ((parent (get-parent element)))
           (cond ((and parent (get-parent parent)) (get-parent parent))
                 (parent parent)
                 (t nil))))
        ((typep element 'elk-label)
         (let ((parent (get-parent element)))
           (loop while (typep parent 'elk-label)
                 do (setf parent (get-parent parent)))
           (%lda-relevant-node parent)))
        (t nil)))

(defjstatic layout-data-content-assist.get-algorithm (element)
  (let ((relevant (%lda-relevant-node element)))
    (when relevant
      (let ((algorithm-id (get-property relevant core-options.algorithm)))
        (unless (%lda-blank-p algorithm-id)
          (get-algorithm-data-by-suffix (%lda-svc) algorithm-id))))))

(defjstatic layout-data-content-assist.get-layout-option-proposals (element prefix)
  (let ((responsible (layout-data-content-assist.get-algorithm element)))
    (cond ((typep element 'elk-node)
           (%lda-option-proposals element responsible
                                  (if (is-empty (get-children element))
                                      (list core-target.nodes)
                                      (list core-target.nodes core-target.parents))
                                  prefix))
          ((typep element 'elk-edge)
           (%lda-option-proposals element responsible (list core-target.edges) prefix))
          ((typep element 'elk-port)
           (%lda-option-proposals element responsible (list core-target.ports) prefix))
          ((typep element 'elk-label)
           (%lda-option-proposals element responsible (list core-target.labels) prefix))
          (t (new-jlist)))))

(defjstatic layout-data-content-assist.get-layout-algorithm-proposals (prefix)
  (let* ((svc (%lda-svc))
         (result (new-jlist)))
    (jdo (algorithm (get-algorithm-data svc))
      (let ((proposal (%lda-matches-prefix
                       algorithm
                       (lambda (s) (get-algorithm-data-by-suffix svc s))
                       prefix)))
        (when proposal (jl-add result proposal))))
    result))

(defjstatic layout-data-content-assist.get-layout-option-value-proposal (option prefix)
  prefix
  (let ((proposals (new-jlist)))
    (case (%lod-kind option)
      ((:boolean :enum :enumset)
       (let ((choices (get-choices option)))
         (dotimes (i (length choices))
           (let* ((proposal (aref choices i))
                  (label proposal)
                  (enum-value (get-enum-value option i)))
             (cond ((elk-graph-util.is-experimental-property-value enum-value)
                    (setf label (jstr label " - Experimental")))
                   ((elk-graph-util.is-advanced-property-value enum-value)
                    (setf label (jstr label " - Advanced"))))
             (jl-add proposals (proposal.of proposal label))))))
      ((:double :int)
       (jl-add proposals (proposal.of (%cu-to-string (get-default-default option))
                                      (name (get-type option)))))
      (:object
       (let ((proposal "")
             (instance (elk-reflect.new-instance (%lod-class-symbol option))))
         (when instance
           (setf proposal (jstr "\"" (%cu-to-string instance) "\"")))
         (jl-add proposals (proposal.of proposal (name (get-type option))))))
      (t nil))
    proposals))
