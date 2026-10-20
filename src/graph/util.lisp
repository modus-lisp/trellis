;;;; util.lisp -- org.eclipse.elk.graph.util: ElkGraphUtil and
;;;; GraphIdentifierGenerator.  (ElkGraphSwitch / ElkGraphAdapterFactory are
;;;; EMF reflection boilerplate; their only use here, the switch in
;;;; GraphIdentifierGenerator, is a TYPECASE.)  ElkReflect is in
;;;; properties.lisp.

(in-package #:elk)

;;; ------------------------------------------------------------ Creation

(defjstatic elk-graph-util.create-graph ()
  (elk-graph-util.create-node/1 nil))

(defjstatic elk-graph-util.create-node (parent)
  (let ((node (create-elk-node/0 elk-graph-factory.e-instance)))
    (when parent (set-parent/1 node parent))
    node))

(defjstatic elk-graph-util.create-port (parent)
  (let ((port (create-elk-port/0 elk-graph-factory.e-instance)))
    (when parent (set-parent/1 port parent))
    port))

(defjstatic elk-graph-util.create-label (parent)
  (let ((label (create-elk-label/0 elk-graph-factory.e-instance)))
    (when parent (set-parent/1 label parent))
    label))

(defjstatic elk-graph-util.create-label (text parent)
  (let ((label (elk-graph-util.create-label/1 parent)))
    (set-text/1 label text)
    label))

(defjstatic elk-graph-util.create-edge (containing-node)
  (let ((edge (create-elk-edge/0 elk-graph-factory.e-instance)))
    (when containing-node (set-containing-node/1 edge containing-node))
    edge))

(defjstatic elk-graph-util.create-simple-edge (source target)
  (unless source (jthrow 'null-pointer-exception "source cannot be null"))
  (unless target (jthrow 'null-pointer-exception "target cannot be null"))
  (let ((edge (elk-graph-util.create-edge/1 nil)))
    (add/1 (get-sources/0 edge) source)
    (add/1 (get-targets/0 edge) target)
    (elk-graph-util.update-containment/1 edge)
    edge))

(defjstatic elk-graph-util.create-hyperedge (sources targets)
  (unless sources (jthrow 'null-pointer-exception "sources cannot be null"))
  (unless targets (jthrow 'null-pointer-exception "targets cannot be null"))
  (let ((edge (elk-graph-util.create-edge/1 nil)))
    (let ((edge-sources (get-sources/0 edge)))
      (jdo (source sources) (add/1 edge-sources source)))
    (let ((edge-targets (get-targets/0 edge)))
      (jdo (target targets) (add/1 edge-targets target)))
    (elk-graph-util.update-containment/1 edge)
    edge))

(defjstatic elk-graph-util.create-edge-section (edge)
  (let ((section (create-elk-edge-section/0 elk-graph-factory.e-instance)))
    (when edge (add/1 (get-sections/0 edge) section))
    section))

(defjstatic elk-graph-util.first-edge-section (edge reset-section remove-other-sections)
  (if (is-empty/0 (get-sections/0 edge))
      ;; Create and return a new section
      (elk-graph-util.create-edge-section/1 edge)
      ;; Retrieve the first section
      (let ((section (j-get/1 (get-sections/0 edge) 0)))
        (when reset-section
          (clear/0 (get-bend-points/0 section))
          (set-start-location/2 section 0 0)
          (set-end-location/2 section 0 0))
        (when remove-other-sections
          (let ((sections (get-sections/0 edge)))
            (loop while (> (size/0 sections) 1)
                  do (j-remove/1 sections (1- (size/0 sections))))))
        section)))

(defjstatic elk-graph-util.create-bend-point (edge-section)
  (elk-graph-util.create-bend-point/3 edge-section 0 0))

(defjstatic elk-graph-util.create-bend-point (edge-section x y)
  (let ((bend-point (create-elk-bend-point/0 elk-graph-factory.e-instance)))
    (j-set/2 bend-point x y)
    (when edge-section (add/1 (get-bend-points/0 edge-section) bend-point))
    bend-point))

;;; ------------------------------------------------------------ Edge Containment

(defjstatic elk-graph-util.update-containment (edge)
  (unless edge (jthrow 'null-pointer-exception "edge cannot be null"))
  (set-containing-node/1 edge (elk-graph-util.find-best-edge-containment/1 edge)))

(defjstatic elk-graph-util.find-best-edge-containment (edge)
  (unless edge (jthrow 'null-pointer-exception "edge cannot be null"))
  (let ((sources (get-sources/0 edge)) (targets (get-targets/0 edge)))
    ;; Corner cases: an edge connected to nothing or to just one shape.
    (case (+ (size/0 sources) (size/0 targets))
      (0 (jthrow 'illegal-argument-exception "The edge must have at least one source or target."))
      (1 (return-from elk-graph-util.find-best-edge-containment/1
           (if (is-empty/0 sources)
               (get-parent/0 (elk-graph-util.connectable-shape-to-node/1 (j-get/1 targets 0)))
               (get-parent/0 (elk-graph-util.connectable-shape-to-node/1 (j-get/1 sources 0)))))))
    ;; The simple common cases: same parent, or one is the other's parent.
    (when (and (= (size/0 sources) 1) (= (size/0 targets) 1))
      (let ((source-node (elk-graph-util.connectable-shape-to-node/1 (j-get/1 sources 0)))
            (target-node (elk-graph-util.connectable-shape-to-node/1 (j-get/1 targets 0))))
        (cond ((eq (get-parent/0 source-node) (get-parent/0 target-node))
               (return-from elk-graph-util.find-best-edge-containment/1 (get-parent/0 source-node)))
              ((eq source-node (get-parent/0 target-node))
               (return-from elk-graph-util.find-best-edge-containment/1 source-node))
              ((eq target-node (get-parent/0 source-node))
               (return-from elk-graph-util.find-best-edge-containment/1 target-node)))))
    ;; The general case: keep track of the highest common ancestor so far.
    (let* ((incident (coerce (elk-graph-util.all-incident-shapes/1 edge) 'list))
           (common-ancestor (elk-graph-util.connectable-shape-to-node/1 (first incident))))
      (dolist (shape (rest incident))
        (let ((incident-node (elk-graph-util.connectable-shape-to-node/1 shape)))
          (when (and (not (eq incident-node common-ancestor))
                     (not (elk-graph-util.is-descendant/2 incident-node common-ancestor)))
            (if (eq (get-parent/0 incident-node) (get-parent/0 common-ancestor))
                ;; The two nodes are siblings, the common ancestor is their parent
                (setf common-ancestor (get-parent/0 incident-node))
                (progn
                  (setf common-ancestor
                        (elk-graph-util.find-lowest-common-ancestor/2 common-ancestor incident-node))
                  (when (null common-ancestor)
                    ;; The nodes are not part of the same graph. Abort.
                    (return-from elk-graph-util.find-best-edge-containment/1 nil)))))))
      common-ancestor)))

(defun %elk-ancestors (node include-node)
  "AncestorIterator, collected: NODE (if INCLUDE-NODE) and its parents upwards."
  (loop for n = (if include-node node (get-parent/0 node)) then (get-parent/0 n)
        while n collect n))

(defjstatic elk-graph-util.find-lowest-common-ancestor (node1 node2)
  ;; Traverse the ancestor hierarchies from the end as long as they agree.
  (let ((a1 (reverse (%elk-ancestors node1 t)))
        (a2 (reverse (%elk-ancestors node2 t)))
        (common nil))
    (loop for x in a1 for y in a2
          do (if (eq x y) (setf common x) (return)))
    common))

;;; ------------------------------------------------------------ Convenience
;;; Guava's concatenated Iterables become fresh jlists.

(defjstatic elk-graph-util.all-incoming-edges (node)
  (let ((out (%el-copy (get-incoming-edges/0 node))))
    (jdo (port (get-ports/0 node)) (jl-add-all out (get-incoming-edges/0 port)))
    out))

(defjstatic elk-graph-util.all-outgoing-edges (node)
  (let ((out (%el-copy (get-outgoing-edges/0 node))))
    (jdo (port (get-ports/0 node)) (jl-add-all out (get-outgoing-edges/0 port)))
    out))

(defjstatic elk-graph-util.all-incident-edges (shape)
  (if (typep shape 'elk-node)
      (let ((out (elk-graph-util.all-outgoing-edges/1 shape)))
        (jl-add-all out (elk-graph-util.all-incoming-edges/1 shape))
        out)
      (let ((out (%el-copy (get-incoming-edges/0 shape))))
        (jl-add-all out (get-outgoing-edges/0 shape))
        out)))

(defjstatic elk-graph-util.all-incident-shapes (edge)
  (let ((out (%el-copy (get-sources/0 edge))))
    (jl-add-all out (get-targets/0 edge))
    out))

(defjstatic elk-graph-util.all-incident-sections (section)
  (let ((out (%el-copy (get-incoming-sections/0 section))))
    (jl-add-all out (get-outgoing-sections/0 section))
    out))

(defjstatic elk-graph-util.is-descendant (child ancestor)
  (loop for current = (get-parent/0 child) then (get-parent/0 current)
        while current
        when (eq current ancestor) return t))

(defjstatic elk-graph-util.containing-graph (element)
  (let ((current element))
    (loop while current
          do (cond ((typep current 'elk-edge)
                    ;; Edges have a direct reference to their containing graph
                    (return-from elk-graph-util.containing-graph/1 (get-containing-node/0 current)))
                   ((typep current 'elk-node)
                    (return-from elk-graph-util.containing-graph/1 (get-parent/0 current)))
                   ((typep (e-container/0 current) 'elk-graph-element)
                    (setf current (e-container/0 current)))
                   (t (return-from elk-graph-util.containing-graph/1 nil))))
    nil))

(defjstatic elk-graph-util.connectable-shape-to-node (connectable-shape)
  (cond ((typep connectable-shape 'elk-node) connectable-shape)
        ((typep connectable-shape 'elk-port) (get-parent/0 connectable-shape))
        ((null connectable-shape) (jthrow 'null-pointer-exception "connectableShape cannot be null"))
        (t (jthrow 'unsupported-operation-exception "Only support nodes and ports."))))

(defjstatic elk-graph-util.connectable-shape-to-port (connectable-shape)
  (cond ((typep connectable-shape 'elk-port) connectable-shape)
        ((null connectable-shape) (jthrow 'null-pointer-exception "connectableShape cannot be null"))
        (t nil)))

(defun %elk-check-simple (simple-edge)
  (when (or (/= (size/0 (get-sources/0 simple-edge)) 1) (/= (size/0 (get-targets/0 simple-edge)) 1))
    (jthrow 'illegal-argument-exception "Passed edge is not 'simple'.")))

(defjstatic elk-graph-util.get-source-node (simple-edge)
  (%elk-check-simple simple-edge)
  (elk-graph-util.connectable-shape-to-node/1 (j-get/1 (get-sources/0 simple-edge) 0)))

(defjstatic elk-graph-util.get-target-node (simple-edge)
  (%elk-check-simple simple-edge)
  (elk-graph-util.connectable-shape-to-node/1 (j-get/1 (get-targets/0 simple-edge) 0)))

(defjstatic elk-graph-util.get-source-port (simple-edge)
  (%elk-check-simple simple-edge)
  (elk-graph-util.connectable-shape-to-port/1 (j-get/1 (get-sources/0 simple-edge) 0)))

(defjstatic elk-graph-util.get-target-port (simple-edge)
  (%elk-check-simple simple-edge)
  (elk-graph-util.connectable-shape-to-port/1 (j-get/1 (get-targets/0 simple-edge) 0)))

(defjstatic elk-graph-util.element-labeled-by (label)
  (let ((element (get-parent/0 label)))
    (loop while (typep element 'elk-label)
          do (setf element (e-container/0 element)))
    (and (typep element 'elk-graph-element) element)))

;;; ------------------------------------------------------------ Iteration

(defjstatic elk-graph-util.properties-skipping-iterator-for (root include-root)
  "A pre-order traversal of ROOT's containment tree, skipping the property map
entries (which the port does not model as objects anyway).  Answered as a
fresh jlist instead of a TreeIterator."
  (let ((out (e-all-contents/0 root)))
    (when include-root (jl-add-first out root))
    out))

;;; ------------------------------------------------------------ Property values

(defjstatic elk-graph-util.is-advanced-property-value (enum-value)
  (%annotated-enum-value-p enum-value *advanced-property-values*))

(defjstatic elk-graph-util.is-experimental-property-value (enum-value)
  (%annotated-enum-value-p enum-value *experimental-property-values*))

;;; ------------------------------------------------------------ GraphIdentifierGenerator

(defjstatic graph-identifier-generator.for-graph (graph)
  (new-graph-identifier-generator/1 graph))

(defjmethod assert-valid ((this graph-identifier-generator)) (setf (.validate this) t) this)
(defjmethod assert-exists ((this graph-identifier-generator)) (setf (.generate this) t) this)
(defjmethod assert-unique ((this graph-identifier-generator)) (setf (.unique this) t) this)

(defjmethod execute ((this graph-identifier-generator))
  (when (.validate this) (validate-identifiers/1 this (.graph this)))
  (when (.generate this) (generate-identifiers/1 this (.graph this)))
  (when (.unique this) (assert-all-ids-unique/1 this (.graph this)))
  nil)

;; The private ElementType enum: (ordinal . prefix)
(defparameter *gig-element-types*
  '((:node 0 "N") (:port 1 "P") (:edge 2 "E") (:edge-section 3 "ES") (:label 4 "L")))

(defun %blank-id-p (id) (or (null id) (zerop (length (string-trim " 	
" id)))))

(defjctor graph-identifier-generator (graph)
  (setf (.graph this) graph)
  (jdo (obj (e-all-contents/0 graph))
    (when (or (typep obj 'elk-graph-element) (typep obj 'elk-edge-section))
      (unless (%blank-id-p (get-identifier/0 obj))
        (js-add (.existing-identifiers this) (get-identifier/0 obj))))))

(defun %gig-children (element)
  "The order in which the switch cases recurse."
  (typecase element
    (elk-node (append (to-cl-list (get-labels/0 element)) (to-cl-list (get-ports/0 element))
                      (to-cl-list (get-contained-edges/0 element)) (to-cl-list (get-children/0 element))))
    (elk-port (to-cl-list (get-labels/0 element)))
    (elk-label (to-cl-list (get-labels/0 element)))
    (elk-edge (append (to-cl-list (get-labels/0 element)) (to-cl-list (get-sections/0 element))))
    (t nil)))

(defjmethod validate-identifiers ((this graph-identifier-generator) element)
  (when (or (typep element 'elk-graph-element) (typep element 'elk-edge-section))
    (validate-identifier/1 this element)
    (dolist (c (%gig-children element)) (validate-identifiers/1 this c)))
  nil)

(defjmethod validate-identifier ((this graph-identifier-generator) element)
  (if (stringp element)
      (%gig-validate-string element)
      (let ((valid (%gig-validate-string (get-identifier/0 element))))
        (when valid (set-identifier/1 element valid))
        nil)))

(defun %gig-validate-string (identifier)
  (if (or (null identifier) (zerop (length identifier)))
      nil
      (let ((chars (copy-seq identifier)) (valid t))
        (dotimes (i (length chars))
          ;; ( 'a' .. 'z' | 'A' .. 'Z' | '_' ) ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' )*
          (let ((c (char chars i)))
            (unless (or (char<= #\A c #\Z) (char<= #\a c #\z) (char= c #\_)
                        (and (> i 0) (char<= #\0 c #\9)))
              (setf (char chars i) #\_ valid nil))))
        (if valid nil chars))))

(defjmethod generate-identifiers ((this graph-identifier-generator) element)
  (typecase element
    (elk-node
     (if (null (get-parent/0 element))
         (when (%blank-id-p (get-identifier/0 element))
           (set-identifier/1 element "G1"))
         (set-identifier-if-missing/2 this element :node)))
    (elk-port (set-identifier-if-missing/2 this element :port))
    (elk-label (set-identifier-if-missing/2 this element :label))
    (elk-edge (set-identifier-if-missing/2 this element :edge))
    (elk-edge-section (set-identifier-if-missing/2 this element :edge-section)))
  (dolist (c (%gig-children element)) (generate-identifiers/1 this c))
  this)

(defjmethod set-identifier-if-missing ((this graph-identifier-generator) element element-type)
  (when (%blank-id-p (get-identifier/0 element))
    (set-identifier/1 element (next-identifier/1 this element-type)))
  nil)

(defjmethod set-identifier-if-missing ((this graph-identifier-generator) section)
  (set-identifier-if-missing/2 this section :edge-section))

(defjmethod next-identifier ((this graph-identifier-generator) element-type)
  (destructuring-bind (ord prefix) (cdr (assoc element-type *gig-element-types*))
    (loop
      (let ((identifier (jstr prefix (incf (aref (.current-i-ds this) ord)))))
        (unless (js-contains (.existing-identifiers this) identifier)
          (return identifier))))))

(defjmethod assert-all-ids-unique ((this graph-identifier-generator) element)
  (let ((known (new-jset 'equal)))
    (jdo (e (e-all-contents/0 element))
      (when (typep e 'elk-graph-element)
        (loop while (js-contains known (get-identifier/0 e))
              do (set-identifier/1 e (jstr (get-identifier/0 e) "_g"
                                           (four-digit-padded-random-number/0 this))))
        (js-add known (get-identifier/0 e)))))
  this)

(defjmethod four-digit-padded-random-number ((this graph-identifier-generator))
  (unless (.random this) (setf (.random this) (new-random)))
  (pad-zeroes/2 this (format nil "~D" (next-int/1 (.random this) 10000)) 4))

(defjmethod pad-zeroes ((this graph-identifier-generator) s length)
  (if (= (length s) length)
      s
      (jstr (make-string (- length (length s)) :initial-element #\0) s)))
