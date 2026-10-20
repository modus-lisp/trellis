;;;; json-graph.lisp -- org.eclipse.elk.graph.json: JsonImportException,
;;;; JsonAdapter (as plain helpers over json.lisp), JsonImporter, JsonExporter,
;;;; JsonMetaDataConverter, ElkGraphJson with its Import/ExportBuilder.

(in-package #:elk)

;;; ------------------------------------------------------------ JsonImportException

(define-condition json-import-exception (runtime-exception) ())

(defun %gio-format-error (fmt &rest args)
  (error 'json-import-exception :message (apply #'format nil fmt args)))

(defun %gio-implementation-error ()
  (error "Severe implementation error in the Json to ElkGraph importer."))

;;; ------------------------------------------------------------ JsonAdapter

(defun %gio-is-int (d) (and (realp d) (not (nan-p d)) (not (infinite-p d)) (= d (ffloor d))))

(defun %gio-as-id (id)
  (cond ((stringp id) id)
        ((and (realp id) (%gio-is-int id)) (to-int id))
        (t (%gio-format-error "Id must be a string or an integer: '~A'."
                              (if (realp id) (json-number-string id) (json-write id))))))

(defun %gio-get-id (o)
  (unless (json-has o "id") (%gio-format-error "Every element must have an id."))
  (%gio-as-id (json-get o "id")))

(defun %gio-string-val (e)
  "JsonElement.getAsString: primitives become strings, anything else fails."
  (cond ((stringp e) e)
        ((realp e) (json-number-string e))
        ((eq e :true) "true")
        ((eq e :false) "false")
        ((eq e :null) (jthrow 'unsupported-operation-exception "JsonNull"))
        (t (jthrow 'illegal-state-exception "Not a JSON primitive: ~A" (json-write e)))))

(defun %gio-opt-string (o key)
  (let ((e (json-get o key))) (and e (%gio-string-val e))))

(defun %gio-id-save (o) (%gio-opt-string o "id"))

(defun %gio-as-double (e)
  (cond ((realp e) (float e 1d0))
        ((stringp e) (double.parse-double e))
        (t (jthrow 'illegal-state-exception "Not a number: ~A" (json-write e)))))

(defun %gio-opt-double (o key)
  (if (json-has o key) (%gio-as-double (json-get o key)) nil))

(defun %gio-opt-array (o key)
  (let ((e (json-get o key)))
    (cond ((null e) nil)
          ((json-array-p e) e)
          (t (jthrow 'illegal-state-exception "Not a JSON Array: ~A" (json-write e))))))

(defun %gio-opt-object (o key)
  (let ((e (json-get o key)))
    (cond ((null e) nil)
          ((json-object-p e) e)
          (t (jthrow 'illegal-state-exception "Not a JSON Object: ~A" (json-write e))))))

(defun %gio-array-object (arr i)
  (let ((e (aref arr i)))
    (cond ((json-object-p e) e)
          (t (jthrow 'illegal-state-exception "Not a JSON Object: ~A" (json-write e))))))

(defun %gio-to-json (x)
  "addJsonObj/addJsonArr of an Object: strings, numbers, booleans."
  (cond ((stringp x) x)
        ((realp x) x)
        ((eq x t) :true)
        ((null x) :false)
        (t (%gio-implementation-error))))

;;; ------------------------------------------------------------ JsonImporter

(defjclass json-importer ()
  ;; Id -> element maps: ids are strings or integers -> EQUAL tables
  ((node-id-map (make-hash-table :test 'equal))
   (port-id-map (make-hash-table :test 'equal))
   (edge-id-map (make-hash-table :test 'equal))
   (edge-section-id-map (make-hash-table :test 'equal))
   ;; element -> json maps (and the inverse BiMap directions)
   (node-json-map (make-hash-table :test 'eq))
   (json-node-map (make-hash-table :test 'eq))
   (node-id-inverse (make-hash-table :test 'eq))
   (port-id-inverse (make-hash-table :test 'eq))
   (edge-section-id-inverse (make-hash-table :test 'eq))
   (port-json-map (make-hash-table :test 'eq))
   (edge-json-map (make-hash-table :test 'eq))
   (edge-section-json-map (make-hash-table :test 'eq))
   (label-json-map (make-hash-table :test 'eq))
   ;; maps to help in adjusting coordinates
   (edge-original-parent-map (make-hash-table :test 'eq))
   (global-x-map (make-hash-table :test 'eq))
   (global-y-map (make-hash-table :test 'eq))
   (shape-coords-map (make-hash-table :test 'eq))
   (edge-coords-map (make-hash-table :test 'eq))
   (input-model nil)))

(define-jcalls :instance transform get-input-model transfer-layout)

(defjmethod transform ((this json-importer) graph)
  "Main entry point: JSON object -> root ElkNode."
  (setf (.input-model this) graph)
  (%gio-clear-maps this)
  (let ((root (%gio-transform-node this graph nil)))
    (%gio-transform-edges this graph)
    root))

(defjmethod get-input-model ((this json-importer)) (.input-model this))

(defun %gio-clear-maps (this)
  (dolist (acc (list #'.node-id-map #'.port-id-map #'.edge-id-map #'.edge-section-id-map
                     #'.node-json-map #'.json-node-map #'.node-id-inverse #'.port-id-inverse
                     #'.edge-section-id-inverse #'.port-json-map #'.edge-json-map
                     #'.edge-section-json-map #'.label-json-map #'.edge-original-parent-map
                     #'.global-x-map #'.global-y-map #'.shape-coords-map #'.edge-coords-map))
    (clrhash (funcall acc this))))

(defun %gio-transform-child-nodes (this json-node parent)
  (let ((children (%gio-opt-array json-node "children")))
    (when children
      (dotimes (i (length children))
        (let ((c (%gio-array-object children i)))
          (when c (%gio-transform-node this c parent)))))))

(defun %gio-transform-node (this json-node parent)
  (let ((node (elk-graph-util.create-node parent)))
    (%gio-register-node this node json-node)
    (set-identifier node (%gio-id-save json-node))
    (%gio-transform-properties this json-node node)
    (%gio-transform-individual-spacings this json-node node)
    (%gio-transform-shape-layout this json-node node)
    (%gio-transform-ports this json-node node)
    (%gio-transform-labels this json-node node)
    (%gio-transform-child-nodes this json-node node)
    node))

(defun %gio-transform-edges (this json-obj)
  (let ((node (gethash json-obj (.json-node-map this))))
    (unless node
      (%gio-format-error "Unable to find elk node for json object '~A' Panic!" (%gio-id-save json-obj)))
    (let ((edges (%gio-opt-array json-obj "edges")))
      (when edges
        (dotimes (i (length edges))
          (let ((edge (%gio-array-object edges i)))
            (if (or (json-has edge "sources") (json-has edge "targets"))
                (%gio-transform-edge this edge node)
                (%gio-transform-primitive-edge this edge node))
            (elk-graph-util.update-containment (gethash (%gio-get-id edge) (.edge-id-map this)))))))
    (let ((children (%gio-opt-array json-obj "children")))
      (when children
        (dotimes (i (length children))
          (let ((c (%gio-array-object children i)))
            (when c (%gio-transform-edges this c))))))))

(defun %gio-opt-id (json-obj key)
  (let ((e (json-get json-obj key))) (and e (%gio-as-id e))))

(defun %gio-transform-primitive-edge (this json-obj parent)
  (let ((edge (elk-graph-util.create-edge parent)))
    (%gio-register-edge this edge json-obj)
    (set-identifier edge (%gio-id-save json-obj))
    ;; source
    (let* ((src-node (gethash (%gio-as-id (json-get json-obj "source")) (.node-id-map this)))
           (src-id (%gio-opt-id json-obj "sourcePort"))
           (src-port (and src-id (gethash src-id (.port-id-map this)))))
      (unless src-node
        (%gio-format-error "An edge must have a source node (edge id: '~A')." (%gio-get-id json-obj)))
      (when (and src-port (not (eq (get-parent src-port) src-node)))
        (%gio-format-error "The source port of an edge must be a port of the edge's source node (edge id: '~A')."
                           (%gio-id-save json-obj)))
      (add (get-sources edge) (or src-port src-node)))
    ;; target
    (let* ((tgt-node (gethash (%gio-as-id (json-get json-obj "target")) (.node-id-map this)))
           (tgt-id (%gio-opt-id json-obj "targetPort"))
           (tgt-port (and tgt-id (gethash tgt-id (.port-id-map this)))))
      (unless tgt-node
        (%gio-format-error "An edge must have a target node (edge id: '~A')." (%gio-get-id json-obj)))
      (when (and tgt-port (not (eq (get-parent tgt-port) tgt-node)))
        (%gio-format-error "The target port of an edge must be a port of the edge's target node (edge id: '~A')."
                           (%gio-id-save json-obj)))
      (add (get-targets edge) (or tgt-port tgt-node)))
    (when (or (is-empty (get-sources edge)) (is-empty (get-targets edge)))
      (%gio-format-error "An edge must have at least one source and one target (edge id: '~A')."
                         (%gio-id-save json-obj)))
    (%gio-transform-properties this json-obj edge)
    (%gio-transform-primitive-edge-layout this json-obj edge)
    (%gio-transform-labels this json-obj edge)))

(defun %gio-transform-primitive-edge-layout (this json-obj edge)
  (declare (ignore this))
  (when (or (json-has json-obj "sourcePoint") (json-has json-obj "targetPoint")
            (json-has json-obj "bendPoints"))
    (let ((section (elk-graph-util.create-edge-section edge)))
      (let ((p (%gio-opt-object json-obj "sourcePoint")))
        (when p
          (let ((x (%gio-opt-double p "x")) (y (%gio-opt-double p "y")))
            (when x (set-start-x section x))
            (when y (set-start-y section y)))))
      (let ((p (%gio-opt-object json-obj "targetPoint")))
        (when p
          (let ((x (%gio-opt-double p "x")) (y (%gio-opt-double p "y")))
            (when x (set-end-x section x))
            (when y (set-end-y section y)))))
      (%gio-create-bend-points json-obj section))))

(defun %gio-create-bend-points (json-obj section)
  (let ((bend-points (%gio-opt-array json-obj "bendPoints")))
    (when bend-points
      (dotimes (i (length bend-points))
        (let ((bp (%gio-array-object bend-points i)))
          (elk-graph-util.create-bend-point section (or (%gio-opt-double bp "x") 0d0)
                                            (or (%gio-opt-double bp "y") 0d0)))))))

(defun %gio-transform-edge (this json-obj parent)
  (let ((edge (elk-graph-util.create-edge parent)))
    (%gio-register-edge this edge json-obj)
    (set-identifier edge (%gio-id-save json-obj))
    (let ((sources (%gio-opt-array json-obj "sources")))
      (when sources
        (dotimes (i (length sources))
          (let ((s (%gio-shape-by-id this (%gio-as-id (aref sources i)))))
            (when s (add (get-sources edge) s))))))
    (let ((targets (%gio-opt-array json-obj "targets")))
      (when targets
        (dotimes (i (length targets))
          (let ((s (%gio-shape-by-id this (%gio-as-id (aref targets i)))))
            (when s (add (get-targets edge) s))))))
    (when (or (is-empty (get-sources edge)) (is-empty (get-targets edge)))
      (%gio-format-error "An edge must have at least one source and one target (edge id: '~A')."
                         (%gio-id-save json-obj)))
    (%gio-transform-properties this json-obj edge)
    (%gio-transform-edge-sections this json-obj edge)
    (%gio-transform-labels this json-obj edge)))

(defun %gio-transform-edge-sections (this json-obj edge)
  ;; Section references are resolved after all sections exist.  Java uses a
  ;; HashMultimap (keySet order unspecified); we keep creation order.
  (let ((incoming '()) (outgoing '()))
    (let ((sections (%gio-opt-array json-obj "sections")))
      (when sections
        (dotimes (i (length sections))
          (let* ((json-section (%gio-array-object sections i))
                 (elk-section (elk-graph-util.create-edge-section edge)))
            (%gio-register-section this elk-section json-section)
            (set-identifier elk-section (%gio-id-save json-section))
            (%gio-fill-edge-section-coordinates json-section elk-section)
            (let ((id (%gio-opt-string json-section "incomingShape")))
              (when id (set-incoming-shape elk-section (%gio-shape-by-id this id))))
            (let ((id (%gio-opt-string json-section "outgoingShape")))
              (when id (set-outgoing-shape elk-section (%gio-shape-by-id this id))))
            (let ((ids (%gio-opt-array json-section "incomingSections")))
              (when ids
                (let ((l '()))
                  (dotimes (j (length ids)) (pushnew (%gio-as-id (aref ids j)) l :test #'equal))
                  (push (cons elk-section (nreverse l)) incoming))))
            (let ((ids (%gio-opt-array json-section "outgoingSections")))
              (when ids
                (let ((l '()))
                  (dotimes (j (length ids)) (pushnew (%gio-as-id (aref ids j)) l :test #'equal))
                  (push (cons elk-section (nreverse l)) outgoing))))))))
    (flet ((resolve (entries getter)
             (loop for (section . ids) in (reverse entries)
                   do (dolist (id ids)
                        (let ((ref (gethash id (.edge-section-id-map this))))
                          (if ref
                              (add (funcall getter section) ref)
                              (%gio-format-error "Referenced edge section does not exist: ~A (edge id: '~A')."
                                                 id (%gio-id-save json-obj))))))))
      (resolve incoming (lambda (s) (get-incoming-sections s)))
      (resolve outgoing (lambda (s) (get-outgoing-sections s)))))
  ;; Special case: one source, one target, one section without incoming and
  ;; outgoing shapes -> the section connects the edge's source and target.
  (when (and (is-connected edge) (not (is-hyperedge edge)) (= (size (get-sections edge)) 1))
    (let ((section (j-get (get-sections edge) 0)))
      (when (and (null (get-incoming-shape section)) (null (get-outgoing-shape section)))
        (set-incoming-shape section (j-get (get-sources edge) 0))
        (set-outgoing-shape section (j-get (get-targets edge) 0))))))

(defun %gio-fill-edge-section-coordinates (json-obj section)
  (let ((p (%gio-opt-object json-obj "startPoint")))
    (if p
        (let ((x (%gio-opt-double p "x")) (y (%gio-opt-double p "y")))
          (when x (set-start-x section x))
          (when y (set-start-y section y)))
        (%gio-format-error "All edge sections need a start point.")))
  (let ((p (%gio-opt-object json-obj "endPoint")))
    (if p
        (let ((x (%gio-opt-double p "x")) (y (%gio-opt-double p "y")))
          (when x (set-end-x section x))
          (when y (set-end-y section y)))
        (%gio-format-error "All edge sections need an end point.")))
  (%gio-create-bend-points json-obj section))

(defun %gio-transform-properties (this json-obj layout-data)
  (declare (ignore this))
  (let ((layout-options (or (%gio-opt-object json-obj "layoutOptions")
                            ;; legacy support
                            (%gio-opt-object json-obj "properties"))))
    (when layout-options
      (dolist (k (json-keys layout-options))
        (let ((e (json-get layout-options k)))
          (%gio-set-option layout-data k (and e (not (eq e :null)) (%gio-string-val e))))))))

(defun %gio-transform-individual-spacings (this json-obj layout-data)
  (declare (ignore this))
  (let ((json-spacings (%gio-opt-object json-obj "individualSpacings")))
    (when json-spacings
      (unless (has-property layout-data core-options.spacing-individual)
        (set-property layout-data core-options.spacing-individual (new-individual-spacings)))
      (let ((individual-spacings (get-property layout-data core-options.spacing-individual)))
        (dolist (k (json-keys json-spacings))
          (let ((e (json-get json-spacings k)))
            (%gio-set-option individual-spacings k (and e (not (eq e :null)) (%gio-string-val e)))))))))

(defun %gio-set-option (holder id value)
  (let ((option-data (get-option-data-by-suffix (layout-meta-data-service.get-instance) id)))
    (when (and option-data value)
      (let ((parsed (parse-value option-data value)))
        (when parsed (set-property holder option-data parsed))))))

(defun %gio-transform-labels (this json-obj element)
  (let ((labels (%gio-opt-array json-obj "labels")))
    (when labels
      (dotimes (i (length labels))
        (let ((json-label (%gio-array-object labels i)))
          (when json-label
            (let ((label (elk-graph-util.create-label (%gio-opt-string json-label "text") element)))
              (setf (gethash label (.label-json-map this)) json-label)
              (when (json-has json-label "id")
                (set-identifier label (%gio-id-save json-label)))
              (%gio-transform-properties this json-label label)
              (%gio-transform-shape-layout this json-label label))))))))

(defun %gio-transform-ports (this json-obj parent)
  (let ((ports (%gio-opt-array json-obj "ports")))
    (when ports
      (dotimes (i (length ports))
        (let ((p (%gio-array-object ports i)))
          (when p (%gio-transform-port this p parent)))))))

(defun %gio-transform-port (this json-port parent)
  (let ((port (elk-graph-util.create-port parent)))
    (%gio-register-port this port json-port)
    (set-identifier port (%gio-id-save json-port))
    (%gio-transform-properties this json-port port)
    (%gio-transform-shape-layout this json-port port)
    (%gio-transform-labels this json-port port)))

(defun %gio-double-value-valid (d)
  (if (or (null d) (infinite-p d) (nan-p d)) 0d0 d))

(defun %gio-transform-shape-layout (this json-obj shape)
  (declare (ignore this))
  (set-x shape (%gio-double-value-valid (%gio-opt-double json-obj "x")))
  (set-y shape (%gio-double-value-valid (%gio-opt-double json-obj "y")))
  (set-width shape (%gio-double-value-valid (%gio-opt-double json-obj "width")))
  (set-height shape (%gio-double-value-valid (%gio-opt-double json-obj "height"))))

(defun %gio-shape-by-id (this id)
  (or (gethash id (.node-id-map this))
      (gethash id (.port-id-map this))
      (%gio-format-error "Referenced shape does not exist: ~A" id)))

;;; ElkGraph positions -> Json

(defjmethod transfer-layout ((this json-importer) graph)
  "Transfer the computed layout back into the JSON that TRANSFORM read."
  ;; first pass: nodes and ports; second pass: edges and labels
  (jdo (element (elk-graph-util.properties-skipping-iterator-for graph t))
    (%gio-transfer-layout-int1 this element))
  (jdo (element (elk-graph-util.properties-skipping-iterator-for graph t))
    (%gio-transfer-layout-int2 this element)))

(defun %gio-transfer-layout-int1 (this element)
  (typecase element
    (elk-node
     (let ((json-obj (gethash element (.node-json-map this))))
       (unless json-obj (%gio-format-error "Node did not exist in input."))
       (%gio-record-global-coords this element)
       (%gio-record-coordinate-modes this element)
       (%gio-transfer-shape-layout this element json-obj)))
    (elk-port
     (let ((json-obj (gethash element (.port-json-map this))))
       (unless json-obj (%gio-format-error "Port did not exist in input."))
       (%gio-record-global-coords this element)
       (%gio-record-coordinate-modes this element)
       (%gio-transfer-shape-layout this element json-obj)))))

(defun %gio-transfer-layout-int2 (this element)
  (typecase element
    (elk-edge (%gio-transfer-edge-layout this element))
    (elk-label
     (let ((json-obj (gethash element (.label-json-map this))))
       (%gio-record-global-coords this element)
       (%gio-record-coordinate-modes this element)
       (%gio-transfer-shape-layout this element json-obj)))))

(defun %gio-transfer-edge-layout (this edge)
  (let ((json-obj (gethash edge (.edge-json-map this))))
    (unless json-obj (%gio-format-error "Edge did not exist in input."))
    (%gio-record-coordinate-modes this edge)
    (let ((edge-id (%gio-get-id json-obj)))
      (unless (is-empty (get-sections edge))
        (let ((sections (make-json-array)) (i 0))
          (jdo (elk-section (get-sections edge))
            (let ((json-section (gethash elk-section (.edge-section-json-map this))))
              (unless json-section
                (setf json-section (make-json-object "id" (jstr edge-id "_s" i))))
              (json-array-add sections json-section)
              (json-put json-section "startPoint"
                        (make-json-object "x" (%gio-adjust-x this edge (get-start-x elk-section))
                                          "y" (%gio-adjust-y this edge (get-start-y elk-section))))
              (json-put json-section "endPoint"
                        (make-json-object "x" (%gio-adjust-x this edge (get-end-x elk-section))
                                          "y" (%gio-adjust-y this edge (get-end-y elk-section))))
              (unless (is-empty (get-bend-points elk-section))
                (let ((bend-points (make-json-array)))
                  (jdo (pnt (get-bend-points elk-section))
                    (json-array-add bend-points
                                    (make-json-object "x" (%gio-adjust-x this edge (get-x pnt))
                                                      "y" (%gio-adjust-y this edge (get-y pnt)))))
                  (json-put json-section "bendPoints" bend-points)))
              (when (get-incoming-shape elk-section)
                (json-put json-section "incomingShape"
                          (%gio-to-json (%gio-id-by-element this (get-incoming-shape elk-section)))))
              (when (get-outgoing-shape elk-section)
                (json-put json-section "outgoingShape"
                          (%gio-to-json (%gio-id-by-element this (get-outgoing-shape elk-section)))))
              (unless (is-empty (get-incoming-sections elk-section))
                (let ((a (make-json-array)))
                  (jdo (sec (get-incoming-sections elk-section))
                    (json-array-add a (%gio-to-json (%gio-id-by-element this sec))))
                  (json-put json-section "incomingSections" a)))
              (unless (is-empty (get-outgoing-sections elk-section))
                (let ((a (make-json-array)))
                  (jdo (sec (get-outgoing-sections elk-section))
                    (json-array-add a (%gio-to-json (%gio-id-by-element this sec))))
                  (json-put json-section "outgoingSections" a))))
            (incf i))
          (json-remove json-obj "sections")
          (json-put json-obj "sections" sections))))
    (when (has-property edge core-options.junction-points)
      (let ((jps (get-property edge core-options.junction-points)))
        (when (and jps (not (is-empty jps)))
          (let ((json-jps (make-json-array)))
            (jdo (jp jps)
              (json-array-add json-jps (make-json-object "x" (%gio-adjust-x this edge (.x jp))
                                                         "y" (%gio-adjust-y this edge (.y jp)))))
            (json-put json-obj "junctionPoints" json-jps)))))
    (when (eq (%gio-edge-coords-mode this (%gio-original-parent this edge)) edge-coords.container)
      (json-put json-obj "container" (%gio-to-json (get-identifier (get-containing-node edge)))))))

(defun %gio-transfer-shape-layout (this shape json-obj)
  (let ((parent (%gio-json-parent this shape)))
    (json-put json-obj "x" (or (and parent (%gio-adjust-x this parent (get-x shape))) (get-x shape)))
    (json-put json-obj "y" (or (and parent (%gio-adjust-y this parent (get-y shape))) (get-y shape)))
    (json-put json-obj "width" (get-width shape))
    (json-put json-obj "height" (get-height shape))))

(defun %gio-adjust-x (this element x)
  (if (typep element 'elk-edge)
      (let ((mode (%gio-edge-coords-mode this (%gio-original-parent this element))))
        (cond ((eq mode edge-coords.root) (+ x (%gio-global-x this (get-containing-node element))))
              ((eq mode edge-coords.parent)
               (- (+ x (%gio-global-x this (get-containing-node element)))
                  (%gio-global-x this (%gio-original-parent this element))))
              (t x)))
      (if (eq (%gio-shape-coords-mode this element) shape-coords.root)
          (+ x (%gio-global-x this element))
          x)))

(defun %gio-adjust-y (this element y)
  (if (typep element 'elk-edge)
      (let ((mode (%gio-edge-coords-mode this (%gio-original-parent this element))))
        (cond ((eq mode edge-coords.root) (+ y (%gio-global-y this (get-containing-node element))))
              ((eq mode edge-coords.parent)
               (- (+ y (%gio-global-y this (get-containing-node element)))
                  (%gio-global-y this (%gio-original-parent this element))))
              (t y)))
      (if (eq (%gio-shape-coords-mode this element) shape-coords.root)
          (+ y (%gio-global-y this element))
          y)))

(defun %gio-id-by-element (this element)
  (typecase element
    (elk-node (gethash element (.node-id-inverse this)))
    (elk-port (gethash element (.port-id-inverse this)))
    (elk-edge-section (gethash element (.edge-section-id-inverse this)))
    (t (error "idByElement: unhandled ~A" element))))

(defun %gio-register-node (this node obj)
  (let ((id (%gio-get-id obj)))
    (setf (gethash id (.node-id-map this)) node
          (gethash node (.node-id-inverse this)) id
          (gethash node (.node-json-map this)) obj
          (gethash obj (.json-node-map this)) node)
    node))

(defun %gio-register-port (this port obj)
  (let ((id (%gio-get-id obj)))
    (setf (gethash id (.port-id-map this)) port
          (gethash port (.port-id-inverse this)) id
          (gethash port (.port-json-map this)) obj)
    port))

(defun %gio-register-edge (this edge obj)
  (let ((id (%gio-get-id obj)))
    (setf (gethash id (.edge-id-map this)) edge
          (gethash edge (.edge-json-map this)) obj
          (gethash edge (.edge-original-parent-map this)) (get-containing-node edge))
    edge))

(defun %gio-register-section (this section obj)
  (let ((id (%gio-get-id obj)))
    (setf (gethash id (.edge-section-id-map this)) section
          (gethash section (.edge-section-id-inverse this)) id
          (gethash section (.edge-section-json-map this)) obj)
    section))

(defun %gio-record-coordinate-modes (this element)
  (let ((parent (%gio-json-parent this element)))
    (let ((scm (or (get-property element core-options.json-shape-coords) shape-coords.inherit)))
      (when (eq scm shape-coords.inherit)
        (setf scm (or (%gio-shape-coords-mode this parent) shape-coords.parent)))
      (setf (gethash element (.shape-coords-map this)) scm))
    (let ((ecm (or (get-property element core-options.json-edge-coords) edge-coords.inherit)))
      (when (eq ecm edge-coords.inherit)
        (setf ecm (or (%gio-edge-coords-mode this parent) edge-coords.container)))
      (setf (gethash element (.edge-coords-map this)) ecm))))

(defun %gio-record-global-coords (this shape)
  (let* ((parent (%gio-json-parent this shape))
         (shape-a (and parent (%gio-shape-ancestor this parent)))
         (dx (or (and shape-a (%gio-global-x this shape-a)) 0))
         (dy (or (and shape-a (%gio-global-y this shape-a)) 0)))
    (setf (gethash shape (.global-x-map this)) (+ (get-x shape) dx)
          (gethash shape (.global-y-map this)) (+ (get-y shape) dy))))

(defun %gio-shape-ancestor (this element)
  (if (typep element 'elk-edge) (%gio-original-parent this element) element))

(defun %gio-json-parent (this element)
  (if (typep element 'elk-edge)
      (%gio-original-parent this element)
      (get-parent element)))

(defun %gio-original-parent (this edge) (gethash edge (.edge-original-parent-map this)))
(defun %gio-global-x (this shape) (and shape (gethash shape (.global-x-map this))))
(defun %gio-global-y (this shape) (and shape (gethash shape (.global-y-map this))))
(defun %gio-shape-coords-mode (this element) (and element (gethash element (.shape-coords-map this))))
(defun %gio-edge-coords-mode (this element) (and element (gethash element (.edge-coords-map this))))

;;; ------------------------------------------------------------ JsonExporter

(defjclass json-exporter ()
  ((node-id-map (make-hash-table :test 'eq))      ; BiMap<ElkNode,String>
   (node-id-inverse (make-hash-table :test 'equal))
   (port-id-map (make-hash-table :test 'eq))
   (port-id-inverse (make-hash-table :test 'equal))
   (edge-id-map (make-hash-table :test 'eq))
   (edge-id-inverse (make-hash-table :test 'equal))
   (edge-section-id-map (make-hash-table :test 'eq))
   (edge-section-id-inverse (make-hash-table :test 'equal))
   (node-json-map (make-hash-table :test 'eq))
   (port-json-map (make-hash-table :test 'eq))
   (edge-json-map (make-hash-table :test 'eq))
   (edge-section-json-map (make-hash-table :test 'eq))
   (node-id-counter 0)
   (port-id-counter 0)
   (edge-id-counter 0)
   (edge-section-id-counter 0)
   (omit-zero-pos t)
   (omit-zero-dim t)
   (omit-layout nil)
   (short-layout-option-keys t)
   (omit-unknown-layout-options t)
   (random (new-random))))

(define-jcalls :instance set-options j-export)

(defjmethod set-options ((this json-exporter) omit-zero-pos omit-zero-dim omit-layout
                         short-layout-option-keys omit-unknown-layout-options)
  (setf (.omit-zero-pos this) omit-zero-pos
        (.omit-zero-dim this) omit-zero-dim
        (.omit-layout this) omit-layout
        (.short-layout-option-keys this) short-layout-option-keys
        (.omit-unknown-layout-options this) omit-unknown-layout-options))

(defjmethod j-export ((this json-exporter) root)
  (%gio-ex-init this)
  (let ((json-array (make-json-array)))
    (%gio-ex-transform-node this root json-array)
    (%gio-ex-transform-edges this root)
    (aref json-array 0)))

(defun %gio-ex-init (this)
  (dolist (acc (list #'.node-id-map #'.node-id-inverse #'.port-id-map #'.port-id-inverse
                     #'.edge-id-map #'.edge-id-inverse #'.edge-section-id-map #'.edge-section-id-inverse
                     #'.node-json-map #'.port-json-map #'.edge-json-map #'.edge-section-json-map))
    (clrhash (funcall acc this)))
  (setf (.node-id-counter this) 0 (.port-id-counter this) 0
        (.edge-id-counter this) 0 (.edge-section-id-counter this) 0))

(defun %gio-ex-transform-labels (this element json-obj)
  (unless (is-empty (get-labels element))
    (let ((labels (make-json-array)))
      (json-put json-obj "labels" labels)
      (jdo (l (get-labels element)) (%gio-ex-transform-label this l labels)))))

(defun %gio-ex-transform-node (this node array)
  (let ((json-obj (%gio-ex-create-and-register this node)))
    (json-array-add array json-obj)
    (%gio-ex-transform-labels this node json-obj)
    (unless (is-empty (get-ports node))
      (let ((ports (make-json-array)))
        (json-put json-obj "ports" ports)
        (jdo (p (get-ports node)) (%gio-ex-transform-port this p ports))))
    (unless (is-empty (get-children node))
      (let ((children (make-json-array)))
        (json-put json-obj "children" children)
        (jdo (c (get-children node)) (%gio-ex-transform-node this c children))))
    (%gio-ex-transform-properties this node json-obj)
    (%gio-ex-transform-individual-spacings this node json-obj)
    (%gio-ex-transfer-shape-layout this node json-obj)))

(defun %gio-ex-transform-port (this port array)
  (let ((json-obj (%gio-ex-create-and-register this port)))
    (json-array-add array json-obj)
    (%gio-ex-transform-labels this port json-obj)
    (%gio-ex-transform-properties this port json-obj)
    (%gio-ex-transfer-shape-layout this port json-obj)))

(defun %gio-ex-transform-edges (this node)
  (unless (is-empty (get-contained-edges node))
    (let ((json-obj (gethash node (.node-json-map this)))
          (edges (make-json-array)))
      (json-put json-obj "edges" edges)
      (jdo (e (get-contained-edges node)) (%gio-ex-transform-edge this e edges))))
  (jdo (c (get-children node)) (%gio-ex-transform-edges this c)))

(defun %gio-ex-transform-edge (this edge array)
  (let ((json-obj (%gio-ex-create-and-register this edge)))
    (json-array-add array json-obj)
    (flet ((ends (shapes)
             (let ((a (make-json-array)))
               (jdo (s shapes)
                 (let ((id (or (gethash s (.port-id-map this)) (gethash s (.node-id-map this)))))
                   ;; Java builds the error but does not throw it
                   (json-array-add a id)))
               a)))
      (json-put json-obj "sources" (ends (get-sources edge)))
      (json-put json-obj "targets" (ends (get-targets edge))))
    (%gio-ex-transform-labels this edge json-obj)
    (when (and (not (.omit-layout this)) (not (is-empty (get-sections edge))))
      (let ((sections (make-json-array)))
        (json-put json-obj "sections" sections)
        (jdo (s (get-sections edge)) (%gio-ex-transform-section this s sections))))
    ;; do not initialize an empty set of junction points by accident (#559)
    (when (and (not (.omit-layout this)) (has-property edge core-options.junction-points))
      (let ((jps (get-property edge core-options.junction-points)))
        (when (and jps (not (is-empty jps)))
          (let ((json-jps (make-json-array)))
            (jdo (jp jps)
              (json-array-add json-jps (make-json-object "x" (.x jp) "y" (.y jp))))
            (json-put json-obj "junctionPoints" json-jps)))))
    (%gio-ex-transform-properties this edge json-obj)))

(defun %gio-ex-transform-section (this section sections)
  (let ((json-obj (%gio-ex-create-and-register this section)))
    (json-array-add sections json-obj)
    (json-put json-obj "startPoint" (make-json-object "x" (get-start-x section) "y" (get-start-y section)))
    (json-put json-obj "endPoint" (make-json-object "x" (get-end-x section) "y" (get-end-y section)))
    (when (and (not (.omit-layout this)) (not (is-empty (get-bend-points section))))
      (let ((bend-points (make-json-array)))
        (jdo (pnt (get-bend-points section))
          (json-array-add bend-points (make-json-object "x" (get-x pnt) "y" (get-y pnt))))
        (json-put json-obj "bendPoints" bend-points)))
    (when (get-incoming-shape section)
      (json-put json-obj "incomingShape" (%gio-ex-id-by-element this (get-incoming-shape section))))
    (when (get-outgoing-shape section)
      (json-put json-obj "outgoingShape" (%gio-ex-id-by-element this (get-outgoing-shape section))))
    (unless (is-empty (get-incoming-sections section))
      (let ((a (make-json-array)))
        (jdo (sec (get-incoming-sections section)) (json-array-add a (%gio-ex-id-by-element this sec)))
        (json-put json-obj "incomingSections" a)))
    (unless (is-empty (get-outgoing-sections section))
      (let ((a (make-json-array)))
        (jdo (sec (get-outgoing-sections section)) (json-array-add a (%gio-ex-id-by-element this sec)))
        (json-put json-obj "outgoingSections" a)))
    (%gio-ex-transform-properties this section json-obj)))

(defun %gio-ex-transform-label (this label array)
  (let ((json-label (make-json-object "text" (or (get-text label) :null))))
    (let ((id (get-identifier label)))
      (when (and id (plusp (length id))) (json-put json-label "id" id)))
    (json-array-add array json-label)
    (%gio-ex-transform-properties this label json-label)
    (%gio-ex-transfer-shape-layout this label json-label)))

(defun %gio-value-string (v)
  "Object.toString of a property value."
  (cond ((stringp v) v)
        ((integerp v) (format nil "~D" v))
        ((floatp v) (json-double-string v))
        ((eq v t) "true")
        ((null v) "false")
        ((typep v 'jenum) (enum-name v))
        ((enum-set-p v)
         (format nil "[~{~A~^, ~}]" (mapcar #'enum-name (enum-set-items v))))
        ((json-array-p v) (format nil "[~{~A~^, ~}]" (map 'list #'%gio-value-string v)))
        (t (to-string v))))

(defun %gio-ex-holder-cells (holder)
  (let ((acc '()))
    (when holder
      (maphash (lambda (k v) (push (cons k v) acc)) (get-all-properties holder)))
    ;; Java iterates a HashMap; we use a stable order (by id)
    (sort acc #'string< :key (lambda (c) (.id (car c))))))

(defun %gio-ex-transform-properties (this holder parent)
  (let ((cells (%gio-ex-holder-cells holder)))
    (when cells
      (let ((json-props (make-json-object)))
        (json-put parent "layoutOptions" json-props)
        (loop for (key . value) in cells
              when (and key (not (string= (.id key) (.id core-options.spacing-individual))))
                do (when (or (not (.omit-unknown-layout-options this)) (%gio-ex-is-known key))
                     (json-put json-props
                               (if (.short-layout-option-keys this)
                                   (%gio-short-option-key (.id key))
                                   (.id key))
                               (%gio-value-string value))))))))

(defun %gio-ex-transform-individual-spacings (this holder parent)
  (when (and holder (has-property holder core-options.spacing-individual))
    (let* ((individual-spacings (get-property holder core-options.spacing-individual))
           (cells (%gio-ex-holder-cells individual-spacings)))
      (when cells
        (let ((json-props (make-json-object)))
          (json-put parent "individualSpacings" json-props)
          (loop for (key . value) in cells
                when key
                  do (when (or (not (.omit-unknown-layout-options this)) (%gio-ex-is-known key))
                       (json-put json-props
                                 (if (.short-layout-option-keys this)
                                     (%gio-short-option-key (.id key))
                                     (.id key))
                                 (%gio-value-string value)))))))))

(defun %gio-ex-transfer-shape-layout (this shape json-obj)
  (unless (.omit-layout this)
    (if (not (.omit-zero-pos this))
        (progn (json-put json-obj "x" (get-x shape))
               (json-put json-obj "y" (get-y shape)))
        (progn (when (/= (get-x shape) 0d0) (json-put json-obj "x" (get-x shape)))
               (when (/= (get-y shape) 0d0) (json-put json-obj "y" (get-y shape))))))
  (if (not (.omit-zero-dim this))
      (progn (json-put json-obj "width" (get-width shape))
             (json-put json-obj "height" (get-height shape)))
      (progn (when (/= (get-width shape) 0d0) (json-put json-obj "width" (get-width shape)))
             (when (/= (get-height shape) 0d0) (json-put json-obj "height" (get-height shape))))))

(defun %gio-ex-create-and-register (this element)
  (multiple-value-bind (prefix counter map inverse json-map)
      (etypecase element
        (elk-node (values "n" '.node-id-counter (.node-id-map this) (.node-id-inverse this) (.node-json-map this)))
        (elk-port (values "p" '.port-id-counter (.port-id-map this) (.port-id-inverse this) (.port-json-map this)))
        (elk-edge (values "e" '.edge-id-counter (.edge-id-map this) (.edge-id-inverse this) (.edge-json-map this)))
        (elk-edge-section (values "s" '.edge-section-id-counter (.edge-section-id-map this)
                                  (.edge-section-id-inverse this) (.edge-section-json-map this))))
    (let ((obj (make-json-object))
          (id (get-identifier element)))
      (unless id
        (let ((n (funcall (fdefinition counter) this)))
          (setf id (format nil "~A~D" prefix n))
          (funcall (fdefinition (list 'setf counter)) (1+ n) this)))
      (setf id (%gio-assert-unique this id inverse))
      (json-put obj "id" id)
      (setf (gethash element map) id
            (gethash id inverse) element
            (gethash element json-map) obj)
      obj)))

(defun %gio-ex-id-by-element (this element)
  (typecase element
    (elk-node (gethash element (.node-id-map this)))
    (elk-port (gethash element (.port-id-map this)))
    (elk-edge-section (gethash element (.edge-section-id-map this)))
    (t (error "idByElement: unhandled ~A" element))))

(defun %gio-short-option-key (full-id)
  (let* ((svc (layout-meta-data-service.get-instance))
         (option (get-option-data-by-suffix svc full-id)))
    (if (null option)
        full-id                         ; unknown: the full id
        (let* ((id-split (%gio-split (.id option) #\.))
               (found-match nil)
               (i (1- (length id-split))))
          (when (and (>= i 1) (equal (get-group option) (nth (1- i) id-split)))
            (decf i))
          (loop while (and (>= i 0) (not found-match))
                do (if (get-option-data-by-suffix svc (%gio-join (nthcdr i id-split) "."))
                       (setf found-match t)
                       (decf i)))
          (if found-match
              (%gio-join (nthcdr i id-split) ".")
              (.id option))))))

(defun %gio-ex-is-known (property)
  (not (null (get-option-data-by-suffix (layout-meta-data-service.get-instance) (.id property)))))

(defun %gio-assert-unique (this id map)
  (let ((tmp id))
    (loop while (nth-value 1 (gethash tmp map))
          do (setf tmp (jstr id "_g" (%gio-pad-zeroes (format nil "~D" (next-int (.random this) 1000000)) 6))))
    tmp))

(defun %gio-pad-zeroes (s length)
  (if (< (length s) length)
      (concatenate 'string (make-string (- length (length s)) :initial-element #\0) s)
      s))

(defun %gio-split (s ch)
  (loop with start = 0
        for p = (position ch s :start start)
        collect (subseq s start p)
        while p do (setf start (1+ p))))

(defun %gio-join (parts sep)
  (with-output-to-string (o)
    (loop for (p . more) on parts do (write-string p o) (when more (write-string sep o)))))

;;; ------------------------------------------------------------ JsonMetaDataConverter

(defun %gio-md-create-common (data)
  (let ((o (make-json-object)))
    (when (.id data) (json-put o "id" (.id data)))
    (when (.name data) (json-put o "name" (.name data)))
    (when (.description data) (json-put o "description" (.description data)))
    o))

(defjstatic json-meta-data-converter.to-json (data)
  "LayoutAlgorithmData / LayoutCategoryData / LayoutOptionData -> JSON object."
  (let ((o (%gio-md-create-common data)))
    (cond
      ((typep data 'layout-algorithm-data)
       (when (.category-id data) (json-put o "category" (.category-id data)))
       (let ((known (loop for k being the hash-keys of (.option-defaults data) collect k)))
         (when known
           (json-put o "knownOptions" (apply #'make-json-array known))))
       (let ((features (.supported-features data)))
         (when features
           (json-put o "supportedFeatures"
                     (apply #'make-json-array
                            (mapcar (lambda (f) (if (typep f 'jenum) (enum-name f)
                                                    (substitute #\_ #\- (string-upcase (string f)))))
                                    (to-cl-list features)))))))
      ((typep data 'layout-option-data)
       (when (.group data) (json-put o "group" (.group data)))
       (when (.type data) (json-put o "type" (if (typep (.type data) 'jenum) (enum-name (.type data))
                                                (princ-to-string (.type data)))))
       (when (.targets data)
         (json-put o "targets" (apply #'make-json-array
                                      (mapcar (lambda (x) (if (typep x 'jenum) (enum-name x)
                                                              (string-upcase (string x))))
                                              (to-cl-list (.targets data)))))))
      (t ;; LayoutCategoryData
       (let ((layouters (ignore-errors (get-layouters data))))
         (when (and layouters (not (is-empty layouters)))
           (let ((a (make-json-array)))
             (json-put o "knownLayouters" a)
             (jdo (l layouters) (when (.id l) (json-array-add a (.id l)))))))))
    o))

;;; ------------------------------------------------------------ ElkGraphJson

(defjclass import-builder ()
  ((json-graph nil)
   (graph nil)
   (importer-maybe nil)
   (lenient t)))

(defjclass export-builder ()
  ((graph nil)
   (pretty-print nil)
   (omit-zero-position t)
   (omit-zero-dimension t)
   (omit-layout-information nil)
   (short-layout-option-keys t)
   (omit-unknown-layout-options t)))

(define-jcalls :instance remember-importer lenient to-elk pretty-print short-layout-option-keys
  omit-zero-positions omit-zero-dimension omit-layout omit-unknown-layout-options to-json)

(defjstatic elk-graph-json.for-graph (graph)
  "A JSON string or JSON object -> an IMPORT-BUILDER; an ElkNode -> an EXPORT-BUILDER."
  (cond ((stringp graph) (let ((ib (new-import-builder))) (setf (.graph ib) graph) ib))
        ((json-object-p graph) (let ((ib (new-import-builder))) (setf (.json-graph ib) graph) ib))
        (t (let ((eb (new-export-builder))) (setf (.graph eb) graph) eb))))

(defjmethod remember-importer ((this import-builder) maybe)
  (setf (.importer-maybe this) maybe) this)

(defjmethod lenient ((this import-builder) be-lenient)
  (setf (.lenient this) be-lenient) this)

(defjmethod to-elk ((this import-builder))
  (unless (.json-graph this)
    (let ((json (json-parse (.graph this) :lenient (.lenient this))))
      (unless (json-object-p json)
        (%gio-format-error "Top-level element of the graph must be a json object."))
      (setf (.json-graph this) json)))
  (let* ((importer (new-json-importer))
         (elk-graph (transform importer (.json-graph this))))
    (when (.importer-maybe this)
      (j-set (.importer-maybe this) importer))
    elk-graph))

(defjmethod pretty-print ((this export-builder) pretty) (setf (.pretty-print this) pretty) this)
(defjmethod short-layout-option-keys ((this export-builder) v) (setf (.short-layout-option-keys this) v) this)
(defjmethod omit-zero-positions ((this export-builder) v) (setf (.omit-zero-position this) v) this)
(defjmethod omit-zero-dimension ((this export-builder) v) (setf (.omit-zero-dimension this) v) this)
(defjmethod omit-layout ((this export-builder) v) (setf (.omit-layout-information this) v) this)
(defjmethod omit-unknown-layout-options ((this export-builder) v) (setf (.omit-unknown-layout-options this) v) this)

(defjmethod to-json ((this export-builder))
  (let ((exporter (new-json-exporter)))
    (set-options exporter (.omit-zero-position this) (.omit-zero-dimension this)
                 (.omit-layout-information this) (.short-layout-option-keys this)
                 (.omit-unknown-layout-options this))
    (json-write (j-export exporter (.graph this)) :pretty (.pretty-print this))))
