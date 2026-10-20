;;;; graph.lisp -- org.eclipse.elk.alg.layered.graph: LGraphElement, LShape,
;;;; LMargin, LPadding, LLabel, Layer, LGraph, LNode (+ NodeType), LPort, LEdge.

(in-package #:elk)

(define-jcalls :instance get-designation get-position get-size get-text get-nodes get-graph
  get-index get-actual-size get-padding get-offset get-layerless-nodes get-layers get-parent-node
  set-parent-node to-node-array get-layer set-layer set-graph get-type set-type get-ports
  get-port-side-view get-incoming-edges get-outgoing-edges get-connected-edges get-labels
  get-nested-graph set-nested-graph get-margin border-to-content-area-coordinates
  get-interactive-reference-point cache-port-sides find-port-indices is-inline-edge-label
  get-color get-node set-node get-side set-side get-anchor is-explicitly-supplied-port-anchor
  set-explicitly-supplied-port-anchor get-absolute-anchor get-name get-degree get-net-flow
  is-connected-to-external-nodes set-connected-to-external-nodes get-predecessor-ports
  get-successor-ports get-connected-ports j-reverse get-source set-source get-target set-target
  set-target-and-insert-at-index is-self-loop is-in-layer-edge get-bend-points get-other
  to-string clone)

(defun %lg-null-or-empty-p (s)
  "Guava Strings.isNullOrEmpty"
  (or (null s) (and (stringp s) (zerop (length s)))))

(defun %lg-to-string (x)
  "String.valueOf for the elements printed by the toString methods."
  (cond ((null x) "null")
        ((typep x 'l-graph-element) (to-string x))
        ((stringp x) x)
        ((and (vectorp x) (not (stringp x)))
         ;; AbstractCollection.toString
         (with-output-to-string (s)
           (write-string "[" s)
           (loop for e across x for i from 0
                 do (when (> i 0) (write-string ", " s))
                    (write-string (%lg-to-string e) s))
           (write-string "]" s)))
        (t (princ-to-string x))))

;;; ------------------------------------------------------------ LGraphElement

(defjmethod get-designation ((this l-graph-element))
  (let ((identifier (layered-elk-graph-transformer.get-origin-identifier this)))
    (if (not (%lg-null-or-empty-p identifier))
        identifier
        nil)))

(defjmethod to-string ((this l-graph-element))
  (format nil "~A@~X" (string-downcase (class-name (class-of this))) (sb-kernel:get-lisp-obj-address this)))

(defmethod print-object ((x l-graph-element) s)
  (print-unreadable-object (x s)
    (write-string (handler-case (to-string x) (error () (string-downcase (class-name (class-of x))))) s)))

;;; ------------------------------------------------------------ LShape

(defjmethod get-position ((this l-shape)) (.pos this))
(defjmethod get-size ((this l-shape)) (.size this))

;;; ------------------------------------------------------------ LPadding

(defjmethod clone ((this l-padding))
  (let ((clone (new-l-padding)))
    (set-left clone (get-left this))
    (set-right clone (get-right this))
    (set-top clone (get-top this))
    (set-bottom clone (get-bottom this))
    clone))

(defmethod jclone ((this l-padding)) (clone this))

;;; ------------------------------------------------------------ LLabel

(defjctor l-label () (this-ctor l-label this ""))
(defjctor l-label (thetext) (setf (.text this) thetext))

(defjmethod get-text ((this l-label)) (.text this))

(defjmethod to-string ((this l-label))
  (let ((designation (get-designation this)))
    (if (null designation) "label" (jstr "l_" designation))))

(defjmethod get-designation ((this l-label))
  (if (not (%lg-null-or-empty-p (.text this)))
      (.text this)
      (call-next-method)))

;;; ------------------------------------------------------------ Layer

(defjctor layer (graph) (setf (.owner this) graph))

(defjmethod get-size ((this layer)) (.size this))
(defjmethod get-nodes ((this layer)) (.nodes this))
(defmethod jiterable-items ((this layer)) (.nodes this))
(defjmethod get-graph ((this layer)) (.owner this))
(defjmethod get-index ((this layer)) (jl-index-of (get-layers (.owner this)) this))
(defjmethod to-string ((this layer))
  (jstr "L_" (get-index this) (%lg-to-string (.nodes this))))

;;; ------------------------------------------------------------ LGraph

(defjmethod get-size ((this l-graph)) (.size this))

(defjmethod get-actual-size ((this l-graph))
  (let ((size (.size this)) (padding (.padding this)))
    (new-k-vector (+ (.x size) (.left padding) (.right padding))
                  (+ (.y size) (.top padding) (.bottom padding)))))

(defjmethod get-padding ((this l-graph)) (.padding this))
(defjmethod get-offset ((this l-graph)) (.offset this))
(defjmethod get-layerless-nodes ((this l-graph)) (.layerless-nodes this))
(defjmethod get-layers ((this l-graph)) (.layers this))
(defjmethod get-parent-node ((this l-graph)) (.parent-node this))
(defjmethod set-parent-node ((this l-graph) parent-node) (setf (.parent-node this) parent-node))
(defmethod jiterable-items ((this l-graph)) (.layers this))

(defjmethod to-node-array ((this l-graph))
  "LNode[][]: a simple vector of simple vectors."
  (let ((result (make-array (length (.layers this)))))
    (loop for layer across (.layers this) for i from 0
          do (setf (aref result i) (l-graph-util.to-node-array (get-nodes layer))))
    result))

(defjmethod to-string ((this l-graph))
  (cond ((jl-is-empty (.layers this))
         (jstr "G-unlayered" (%lg-to-string (.layerless-nodes this))))
        ((jl-is-empty (.layerless-nodes this))
         (jstr "G-layered" (%lg-to-string (.layers this))))
        (t (jstr "G[layerless" (%lg-to-string (.layerless-nodes this))
                 ", layers" (%lg-to-string (.layers this)) "]"))))

;;; ------------------------------------------------------------ LNode.NodeType

(defjmethod get-color ((this node-type))
  (enum-case this
    (node-type.external-port "#cc99cc")
    (node-type.long-edge "#eaed00")
    (node-type.north-south-port "#0034de")
    (node-type.label "#75c3c3")
    (node-type.breaking-point "#eeeeff")
    (t "#eeeeee")))

;;; ------------------------------------------------------------ LNode

(defjctor l-node (graph) (setf (.graph this) graph))

(defjmethod get-layer ((this l-node)) (.layer this))

(defjmethod set-layer ((this l-node) thelayer)
  (when (.layer this)
    (jl-remove (get-nodes (.layer this)) this))
  (setf (.layer this) thelayer)
  (when (.layer this)
    (jl-add (get-nodes (.layer this)) this))
  nil)

(defjmethod get-graph ((this l-node))
  (if (and (null (.graph this)) (.layer this))
      (get-graph (.layer this))
      (.graph this)))

(defjmethod set-graph ((this l-node) new-graph)
  (setf (.graph this) new-graph))

(defjmethod set-layer ((this l-node) index newlayer)
  (when (and newlayer (or (< index 0) (> index (length (get-nodes newlayer)))))
    (jthrow 'illegal-argument-exception "index must be >= 0 and <= layer node count"))
  (when (.layer this)
    (jl-remove (get-nodes (.layer this)) this))
  (setf (.layer this) newlayer)
  (when newlayer
    (jl-add-at (get-nodes newlayer) index this))
  nil)

(defjmethod get-type ((this l-node)) (.type this))
(defjmethod set-type ((this l-node) type) (setf (.type this) type))

(defjmethod get-ports ((this l-node)) (.ports this))

(defun %lg-filter-ports (ports pred)
  (let ((result (new-jlist)))
    (loop for p across ports when (funcall pred p) do (vector-push-extend p result))
    result))

(defun %lg-type-predicate (port-type)
  (enum-case port-type
    (port-type.input l-port.input-predicate)
    (port-type.output l-port.output-predicate)
    (t nil)))

(defun %lg-side-predicate (side)
  (enum-case side
    (port-side.north l-port.north-predicate)
    (port-side.east l-port.east-predicate)
    (port-side.south l-port.south-predicate)
    (port-side.west l-port.west-predicate)
    (t nil)))

(defjmethod get-ports ((this l-node) (port-type port-type))
  (let ((pred (%lg-type-predicate port-type)))
    (if pred (%lg-filter-ports (.ports this) pred) (new-jlist))))

(defjmethod get-ports ((this l-node) (side port-side))
  (let ((pred (%lg-side-predicate side)))
    (if pred (%lg-filter-ports (.ports this) pred) (new-jlist))))

(defjmethod get-port-side-view ((this l-node) side)
  (unless (.port-sides-cached this)
    ;; If not explicitly cached, this will be repeated each time. However, this has the same
    ;; complexity as filtering by side.
    (find-port-indices this))
  (let ((indices (gethash side (.port-side-indices this))))
    (if (null indices)
        (new-jlist)
        ;; A fresh copy: the order of the ports on one side can change.
        (jl-sub-list (.ports this) (car indices) (cdr indices)))))

(defjmethod get-ports ((this l-node) port-type side)
  (let ((type-predicate (%lg-type-predicate port-type))
        (side-predicate (%lg-side-predicate side)))
    (if (and type-predicate side-predicate)
        (%lg-filter-ports (.ports this) (lambda (p) (and (funcall type-predicate p)
                                                         (funcall side-predicate p))))
        (new-jlist))))

(defjmethod get-incoming-edges ((this l-node))
  (let ((result (new-jlist)))
    (loop for port across (.ports this) do (jl-add-all result (get-incoming-edges port)))
    result))

(defjmethod get-outgoing-edges ((this l-node))
  (let ((result (new-jlist)))
    (loop for port across (.ports this) do (jl-add-all result (get-outgoing-edges port)))
    result))

(defjmethod get-connected-edges ((this l-node))
  (let ((result (new-jlist)))
    (loop for port across (.ports this) do (jl-add-all result (get-connected-edges port)))
    result))

(defjmethod get-labels ((this l-node)) (.labels this))
(defjmethod get-nested-graph ((this l-node)) (.nested-graph this))
(defjmethod set-nested-graph ((this l-node) nested-graph) (setf (.nested-graph this) nested-graph))
(defjmethod get-margin ((this l-node)) (.margin this))
(defjmethod get-padding ((this l-node)) (.padding this))

(defjmethod get-index ((this l-node))
  (if (null (.layer this))
      -1
      (jl-index-of (get-nodes (.layer this)) this)))

(defjmethod border-to-content-area-coordinates ((this l-node) horizontal vertical)
  (let* ((thegraph (get-graph this))
         (graph-padding (get-padding thegraph))
         (offset (get-offset thegraph))
         (pos (get-position this)))
    (when horizontal
      (setf (.x pos) (- (.x pos) (.left graph-padding) (.x offset))))
    (when vertical
      (setf (.y pos) (- (.y pos) (.top graph-padding) (.y offset))))
    nil))

(defjmethod get-interactive-reference-point ((this l-node))
  (enum-case (get-property (get-graph this) layered-options.interactive-reference-point)
    (interactive-reference-point.center
     (let ((node-pos (get-position this)) (node-size (get-size this)))
       (new-k-vector (+ (.x node-pos) (/ (.x node-size) 2.0))
                     (+ (.y node-pos) (/ (.y node-size) 2.0)))))
    (interactive-reference-point.top-left
     (new-k-vector (get-position this)))
    ;; This shouldn't happen.
    (t nil)))

(defjmethod cache-port-sides ((this l-node))
  (setf (.port-sides-cached this) t)
  (find-port-indices this))

(defjmethod find-port-indices ((this l-node))
  (let ((indices (make-hash-table :test 'eq))
        (ports (.ports this))
        (first-index-for-current-side 0)
        (current-side port-side.north)
        (current-index 0))
    (loop while (< current-index (length ports))
          do (let ((port (aref ports current-index)))
               (unless (eq (get-side port) current-side)
                 (unless (= first-index-for-current-side current-index)
                   (setf (gethash current-side indices)
                         (cons first-index-for-current-side current-index)))
                 (setf current-side (get-side port)
                       first-index-for-current-side current-index)))
             (incf current-index))
    (setf (gethash current-side indices) (cons first-index-for-current-side current-index))
    (setf (.port-side-indices this) indices)
    nil))

(defjmethod get-designation ((this l-node))
  (let ((labels (.labels this)))
    (if (and (not (jl-is-empty labels)) (not (%lg-null-or-empty-p (get-text (aref labels 0)))))
        (get-text (aref labels 0))
        (let ((id (call-next-method)))
          (or id (princ-to-string (get-index this)))))))

(defjmethod is-inline-edge-label ((this l-node))
  (and (eq (get-type this) node-type.label)
       (block all-match
         (jdo (label (get-property this layered-internal-properties.represented-labels))
           (unless (get-property label layered-options.edge-labels-inline)
             (return-from all-match nil)))
         t)))

(defjmethod to-string ((this l-node))
  (with-output-to-string (s)
    (write-string "n" s)
    (unless (eq (.type this) node-type.normal)
      (format s "(~A)" (string-downcase (name (.type this)))))
    (write-string "_" s)
    (write-string (%lg-to-string (get-designation this)) s)))

;;; ------------------------------------------------------------ LPort

(defjconst l-port.output-predicate (lambda (port) (not (jl-is-empty (.outgoing-edges port)))))
(defjconst l-port.input-predicate (lambda (port) (not (jl-is-empty (.incoming-edges port)))))
(defjconst l-port.north-predicate (lambda (port) (eq (.side port) port-side.north)))
(defjconst l-port.east-predicate (lambda (port) (eq (.side port) port-side.east)))
(defjconst l-port.south-predicate (lambda (port) (eq (.side port) port-side.south)))
(defjconst l-port.west-predicate (lambda (port) (eq (.side port) port-side.west)))

(defjmethod get-node ((this l-port)) (.owner this))

(defjmethod set-node ((this l-port) node)
  (when (.owner this)
    (jl-remove (get-ports (.owner this)) this))
  (setf (.owner this) node)
  (when (.owner this)
    (jl-add (get-ports (.owner this)) this))
  nil)

(defjmethod get-side ((this l-port)) (.side this))

(defjmethod set-side ((this l-port) theside)
  (when (null theside)
    (jthrow 'null-pointer-exception))
  (setf (.side this) theside)
  (unless (is-explicitly-supplied-port-anchor this)
    (let ((anchor (get-anchor this)) (size (get-size this)))
      (enum-case (.side this)
        (port-side.north
         ;; adapt the anchor so edges are attached top center
         (setf (.x anchor) (/ (.x size) 2)
               (.y anchor) 0.0))
        (port-side.east
         ;; adapt the anchor so edges are attached center right
         (setf (.x anchor) (.x size)
               (.y anchor) (/ (.y size) 2)))
        (port-side.south
         ;; adapt the anchor so edges are attached bottom center
         (setf (.x anchor) (/ (.x size) 2)
               (.y anchor) (.y size)))
        (port-side.west
         ;; adapt the anchor so edges are attached center left
         (setf (.x anchor) 0.0
               (.y anchor) (/ (.y size) 2))))))
  nil)

(defjmethod get-anchor ((this l-port)) (.anchor this))
(defjmethod is-explicitly-supplied-port-anchor ((this l-port)) (.explicitly-supplied-port-anchor this))
(defjmethod set-explicitly-supplied-port-anchor ((this l-port) fixed)
  (setf (.explicitly-supplied-port-anchor this) fixed))

(defjmethod get-absolute-anchor ((this l-port))
  (k-vector.sum (list (get-position (.owner this)) (get-position this) (.anchor this))))

(defjmethod get-margin ((this l-port)) (.margin this))
(defjmethod get-labels ((this l-port)) (.labels this))

(defjmethod get-name ((this l-port))
  (if (not (jl-is-empty (.labels this)))
      (get-text (aref (.labels this) 0))
      nil))

(defjmethod get-degree ((this l-port))
  (+ (length (.incoming-edges this)) (length (.outgoing-edges this))))

(defjmethod get-net-flow ((this l-port))
  (- (length (.incoming-edges this)) (length (.outgoing-edges this))))

(defjmethod get-incoming-edges ((this l-port)) (.incoming-edges this))
(defjmethod get-outgoing-edges ((this l-port)) (.outgoing-edges this))

(defjmethod get-connected-edges ((this l-port))
  "CombineIter(incoming, outgoing) -> a fresh list, incoming edges first."
  (let ((result (new-jlist (+ (length (.incoming-edges this)) (length (.outgoing-edges this))))))
    (jl-add-all result (.incoming-edges this))
    (jl-add-all result (.outgoing-edges this))
    result))

(defjmethod is-connected-to-external-nodes ((this l-port)) (.connected-to-external-nodes this))
(defjmethod set-connected-to-external-nodes ((this l-port) conn)
  (setf (.connected-to-external-nodes this) conn))

(defjmethod get-predecessor-ports ((this l-port))
  (let ((result (new-jlist)))
    (loop for e across (.incoming-edges this) do (vector-push-extend (get-source e) result))
    result))

(defjmethod get-successor-ports ((this l-port))
  (let ((result (new-jlist)))
    (loop for e across (.outgoing-edges this) do (vector-push-extend (get-target e) result))
    result))

(defjmethod get-connected-ports ((this l-port))
  (let ((result (get-predecessor-ports this)))
    (jl-add-all result (get-successor-ports this))
    result))

(defjmethod get-index ((this l-port))
  (if (null (.owner this))
      -1
      (jl-index-of (get-ports (.owner this)) this)))

(defjmethod get-designation ((this l-port))
  (let ((labels (.labels this)))
    (if (and (not (jl-is-empty labels)) (not (%lg-null-or-empty-p (get-text (aref labels 0)))))
        (get-text (aref labels 0))
        (let ((id (call-next-method)))
          (or id (princ-to-string (get-index this)))))))

(defjmethod to-string ((this l-port))
  (with-output-to-string (s)
    (write-string "p_" s)
    (write-string (%lg-to-string (get-designation this)) s)
    (when (.owner this)
      (format s "[~A]" (%lg-to-string (.owner this))))
    (let ((in (.incoming-edges this)) (out (.outgoing-edges this)))
      (when (and (= (length in) 1) (jl-is-empty out) (not (eq (get-source (aref in 0)) this)))
        (let ((source (get-source (aref in 0))))
          (format s " << ~A[~A]" (%lg-to-string (get-designation source)) (%lg-to-string (.owner source)))))
      (when (and (jl-is-empty in) (= (length out) 1) (not (eq (get-target (aref out 0)) this)))
        (let ((target (get-target (aref out 0))))
          (format s " >> ~A[~A]" (%lg-to-string (get-designation target)) (%lg-to-string (.owner target))))))))

;;; ------------------------------------------------------------ LEdge

(defjmethod j-reverse ((this l-edge) layered-graph adapt-ports)
  (let ((old-source (get-source this))
        (old-target (get-target this)))
    (set-source this nil)
    (set-target this nil)
    (if (and adapt-ports (get-property old-target layered-internal-properties.input-collect))
        (set-source this (l-graph-util.provide-collector-port layered-graph (get-node old-target)
                                                              port-type.output port-side.east))
        (set-source this old-target))
    (if (and adapt-ports (get-property old-source layered-internal-properties.output-collect))
        (set-target this (l-graph-util.provide-collector-port layered-graph (get-node old-source)
                                                              port-type.input port-side.west))
        (set-target this old-source))
    ;; Switch end labels
    (jdo (label (.labels this))
      (let ((label-placement (get-property label layered-options.edge-labels-placement)))
        (cond ((eq label-placement edge-label-placement.tail)
               (set-property label layered-options.edge-labels-placement edge-label-placement.head))
              ((eq label-placement edge-label-placement.head)
               (set-property label layered-options.edge-labels-placement edge-label-placement.tail)))))
    (let ((reversed (get-property this layered-internal-properties.reversed)))
      (set-property this layered-internal-properties.reversed (not reversed)))
    (setf (.bend-points this) (k-vector-chain.reverse (.bend-points this)))
    nil))

(defjmethod get-source ((this l-edge)) (.source this))

(defjmethod set-source ((this l-edge) source)
  (when (.source this)
    (jl-remove (get-outgoing-edges (.source this)) this))
  (setf (.source this) source)
  (when (.source this)
    (jl-add (get-outgoing-edges (.source this)) this))
  nil)

(defjmethod get-target ((this l-edge)) (.target this))

(defjmethod set-target ((this l-edge) target)
  (when (.target this)
    (jl-remove (get-incoming-edges (.target this)) this))
  (setf (.target this) target)
  (when (.target this)
    (jl-add (get-incoming-edges (.target this)) this))
  nil)

(defjmethod set-target-and-insert-at-index ((this l-edge) target-port index)
  (when (.target this)
    (jl-remove (get-incoming-edges (.target this)) this))
  (setf (.target this) target-port)
  (when (.target this)
    ;; The insertion index below is the only difference to setTarget(LPort)
    (jl-add-at (get-incoming-edges (.target this)) index this))
  nil)

(defjmethod is-self-loop ((this l-edge))
  (if (or (null (.source this)) (null (.target this)))
      nil
      (and (get-node (.source this)) (eq (get-node (.source this)) (get-node (.target this))))))

(defjmethod is-in-layer-edge ((this l-edge))
  (and (not (is-self-loop this))
       (eq (get-layer (get-node (.source this))) (get-layer (get-node (.target this))))))

(defjmethod get-bend-points ((this l-edge)) (.bend-points this))
(defjmethod get-labels ((this l-edge)) (.labels this))

(defjmethod get-other ((this l-edge) (port l-port))
  (cond ((eq port (.source this)) (.target this))
        ((eq port (.target this)) (.source this))
        (t (jthrow 'illegal-argument-exception
                   "'port' must be either the source port or target port of the edge."))))

(defjmethod get-other ((this l-edge) (node l-node))
  (cond ((eq node (get-node (.source this))) (get-node (.target this)))
        ((eq node (get-node (.target this))) (get-node (.source this)))
        (t (jthrow 'illegal-argument-exception
                   "'node' must either be the source node or target node of the edge."))))

(defjmethod get-designation ((this l-edge))
  (let ((labels (.labels this)))
    (if (and (not (jl-is-empty labels)) (not (%lg-null-or-empty-p (get-text (aref labels 0)))))
        (get-text (aref labels 0))
        (call-next-method))))

(defjmethod to-string ((this l-edge))
  (with-output-to-string (s)
    (write-string "e_" s)
    (let ((designation (get-designation this)))
      (when designation (write-string designation s)))
    (when (and (.source this) (.target this))
      (format s " ~A[~A] -> ~A[~A]"
              (%lg-to-string (get-designation (.source this))) (%lg-to-string (get-node (.source this)))
              (%lg-to-string (get-designation (.target this))) (%lg-to-string (get-node (.target this)))))))
