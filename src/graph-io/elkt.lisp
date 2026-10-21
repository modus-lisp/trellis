;;;; elkt.lisp -- the ELK Graph Text (.elkt) reader.
;;;;
;;;; Upstream this is an Xtext grammar (plugins/org.eclipse.elk.graph.text/
;;;; src/.../ElkGraph.xtext) with value converters (conversion/*.xtend), a
;;;; qualified-name provider (naming/) and a scope provider (scoping/).  Xtext
;;;; generates an ANTLR parser; here it is a hand-written recursive-descent
;;;; parser over a token vector, following the grammar rule for rule.  Every
;;;; rule of the grammar has a function below with the same name.
;;;;
;;;; Differences that are deliberate, all noted in NOTES.md:
;;;;   - keywords are recognised by position, not reserved: an identifier may
;;;;     be spelled like a keyword wherever the grammar expects an identifier;
;;;;   - references are linked after the whole file is parsed (Xtext does the
;;;;     same, in its linking phase), so forward references work;
;;;;   - the Xtext scope is approximated by a path walk: a reference a.b.c is
;;;;     looked up from the edge's containing node outwards to the root, and
;;;;     each step matches a child node or a port by identifier.  That is the
;;;;     qualified-name rule of ElkGraphQualifiedNameProvider in its common
;;;;     form.

(in-package #:elk)

(define-condition elkt-parse-exception (error)
  ((message :initarg :message :reader elkt-parse-exception-message))
  (:report (lambda (c s) (format s "~A" (elkt-parse-exception-message c))))
  (:documentation "A syntax or linking error in an .elkt document."))

(defun %elkt-fail (fmt &rest args)
  (error 'elkt-parse-exception :message (apply #'format nil fmt args)))

;;; ------------------------------------------------------------ lexer

(defstruct (elkt-token (:constructor make-elkt-token (kind text pos)))
  kind    ; :id :string :number :punct :eof
  text
  pos)

(defun %elkt-id-start-p (c) (or (alpha-char-p c) (char= c #\_) (char= c #\$)))
(defun %elkt-id-part-p (c) (or (alphanumericp c) (char= c #\_) (char= c #\$)))
(defun %elkt-digit-p (c) (and c (digit-char-p c)))

(defun %elkt-tokenize (text)
  "The token vector of TEXT.  Whitespace and comments are dropped."
  (let* ((n (length text))
         (i 0)
         (out (make-array 256 :adjustable t :fill-pointer 0)))
    (flet ((at (k) (and (< k n) (char text k))))
      (loop while (< i n) do
        (let ((c (char text i)))
          (cond
            ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
            ((and (char= c #\/) (eql (at (1+ i)) #\/))
             (loop while (and (< i n) (char/= (char text i) #\Newline)) do (incf i)))
            ((and (char= c #\/) (eql (at (1+ i)) #\*))
             (let ((end (search "*/" text :start2 (+ i 2))))
               (unless end (%elkt-fail "Unterminated comment at ~D" i))
               (setf i (+ end 2))))
            ((or (char= c #\") (char= c #\'))
             (let ((j (1+ i)))
               (loop
                 (when (>= j n) (%elkt-fail "Unterminated string at ~D" i))
                 (let ((ch (char text j)))
                   (cond ((char= ch #\\) (incf j 2))
                         ((char= ch c) (return))
                         (t (incf j)))))
               (vector-push-extend (make-elkt-token :string (subseq text i (1+ j)) i) out)
               (setf i (1+ j))))
            ((or (%elkt-digit-p c)
                 (and (member c '(#\+ #\-)) (%elkt-digit-p (at (1+ i)))))
             ;; SIGNED_INT | FLOAT
             (let ((j (1+ i)))
               (loop while (%elkt-digit-p (at j)) do (incf j))
               (when (and (eql (at j) #\.) (%elkt-digit-p (at (1+ j))))
                 (incf j)
                 (loop while (%elkt-digit-p (at j)) do (incf j)))
               (when (and (member (at j) '(#\e #\E))
                          (or (%elkt-digit-p (at (1+ j)))
                              (and (member (at (1+ j)) '(#\+ #\-))
                                   (%elkt-digit-p (at (+ j 2))))))
                 (incf j 2)
                 (loop while (%elkt-digit-p (at j)) do (incf j)))
               (vector-push-extend (make-elkt-token :number (subseq text i j) i) out)
               (setf i j)))
            ((and (char= c #\-) (eql (at (1+ i)) #\>))
             (vector-push-extend (make-elkt-token :punct "->" i) out)
             (incf i 2))
            ((or (%elkt-id-start-p c)
                 (and (char= c #\^) (at (1+ i)) (%elkt-id-start-p (at (1+ i)))))
             (let ((j (if (char= c #\^) (1+ i) i)))
               (loop while (and (< j n) (%elkt-id-part-p (char text j))) do (incf j))
               (vector-push-extend (make-elkt-token :id (subseq text i j) i) out)
               (setf i j)))
            ((find c "{}[]:,|.")
             (vector-push-extend (make-elkt-token :punct (string c) i) out)
             (incf i))
            (t (%elkt-fail "Unexpected character ~S at ~D" c i))))))
    (vector-push-extend (make-elkt-token :eof "" n) out)
    out))

;;; ------------------------------------------------------------ parser state

(defvar *elkt-toks* nil)
(defvar *elkt-pos* 0)
(defvar *elkt-deferred* nil
  "Reference-linking closures, run in source order after the whole parse.")

(defun %elkt-peek (&optional (k 0))
  (let ((i (min (+ *elkt-pos* k) (1- (length *elkt-toks*)))))
    (aref *elkt-toks* i)))

(defun %elkt-next ()
  (prog1 (%elkt-peek) (when (< *elkt-pos* (1- (length *elkt-toks*))) (incf *elkt-pos*))))

(defun %elkt-describe (tok)
  (if (eq (elkt-token-kind tok) :eof)
      "end of input"
      (format nil "'~A' at ~D" (elkt-token-text tok) (elkt-token-pos tok))))

(defun %elkt-punct-p (tok text)
  (and (eq (elkt-token-kind tok) :punct) (string= (elkt-token-text tok) text)))

(defun %elkt-keyword-p (tok word)
  (and (eq (elkt-token-kind tok) :id) (string= (elkt-token-text tok) word)))

(defun %elkt-at-punct (text) (%elkt-punct-p (%elkt-peek) text))
(defun %elkt-at-keyword (word) (%elkt-keyword-p (%elkt-peek) word))

(defun %elkt-expect-punct (text)
  (let ((tok (%elkt-next)))
    (unless (%elkt-punct-p tok text)
      (%elkt-fail "Expected '~A' but found ~A" text (%elkt-describe tok)))
    tok))

(defun %elkt-expect-keyword (word)
  (let ((tok (%elkt-next)))
    (unless (%elkt-keyword-p tok word)
      (%elkt-fail "Expected '~A' but found ~A" word (%elkt-describe tok)))
    tok))

(defun %elkt-expect-id ()
  "ID: the identifier as written, '^' included."
  (let ((tok (%elkt-next)))
    (unless (eq (elkt-token-kind tok) :id)
      (%elkt-fail "Expected an identifier but found ~A" (%elkt-describe tok)))
    (elkt-token-text tok)))

(defun %elkt-unescape-id (s)
  "IDValueConverter.toValue: a leading '^' only protects a keyword."
  (if (and (plusp (length s)) (char= (char s 0) #\^)) (subseq s 1) s))

(defun %elkt-qualified-id ()
  "QualifiedId: ID ('.' ID)*.  Returns the raw segments."
  (let ((segs (list (%elkt-expect-id))))
    (loop while (and (%elkt-at-punct ".") (eq (elkt-token-kind (%elkt-peek 1)) :id))
          do (%elkt-next) (push (%elkt-expect-id) segs))
    (nreverse segs)))

(defun %elkt-join (segs sep)
  (format nil (concatenate 'string "~{~A~^" sep "~}") segs))

(defun %elkt-property-ahead-p ()
  "True when the tokens ahead are PropertyKey ':' (ID ('.' ID)* ':')."
  (and (eq (elkt-token-kind (%elkt-peek)) :id)
       (let ((k 1))
         (loop while (and (%elkt-punct-p (%elkt-peek k) ".")
                          (eq (elkt-token-kind (%elkt-peek (1+ k))) :id))
               do (incf k 2))
         (%elkt-punct-p (%elkt-peek k) ":"))))

(defun %elkt-number-value (tok)
  "Number returns ecore::EDouble: NumberValueConverter.toValue."
  (unless (member (elkt-token-kind tok) '(:number))
    (%elkt-fail "Expected a number but found ~A" (%elkt-describe tok)))
  (let ((*read-default-float-format* 'double-float)
        (*read-eval* nil))
    (coerce (read-from-string (elkt-token-text tok)) 'double-float)))

(defun %elkt-number ()
  (%elkt-number-value (%elkt-next)))

(defun %elkt-unquote-string (raw)
  "PropertyValueValueConverter.unquoteIfNecessary: quotes only, no escapes."
  (if (and (>= (length raw) 2)
           (member (char raw 0) '(#\" #\'))
           (char= (char raw 0) (char raw (1- (length raw)))))
      (subseq raw 1 (1- (length raw)))
      raw))

(defun %elkt-unescape-string (raw)
  "STRING terminal converter (labels' text): quotes plus backslash escapes."
  (let ((s (%elkt-unquote-string raw)))
    (with-output-to-string (out)
      (let ((i 0) (n (length s)))
        (loop while (< i n) do
          (let ((c (char s i)))
            (if (and (char= c #\\) (< (1+ i) n))
                (let ((e (char s (1+ i))))
                  (write-char (case e
                                (#\b (code-char 8)) (#\t #\Tab) (#\n #\Newline)
                                (#\f (code-char 12)) (#\r #\Return)
                                (t e))
                              out)
                  (incf i 2))
                (progn (write-char c out) (incf i)))))))))

;;; ------------------------------------------------------------ linking

(defun %elkt-defer (fn)
  (vector-push-extend fn *elkt-deferred*))

(defun %elkt-children-and-ports (node)
  (let ((all '()))
    (jdo (c (get-children node)) (push c all))
    (jdo (p (get-ports node)) (push p all))
    (nreverse all)))

(defun %elkt-resolve-path (scope segs)
  "Walk SEGS (stripped identifiers) down from SCOPE through children and ports."
  (let ((cur scope))
    (dolist (seg segs cur)
      (let ((found nil))
        (when (typep cur 'elk-node)
          (dolist (c (%elkt-children-and-ports cur))
            (when (and (null found) (equal (get-identifier c) seg))
              (setf found c))))
        (unless found (return nil))
        (setf cur found)))))

(defun %elkt-resolve-shape (scope segs raw)
  "ElkConnectableShape reference: innermost enclosing node first, then outwards."
  (let ((a scope))
    (loop while a do
      (let ((hit (%elkt-resolve-path a segs)))
        (when (and hit (typep hit 'elk-connectable-shape))
          (return-from %elkt-resolve-shape hit)))
      (setf a (get-parent a)))
    (%elkt-fail "Couldn't resolve reference to ElkConnectableShape '~A'." raw)))

(defun %elkt-resolve-section (edge raw)
  (jdo (s (get-sections edge))
    (when (equal (get-identifier s) raw) (return-from %elkt-resolve-section s)))
  (%elkt-fail "Couldn't resolve reference to ElkEdgeSection '~A'." raw))

(defun %elkt-run-deferred ()
  (loop for fn across *elkt-deferred* do (funcall fn)))

;;; ------------------------------------------------------------ grammar

(defun %elkt-property (holder)
  "Property returns ElkPropertyToValueMapEntry, with the value converters."
  (let* ((raw-segs (%elkt-qualified-id))
         (raw-key (%elkt-join raw-segs "."))
         (suffix (%elkt-join (mapcar #'%elkt-unescape-id raw-segs) "."))
         (svc (layout-meta-data-service.get-instance)))
    (%elkt-expect-punct ":")
    (let* ((value-tok (%elkt-peek))
           (raw-value
             (case (elkt-token-kind value-tok)
               (:string (%elkt-next) (%elkt-unquote-string (elkt-token-text value-tok)))
               (:number (%elkt-next) (elkt-token-text value-tok))
               (:id (cond ((member (elkt-token-text value-tok) '("true" "false") :test #'string=)
                           (%elkt-next) (elkt-token-text value-tok))
                          ((string= (elkt-token-text value-tok) "null") (%elkt-next) nil)
                          (t (%elkt-join (mapcar #'%elkt-unescape-id (%elkt-qualified-id)) "."))))
               (t (%elkt-fail "Expected a property value but found ~A" (%elkt-describe value-tok)))))
           (option (get-option-data-by-suffix svc suffix))
           (key (or option (new-property/1 raw-key))))
      (set-property holder key
                    (and raw-value
                         (let* ((data (or option (get-option-data svc raw-key)))
                                (parsed (and data (parse-value data raw-value))))
                           (or parsed (new-layout-option-proxy/1 raw-value))))))))

(defun %elkt-shape-layout (shape &optional scope)
  (declare (ignore scope))
  "ShapeLayout: 'layout' '[' ('position' ':' x ',' y)? & ('size' ':' w ',' h)? ']'"
  (%elkt-expect-keyword "layout")
  (%elkt-expect-punct "[")
  (loop until (%elkt-at-punct "]") do
    (cond ((%elkt-at-keyword "position")
           (%elkt-next) (%elkt-expect-punct ":")
           (set-x shape (%elkt-number))
           (%elkt-expect-punct ",")
           (set-y shape (%elkt-number)))
          ((%elkt-at-keyword "size")
           (%elkt-next) (%elkt-expect-punct ":")
           (set-width shape (%elkt-number))
           (%elkt-expect-punct ",")
           (set-height shape (%elkt-number)))
          (t (%elkt-fail "Expected 'position' or 'size' in layout but found ~A"
                         (%elkt-describe (%elkt-peek))))))
  (%elkt-expect-punct "]"))

(defun %elkt-section-items (section scope)
  "The items of an ElkSingleEdgeSection or ElkEdgeSection body, up to ']'."
  (loop until (%elkt-at-punct "]") do
    (cond
      ((and (or (%elkt-at-keyword "incoming") (%elkt-at-keyword "outgoing"))
            (%elkt-punct-p (%elkt-peek 1) ":"))
       (let* ((incoming (%elkt-at-keyword "incoming"))
              (segs (progn (%elkt-next) (%elkt-next) (%elkt-qualified-id)))
              (raw (%elkt-join segs ".")))
         (let ((stripped (mapcar #'%elkt-unescape-id segs)))
           (%elkt-defer (lambda ()
                          (let ((shape (%elkt-resolve-shape scope stripped raw)))
                            (if incoming
                                (set-incoming-shape section shape)
                                (set-outgoing-shape section shape))))))))
      ((and (or (%elkt-at-keyword "start") (%elkt-at-keyword "end"))
            (%elkt-punct-p (%elkt-peek 1) ":"))
       (let ((start (%elkt-at-keyword "start")))
         (%elkt-next) (%elkt-next)
         (let ((x (%elkt-number)))
           (%elkt-expect-punct ",")
           (let ((y (%elkt-number)))
             (if start
                 (progn (set-start-x section x) (set-start-y section y))
                 (progn (set-end-x section x) (set-end-y section y)))))))
      ((%elkt-at-keyword "bends")
       (%elkt-next) (%elkt-expect-punct ":")
       (loop
         (let ((x (%elkt-number)))
           (%elkt-expect-punct ",")
           (let ((y (%elkt-number)))
             (elk-graph-util.create-bend-point section x y)))
         (if (%elkt-at-punct "|") (%elkt-next) (return))))
      ((%elkt-property-ahead-p) (%elkt-property section))
      (t (%elkt-fail "Unexpected ~A in edge section" (%elkt-describe (%elkt-peek))))))
  (%elkt-expect-punct "]"))

(defun %elkt-edge-section (edge scope)
  "ElkEdgeSection: 'section' ID ('->' ID (',' ID)*)? '[' items ']'"
  (%elkt-expect-keyword "section")
  (let* ((section (elk-graph-util.create-edge-section edge))
         (ident (%elkt-expect-id)))
    (set-identifier section (%elkt-unescape-id ident))
    (when (%elkt-at-punct "->")
      (%elkt-next)
      (loop
        (let* ((raw (%elkt-expect-id)) (target (%elkt-unescape-id raw)))
          (%elkt-defer (lambda ()
                         (add (get-outgoing-sections section)
                              (%elkt-resolve-section edge target)))))
        (if (%elkt-at-punct ",") (%elkt-next) (return))))
    (%elkt-expect-punct "[")
    (%elkt-section-items section scope)
    section))

(defun %elkt-edge-layout (edge scope)
  "EdgeLayout: 'layout' '[' (ElkSingleEdgeSection | ElkEdgeSection+) ']'"
  (%elkt-expect-keyword "layout")
  (%elkt-expect-punct "[")
  (if (%elkt-at-keyword "section")
      (progn
        (loop while (%elkt-at-keyword "section")
              do (%elkt-edge-section edge scope))
        (%elkt-expect-punct "]"))
      ;; ElkSingleEdgeSection: the items are the section's own, and their
      ;; body consumes the closing bracket.
      (%elkt-section-items (elk-graph-util.create-edge-section edge) scope)))

(defun %elkt-label (parent scope)
  "ElkLabel: 'label' (ID ':')? STRING ('{' ShapeLayout? Property* ElkLabel* '}')?"
  (%elkt-expect-keyword "label")
  (let ((ident nil))
    (when (and (eq (elkt-token-kind (%elkt-peek)) :id)
               (%elkt-punct-p (%elkt-peek 1) ":"))
      (setf ident (%elkt-unescape-id (%elkt-expect-id)))
      (%elkt-next))
    (let ((tok (%elkt-next)))
      (unless (eq (elkt-token-kind tok) :string)
        (%elkt-fail "Expected a label text but found ~A" (%elkt-describe tok)))
      (let ((label (elk-graph-util.create-label (%elkt-unescape-string (elkt-token-text tok)) parent)))
        (when ident (set-identifier label ident))
        (when (%elkt-at-punct "{")
          (%elkt-next)
          (%elkt-body label '("layout" "label") #'%elkt-shape-layout scope t))
        label))))

(defun %elkt-port (node scope)
  "ElkPort: 'port' ID ('{' ShapeLayout? Property* ElkLabel* '}')?"
  (%elkt-expect-keyword "port")
  (let ((port (elk-graph-util.create-port node)))
    (set-identifier port (%elkt-unescape-id (%elkt-expect-id)))
    (when (%elkt-at-punct "{")
      (%elkt-next)
      (%elkt-body port '("layout" "label") #'%elkt-shape-layout scope t))
    port))

(defun %elkt-node (container scope)
  "ElkNode: 'node' ID ('{' ... '}')?"
  (%elkt-expect-keyword "node")
  (let ((node (elk-graph-util.create-node container)))
    (set-identifier node (%elkt-unescape-id (%elkt-expect-id)))
    (when (%elkt-at-punct "{")
      (%elkt-next)
      (%elkt-body node '("layout" "label" "port" "node" "edge") #'%elkt-shape-layout scope t))
    node))

(defun %elkt-edge (container)
  "ElkEdge: 'edge' (ID ':')? QualifiedId (',' QualifiedId)* '->' QualifiedId (',' ...)* ('{' EdgeLayout? Property* ElkLabel* '}')?"
  (%elkt-expect-keyword "edge")
  (let ((edge (elk-graph-util.create-edge container))
        (ident nil))
    (when (and (eq (elkt-token-kind (%elkt-peek)) :id)
               (%elkt-punct-p (%elkt-peek 1) ":"))
      (setf ident (%elkt-unescape-id (%elkt-expect-id)))
      (%elkt-next))
    (when ident (set-identifier edge ident))
    (flet ((ends (list-getter)
             (loop
               (let* ((segs (%elkt-qualified-id))
                      (raw (%elkt-join segs "."))
                      (stripped (mapcar #'%elkt-unescape-id segs))
                      (scope container)
                      (getter list-getter))
                 (%elkt-defer (lambda ()
                                (add (funcall getter edge)
                                     (%elkt-resolve-shape scope stripped raw)))))
               (if (%elkt-at-punct ",") (%elkt-next) (return)))))
      (ends (jfun get-sources 0))
      (%elkt-expect-punct "->")
      (ends (jfun get-targets 0)))
    (when (%elkt-at-punct "{")
      (%elkt-next)
      (%elkt-body edge '("layout" "label") #'%elkt-edge-layout container t))
    edge))

(defun %elkt-body (container allowed layout-fn scope closing)
  "The items of a container's braces (CLOSING true) or of the document root
(CLOSING nil).  LAYOUT-FN is the layout rule for this kind of element."
  (loop
    (let ((tok (%elkt-peek)))
      (cond
        ((eq (elkt-token-kind tok) :eof)
         (when closing (%elkt-fail "Expected '}' but found end of input"))
         (return))
        ((%elkt-punct-p tok "}")
         (unless closing (%elkt-fail "Unexpected '}' at ~D" (elkt-token-pos tok)))
         (%elkt-next)
         (return))
        ((and (%elkt-keyword-p tok "layout") (member "layout" allowed :test #'string=))
         (funcall layout-fn container scope))
        ((and (%elkt-keyword-p tok "label") (member "label" allowed :test #'string=))
         (%elkt-label container scope))
        ((and (%elkt-keyword-p tok "port") (member "port" allowed :test #'string=))
         (%elkt-port container scope))
        ((and (%elkt-keyword-p tok "node") (member "node" allowed :test #'string=))
         (%elkt-node container scope))
        ((and (%elkt-keyword-p tok "edge") (member "edge" allowed :test #'string=))
         (%elkt-edge container))
        ((%elkt-property-ahead-p) (%elkt-property container))
        (t (%elkt-fail "Unexpected ~A" (%elkt-describe tok)))))))

(defun %elkt-parse-root ()
  "RootNode returns ElkNode: ('graph' ID)? ShapeLayout? Property* (...)*"
  (let ((root (elk-graph-util.create-node nil)))
    (when (%elkt-at-keyword "graph")
      (%elkt-next)
      (set-identifier root (%elkt-unescape-id (%elkt-expect-id))))
    (%elkt-body root '("layout" "label" "port" "node" "edge") #'%elkt-shape-layout root nil)
    root))

(defun parse-elkt-string (text)
  "Parse an .elkt document; returns its root ElkNode."
  (let ((*elkt-toks* (%elkt-tokenize text))
        (*elkt-pos* 0)
        (*elkt-deferred* (make-array 16 :adjustable t :fill-pointer 0)))
    (let ((root (%elkt-parse-root)))
      (%elkt-run-deferred)
      root)))

(defun parse-elkt-file (path)
  (parse-elkt-string (%gio-read-file path)))
