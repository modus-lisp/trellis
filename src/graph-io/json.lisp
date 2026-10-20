;;;; json.lisp -- a small dependency-free JSON reader/writer standing in for
;;;; Gson (what org.eclipse.elk.graph.json's JsonAdapter wraps).
;;;;
;;;; Representation (documented in NOTES.md, tests rely on it):
;;;;   object  (:object . alist)   string keys, insertion order, MUTABLE
;;;;   array   adjustable vector with fill pointer (a jlist)
;;;;   string  CL string
;;;;   number  integer (no '.', 'e' in the text) or double-float
;;;;   true / false / null   :true / :false / :null
;;;;
;;;; JSON-PARSE has Gson's two modes: strict, and LENIENT (the ElkGraphJson
;;;; default) which accepts comments, unquoted and single-quoted names and
;;;; strings, ';' as a separator and '=' / '=>' as a name separator.

(in-package #:elk)

(define-condition json-io-exception (runtime-exception) ()
  (:documentation "com.google.gson.JsonIOException / MalformedJsonException."))

;;; ------------------------------------------------------------ values

(defun json-object-p (x) (and (consp x) (eq (car x) :object)))
(defun json-array-p (x) (and (vectorp x) (not (stringp x))))
(defun json-primitive-p (x) (or (stringp x) (realp x) (eq x :true) (eq x :false)))
(defun json-null-p (x) (eq x :null))

(defun make-json-object (&rest keys-and-values)
  (cons :object (loop for (k v) on keys-and-values by #'cddr collect (cons k v))))

(defun make-json-array (&rest items)
  (let ((v (new-jlist (max 4 (length items))))) (dolist (i items v) (vector-push-extend i v))))

(defun json-get (obj key)
  "JsonObject.get: the value or NIL if absent (a JSON null is :NULL)."
  (and (json-object-p obj) (cdr (assoc key (cdr obj) :test #'string=))))

(defun json-has (obj key)
  (and (json-object-p obj) (assoc key (cdr obj) :test #'string=) t))

(defun json-put (obj key value)
  "JsonObject.add: replaces an existing member in place, else appends."
  (let ((cell (assoc key (cdr obj) :test #'string=)))
    (if cell
        (setf (cdr cell) value)
        (setf (cdr (last obj)) (list (cons key value))))
    value))

(defun json-remove (obj key)
  (setf (cdr obj) (remove key (cdr obj) :key #'car :test #'string=)))

(defun json-keys (obj) (mapcar #'car (cdr obj)))

(defun json-array-add (arr x) (vector-push-extend x arr) arr)

(defun json-bool (x) (if x :true :false))

(defun json-equal (a b)
  "Gson's JsonElement.equals: structural, object member order irrelevant,
numbers compared by value."
  (cond ((and (json-object-p a) (json-object-p b))
         (and (= (length (cdr a)) (length (cdr b)))
              (every (lambda (cell)
                       (let ((o (assoc (car cell) (cdr b) :test #'string=)))
                         (and o (json-equal (cdr cell) (cdr o)))))
                     (cdr a))))
        ((and (json-array-p a) (json-array-p b))
         (and (= (length a) (length b)) (every #'json-equal a b)))
        ((and (realp a) (realp b))
         (or (= a b) (and (nan-p a) (nan-p b))))
        ((and (stringp a) (stringp b)) (string= a b))
        (t (eq a b))))

;;; ------------------------------------------------------------ numbers as Java prints them

(defun json-double-string (x)
  "Java's Double.toString."
  (let ((x (float x 1d0)))
    (cond ((nan-p x) "NaN")
          ((infinite-p x) (if (> x 0) "Infinity" "-Infinity"))
          ((zerop x) (if (minusp (float-sign x)) "-0.0" "0.0"))
          (t
           (multiple-value-bind (k digits) (sb-impl::flonum-to-digits (abs x))
             (let ((sign (if (< x 0) "-" "")) (n (length digits)) (ax (abs x)))
               (if (and (>= ax 1d-3) (< ax 1d7))
                   (cond ((<= k 0)
                          (format nil "~A0.~A~A" sign (make-string (- k) :initial-element #\0) digits))
                         ((>= k n)
                          (format nil "~A~A~A.0" sign digits (make-string (- k n) :initial-element #\0)))
                         (t (format nil "~A~A.~A" sign (subseq digits 0 k) (subseq digits k))))
                   (format nil "~A~A.~AE~D" sign (char digits 0)
                           (if (> n 1) (subseq digits 1) "0") (1- k)))))))))

(defun json-number-string (x)
  (if (integerp x) (format nil "~D" x) (json-double-string x)))

;;; ------------------------------------------------------------ reading

(defun json-parse (string &key (lenient t))
  "Parse STRING into the representation above.  Malformed input signals
JSON-IO-EXCEPTION.  Trailing content after the first value is ignored (as
Gson's TypeAdapter.read does)."
  (let ((pos 0) (len (length string)))
    (labels ((peek () (and (< pos len) (char string pos)))
             (fail (msg) (jthrow 'json-io-exception "~A at offset ~D" msg pos))
             (need-lenient (what) (unless lenient (fail (format nil "Use lenient mode to accept ~A" what))))
             (ws ()
               (loop
                 (let ((c (peek)))
                   (cond ((null c) (return))
                         ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf pos))
                         ((char= c #\#) (need-lenient "comments") (skip-line))
                         ((and (char= c #\/) (< (1+ pos) len) (char= (char string (1+ pos)) #\/))
                          (need-lenient "comments") (skip-line))
                         ((and (char= c #\/) (< (1+ pos) len) (char= (char string (1+ pos)) #\*))
                          (need-lenient "comments")
                          (let ((e (search "*/" string :start2 (+ pos 2))))
                            (unless e (fail "Unterminated comment"))
                            (setf pos (+ e 2))))
                         (t (return))))))
             (skip-line ()
               (loop while (and (< pos len) (not (member (char string pos) '(#\Newline #\Return))))
                     do (incf pos)))
             (literal-char-p (c)
               (not (or (find c "/\\;#={}[]:,")
                        (member c '(#\Space #\Tab #\Newline #\Return #\Page)))))
             (value ()
               (ws)
               (let ((c (peek)))
                 (case c
                   ((nil) (fail "End of input"))
                   (#\{ (incf pos) (read-object))
                   (#\[ (incf pos) (read-array))
                   (#\" (incf pos) (read-string #\"))
                   (#\' (need-lenient "single quoted strings") (incf pos) (read-string #\'))
                   (t (read-unquoted)))))
             (read-unquoted ()
               (let ((start pos))
                 (loop while (and (< pos len) (literal-char-p (char string pos))) do (incf pos))
                 (when (= start pos) (fail "Expected value"))
                 (let ((text (subseq string start pos)))
                   (cond ((string-equal text "true") (unless (string= text "true") (need-lenient "keywords")) :true)
                         ((string-equal text "false") (unless (string= text "false") (need-lenient "keywords")) :false)
                         ((string-equal text "null") (unless (string= text "null") (need-lenient "keywords")) :null)
                         ((json-number-text-p text) (parse-number text))
                         ((and lenient (member text '("NaN" "Infinity" "-Infinity") :test #'string=))
                          (cond ((string= text "NaN") double.nan)
                                ((string= text "Infinity") double.positive-infinity)
                                (t double.negative-infinity)))
                         (t (need-lenient "unquoted strings") text)))))
             (parse-number (text)
               (if (find-if (lambda (c) (find c ".eE")) text)
                   (with-java-floats
                     (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
                       (let ((v (ignore-errors (read-from-string (substitute #\d #\E (substitute #\d #\e text))))))
                         (unless (realp v) (fail "Bad number"))
                         (float v 1d0))))
                   (parse-integer text)))
             (read-name ()
               (ws)
               (let ((c (peek)))
                 (case c
                   (#\" (incf pos) (read-string #\"))
                   (#\' (need-lenient "single quoted names") (incf pos) (read-string #\'))
                   (t (need-lenient "unquoted names")
                    (let ((start pos))
                      (loop while (and (< pos len) (literal-char-p (char string pos))) do (incf pos))
                      (when (= start pos) (fail "Expected name"))
                      (subseq string start pos))))))
             (name-separator ()
               (ws)
               (let ((c (peek)))
                 (cond ((eql c #\:) (incf pos))
                       ((eql c #\=)
                        (need-lenient "'=' as a name separator")
                        (incf pos)
                        (when (eql (peek) #\>) (incf pos)))
                       (t (fail "Expected ':'")))))
             (read-object ()
               (ws)
               (when (eql (peek) #\}) (incf pos) (return-from read-object (list :object)))
               (let* ((obj (list :object)) (tail obj))
                 (loop
                   (let ((key (read-name)))
                     (name-separator)
                     (let ((v (value)))
                       ;; Gson: a duplicate name replaces the earlier value
                       (let ((cell (assoc key (cdr obj) :test #'string=)))
                         (if cell
                             (setf (cdr cell) v)
                             (setf tail (setf (cdr tail) (list (cons key v))))))))
                   (ws)
                   (case (peek)
                     (#\, (incf pos))
                     (#\; (need-lenient "';' separators") (incf pos))
                     (#\} (incf pos) (return obj))
                     (t (fail "Unterminated object")))
                   (ws)
                   (when (and lenient (eql (peek) #\})) (incf pos) (return obj)))))
             (read-array ()
               (ws)
               (let ((acc (new-jlist)))
                 (when (eql (peek) #\]) (incf pos) (return-from read-array acc))
                 (loop
                   (vector-push-extend (value) acc)
                   (ws)
                   (case (peek)
                     (#\, (incf pos))
                     (#\; (need-lenient "';' separators") (incf pos))
                     (#\] (incf pos) (return acc))
                     (t (fail "Unterminated array")))
                   (ws)
                   (when (and lenient (eql (peek) #\])) (incf pos) (return acc)))))
             (hex4 ()
               (when (> (+ pos 4) len) (fail "Unterminated escape sequence"))
               (prog1 (or (ignore-errors (parse-integer string :start pos :end (+ pos 4) :radix 16))
                          (fail "Malformed Unicode escape"))
                 (incf pos 4)))
             (read-string (quote)
               (with-output-to-string (out)
                 (loop
                   (when (>= pos len) (fail "Unterminated string"))
                   (let ((c (char string pos)))
                     (incf pos)
                     (cond ((char= c quote) (return))
                           ((char= c #\\)
                            (when (>= pos len) (fail "Unterminated escape sequence"))
                            (let ((e (char string pos)))
                              (incf pos)
                              (case e
                                (#\n (write-char #\Newline out))
                                (#\t (write-char #\Tab out))
                                (#\r (write-char #\Return out))
                                (#\b (write-char #\Backspace out))
                                (#\f (write-char #\Page out))
                                (#\u (let ((code (hex4)))
                                       (when (and (<= #xD800 code #xDBFF)
                                                  (< (+ pos 1) len)
                                                  (char= (char string pos) #\\)
                                                  (char= (char string (1+ pos)) #\u))
                                         (incf pos 2)
                                         (let ((lo (hex4)))
                                           (setf code (+ #x10000 (ash (- code #xD800) 10) (- lo #xDC00)))))
                                       (write-char (code-char code) out)))
                                (t (write-char e out)))))
                           (t (write-char c out))))))))
      (value))))

(defun json-number-text-p (text)
  "Does TEXT read as a JSON number (-?digits(.digits)?([eE][+-]?digits)?)."
  (let ((i 0) (n (length text)))
    (flet ((digits () (let ((s i)) (loop while (and (< i n) (digit-char-p (char text i))) do (incf i)) (> i s))))
      (when (and (< i n) (char= (char text i) #\-)) (incf i))
      (and (digits)
           (progn (when (and (< i n) (char= (char text i) #\.))
                    (incf i) (unless (digits) (return-from json-number-text-p nil)))
                  t)
           (progn (when (and (< i n) (char-equal (char text i) #\e))
                    (incf i)
                    (when (and (< i n) (find (char text i) "+-")) (incf i))
                    (unless (digits) (return-from json-number-text-p nil)))
                  t)
           (= i n)))))

(defun json-parse-file (path &key (lenient t))
  (json-parse (%gio-read-file path) :lenient lenient))

(defun %gio-read-file (path)
  (with-open-file (s path :external-format :utf-8)
    (let* ((buf (make-string (file-length s)))
           (n (read-sequence buf s)))
      (subseq buf 0 n))))

;;; ------------------------------------------------------------ writing (Gson style)

(defun %json-write-string (s out)
  (write-char #\" out)
  (loop for c across s
        do (case c
             (#\" (write-string "\\\"" out))
             (#\\ (write-string "\\\\" out))
             (#\Newline (write-string "\\n" out))
             (#\Return (write-string "\\r" out))
             (#\Tab (write-string "\\t" out))
             (#\Backspace (write-string "\\b" out))
             (#\Page (write-string "\\f" out))
             (t (if (or (< (char-code c) 32) (= (char-code c) #x2028) (= (char-code c) #x2029))
                    (format out "\\u~(~4,'0X~)" (char-code c))
                    (write-char c out)))))
  (write-char #\" out))

(defun json-write (value &key stream pretty)
  "Serialize VALUE as Gson does (compact, or 2-space PRETTY printing).
STREAM NIL returns a string."
  (if (null stream)
      (with-output-to-string (s) (json-write value :stream s :pretty pretty))
      (labels ((nl (depth)
                 (when pretty
                   (terpri stream)
                   (loop repeat (* 2 depth) do (write-char #\Space stream))))
               (emit (v depth)
                 (cond ((eq v :true) (write-string "true" stream))
                       ((eq v :false) (write-string "false" stream))
                       ((or (eq v :null) (null v)) (write-string "null" stream))
                       ((eq v t) (write-string "true" stream))
                       ((stringp v) (%json-write-string v stream))
                       ((realp v) (write-string (json-number-string v) stream))
                       ((json-object-p v)
                        (if (null (cdr v))
                            (write-string "{}" stream)
                            (progn
                              (write-char #\{ stream)
                              (loop for (cell . more) on (cdr v)
                                    do (nl (1+ depth))
                                       (%json-write-string (car cell) stream)
                                       (write-string (if pretty ": " ":") stream)
                                       (emit (cdr cell) (1+ depth))
                                       (when more (write-char #\, stream)))
                              (nl depth)
                              (write-char #\} stream))))
                       ((json-array-p v)
                        (if (zerop (length v))
                            (write-string "[]" stream)
                            (progn
                              (write-char #\[ stream)
                              (loop for i from 0 below (length v)
                                    do (nl (1+ depth))
                                       (emit (aref v i) (1+ depth))
                                       (when (< i (1- (length v))) (write-char #\, stream)))
                              (nl depth)
                              (write-char #\] stream))))
                       (t (jthrow 'json-io-exception "Cannot write ~S as JSON" v)))))
        (emit value 0)
        value)))
