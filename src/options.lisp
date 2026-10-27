;;;; options.lisp -- layout options.
;;;;
;;;; Options are keywords.  An ELK option id maps onto one mechanically: drop
;;;; "org.eclipse.elk." and an algorithm segment ("layered.", "mrtree." ...),
;;;; then split camelCase and dots into hyphens, so
;;;;   org.eclipse.elk.layered.spacing.edgeEdgeBetweenLayers
;;;;   -> :spacing-edge-edge-between-layers
;;;; Values parse the same way: "FIXED_SIDE" -> :fixed-side, "[H_LEFT, V_TOP]"
;;;; -> (:h-left :v-top), "[top=1,left=2,...]" -> a PAD, "(1,2)" -> (1d0 . 2d0),
;;;; numbers -> numbers, "true"/"false" -> T/NIL.  Defaults are not stored here:
;;;; each algorithm states its own where it reads an option.

(in-package #:trellis)

(defparameter *algorithm-segments*
  '("layered" "mrtree" "force" "stress" "radial" "rectpacking" "disco" "spore"
    "sporeOverlap" "sporeCompaction" "topdownpacking" "vertiflex" "box" "fixed"
    "random" "alg" "common" "core" "graphviz" "conn" "elk" "json" "libavoid" "trellis"))

(defun lispify (string)
  "camelCase.dotted_NAME -> camel-case-dotted-name"
  (with-output-to-string (out)
    (loop with prev = nil
          for c across string
          do (cond ((member c '(#\. #\_ #\- #\Space))
                    (unless (or (null prev) (eql prev #\-)) (write-char #\- out))
                    (setf prev #\-))
                   ((and (upper-case-p c) prev (not (eql prev #\-))
                         (or (lower-case-p prev) (digit-char-p prev)))
                    (write-char #\- out) (write-char (char-downcase c) out)
                    (setf prev c))
                   (t (write-char (char-downcase c) out) (setf prev c))))))

(defun option-key (id)
  "The keyword for an ELK option id or suffix, or for a Lisp name."
  (etypecase id
    (keyword id)
    (symbol (intern (lispify (symbol-name id)) :keyword))
    (string
     (let ((s id))
       (when (and (> (length s) 16) (string= "org.eclipse.elk." s :end2 16))
         (setf s (subseq s 16)))
       (loop for dot = (position #\. s)
             while (and dot (member (subseq s 0 dot) *algorithm-segments* :test #'string=))
             do (setf s (subseq s (1+ dot))))
       (intern (string-upcase (lispify s)) :keyword)))))

(defun enum-keyword (string)
  (let ((s (string-trim " " string)))
    (when (and (> (length s) 16) (string= "org.eclipse.elk." s :end2 16))
      (setf s (subseq s 16)))
    (intern (string-upcase (lispify s)) :keyword)))

(defun parse-number (s)
  "A number from a JSON-ish string, or NIL."
  (let ((s (string-trim " " s)))
    (cond ((string-equal s "NaN") nil)
          ((string-equal s "Infinity") most-positive-double-float)
          ((string-equal s "-Infinity") most-negative-double-float)
          ((and (plusp (length s))
                (every (lambda (c) (or (digit-char-p c) (find c "+-.eE"))) s)
                (some #'digit-char-p s))
           (handler-case
               (let ((*read-default-float-format* 'double-float))
                 (let ((v (with-standard-io-syntax
                            (let ((*read-default-float-format* 'double-float)
                                  (*read-eval* nil))
                              (read-from-string s)))))
                   (and (realp v) (if (integerp v) v (d v)))))
             (error () nil))))))

(defun parse-pad (s)
  "\"[top=1.0,left=2.0,bottom=3.0,right=4.0]\" -> PAD; missing sides are 0."
  (let ((p (pad)))
    (dolist (part (split-string (string-trim "[] " s) #\,) p)
      (let* ((eq (position #\= part))
             (k (and eq (string-trim " " (subseq part 0 eq))))
             (v (and eq (parse-number (subseq part (1+ eq))))))
        (when (and k v)
          (let ((v (sane v)))
            (cond ((string-equal k "top") (setf (pad-top p) v))
                  ((string-equal k "left") (setf (pad-left p) v))
                  ((string-equal k "bottom") (setf (pad-bottom p) v))
                  ((string-equal k "right") (setf (pad-right p) v)))))))))

(defun split-string (s char)
  (loop with start = 0
        for pos = (position char s :start start)
        collect (subseq s start pos)
        while pos do (setf start (1+ pos))))

(defun parse-option-value (v)
  "Parse an option value as it comes from JSON or from Lisp."
  (cond ((not (stringp v)) (if (and (realp v) (not (integerp v))) (d v) v))
        (t (let ((s (string-trim " " v)))
             (cond ((string-equal s "true") t)
                   ((string-equal s "false") nil)
                   ((parse-number s))
                   ((and (plusp (length s)) (char= (char s 0) #\[) (find #\= s))
                    (parse-pad s))
                   ((and (plusp (length s)) (char= (char s 0) #\[))
                    (loop for part in (split-string (string-trim "[]" s) #\,)
                          for x = (string-trim " " part)
                          unless (string= x "") collect (enum-keyword x)))
                   ((and (plusp (length s)) (char= (char s 0) #\()
                         (= 2 (length (split-string s #\,))))
                    (let ((xy (split-string (string-trim "() " s) #\,)))
                      (pt (sane (parse-number (first xy))) (sane (parse-number (second xy))))))
                   ((and (plusp (length s))
                         (every (lambda (c) (or (alphanumericp c) (find c "_."))) s)
                         (or (search "org.eclipse.elk." s)
                             (every (lambda (c) (or (upper-case-p c) (digit-char-p c) (char= c #\_))) s)))
                    (enum-keyword s))
                   (t v))))))

(defun set-option (element key value)
  "Set a layout option on ELEMENT.  KEY is a keyword or an ELK option id."
  (let ((k (option-key key)))
    (unless (element-options element)
      (setf (element-options element) (make-hash-table :test 'eq)))
    (setf (gethash k (element-options element)) (parse-option-value value))
    value))

(defvar *inherited-options* nil
  "During layout: element -> hash of options it takes from an ancestor.")

(defun option (element key &optional default)
  "The value of option KEY on ELEMENT, or DEFAULT when it is not set."
  (let ((k (option-key key)))
    (multiple-value-bind (v found)
        (let ((h (element-options element))) (if h (gethash k h) (values nil nil)))
      (if found
          v
          (let ((inh (and *inherited-options* (gethash element *inherited-options*))))
            (multiple-value-bind (v2 found2) (if inh (gethash k inh) (values nil nil))
              (if found2 v2 default)))))))

(defun option-set-p (element key)
  (let ((h (element-options element)))
    (and h (nth-value 1 (gethash (option-key key) h)))))

;;; Typed readers: an option of the wrong shape counts as unset.

(defun opt-num (element key default &key (min nil) (max nil))
  (let ((v (option element key)))
    (let ((x (if (and (realp v) (finite-p v)) (d v) (d default))))
      (when min (setf x (max (d min) x)))
      (when max (setf x (min (d max) x)))
      x)))

(defun opt-int (element key default &key (min nil) (max nil))
  (let* ((v (option element key))
         (x (if (and (realp v) (finite-p v)) (round v) default)))
    (when min (setf x (max min x)))
    (when max (setf x (min max x)))
    x))

(defun opt-bool (element key default)
  (let ((v (option element key :unset)))
    (if (eq v :unset) default (and v t))))

(defun opt-enum (element key default &optional allowed)
  (let ((v (option element key)))
    (if (and (keywordp v) (or (null allowed) (member v allowed))) v default)))

(defun opt-set (element key default)
  (let ((v (option element key :unset)))
    (cond ((eq v :unset) default)
          ((listp v) v)
          ((keywordp v) (list v))
          (t default))))

(defun opt-pad (element key default)
  (let ((v (option element key)))
    (if (pad-p v) v default)))

(defun options-of (element)
  "ELEMENT's options as a plist."
  (let ((h (element-options element)))
    (and h (loop for k being the hash-keys of h using (hash-value v) collect k collect v))))

(defun describe-option (key)
  "The keyword an ELK option id or name maps to."
  (option-key key))
