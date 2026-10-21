;;;; comments.lisp -- org.eclipse.elk.core.comments (matchers, filters, bounds and
;;;; data providers, the decider and CommentAttacher) plus the default methods of
;;;; the IBoundsProvider / IDataProvider / IMatcher / IFilter interfaces.
;;;;
;;;; Java lambdas are accepted where the Java API takes a functional interface:
;;;; a CL function is called directly, anything else is an object dispatched on
;;;; by name (see %CU-EXPLICIT-ATTACHMENT and friends).

(in-package #:elk)

;;; ------------------------------------------------------------ small helpers

(defun %cu-require-non-null (x message)
  (when (null x) (jthrow 'null-pointer-exception message))
  x)

(defun %cu-string-null-or-empty-p (s)
  "Strings.isNullOrEmpty"
  (or (null s) (zerop (length s))))

;;; Interface defaults.  Lambda-or-object dispatch for the functional interfaces.

(defun %cu-explicit-attachment (provider comment)
  (if (functionp provider)
      (funcall provider comment)
      (find-explicit-attachment provider comment)))

(defun %cu-filter-eligible (filter comment)
  (if (functionp filter)
      (funcall filter comment)
      (eligible-for-attachment filter comment)))

(defun %cu-decide (decider normalized-heuristics)
  (if (functionp decider)
      (funcall decider normalized-heuristics)
      (make-attachment-decision decider normalized-heuristics)))

(defun %cu-preprocess-optional (component data-provider include-hierarchy)
  (unless (functionp component)
    (preprocess component data-provider include-hierarchy)))

(defun %cu-cleanup-optional (component)
  (unless (functionp component)
    (cleanup component)))

;;; ------------------------------------------------------------ interface defaults

(defjmethod preprocess ((this i-matcher) data-provider include-hierarchy)
  nil)
(defjmethod cleanup ((this i-matcher)) nil)

(defjmethod preprocess ((this i-filter) data-provider include-hierarchy)
  nil)
(defjmethod cleanup ((this i-filter)) nil)

(defjgeneric find-explicit-attachment 1)

(defjmethod preprocess ((this i-explicit-attachment-provider) data-provider include-hierarchy)
  nil)
(defjmethod cleanup ((this i-explicit-attachment-provider)) nil)

(defjmethod preprocess ((this i-bounds-provider) data-provider include-hierarchy)
  nil)
(defjmethod cleanup ((this i-bounds-provider)) nil)

(defjmethod provide-targets-for ((this i-data-provider) comment)
  (provide-targets this))

;;; IBoundsProvider.cached(): a caching wrapper around this provider.
(defjmethod cached ((this i-bounds-provider))
  (let ((wrapper (new-%cu-cached-bounds-provider/0)))
    (setf (.delegate wrapper) this)
    wrapper))

(defjmethod bounds-for-comment ((this %cu-cached-bounds-provider) comment)
  (multiple-value-bind (cached found) (gethash comment (.comment-bounds-cache this))
    (if found
        cached
        (let ((bounds (bounds-for-comment (.delegate this) comment)))
          (setf (gethash comment (.comment-bounds-cache this)) bounds)
          bounds))))

(defjmethod bounds-for-target ((this %cu-cached-bounds-provider) target)
  (multiple-value-bind (cached found) (gethash target (.target-bounds-cache this))
    (if found
        cached
        (let ((bounds (bounds-for-target (.delegate this) target)))
          (setf (gethash target (.target-bounds-cache this)) bounds)
          bounds))))

(defjmethod preprocess ((this %cu-cached-bounds-provider) data-provider include-hierarchy)
  (preprocess (.delegate this) data-provider include-hierarchy)
  nil)

(defjmethod cleanup ((this %cu-cached-bounds-provider))
  (clrhash (.comment-bounds-cache this))
  (clrhash (.target-bounds-cache this))
  (cleanup (.delegate this))
  nil)

;;; IDataProvider.cached()
(defjmethod cached ((this i-data-provider))
  (let ((wrapper (new-%cu-cached-data-provider/0)))
    (setf (.delegate wrapper) this)
    wrapper))

(defjmethod provide-comments ((this %cu-cached-data-provider))
  (or (.comments-cache this)
      (let ((cache (new-jlist)))
        (jl-add-all cache (provide-comments (.delegate this)))
        (setf (.comments-cache this) cache))))

(defjmethod provide-targets ((this %cu-cached-data-provider))
  (or (.targets-cache this)
      (let ((cache (new-jlist)))
        (jl-add-all cache (provide-targets (.delegate this)))
        (setf (.targets-cache this) cache))))

(defjmethod provide-targets-for ((this %cu-cached-data-provider) comment)
  ;; A HashMultimap: a key exists only while it has values, and values are a set.
  (let ((stored (gethash comment (.comment-targets-cache this))))
    (if (and stored (plusp (length stored)))
        (jlist-from stored)
        (let ((targets (provide-targets-for (.delegate this) comment)))
          (jdo (target targets)
            (unless (member target (gethash comment (.comment-targets-cache this)) :test #'eq)
              (setf (gethash comment (.comment-targets-cache this))
                    (append (gethash comment (.comment-targets-cache this)) (list target)))))
          targets))))

(defjmethod provide-sub-hierarchies ((this %cu-cached-data-provider))
  (or (.sub-provider-cache this)
      (let ((cache (new-jlist)))
        (jdo (provider (provide-sub-hierarchies (.delegate this)))
          (jl-add cache (cached provider)))
        (setf (.sub-provider-cache this) cache))))

(defjmethod attach ((this %cu-cached-data-provider) comment target)
  (attach (.delegate this) comment target)
  nil)

;;; ------------------------------------------------------------ java.awt.geom.Rectangle2D.Double

(defjconst rectangle2-d-double.out-left 1)
(defjconst rectangle2-d-double.out-top 2)
(defjconst rectangle2-d-double.out-right 4)
(defjconst rectangle2-d-double.out-bottom 8)

(defjctor rectangle2-d-double (x y width height)
  (setf (.x this) x (.y this) y (.width this) width (.height this) height))

(defun %cu-rect-empty-p (r)
  (or (<= (.width r) 0) (<= (.height r) 0)))

(defjmethod outcode ((this rectangle2-d-double) x y)
  "Rectangle2D.outcode(x, y)."
  (let ((out 0))
    (cond ((<= (.width this) 0)
           (setf out (logior out rectangle2-d-double.out-left rectangle2-d-double.out-right)))
          ((< x (.x this))
           (setf out (logior out rectangle2-d-double.out-left)))
          ((> x (+ (.x this) (.width this)))
           (setf out (logior out rectangle2-d-double.out-right))))
    (cond ((<= (.height this) 0)
           (setf out (logior out rectangle2-d-double.out-top rectangle2-d-double.out-bottom)))
          ((< y (.y this))
           (setf out (logior out rectangle2-d-double.out-top)))
          ((> y (+ (.y this) (.height this)))
           (setf out (logior out rectangle2-d-double.out-bottom))))
    out))

(defjmethod intersects ((this rectangle2-d-double) other)
  "Rectangle2D.intersects(Rectangle2D)."
  (let ((x (.x other)) (y (.y other)) (w (.width other)) (h (.height other)))
    (cond ((or (%cu-rect-empty-p this) (<= w 0) (<= h 0)) nil)
          (t (and (> (+ x w) (.x this))
                  (> (+ y h) (.y this))
                  (< x (+ (.x this) (.width this)))
                  (< y (+ (.y this) (.height this))))))))

;;; ------------------------------------------------------------ AbstractNormalizedMatcher

(defjmethod with-bounds ((this abstract-normalized-matcher) worst-raw-val best-raw-val)
  (setf (.worst-raw-value this) worst-raw-val
        (.best-raw-value this) best-raw-val)
  this)

(defjmethod with-normalization-function ((this abstract-normalized-matcher) fn)
  (%cu-require-non-null fn "Normalization function cannot be null.")
  (setf (.normalization-function this) fn)
  this)

(defjmethod get-worst-raw-value ((this abstract-normalized-matcher))
  (.worst-raw-value this))

(defjmethod get-best-raw-value ((this abstract-normalized-matcher))
  (.best-raw-value this))

(defjmethod normalized ((this abstract-normalized-matcher) comment element)
  (let ((raw-value (raw this comment element)))
    (cond ((eq (.normalization-function this) normalization-function.linear)
           (normalize-linear this raw-value))
          ((eq (.normalization-function this) normalization-function.binary)
           (normalize-binary this raw-value))
          (t 0d0))))

(defjmethod normalize-linear ((this abstract-normalized-matcher) raw-value)
  (let ((worst (.worst-raw-value this))
        (best (.best-raw-value this)))
    (cond ((< worst best)
           (cond ((<= raw-value worst) 0d0)
                 ((>= raw-value best) 1d0)
                 (t (/ (- raw-value worst) (- best worst)))))
          ((< best worst)
           (cond ((<= raw-value best) 1d0)
                 ((>= raw-value worst) 0d0)
                 (t (- 1 (/ (- raw-value best) (- worst best))))))
          (t (if (= raw-value best) 1d0 0d0)))))

(defjmethod normalize-binary ((this abstract-normalized-matcher) raw-value)
  (let ((worst (.worst-raw-value this))
        (best (.best-raw-value this)))
    (cond ((< worst best) (if (<= raw-value worst) 0d0 1d0))
          ((< best worst) (if (>= raw-value worst) 0d0 1d0))
          (t (if (= raw-value best) 1d0 0d0)))))

;;; ------------------------------------------------------------ AlignmentMatcher

(defjconst alignment-matcher.top rectangle2-d-double.out-top)
(defjconst alignment-matcher.left rectangle2-d-double.out-left)
(defjconst alignment-matcher.right rectangle2-d-double.out-right)
(defjconst alignment-matcher.bottom rectangle2-d-double.out-bottom)
(defjconst alignment-matcher.top-left (logior rectangle2-d-double.out-top rectangle2-d-double.out-left))
(defjconst alignment-matcher.bottom-left (logior rectangle2-d-double.out-bottom rectangle2-d-double.out-left))
(defjconst alignment-matcher.top-right (logior rectangle2-d-double.out-top rectangle2-d-double.out-right))
(defjconst alignment-matcher.bottom-right (logior rectangle2-d-double.out-bottom rectangle2-d-double.out-right))

(defjmethod with-maximum-alignment-offset ((this alignment-matcher) offset)
  (when (<= offset 0)
    (jthrow 'illegal-argument-exception "Maximum alignment offset must be > 0."))
  (with-bounds this offset 0d0)
  this)

(defjmethod with-bounds-provider ((this alignment-matcher) provider)
  (%cu-require-non-null provider "Bounds provider must not be null.")
  (setf (.bounds-provider this) provider)
  this)

(defjmethod check-configuration ((this alignment-matcher))
  (when (null (.bounds-provider this))
    (jthrow 'illegal-state-exception "A bounds provider is required.")))

(defjmethod preprocess ((this alignment-matcher) data-provider include-hierarchy)
  (call-next-method this data-provider include-hierarchy)
  (check-configuration this))

(defjmethod raw ((this alignment-matcher) comment target)
  (let* ((comment-bounds (bounds-for-comment (.bounds-provider this) comment))
         (node-bounds (bounds-for-target (.bounds-provider this) target))
         (alignment (alignment-matcher.alignment comment-bounds node-bounds)))
    (if (= alignment -1) (get-worst-raw-value this) alignment)))

(define-jcall alignment-matcher.alignment :static)
(defun alignment-matcher.alignment/2 (bounds1 bounds2)
  (block alignment-matcher.alignment/2
  (let* ((top-left-outcode (outcode bounds2 (.x bounds1) (.y bounds1)))
         (bottom-right-outcode (outcode bounds2 (+ (.x bounds1) (.width bounds1))
                                        (+ (.y bounds1) (.height bounds1)))))
    (if (or (= (logand bottom-right-outcode top-left-outcode alignment-matcher.top-left)
               alignment-matcher.top-left)
            (= (logand bottom-right-outcode top-left-outcode alignment-matcher.top-right)
               alignment-matcher.top-right)
            (= (logand bottom-right-outcode top-left-outcode alignment-matcher.bottom-left)
               alignment-matcher.bottom-left)
            (= (logand bottom-right-outcode top-left-outcode alignment-matcher.bottom-right)
               alignment-matcher.bottom-right))
        -1d0
        (let ((horizontal-offset
                (math.min (abs (- (.x bounds2) (.x bounds1)))
                          (abs (- (- (+ (.x bounds2) (.width bounds2)) (.x bounds1)) (.width bounds1)))))
              (vertical-offset
                (math.min (abs (- (.y bounds2) (.y bounds1)))
                          (abs (- (- (+ (.y bounds2) (.height bounds2)) (.y bounds1)) (.height bounds1))))))
          (block alignment
            (when (intersects bounds2 bounds1)
              (return-from alignment (math.min horizontal-offset vertical-offset)))
            (cond ((or (/= (logand bottom-right-outcode alignment-matcher.top) 0)
                       (/= (logand top-left-outcode alignment-matcher.bottom) 0))
                   (return-from alignment horizontal-offset))
                  ((or (/= (logand bottom-right-outcode alignment-matcher.left) 0)
                       (/= (logand top-left-outcode alignment-matcher.right) 0))
                   (return-from alignment vertical-offset))
                  ((or (= (.y bounds1) (+ (.y bounds2) (.height bounds2)))
                       (= (+ (.y bounds1) (.height bounds1)) (.y bounds2)))
                   (return-from alignment horizontal-offset))
                  ((or (= (.x bounds1) (+ (.x bounds2) (.width bounds2)))
                       (= (+ (.x bounds1) (.width bounds1)) (.x bounds2)))
                   (return-from alignment vertical-offset)))
            -1d0))))))

;;; ------------------------------------------------------------ DistanceMatcher

(defjconst distance-matcher.top rectangle2-d-double.out-top)
(defjconst distance-matcher.left rectangle2-d-double.out-left)
(defjconst distance-matcher.right rectangle2-d-double.out-right)
(defjconst distance-matcher.bottom rectangle2-d-double.out-bottom)
(defjconst distance-matcher.top-left (logior rectangle2-d-double.out-top rectangle2-d-double.out-left))
(defjconst distance-matcher.bottom-left (logior rectangle2-d-double.out-bottom rectangle2-d-double.out-left))
(defjconst distance-matcher.top-right (logior rectangle2-d-double.out-top rectangle2-d-double.out-right))
(defjconst distance-matcher.bottom-right (logior rectangle2-d-double.out-bottom rectangle2-d-double.out-right))

(defjmethod with-maximum-attachment-distance ((this distance-matcher) distance)
  (when (< distance 0)
    (jthrow 'illegal-argument-exception "Maximum attachment distance must be >= 0."))
  (with-bounds this distance 0d0)
  this)

(defjmethod with-bounds-provider ((this distance-matcher) provider)
  (%cu-require-non-null provider "Bounds provider must not be null.")
  (setf (.bounds-provider this) provider)
  this)

(defjmethod check-configuration ((this distance-matcher))
  (when (null (.bounds-provider this))
    (jthrow 'illegal-state-exception "A bounds provider is required.")))

(defjmethod preprocess ((this distance-matcher) data-provider include-hierarchy)
  (call-next-method this data-provider include-hierarchy)
  (check-configuration this))

(defjmethod raw ((this distance-matcher) comment target)
  (let* ((comment-bounds (bounds-for-comment (.bounds-provider this) comment))
         (node-bounds (bounds-for-target (.bounds-provider this) target))
         (distance (distance-matcher.distance comment-bounds node-bounds)))
    (if (= distance -1) (get-worst-raw-value this) distance)))

(define-jcall distance-matcher.distance :static)
(defun distance-matcher.distance/2 (bounds1 bounds2)
  (block distance-matcher.distance/2
  (when (intersects bounds1 bounds2)
    (return-from distance-matcher.distance/2 0d0))
  (let* ((top-left-outcode (outcode bounds2 (.x bounds1) (.y bounds1)))
         (bottom-right-outcode (outcode bounds2 (+ (.x bounds1) (.width bounds1))
                                        (+ (.y bounds1) (.height bounds1))))
         (x1 (.x bounds1)) (y1 (.y bounds1)) (w1 (.width bounds1)) (h1 (.height bounds1))
         (x2 (.x bounds2)) (y2 (.y bounds2)) (w2 (.width bounds2)) (h2 (.height bounds2)))
    (flet ((both (mask) (= (logand top-left-outcode mask) mask)))
      (cond ((and (both distance-matcher.top-left)
                  (= (logand bottom-right-outcode distance-matcher.top-left) distance-matcher.top-left))
             (distance-matcher.distance (+ x1 w1) (+ y1 h1) x2 y2))
            ((and (both distance-matcher.bottom-left)
                  (= (logand bottom-right-outcode distance-matcher.bottom-left) distance-matcher.bottom-left))
             (distance-matcher.distance (+ x1 w1) y1 x2 (+ y2 h2)))
            ((and (both distance-matcher.top-right)
                  (= (logand bottom-right-outcode distance-matcher.top-right) distance-matcher.top-right))
             (distance-matcher.distance x1 (+ y1 h1) (+ x2 w2) y2))
            ((and (both distance-matcher.bottom-right)
                  (= (logand bottom-right-outcode distance-matcher.bottom-right) distance-matcher.bottom-right))
             (distance-matcher.distance x1 y1 (+ x2 w2) (+ y2 h2)))
            ((and (/= (logand top-left-outcode distance-matcher.left) 0)
                  (/= (logand bottom-right-outcode distance-matcher.left) 0))
             (+ (- x2 x1) w1))
            ((and (/= (logand top-left-outcode distance-matcher.right) 0)
                  (/= (logand bottom-right-outcode distance-matcher.right) 0))
             (- (- x1 x2) w2))
            ((and (/= (logand top-left-outcode distance-matcher.top) 0)
                  (/= (logand bottom-right-outcode distance-matcher.top) 0))
             (+ (- y2 y1) h1))
            ((and (/= (logand top-left-outcode distance-matcher.bottom) 0)
                  (/= (logand bottom-right-outcode distance-matcher.bottom) 0))
             (- (- y1 y2) h2))
            (t
             (block nil
               (when (and (<= x2 (+ x1 w1)) (>= (+ x2 w2) x1))
                 (when (or (= (+ y2 h2) y1) (= y2 (+ y1 h1)))
                   (return 0d0)))
               (when (and (not (and (<= x2 (+ x1 w1)) (>= (+ x2 w2) x1)))
                          (<= y2 (+ y1 h1)) (>= (+ y2 h2) y1))
                 (when (or (= (+ x2 w2) x1) (= x2 (+ x1 w1)))
                   (return 0d0)))
               -1d0)))))))

(define-jcall distance-matcher.distance :static)
(defun distance-matcher.distance/4 (x1 y1 x2 y2)
  (block distance-matcher.distance/4
  (let ((delta-x (- x2 x1))
        (delta-y (- y2 y1)))
    (math.sqrt (+ (* delta-x delta-x) (* delta-y delta-y))))))

;;; ------------------------------------------------------------ AggregatedMatchDecider

(defun %cu-agg-max (values)
  "AggregatedMatchDecider.max: Collections.max."
  (when (zerop (length values))
    (jthrow 'no-such-element-exception "empty collection"))
  (reduce #'max values))

(defun %cu-agg-min (values)
  (when (zerop (length values))
    (jthrow 'no-such-element-exception "empty collection"))
  (reduce #'min values))

(defun %cu-kahan-sum (values)
  "Collectors.summingDouble / averagingDouble: compensated summation as in the JDK.
Returns (values final-sum count)."
  (let ((sum 0d0) (compensation 0d0) (simple-sum 0d0) (count 0))
    (jdo (v values)
      (incf count)
      (setf simple-sum (+ simple-sum v))
      (let* ((tmp (- v compensation))
             (velvel (+ sum tmp)))
        (setf compensation (- (- velvel sum) tmp))
        (setf sum velvel)))
    (let ((tmp (- sum compensation)))
      (values (if (and (nan-p tmp) (infinite-p simple-sum)) simple-sum tmp)
              count))))

(defun %cu-agg-avg (values)
  (multiple-value-bind (total count) (%cu-kahan-sum values)
    (if (zerop count) 0d0 (/ total count))))

(defun %cu-agg-sum (values)
  (values (%cu-kahan-sum values)))

(defjmethod with-aggregator ((this aggregated-match-decider) f)
  (%cu-require-non-null f "Aggregator cannot be null.")
  (setf (.aggregator this) f)
  this)

(defjmethod with-lower-attachment-boundary ((this aggregated-match-decider) lower)
  (when (< lower 0)
    (jthrow 'illegal-argument-exception "Lower boundary must be >= 0."))
  (setf (.lower-boundary this) lower)
  this)

(defjmethod with-lower-boundary-included ((this aggregated-match-decider) include)
  (setf (.include-lower-boundary this) include)
  this)

(defjmethod make-attachment-decision ((this aggregated-match-decider) normalized-heuristics)
  (let ((max double.negative-infinity)
        (max-target nil))
    (maphash (lambda (target heuristics)
               (let ((aggregate (funcall (.aggregator this)
                                         (jlist-from (loop for v being the hash-values of heuristics
                                                           collect v)))))
                 (when (< aggregate 0)
                   (jthrow 'illegal-state-exception "The aggregator provided a value < 0."))
                 (when (> aggregate max)
                   (setf max aggregate
                         max-target target))))
             normalized-heuristics)
    (if (.include-lower-boundary this)
        (if (>= max (.lower-boundary this)) max-target nil)
        (if (> max (.lower-boundary this)) max-target nil))))

(define-jcall aggregated-match-decider.max :static)
(defun aggregated-match-decider.max/1 (values) (%cu-agg-max values))
(define-jcall aggregated-match-decider.min :static)
(defun aggregated-match-decider.min/1 (values) (%cu-agg-min values))
(define-jcall aggregated-match-decider.avg :static)
(defun aggregated-match-decider.avg/1 (values) (%cu-agg-avg values))
(define-jcall aggregated-match-decider.sum :static)
(defun aggregated-match-decider.sum/1 (values) (%cu-agg-sum values))

;;; ------------------------------------------------------------ text matching
;;;
;;; ELK builds two kinds of java.util.regex patterns for node names.  The
;;; runtime has no regex engine, so these are matched directly.  Both are
;;; anchored by \b on each side.
;;;   strict: \b<quoted name>\b, case-sensitive.
;;;   fuzzy:  \b<seg1>[\h\v]*<seg2>...\b, case-insensitive (see FUZZY-SEGMENTS).

(defun %cu-regex-word-char-p (c)
  (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9) (char= c #\_)))

(defun %cu-regex-space-p (c)
  "[\\h\\v]: horizontal and vertical whitespace."
  (let ((code (char-code c)))
    (or (= code 32) (= code 9) (= code 10) (= code 11) (= code 12) (= code 13)
        (= code #xA0) (= code #x85) (= code #x1680) (= code #x180E)
        (<= #x2000 code #x200A) (= code #x2028) (= code #x2029)
        (= code #x202F) (= code #x205F) (= code #x3000))))

(defun %cu-java-whitespace-p (c)
  "Character.isWhitespace."
  (let ((code (char-code c)))
    (or (<= 9 code 13) (<= 28 code 32)
        (and (> code 127) (%cu-regex-space-p c) (/= code #xA0)
             (/= code #x2007) (/= code #x202F)))))

(defun %cu-word-boundary-p (text pos)
  (let ((before (and (> pos 0) (%cu-regex-word-char-p (char text (1- pos)))))
        (after (and (< pos (length text)) (%cu-regex-word-char-p (char text pos)))))
    (not (eq (and before t) (and after t)))))

(defun %cu-segment-at-p (segment text pos &optional (test #'string-equal))
  (let ((end (+ pos (length segment))))
    (and (<= end (length text))
         (funcall test segment text :start2 pos :end2 end))))

(defun %cu-strict-find-p (name text)
  "Case-sensitive: the strict pattern is not CASE_INSENSITIVE."
  (loop for pos from 0 to (length text)
        thereis (and (%cu-word-boundary-p text pos)
                     (%cu-segment-at-p name text pos #'string=)
                     (%cu-word-boundary-p text (+ pos (length name))))))

(defun %cu-fuzzy-find-p (segments text)
  (loop for start from 0 to (length text)
        thereis
        (and (%cu-word-boundary-p text start)
             (let ((pos start) (ok t))
               (loop for (segment . rest) on segments
                     for first = t then nil
                     do (unless first
                          (loop while (and (< pos (length text))
                                           (%cu-regex-space-p (char text pos)))
                                do (incf pos)))
                        (unless (%cu-segment-at-p segment text pos)
                          (setf ok nil)
                          (return))
                        (incf pos (length segment)))
               (and ok (%cu-word-boundary-p text pos))))))

(defun %cu-java-trim (s)
  "String.trim: strips characters <= U+0020 from both ends."
  (let* ((start (or (position-if (lambda (c) (> (char-code c) 32)) s) (length s)))
         (end (let ((p (position-if (lambda (c) (> (char-code c) 32)) s :from-end t)))
                (if p (1+ p) start))))
    (subseq s start (max start end))))

(defun %cu-fuzzy-segments (target-name)
  "The literal segments of NodeReferenceMatcher.fuzzyRegexpFor: the name is split
at upper-case letters that follow a lower-case one and at whitespace."
  (let* ((trimmed (%cu-java-trim target-name))
         (segments '())
         (current (make-string-output-stream)))
    (flet ((push-segment ()
             (push (get-output-stream-string current) segments)))
      (dotimes (i (length trimmed))
        (let ((c (char trimmed i)))
          (cond ((upper-case-p c)
                 (when (and (> i 0) (lower-case-p (char trimmed (1- i))))
                   (push-segment))
                 (write-char c current))
                ((%cu-java-whitespace-p c)
                 (when (and (> i 0) (not (%cu-java-whitespace-p (char trimmed (1- i)))))
                   (push-segment)))
                (t (write-char c current)))))
      (push-segment))
    (nreverse segments)))

(defun %cu-name-pattern (target-name fuzzy)
  (if fuzzy
      (list :fuzzy (%cu-fuzzy-segments target-name))
      (list :strict target-name)))

(defun %cu-pattern-find-p (pattern text)
  (ecase (first pattern)
    (:fuzzy (%cu-fuzzy-find-p (second pattern) text))
    (:strict (%cu-strict-find-p (second pattern) text))))

;;; ------------------------------------------------------------ NodeReferenceMatcher

(defjmethod with-comment-text-provider ((this node-reference-matcher) f)
  (%cu-require-non-null f "Comment text function cannot be null.")
  (setf (.comment-text-function this) f)
  this)

(defjmethod with-target-name-provider ((this node-reference-matcher) f)
  (%cu-require-non-null f "Node name function cannot be null.")
  (setf (.target-name-function this) f)
  this)

(defjmethod with-fuzzy-matching ((this node-reference-matcher))
  (setf (.fuzzy this) t)
  this)

(defjmethod with-maximum-attachment-distance ((this node-reference-matcher) distance)
  (setf (.max-distance this) distance)
  this)

(defjmethod with-bounds-provider ((this node-reference-matcher) provider)
  (%cu-require-non-null provider "Bounds provider must not be null.")
  (setf (.bounds-provider this) provider)
  this)

(defjmethod check-configuration ((this node-reference-matcher))
  (when (null (.comment-text-function this))
    (jthrow 'illegal-state-exception "A comment text function is required."))
  (when (null (.target-name-function this))
    (jthrow 'illegal-state-exception "A node name function is required."))
  (when (and (>= (.max-distance this) 0) (null (.bounds-provider this)))
    (jthrow 'illegal-state-exception
            "A bounds provider must be installed if a maximum attachment distance is set.")))

(defjmethod preprocess ((this node-reference-matcher) data-provider include-hierarchy)
  (check-configuration this)
  (let ((comment-texts (new-jlist))
        (target-names (new-jlist)))
    (jdo (comment (provide-comments data-provider))
      (let ((comment-text (funcall (.comment-text-function this) comment)))
        (unless (%cu-string-null-or-empty-p comment-text)
          (jl-add comment-texts (pair.of comment comment-text)))))
    (jdo (target (provide-targets data-provider))
      (let ((target-name (funcall (.target-name-function this) target)))
        (unless (%cu-string-null-or-empty-p target-name)
          (jl-add target-names (pair.of target target-name)))))
    (go-find-matches this comment-texts target-names)
    (when include-hierarchy
      (jdo (sub-provider (provide-sub-hierarchies data-provider))
        (preprocess this sub-provider t))))
  nil)

(defjmethod cleanup ((this node-reference-matcher))
  (clrhash (.found-attachments this))
  nil)

(defjmethod raw ((this node-reference-matcher) comment target)
  (if (eq (gethash comment (.found-attachments this)) target) 1d0 0d0))

(defjmethod normalized ((this node-reference-matcher) comment target)
  (raw this comment target))

(defjmethod go-find-matches ((this node-reference-matcher) comment-texts target-names)
  (let ((target-patterns (new-jlist)))
    (jdo (target-name-pair target-names)
      (jl-add target-patterns
              (pair.of (get-first target-name-pair)
                       (%cu-name-pattern (get-second target-name-pair) (.fuzzy this)))))
    (jdo (comment-text-pair comment-texts)
      (let ((found-target nil))
        (jdo (target-pattern-pair target-patterns)
          (when (%cu-pattern-find-p (get-second target-pattern-pair)
                                    (get-second comment-text-pair))
            (if (null found-target)
                (setf found-target (get-first target-pattern-pair))
                (progn
                  (setf found-target nil)
                  (return)))))
        (when found-target
          (if (< (.max-distance this) 0)
              (setf (gethash (get-first comment-text-pair) (.found-attachments this)) found-target)
              (let ((comment-bounds (bounds-for-comment (.bounds-provider this)
                                                        (get-first comment-text-pair)))
                    (node-bounds (bounds-for-target (.bounds-provider this) found-target)))
                (when (<= (distance-matcher.distance comment-bounds node-bounds)
                          (.max-distance this))
                  (setf (gethash (get-first comment-text-pair) (.found-attachments this))
                        found-target)))))))))

(defjmethod get-attachments ((this node-reference-matcher))
  (.found-attachments this))

;;; ------------------------------------------------------------ SizeFilter

(defjmethod with-maximum-area ((this size-filter) area)
  (when (< area 0)
    (jthrow 'illegal-argument-exception "Maximum area must be >= 0."))
  (setf (.max-area this) area)
  this)

(defjmethod with-bounds-provider ((this size-filter) provider)
  (%cu-require-non-null provider "Bounds provider must not be null.")
  (setf (.bounds-provider this) provider)
  this)

(defjmethod check-configuration ((this size-filter))
  (when (null (.bounds-provider this))
    (jthrow 'illegal-state-exception "A bounds provider is required.")))

(defjmethod preprocess ((this size-filter) data-provider include-hierarchy)
  (check-configuration this)
  nil)

(defjmethod eligible-for-attachment ((this size-filter) comment)
  (let ((bounds (bounds-for-comment (.bounds-provider this) comment)))
    (<= (* (.height bounds) (.width bounds)) (.max-area this))))

;;; ------------------------------------------------------------ TextPrefixFilter

(defjmethod with-comment-text-provider ((this text-prefix-filter) f)
  (%cu-require-non-null f "Comment text function cannot be null.")
  (setf (.comment-text-provider this) f)
  this)

(defjmethod with-prefix-match-required-for-eligibility ((this text-prefix-filter))
  (setf (.reject-comment-on-prefix-match this) nil)
  this)

(defjmethod with-case-sensitive-matching ((this text-prefix-filter))
  (setf (.case-sensitive this) t)
  this)

(defjmethod add-prefix ((this text-prefix-filter) prefix)
  (when (%cu-string-null-or-empty-p prefix)
    (jthrow 'illegal-argument-exception "Prefix cannot be null or empty. Wouldn't make sense."))
  (jl-add (.prefixes this) prefix)
  this)

(defjmethod check-configuration ((this text-prefix-filter))
  (when (null (.comment-text-provider this))
    (jthrow 'illegal-state-exception "A comment text provider is required."))
  (when (zerop (length (.prefixes this)))
    (jthrow 'illegal-state-exception "At least one prefix is required.")))

(defjmethod preprocess ((this text-prefix-filter) data-provider include-hierarchy)
  (check-configuration this)
  nil)

(defjmethod eligible-for-attachment ((this text-prefix-filter) comment)
  (let ((comment-text (funcall (.comment-text-provider this) comment)))
    (when (not (%cu-string-null-or-empty-p comment-text))
      (jdo (prefix (.prefixes this))
        (when (>= (length comment-text) (length prefix))
          (let* ((comment-prefix (subseq comment-text 0 (length prefix)))
                 (starts-with (if (.case-sensitive this)
                                  (string= comment-prefix prefix)
                                  (string-equal comment-prefix prefix))))
            (when starts-with
              (return-from eligible-for-attachment
                (not (.reject-comment-on-prefix-match this))))))))
    (.reject-comment-on-prefix-match this)))

;;; ------------------------------------------------------------ ElkGraph providers

(defjmethod bounds-for-target ((this elk-graph-bounds-provider) node)
  (new-rectangle2-d-double/4 (get-x node) (get-y node) (get-width node) (get-height node)))

(defjmethod bounds-for-comment ((this elk-graph-bounds-provider) node)
  (new-rectangle2-d-double/4 (get-x node) (get-y node) (get-width node) (get-height node)))

(defjctor elk-graph-data-provider (graph)
  (%cu-require-non-null graph "Comment attachment must be run on a graph.")
  (setf (.graph this) graph))

(defjmethod provide-comments ((this elk-graph-data-provider))
  (let ((result (new-jlist)))
    (jdo (node (get-children (.graph this)))
      (when (get-property node core-options.comment-box)
        (jl-add result node)))
    result))

(defjmethod provide-targets ((this elk-graph-data-provider))
  (let ((result (new-jlist)))
    (jdo (node (get-children (.graph this)))
      (unless (get-property node core-options.comment-box)
        (jl-add result node)))
    result))

(defjmethod provide-sub-hierarchies ((this elk-graph-data-provider))
  (let ((result (new-jlist)))
    (jdo (node (get-children (.graph this)))
      (when (plusp (length (get-children node)))
        (jl-add result (new-elk-graph-data-provider/1 node))))
    result))

(defjmethod attach ((this elk-graph-data-provider) comment target)
  (elk-graph-util.create-simple-edge comment target)
  nil)

;;; ------------------------------------------------------------ CommentAttacher

(defjmethod limit-to-current-hierarchy-level ((this comment-attacher) limit)
  (setf (.include-hierarchy this) (not limit))
  this)

(defjmethod keep-heuristics-enabled-with-explicit-attachments ((this comment-attacher) keep-enabled)
  (setf (.explicit-attachments-disable-heuristics this) (not keep-enabled))
  this)

(defjmethod with-explicit-attachment-provider ((this comment-attacher) provider)
  (setf (.explicit-attachment-provider this)
        (if (null provider)
            (lambda (a) (declare (ignore a)) nil)
            provider))
  this)

(defjmethod with-bounds-provider ((this comment-attacher) provider)
  (%cu-require-non-null provider "The bounds provider must not be null.")
  (setf (.bounds-provider this) provider)
  this)

(defjmethod add-filter ((this comment-attacher) filter)
  (%cu-require-non-null filter "The filter must not be null.")
  (jl-add (.filters this) filter)
  this)

(defjmethod add-matcher ((this comment-attacher) matcher)
  (%cu-require-non-null matcher "The matcher must not be null.")
  (jl-add (.matchers this) matcher)
  this)

(defjmethod with-attachment-decider ((this comment-attacher) attachment-decider)
  (%cu-require-non-null attachment-decider "The attachment decider must not be null.")
  (setf (.decider this) attachment-decider)
  this)

(defjmethod attach-comments ((this comment-attacher) data-provider)
  (%cu-preprocess-attacher this data-provider)
  (let ((explicit-attachments (new-jlist))
        (heuristic-attachments (new-jlist))
        (processing-queue (new-jlist)))
    (jl-add processing-queue data-provider)
    (loop until (jl-is-empty processing-queue)
          do (let ((current-hierarchy (jl-poll-first processing-queue)))
               (jdo (comment (provide-comments current-hierarchy))
                 (let ((explicit-attachment
                         (%cu-explicit-attachment (.explicit-attachment-provider this) comment)))
                   (cond ((not (null explicit-attachment))
                          (jl-add explicit-attachments (pair.of comment explicit-attachment)))
                         ((and (or (jl-is-empty explicit-attachments)
                                   (not (.explicit-attachments-disable-heuristics this)))
                               (%cu-eligible-for-heuristic-attachment this comment))
                          (let ((heuristic-attachment (find-match this current-hierarchy comment)))
                            (when heuristic-attachment
                              (jl-add heuristic-attachments
                                      (pair.of comment heuristic-attachment))))))))
               (when (.include-hierarchy this)
                 (jl-add-all processing-queue (provide-sub-hierarchies current-hierarchy)))))
    (%cu-cleanup-attacher this)
    (edgeify-found-attachments this data-provider explicit-attachments heuristic-attachments)))

(defun %cu-preprocess-attacher (this data-provider)
  (let ((include-hierarchy (.include-hierarchy this)))
    (%cu-preprocess-optional (.explicit-attachment-provider this) data-provider include-hierarchy)
    (jdo (f (.filters this))
      (%cu-preprocess-optional f data-provider include-hierarchy))
    (jdo (h (.matchers this))
      (preprocess h data-provider include-hierarchy))
    (when (.bounds-provider this)
      (preprocess (.bounds-provider this) data-provider include-hierarchy))))

(defun %cu-eligible-for-heuristic-attachment (this comment)
  (every (lambda (f) (%cu-filter-eligible f comment)) (.filters this)))

(defjmethod find-match ((this comment-attacher) data-provider comment)
  (when (jl-is-empty (.matchers this))
    (return-from find-match nil))
  (let ((candidates (provide-targets-for data-provider comment)))
    (when (or (null candidates) (zerop (length candidates)))
      (return-from find-match nil))
    (let ((results (make-hash-table :test 'eq)))
      (jdo (candidate candidates)
        (let ((candidate-results (make-hash-table :test 'eq)))
          (setf (gethash candidate results) candidate-results)
          (jdo (heuristic (.matchers this))
            (setf (gethash (class-of heuristic) candidate-results)
                  (normalized heuristic comment candidate)))))
      (%cu-decide (.decider this) results))))

(defjmethod edgeify-found-attachments ((this comment-attacher) data-provider
                                       explicit-attachments heuristic-attachments)
  (jdo (explicit-attachment explicit-attachments)
    (attach data-provider (get-first explicit-attachment) (get-second explicit-attachment)))
  (when (or (jl-is-empty explicit-attachments)
            (not (.explicit-attachments-disable-heuristics this)))
    (jdo (heuristic-attachment heuristic-attachments)
      (attach data-provider (get-first heuristic-attachment) (get-second heuristic-attachment))))
  nil)

(defun %cu-cleanup-attacher (this)
  (%cu-cleanup-optional (.explicit-attachment-provider this))
  (jdo (f (.filters this))
    (%cu-cleanup-optional f))
  (jdo (h (.matchers this))
    (cleanup h))
  (when (.bounds-provider this)
    (cleanup (.bounds-provider this)))
  nil)
