;;;; corpus.lisp -- the ELK corpus as a conformance suite.
;;;;
;;;; test/corpus holds 1000 random graphs laid out by ELK 0.13 (see its README).
;;;; Each case is run through trellis with the harness's 30 s budget and
;;;; checked with the harness's checks (CHECK-LAYOUT).  Where ELK produced a
;;;; layout too, both are measured with LAYOUT-METRICS and compared.
;;;;
;;;;   (trellis-test:run-corpus)                      everything
;;;;   (trellis-test:run-corpus :algorithms '(:layered))
;;;;   (trellis-test:run-corpus :ids '("0001" "0042"))
;;;;
;;;; Results go to test/corpus-results.jsonl and a summary to
;;;; test/corpus-report.md.

(in-package #:trellis-test)

(defparameter *corpus-dir*
  (asdf:system-relative-pathname "trellis" "test/corpus/"))

(defun read-manifest ()
  (with-open-file (in (merge-pathnames "manifest.jsonl" *corpus-dir*))
    (loop for line = (read-line in nil) while line
          when (> (length line) 2)
            collect (json-simple:parse line))))

(defun mget (m k) (gethash k m))

(defun short-algorithm (name)
  (trellis::enum-keyword name))

(defun case-file (id kind)
  (merge-pathnames (format nil "cases/~A.~A.json" id kind) *corpus-dir*))

(defun issue-list (check)
  (loop for (k v) on check by #'cddr when (plusp v) collect k))

(defun run-case (id &key (timeout 30))
  "Lay out corpus case ID.  Returns a plist: :status (:ok :warn :error
   :timeout), :check, :metrics, :millis, :error."
  (let ((start (get-internal-real-time)) g)
    (flet ((ms () (round (* 1000 (- (get-internal-real-time) start))
                         internal-time-units-per-second)))
      (handler-case
          (progn
            (setf g (read-elk-json (case-file id "in")))
            (sb-ext:with-timeout timeout (layout g))
            (let* ((millis (ms)) (check (check-layout g)))
              (list :status (if (issue-list check) :warn :ok) :check check
                    :metrics (layout-metrics g :edges (comparable-edges g id)) :millis millis :graph g)))
        (sb-ext:timeout () (list :status :timeout :millis (ms)))
        (error (e) (list :status :error :millis (ms)
                         :error (let ((*print-length* 5) (*print-level* 3))
                                  (princ-to-string e))))))))

(defun elk-routed-ids (id)
  "Ids of the edges ELK gave a route in case ID, or NIL without an ELK layout."
  (let ((f (case-file id "out")))
    (when (probe-file f)
      (let ((h (make-hash-table :test 'equal)))
        (dolist (e (all-edges (read-elk-json f)) h)
          (when (edge-sections e) (setf (gethash (edge-id e) h) t)))))))

(defun comparable-edges (g id)
  "G's edges that ELK routed too, so quality compares like with like."
  (let ((routed (elk-routed-ids id)))
    (if routed
        (remove-if-not (lambda (e) (gethash (edge-id e) routed)) (all-edges g))
        (all-edges g))))

(defun elk-case (id)
  "ELK's layout of case ID, checked and measured, or NIL."
  (let ((f (case-file id "out")))
    (when (probe-file f)
      (let ((g (read-elk-json f)))
        (list :check (check-layout g) :metrics (layout-metrics g))))))

(defun safe-ratio (a b)
  (cond ((and (zerop a) (zerop b)) 1d0)
        ((zerop b) nil)
        (t (/ (float a 1d0) b))))

(defun median (xs)
  (when xs
    (let ((v (sort (coerce xs 'vector) #'<)))
      (aref v (floor (length v) 2)))))

(defun run-corpus (&key algorithms ids (timeout 30) (report t) (verbose t))
  (let ((results '()))
    (dolist (m (read-manifest))
      (let* ((id (mget m "id")) (alg (short-algorithm (mget m "algorithm"))))
        (when (and (or (null ids) (member id ids :test #'string=))
                   (or (null algorithms) (member alg algorithms)))
          (let* ((r (run-case id :timeout timeout))
                 (elk (elk-case id))
                 (row (list :id id :algorithm alg :family (mget m "family")
                            :nodes (mget m "nodes") :edges (mget m "edges")
                            :elk-status (intern (string-upcase (mget m "status")) :keyword)
                            :elk elk)))
            (remf r :graph)
            (setf row (append row r))
            (when verbose
              (format t "~&~A ~14A ~8A ~6Dms ~@[~A~]~@[ ~A~]~%" id alg (getf r :status) (getf r :millis)
                      (issue-list (getf r :check)) (getf r :error))
              (finish-output))
            (push row results)))))
    (setf results (nreverse results))
    (when report (write-results results))
    results))

;;; ---- reporting -------------------------------------------------------------

(defun write-results (results)
  (with-open-file (out (asdf:system-relative-pathname "trellis" "test/corpus-results.jsonl")
                       :direction :output :if-exists :supersede)
    (dolist (r results)
      (let ((h (make-hash-table :test 'equal)))
        (setf (gethash "id" h) (getf r :id)
              (gethash "algorithm" h) (string-downcase (getf r :algorithm))
              (gethash "status" h) (string-downcase (getf r :status))
              (gethash "elkStatus" h) (string-downcase (getf r :elk-status))
              (gethash "millis" h) (getf r :millis)
              (gethash "issues" h) (coerce (mapcar #'string-downcase (issue-list (getf r :check))) 'vector))
        (when (getf r :error) (setf (gethash "error" h) (getf r :error)))
        (flet ((metrics (m)
                 (let ((mh (make-hash-table :test 'equal)))
                   (loop for (k v) on m by #'cddr
                         do (setf (gethash (string-downcase k) mh) v))
                   mh)))
          (when (getf r :metrics) (setf (gethash "metrics" h) (metrics (getf r :metrics))))
          (when (getf (getf r :elk) :metrics)
            (setf (gethash "elkMetrics" h) (metrics (getf (getf r :elk) :metrics)))))
        (json-simple:stringify h :stream out)
        (terpri out))))
  (with-open-file (out (asdf:system-relative-pathname "trellis" "test/corpus-report.md")
                       :direction :output :if-exists :supersede)
    (write-report results out)))

(defun write-report (results out)
  (let ((algs (remove-duplicates (mapcar (lambda (r) (getf r :algorithm)) results) :from-end t))
        (statuses '(:ok :warn :error :timeout)))
    (format out "# trellis on the ELK corpus~%~%~D cases.  `ok` = laid out and passed every harness check; `warn` = laid out but a check failed; `error` = signalled an error; `timeout` = over 30 s.~%~%"
            (length results))
    (format out "## Outcomes~%~%| | trellis | ELK |~%|---|---|---|~%")
    (dolist (s statuses)
      (format out "| ~(~A~) | ~D | ~D |~%" s
              (count s results :key (lambda (r) (getf r :status)))
              (count s results :key (lambda (r) (getf r :elk-status)))))
    (format out "~%## By algorithm~%~%| algorithm | cases | ok | warn | error | timeout | ELK ok |~%|---|---|---|---|---|---|---|~%")
    (dolist (a (sort (copy-list algs) #'string< :key #'symbol-name))
      (let ((rs (remove a results :key (lambda (r) (getf r :algorithm)) :test-not #'eq)))
        (format out "| ~(~A~) | ~D | ~{~D~^ | ~} | ~D |~%" a (length rs)
                (mapcar (lambda (s) (count s rs :key (lambda (r) (getf r :status)))) statuses)
                (count :ok rs :key (lambda (r) (getf r :elk-status))))))
    (format out "~%## Checks failed~%~%| check | trellis cases | ELK cases |~%|---|---|---|~%")
    (dolist (k '(:non-finite :overlaps :detached :unrouted :outside))
      (format out "| ~(~A~) | ~D | ~D |~%" k
              (count-if (lambda (r) (plusp (or (getf (getf r :check) k) 0))) results)
              (count-if (lambda (r) (plusp (or (getf (getf (getf r :elk) :check) k) 0))) results)))
    (format out "~%## Quality against ELK~%~%Over cases where ELK's layout passed every check, and over the edges ELK routed.  Ratio = trellis / ELK; below 1 is better for trellis.~%~%| algorithm | cases | crossings (median ratio, better/worse) | bends | edge length | area | edges through nodes (trellis/ELK total) |~%|---|---|---|---|---|---|---|~%")
    (dolist (a (sort (copy-list algs) #'string< :key #'symbol-name))
      (let ((rs (remove-if-not (lambda (r) (and (eq (getf r :algorithm) a) (getf r :metrics)
                                                (getf (getf r :elk) :metrics)
                                                (null (issue-list (getf (getf r :elk) :check)))))
                               results)))
        (when rs
          (flet ((cmp (key)
                   (let ((ratios '()) (better 0) (worse 0))
                     (dolist (r rs)
                       (let ((x (getf (getf r :metrics) key)) (y (getf (getf (getf r :elk) :metrics) key)))
                         (when (< x (- y 1d-6)) (incf better))
                         (when (> x (+ y 1d-6)) (incf worse))
                         (let ((q (safe-ratio x y))) (when q (push q ratios)))))
                     (format nil "~,2F (~D/~D)" (or (median ratios) 0) better worse))))
            (format out "| ~(~A~) | ~D | ~A | ~A | ~A | ~A | ~D/~D |~%" a (length rs)
                    (cmp :crossings) (cmp :bends) (cmp :edge-length) (cmp :area)
                    (reduce #'+ rs :key (lambda (r) (getf (getf r :metrics) :node-hits)))
                    (reduce #'+ rs :key (lambda (r) (getf (getf (getf r :elk) :metrics) :node-hits))))))))
    (let ((bad (remove-if-not (lambda (r) (member (getf r :status) '(:error :timeout))) results)))
      (when bad
        (format out "~%## Errors and timeouts~%~%| case | algorithm | status | error |~%|---|---|---|---|~%")
        (dolist (r bad)
          (format out "| ~A | ~(~A~) | ~(~A~) | ~A |~%" (getf r :id) (getf r :algorithm) (getf r :status)
                  (substitute #\Space #\Newline (substitute #\/ #\| (or (getf r :error) "")))))))))
