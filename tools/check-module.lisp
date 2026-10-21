;;;; check-module.lisp MODULE -- compile one module of the port against
;;;; everything that loads before it, and report what is wrong with it.
;;;;
;;;;   sbcl --script tools/check-module.lisp layered-p4
;;;;
;;;; Earlier modules are loaded form by form with errors counted, not fatal,
;;;; because they may be unfinished.  The target module is compiled with
;;;; COMPILE-FILE so its warnings are reported.

(require :asdf)
(require :sb-posix)
(setf *read-default-float-format* 'double-float)
(defvar *root* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(defun lines (path)
  (with-open-file (s path :if-does-not-exist nil)
    (when s (loop for l = (read-line s nil) while l
                  for x = (string-trim " " l)
                  unless (or (string= x "") (char= (char x 0) #\#)) collect x))))

(defun module-files (m)
  ;; files.txt lists names without extension; tolerate "x.lisp" too
  (mapcar (lambda (f)
            (let ((base (if (and (> (length f) 5) (string= ".lisp" f :start2 (- (length f) 5)))
                            (subseq f 0 (- (length f) 5)) f)))
              (merge-pathnames (format nil "src/~A/~A.lisp" m base) *root*)))
          (mapcar (lambda (l) (pathname-name l)) (lines (merge-pathnames (format nil "src/~A/files.txt" m) *root*)))))

(defun quiet-load (file)
  "Load FILE form by form (interpreted, to keep the heap small); return the number of forms that failed."
  (let ((failed 0) (*package* *package*) (sb-ext:*evaluator-mode* :interpret))
    (with-open-file (s file)
      (loop for form = (handler-case (read s nil s)
                         (error (e) (format t "  READ ERROR in ~A: ~A~%" (file-namestring file) e) s))
            until (eq form s)
            do (handler-case
                   (handler-bind ((warning #'muffle-warning))
                     (eval form))
                 (error (e)
                   (incf failed)
                   (when (<= failed 3)
                     (format t "  load error in ~A: ~A~%" (file-namestring file)
                             (subseq (princ-to-string e) 0 (min 200 (length (princ-to-string e))))))))))
    failed))

(let* ((target (second sb-ext:*posix-argv*))
       (modules (lines (merge-pathnames "src/MODULES" *root*)))
       (before (subseq modules 0 (or (position target modules :test #'string=)
                                     (error "No module ~A in src/MODULES" target)))))
  (format t ";; loading ~D earlier modules~%" (length before))
  (sb-ext:gc :full t)
  (dolist (m before)
    (let ((failed (reduce #'+ (mapcar (lambda (f)
                                        (when (sb-posix:getenv "CHECK_VERBOSE")
                                          (format t "  ~A~%" (file-namestring f)) (finish-output))
                                        (prog1 (quiet-load f) (sb-ext:gc :full t)))
                                      (module-files m)))))
      (when (plusp failed) (format t ";; ~A: ~D forms failed to load~%" m failed))))
  (format t ";; compiling ~A~%" target)
  (let ((undefined '()) (other 0))
    (dolist (f (module-files target))
      (format t "~&==== ~A~%" (file-namestring f))
      (handler-bind ((warning
                       (lambda (w)
                         (let ((text (princ-to-string w)))
                           (cond ((search "undefined" text)
                                  (push text undefined))
                                 (t (incf other)
                                    (format t "WARNING: ~A~%" text))))
                         (muffle-warning w))))
        (multiple-value-bind (fasl warnings-p failure-p)
            (compile-file f :output-file (format nil "/tmp/check-~A-~A.fasl" target (pathname-name f)))
          (declare (ignore warnings-p))
          (when failure-p (format t "!! compile FAILURE in ~A~%" (file-namestring f)))
          (when fasl (handler-case (load fasl)
                       (error (e) (format t "!! LOAD ERROR in ~A: ~A~%" (file-namestring f) e)))))))
    (format t "~&;; ~D other warnings~%" other)
    (format t ";; undefined (functions/variables in other modules, or typos):~%")
    (let ((names (remove-duplicates
                  (loop for u in undefined
                        nconc (let ((p (search ":" u)))
                                (list (string-trim '(#\Space #\Newline) (if p (subseq u (1+ p)) u)))))
                  :test #'string=)))
      (dolist (n names) (format t "   ~A~%" (substitute #\Space #\Newline n))))))
