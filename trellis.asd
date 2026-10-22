;;;; trellis.asd -- Eclipse Layout Kernel, ported to Common Lisp.
;;;;
;;;; The component list is read from src/MODULES and each module's files.txt,
;;;; so modules can be ported independently (see PORTING.md).

(defun trellis-module-components (base)
  (flet ((lines (path)
           (with-open-file (s path :if-does-not-exist nil)
             (when s
               (loop for l = (read-line s nil) while l
                     for x = (string-trim " " l)
                     unless (or (string= x "") (char= (char x 0) #\#))
                       collect (let ((n (length x)))
                                 ;; "foo.lisp" and "foo" name the same file
                                 (if (and (> n 5) (string= ".lisp" x :start2 (- n 5)))
                                     (subseq x 0 (- n 5))
                                     x)))))))
    (loop for m in (lines (merge-pathnames "src/MODULES" base))
          for files = (lines (merge-pathnames (format nil "src/~A/files.txt" m) base))
          when files
            collect `(:module ,m :pathname ,(format nil "src/~A/" m) :serial t
                      :components ,(mapcar (lambda (f) `(:file ,f)) files)))))

(defun call-with-java-reader (thunk)
  (let ((*read-default-float-format* 'double-float))
    (funcall thunk)))

(eval `(asdf:defsystem "trellis"
         :version "1.0.0"
         :description "Automatic graph layout in Common Lisp, after the Eclipse Layout Kernel"
         :license "MIT"
         :depends-on ("json-simple")
         :serial t
         :around-compile call-with-java-reader
         :components ,(trellis-module-components (directory-namestring *load-truename*))))
