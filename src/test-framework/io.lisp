;;;; io.lisp -- org.eclipse.elk.alg.test.framework.io: resource paths and
;;;; file filters.  A path string ending in "/**" lists the files of a
;;;; directory; ending in "/**/" lists them recursively (AbstractResourcePath).
;;;;
;;;; Deviations: FileNameFilter takes a predicate on the file name instead of
;;;; a java.util.regex Pattern (there is no regex engine in the port); directory
;;;; listings are sorted by name, because Java's listFiles order is undefined.

(in-package #:elk)

(defparameter *all-files-suffix* "/**")
(defparameter *all-files-recursive-suffix* "/**/")

(defstruct abstract-resource-path
  (file nil)                 ; native path string (no trailing "/")
  (directory-p nil)
  (recursive-p nil)
  (filter nil))              ; predicate on a native path string, or nil

(defun %tf-ends-with (s suffix)
  (and (>= (length s) (length suffix))
       (string= suffix s :start2 (- (length s) (length suffix)))))

(defun %tf-join-path (base relative)
  (let ((rel (substitute #\/ #\\ relative)))
    (cond ((or (null base) (zerop (length base))) rel)
          ((char= (char base (1- (length base))) #\/) (concatenate 'string base rel))
          (t (concatenate 'string base "/" rel)))))

(defun make-resource-path-for (base relative)
  "AbstractResourcePath.initialize(base, relative)."
  (unless relative (error 'illegal-argument-exception :message "Relative path cannot be null."))
  (let ((p (make-abstract-resource-path)))
    (cond ((%tf-ends-with relative *all-files-recursive-suffix*)
           (setf (abstract-resource-path-directory-p p) t
                 (abstract-resource-path-recursive-p p) t)
           (setf (abstract-resource-path-file p)
                 (%tf-join-path base (subseq relative 0 (- (length relative)
                                                          (length *all-files-recursive-suffix*))))))
          ((%tf-ends-with relative *all-files-suffix*)
           (setf (abstract-resource-path-directory-p p) t)
           (setf (abstract-resource-path-file p)
                 (%tf-join-path base (subseq relative 0 (- (length relative)
                                                          (length *all-files-suffix*))))))
          (t (setf (abstract-resource-path-file p) (%tf-join-path base relative))))
    (setf (abstract-resource-path-file p)
          (string-right-trim "/" (abstract-resource-path-file p)))
    p))

(defun absolute-resource-path (file-path)
  "new AbsoluteResourcePath(filePath)."
  (make-resource-path-for nil file-path))

(defun model-resource-path (file-path)
  "new ModelResourcePath(filePath).  The base is $MODELS_REPO (Java: a system
property or environment variable, which must be set); the port defaults it to
/tmp/elk-src/models when unset."
  (when (or (null file-path) (zerop (length file-path)))
    (error 'illegal-argument-exception :message "The file path cannot be empty."))
  (make-resource-path-for (or (sb-ext:posix-getenv "MODELS_REPO") "/tmp/elk-src/models")
                          file-path))

(defun file-extension-filter (&rest extensions)
  "new FileExtensionFilter(...).  Directories are always accepted."
  (let ((exts (mapcar (lambda (e) (if (char= (char e 0) #\.) e (concatenate 'string "." e)))
                      extensions)))
    (lambda (path)
      (cond ((not (probe-file path)) nil)
            ((%tf-directory-p path) t)
            (t (let ((name (%tf-file-name path)))
                 (some (lambda (ext) (%tf-ends-with name ext)) exts)))))))

(defun file-name-filter (predicate)
  "FileNameFilter, with PREDICATE on the file name in place of a regex."
  (lambda (path)
    (cond ((not (probe-file path)) nil)
          ((%tf-directory-p path) t)
          (t (funcall predicate (%tf-file-name path))))))

(defun %tf-file-name (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun %tf-directory-p (path)
  (let ((truename (probe-file path)))
    (and truename (null (pathname-name truename)) (null (pathname-type truename)))))

(defun %tf-list-directory (dir)
  "Children of DIR as (name . directory-p), sorted by name."
  (let ((entries '()))
    (dolist (child (directory (concatenate 'string (string-right-trim "/" dir) "/*.*")
                              :resolve-symlinks nil))
      (let* ((native (sb-ext:native-namestring child))
             (native (string-right-trim "/" native)))
        (push (cons native (%tf-directory-p native)) entries)))
    (sort entries #'string< :key #'car)))

(defun abstract-resource-path-list-resources (path)
  "AbstractResourcePath.listResources(): a list of ABSOLUTE resource paths."
  (let ((file (abstract-resource-path-file path))
        (result '()))
    (labels ((fill-list (dir)
               (dolist (child (%tf-list-directory dir))
                 (let ((name (car child)) (is-dir (cdr child)))
                   (cond ((and (not is-dir)
                               (or (null (abstract-resource-path-filter path))
                                   (funcall (abstract-resource-path-filter path) name)))
                          (push (absolute-resource-path name) result))
                         ((and is-dir (abstract-resource-path-recursive-p path))
                          (fill-list name)))))))
      (when (probe-file file)
        (cond ((and (abstract-resource-path-directory-p path) (%tf-directory-p file))
               (fill-list file))
              ((and (not (abstract-resource-path-directory-p path))
                    (not (%tf-directory-p file)))
               (push (absolute-resource-path file) result)))))
    (nreverse result)))
