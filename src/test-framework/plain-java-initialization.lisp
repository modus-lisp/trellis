;;;; plain-java-initialization.lisp -- org.eclipse.elk.alg.test.PlainJavaInitialization.
;;;;
;;;; Java registers every layout meta data provider with the service here, and
;;;; sets up the .elkr/.elkg/.elkt readers, once per process.  In this port the
;;;; providers' options and algorithms are registered as the modules load
;;;; (define-layout-option forms and the algorithm data they carry), and the
;;;; readers are part of the image, so there is nothing left to do at run time.
;;;; The static exists so tests can call it as Java tests do, and it stays
;;;; idempotent like the Java `initialized` flag.

(in-package #:elk)

(defvar *plain-java-layout-initialized* nil)

(defjstatic plain-java-initialization.initialize-plain-java-layout ()
  (unless *plain-java-layout-initialized*
    (layout-meta-data-service.get-instance)
    (setf *plain-java-layout-initialized* t))
  nil)
