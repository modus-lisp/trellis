;;;; transform-util.lisp -- helpers shared by the ElkGraphImporter and
;;;; ElkGraphLayoutTransferrer ports (no Java counterpart).

(in-package #:elk)

(defun %lg-any-p (coll pred)
  "Java stream().anyMatch(pred) over a collection, short-circuiting."
  (jdo (x coll) (when (funcall pred x) (return-from %lg-any-p t)))
  nil)

(defun %lg-ends-with (s suffix)
  "Java String.endsWith."
  (let ((ls (length s)) (lf (length suffix)))
    (and (<= lf ls) (string= s suffix :start1 (- ls lf)))))

(defun %lg-root-container (obj)
  "EcoreUtil.getRootContainer: walk eContainer() up to the top."
  (let ((root obj))
    (loop for c = (e-container root) while c do (setf root c))
    root))
