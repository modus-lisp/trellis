;;;; issue700-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue700Test.

(in-package #:elk)

(defun %tlc-name-matches-p (name prefix suffix)
  "FileNameFilter(\"PREFIX.+SUFFIX\"): the name starts with PREFIX, has at least
one more character, and ends with SUFFIX.  (No regex engine in the port.)"
  (let ((n (length name)) (p (length prefix)) (s (length suffix)))
    (and (> n (+ p s))
         (string= prefix name :end2 p)
         (string= suffix name :start2 (- n s)))))

(defun %tlc-with-name-filter (path prefix suffix)
  "new ModelResourcePath(p).withFilter(new FileNameFilter(\"PREFIX.+SUFFIX\"))."
  (setf (abstract-resource-path-filter path)
        (file-name-filter (lambda (name) (%tlc-name-matches-p name prefix suffix))))
  path)

(deflayouttest issue700-test
  (:default-configuration)
  ;; The bug was introduced while fixing #665, so ensure that the fix for #700 does not return the favour...
  (:graph-resources (list (%tlc-with-name-filter (model-resource-path "tickets/layered/**") "665" ".elkt")
                          (%tlc-with-name-filter (model-resource-path "tickets/layered/**") "700" ".elkt")
                          (model-resource-path "examples/hierarchy/**")))
  (:test test-edges-routed () (graph)
    (jdo (node (graph-test-utils.all-nodes graph t))
      (jdo (edge (get-contained-edges node))
        (%issue700-test-edge-routed edge)))))

(defun %issue700-test-edge-routed (edge)
  (assert-equals "Edge has no sections." 1 (size (get-sections edge)))
  ;; The start and end point may not be at the origin
  (let ((section (j-get (get-sections edge) 0)))
    (assert-true "Edge has no proper start and end coordinates."
                 (or (/= (get-start-x section) 0)
                     (/= (get-start-y section) 0)
                     (/= (get-end-x section) 0)
                     (/= (get-end-y section) 0)))))
