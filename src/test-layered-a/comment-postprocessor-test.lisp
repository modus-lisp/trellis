;;;; comment-postprocessor-test.lisp -- port of
;;;; org.eclipse.elk.alg.layered.intermediate.CommentPostprocessorTest.

(in-package #:elk)

(defparameter *comment-postprocessor-test.tolerance* 0.1d0)

(defun %cpt-gather-connected-comments-sorted-by-x (node)
  (let ((comments (new-jlist)))
    (jdo (in-edge (get-incoming-edges node))
      (let* ((shape (j-get (get-sources in-edge) 0))
             (comment (elk-graph-util.connectable-shape-to-node shape)))
        (when (get-property comment layered-options.comment-box)
          (jl-add comments comment))))
    (jdo (out-edge (get-outgoing-edges node))
      (let* ((shape (j-get (get-targets out-edge) 0))
             (comment (elk-graph-util.connectable-shape-to-node shape)))
        (when (get-property comment layered-options.comment-box)
          (jl-add comments comment))))
    (jl-sort comments (lambda (n1 n2) (double.compare (get-x n1) (get-x n2))))))

(deflayouttest comment-postprocessor-test
  (:algorithm "org.eclipse.elk.layered")
  (:default-configuration)
  (:graph-resources (list (model-resource-path "tests/layered/comment_boxes/**")))
  (:test test-spacings () (graph)
    ;; Run our test on all non-comment nodes
    (jdo (node (get-children graph))
      ;; Ignore comment boxes
      (unless (get-property node layered-options.comment-box)
        (let* ((tolerance *comment-postprocessor-test.tolerance*)
               ;; Retrieve applicable spacings
               (comment-comment-spacing (get-property graph layered-options.spacing-comment-comment))
               (comment-node-spacing (get-property graph layered-options.spacing-comment-node))
               ;; Next expected x coordinates of top and bottom comments (NaN = none yet)
               (next-top-x double.nan)
               (next-bottom-x double.nan)
               (top-comments-width 0d0)
               (bottom-comments-width 0d0))
          (jdo (comment (%cpt-gather-connected-comments-sorted-by-x node))
            (if (< (get-y comment) (get-y node))
                (progn
                  ;; Top comment; check y coordinate
                  (%la-assert-equals-delta
                   (format nil "Comment node spacing ~A violated by comment ~A" comment-node-spacing comment)
                   (- (get-y node) comment-node-spacing (get-height comment))
                   (get-y comment) tolerance)
                  ;; Check x coordinate
                  (unless (nan-p next-top-x)
                    (%la-assert-equals-delta
                     (format nil "Comment comment spacing ~A violated by comment ~A" comment-comment-spacing comment)
                     next-top-x (get-x comment) tolerance))
                  (setf next-top-x (+ (get-x comment) (get-width comment) comment-comment-spacing))
                  (incf top-comments-width (+ (get-width comment) comment-comment-spacing)))
                (progn
                  ;; Bottom comment; check y coordinate
                  (%la-assert-equals-delta
                   (format nil "Comment node spacing ~A violated by comment ~A" comment-node-spacing comment)
                   (+ (get-y node) (get-height node) comment-node-spacing)
                   (get-y comment) tolerance)
                  ;; Check x coordinate
                  (unless (nan-p next-bottom-x)
                    (%la-assert-equals-delta
                     (format nil "Comment comment spacing ~A violated by comment ~A" comment-comment-spacing comment)
                     next-bottom-x (get-x comment) tolerance))
                  (setf next-bottom-x (+ (get-x comment) (get-width comment) comment-comment-spacing))
                  (incf bottom-comments-width (+ (get-width comment) comment-comment-spacing)))))
          ;; Check if comments are centred
          (when (> top-comments-width 0)
            (decf top-comments-width comment-comment-spacing)
            (%la-assert-equals-delta
             (format nil "Top comments not centred above node ~A" node)
             (+ (get-x node) (/ (get-width node) 2))
             (- next-top-x comment-comment-spacing (/ top-comments-width 2)) tolerance))
          (when (> bottom-comments-width 0)
            (decf bottom-comments-width comment-comment-spacing)
            (%la-assert-equals-delta
             (format nil "Bottom comments not centred below node ~A" node)
             (+ (get-x node) (/ (get-width node) 2))
             (- next-bottom-x comment-comment-spacing (/ bottom-comments-width 2)) tolerance)))))))
