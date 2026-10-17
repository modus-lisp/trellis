(require :asdf)
(push (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(asdf:load-system "diagram/test")
(sb-ext:exit :code (if (diagram-test:run) 0 1))
