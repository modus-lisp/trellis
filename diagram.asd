;;;; diagram.asd -- automatic graph layout for Common Lisp, after Eclipse ELK

(asdf:defsystem "diagram"
  :description "Automatic graph layout (layered/Sugiyama, orthogonal routing) with an ELK-compatible JSON graph format"
  :license "EPL-2.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "json")
                             (:file "graph")
                             (:file "layered")
                             (:file "simple")
                             (:file "layout")
                             (:file "svg")))))

(asdf:defsystem "diagram/test"
  :depends-on ("diagram")
  :components ((:file "test/tests")))
