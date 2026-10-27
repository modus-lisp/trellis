;;;; trellis.asd -- automatic graph layout in Common Lisp.

(asdf:defsystem "trellis"
  :version "2.2.0"
  :description "Automatic graph layout in Common Lisp: layered (Sugiyama) and
other algorithms, built from the published papers, reading and writing ELK JSON."
  :license "MIT"
  :depends-on ("json-simple")
  :serial t
  :components
  ((:module "src" :serial t
    :components ((:file "package")
                 (:file "util")
                 (:file "graph")
                 (:file "options")
                 (:file "json")
                 (:file "labels")
                 (:file "check")
                 (:file "svg")
                 (:file "engine")
                 (:module "layered" :serial t
                  :components ((:file "lgraph")
                               (:file "import")
                               (:file "cycles")
                               (:file "layering")
                               (:file "dummies")
                               (:file "ports")
                               (:file "crossings")
                               (:file "placement")
                               (:file "routing")
                               (:file "splines")
                               (:file "export")
                               (:file "layered")))
                 (:file "simple")
                 (:file "packing")
                 (:file "tree")
                 (:file "force")))))

(asdf:defsystem "trellis/test"
  :description "Unit tests and the ELK corpus conformance suite."
  :depends-on ("trellis")
  :serial t
  :components ((:module "test" :serial t
                :components ((:file "package")
                             (:file "unit")
                             (:file "corpus")))))
