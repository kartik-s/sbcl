(error "Can't build contribs with ASDF")

(defsystem "sb-tcc"
  :name "SB-TCC"
  :version "0.1"
  :description "Dynamic code generation from C using the TinyCC library (libtcc)"
  :serial t
  :components ((:file "tcc")))
