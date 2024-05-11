(error "Can't build contribs with ASDF")

(defsystem "sb-tcc"
  :name "SB-TCC"
  :version "0.1"
  :description "Compile C dynamically using libtcc"
  :serial t
  :components ((:file "tcc")))
