(defsystem 3b-spirv
  :description "SPIRV assembler/disassembler and related tools"
  :depends-on (alexandria babel ieee-floats)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components ((:file "package")
               (:file "op-packages")
               (:file "spec")
               (:file "patch-spec")
               (:file "writer")
               (:file "low-level")))

