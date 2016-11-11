(in-package #:3b-spirv)

;;; fix up some missing data in the spec file

(defun patch (op desc)
  (assert (= (getf desc :value)
             (getf (gethash op (getf 3b-spirv::*spec* :opcodes)) :value)))
  (setf (gethash op (getf 3b-spirv::*spec* :opcodes))
        desc))

(patch 'spirv-core:decorate
       (list :value 71 :required-capabilities nil :operands
             (list (list :id-ref "'target'")
                   (list :decoration)
                   (list (list :* :literal-integer)))))

(patch 'spirv-core:member-decorate
       (list :value 72 :required-capabilities nil :operands
             (list (list :id-ref "'structure type'")
                   (list :literal-integer "'member'")
                   (list :decoration)
                   (list (list :* :literal-integer)))))

