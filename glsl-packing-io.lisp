(defpackage #:glsl-packing-io
  (:use :cl)
  (:export
   ))
(in-package #:glsl-packing-io)
;;; this should probably be moved to a separate project with glsl-packing

(defparameter *half-float-ext* "GL_NV_gpu_shader5")
(defparameter *int-8/16-ext* "GL_NV_gpu_shader5")

(defun translate-name (n)
  (if (stringp n)
      n
      (string-downcase n)))

(defun print-member (name type)
  (if name
      (format t "~a ~a" type (translate-name name))
      type))

(defmethod dump-type (name (base (eql :bool)) type)
  (print-member name
                "bool"))

(defmethod dump-type (name (base (eql :int)) type)
  (print-member name
                (ecase (second type)
                  (8 "int8_t")
                  (16 "uint16_t")
                  (32 "int")
                  (64 "int64"))))

(defmethod dump-type (name (base (eql :uint)) type)
  (print-member name
                (ecase (second type)
                  (8 "uint8_t")
                  (16 "uint16_t")
                  (32 "uint")
                  (64 "uint64"))))

(defmethod dump-type (name (base (eql :float)) type)
  (print-member name
                (ecase (second type)
                  (16 "float16_t")
                  (32 "float")
                  (64 "double"))))

(defmethod dump-type (name (base (eql :vec)) type)
  (print-member name
                (format nil
                        "~a~a"
                        (ecase (first (second type))
                          (:bool "bvec")
                          (:int (ecase (second (second type))
                                  (8 "i8vec")
                                  (16 "i16vec")
                                  (32 "ivec")
                                  (64 "i64vec")))
                          (:uint "uvec")
                          (:float
                           (ecase (second (second type))
                             (16 "f16vec")
                             (32 "vec")
                             (64 "dvec"))))
                        (third type)
                        #++(if (= (third type) 4)
                            ""
                            (third type)))))

(defmethod dump-type (name (base (eql :mat)) type)
  (print-member name
                (format nil
                        "~a~ax~a"
                        (ecase (first (second type))
                          (:float
                           (ecase (second (second type))
                             ;;nv_gpu_shader5/AMD_gpu_shader_half_float
                             (16 "f16mat")
                             (32 "mat")
                             (64 "dmat"))))
                        (third type)
                        (fourth type))))

(defmethod dump-type (name (base (eql :array)) type)
  (cond
    ((typep (second type) '(cons (eql :array)))
     (dump-type name :array (second type))
     (format t "[~a]" (if (eq (third type) '*) "" (third type))))
    (name
     (format t "~a ~a[~a]"
             (if (stringp (second type))
                 (second type)
                 (dump-type nil (caadr type) (second type)))
             name
             (if (eq (third type) '*) "" (third type))))
    (t
     (format t "~a[~a]"
             (if (stringp (second type))
                 (second type)
                 (dump-type nil (caadr type) (second type)))
             (if (eq (third type) '*) "" (third type))))))

(defmethod dump-type (name base type)
  (print-member name (format nil "~a" type)))

(defun dump-members (members)
  (format t "{~%  ")
  (pprint-logical-block (nil members :prefix "" :suffix "}")
    (loop for ((name type . options) . more) on members
          do (apply #'dump-layout :suffix " " options)
             (if (consp type)
                 (dump-type name (car type) type)
                 (print-member name type))
             (format t ";")
          when more
            do (pprint-newline :mandatory))
    (pprint-indent :block -2)
    (pprint-newline :mandatory)))

(defun dump-layout (&key major packing suffix &allow-other-keys)
  (when (or major packing)
    (format t "layout~((~@[~a~]~@[~*,~]~@[~a_major~])~)~@[~@?~]"
            packing (and major packing) major suffix)))

(defmethod dump-type (name (base (eql :struct)) type)
  (destructuring-bind (b opt &rest members) type
    (declare (ignore b opt))
    (format t "struct ~a " (translate-name name))
    (dump-members members)
    (format t ";~%")))

(defmethod dump-type (name (base (eql :block)) type)
  (destructuring-bind (b (&key major packing instance) &rest members) type
    (declare (ignore b major packing))
    (apply #'dump-layout :suffix " " (second type))
    (format t "buffer ~a " (translate-name name))
    (dump-members members)
    (format t "~@[ ~a~];~%" (translate-name instance))))

(defun print-structs (types)
  "dumps a glsl representation of the structs/blocks specified in TYPES (in same format used by GLSL-PACKING:PACK-STRUCTS."
  (loop for s in types
        for (name type) = s
        when (member name '(:major :packing))
          do (apply #'dump-layout :suffix ";~%" s)
        when (consp type)
          do (dump-type name (car type) type)))



#++
(print-structs '((:packing :std140)
                 ("S" (:struct ()
                       (b (:bool))
                       (v (:array (:vec (:float 32) 4) 5)
                        :packing :std430)
                       (i (:int 32))))
                 (foo
                  (:block (:major :row :packing :std430 :instance foo!)
                    (s "S")
                    (cond (:bool))))))
