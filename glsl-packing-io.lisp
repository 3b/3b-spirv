(defpackage #:glsl-packing-io
  (:use :cl)
  (:export
   ))
(in-package #:glsl-packing-io)
;;; this should probably be moved to a separate project with glsl-packing

(defun translate-name (n)
  (if (stringp n)
      n
      (string-downcase n)))

(defun print-member (name type)
  (if name
      (format t "~a ~a;" type (translate-name name))
      type))

(defmethod dump-type (name (base (eql :bool)) type)
  (print-member name
                "bool"))

(defmethod dump-type (name (base (eql :int)) type)
  (print-member name
                (ecase (second type)
                  (1 "int8")
                  (2 "uint16")
                  (4 "int")
                  (8 "int64"))))

(defmethod dump-type (name (base (eql :uint)) type)
  (print-member name
                (ecase (second type)
                  (1 "uint8")
                  (2 "uint16")
                  (4 "uint")
                  (8 "uint64"))))

(defmethod dump-type (name (base (eql :float)) type)
  (print-member name
                (ecase (second type)
                  (2 "half")
                  (4 "float")
                  (8 "double"))))

(defmethod dump-type (name (base (eql :vec)) type)
  (print-member name
                (format nil
                        "~a~a"
                        (ecase (first (second type))
                          (:bool "bvec")
                          (:int "ivec")
                          (:uint "uvec")
                          (:float
                           (ecase (second (second type))
                             (4 "vec")
                             (8 "dvec"))))
                        (third type))))

(defmethod dump-type (name (base (eql :mat)) type)
  (print-member name
                (format nil
                        "~a~ax~a"
                        (ecase (first (second type))
                          (:float
                           (ecase (second (second type))
                             (4 "mat")
                             (8 "dmat"))))
                        (third type)
                        (fourth type))))

(defmethod dump-type (name (base (eql :array)) type)
  (if name
      (format t "~a ~a[~a]" (dump-type nil (caadr type) (second type))
              name
              (if (eq (third type) '*) "" (third type)))
      (format t "~a[~a]" (dump-type nil (caadr type) (second type))
              (if (eq (third type) '*) "" (third type)))))

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
                       (v (:array (:vec (:float 4) 4) 5)
                        :packing :std430)
                       (i (:int 4))))
                 (foo
                  (:block (:major :row :packing :std430 :instance foo!)
                    (s "S")
                    (cond (:bool))))))
