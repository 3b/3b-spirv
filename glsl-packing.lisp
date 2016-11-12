(defpackage #:glsl-packing
  (:use :cl)
  (:export #:pack-struct))
(in-package #:glsl-packing)
;;; this should probably be moved to a separate project at some point...


;;; given a simple sexp representation of a glsl struct, return
;;; alignment for struct and offsets for all members, using either
;;; std140 or std430 layout from glsl specification

;;; sexp format:
;; (todo: add utility for converting from various type names from glsl
;;        and extensions?)
;; scalar types:
;;   (:bool) (:int size), (:uint size), (:float size)
;;    where size is number of bytes, 1,2,4,8 for int/uint, 2,4,8 for float
;;    (1,2,8 may require extensions)
;;    (bool are stored as uint, so can be specified as either (:bool)
;;     or (:uint 4) which might eventually affect whether a hypothetical
;;     accessor generator expected <true>/nil or 1/0)
;;   (not sure if we need to distinguish atomic_uint and similar?
;; vector/matrix types
;;   (:vec scalar-component-type count)
;;   (:mat scalar-component-type columns rows)
;;     where scalar-component-type is one of the scalar types above
;;     and count,columns,rows are 2,3,4
;; arrays
;;   (:array element-type count)
;;     count can be a number or * to indicate variable size
;;     ELEMENT-TYPE is any type (aside from variable size array)
;; struct/block
;;   (:struct &rest members)
;;     MEMBERS is any number of (name type &key offset align major)
;;       lists
;;     NAME is any symbol (or string?) used to identify member in output
;;     TYPE is any of the above types (including :struct and :array)
;;       (variable size arrays can only be last member of top-level struct)
;;     PACKING is nil to inherit default packing, :std140, or :std430
;;     ALIGN is 0 or a power of 2 to specify minimum alignment of
;;        the member
;;     MAJOR is nil for default matrix layout, or :column, or :row


(defparameter *packing* :std140)
(defparameter *major* :column)
(defparameter *member-offsets* nil)
(defparameter *member-path* nil)
(defparameter *container-offset* 0)

(defun round-to-multiple-of (value granularity)
  (* granularity (ceiling value granularity)))

(defgeneric size* (base type))
(defgeneric align* (base type))

(defun size (type)
  (size* (car type) type))

(defun align (type)
  (align* (car type) type))

(defmethod align* (base type)
  ;; default to size of type
  (size type))

(defmethod align* ((base (eql :vec)) type)
  (destructuring-bind (b el n) type
    (declare (ignore b))
    (ecase n
      ;; 2 or 4 element vectors are 2 or 4 times alignment of element type
      ((2 4)
       (* n (size el)))
      ;; 3 element vectors are 4 times alignment of element type
      (3 (* 4 (size el))))))

(defmethod align* ((base (eql :mat)) type)

  ;;;;; todo

)



(defmethod align* ((base (eql :array)) type)
  (destructuring-bind (b el n) type
    (declare (ignore b n))
    (ecase *packing*
      (:std140
       ;; std140 alignment/stride is alignment of elements rounded up
       ;; to a multiple of alignment of vec4
       (round-to-multiple-of (align el)
                             (align '(:vec (:float 4) 4))))
      (:std430
       ;; std430 removes the rounding to multiple of vec4
       (align el)))))


(defmethod align* ((base (eql :struct)) type)
  (destructuring-bind (b &rest members) type
    (declare (ignore b))
    (let ((base-align
            (loop for (name mtype . keys) in members
                  maximize
                  (destructuring-bind (&key offset align major packing)
                      keys
                    (declare (ignore offset align))
                    (let ((*packing* (or packing *packing*))
                          (*major* (or major *major*)))
                      (align mtype))))))
      (ecase *packing*
        (:std140
         ;; std140 alignment of struct is max of alignments of members,
         ;; rounded up to multiple of vec4
         (round-to-multiple-of base-align (align '(:vec (:float 4) 4))))
        (:std430
         ;; std430 removes the rounding to multiple of vec4
         base-align)))))

(defmethod size* ((base (eql :bool)) type)
  4)

(defmethod size* ((base (eql :int)) type)
  (second type))

(defmethod size* ((base (eql :uint)) type)
  (second type))

(defmethod size* ((base (eql :float)) type)
  (second type))

(defmethod size* ((base (eql :vec)) type)
  (destructuring-bind (b el n) type
    (declare (ignore b))
    (* n (size el))))

(defmethod size* ((base (eql :mat)) type)
  (destructuring-bind (b el c r) type
    (declare (ignore b))
    (ecase *major*
      (:row
;;;;;;; todo
       )
      (:column
;;;;;;; todo
       ))))

(defmethod size* ((base (eql :array)) type)
  (destructuring-bind (b el n) type
    (declare (ignore b el))
    (let ((a (align type)))
      ;; may have padding if alignment is larger than element size
      ;; (either due to std140 rounding or due to 3 element taking up
      ;;  size of 4 element)
      (if (eq n '*)
          ;; not sure unsized array should return size of 0 or 1 element?
          0
          (round-to-multiple-of (* a n) a)))))

(defmethod size* ((base (eql :struct)) type)
  (destructuring-bind (b &rest members) type
    (declare (ignore b))
    (let* ((a (align type))
           (start (round-to-multiple-of *container-offset* a))
           (base-size
             (loop with next-offset = 0
                   for (name mtype . keys) in members
                   do (destructuring-bind (&key offset align major packing)
                          keys
                        (when align
                          ;; align must be a power or 2
                          (assert (or (zerop align)
                                      (= 1 (logcount align)))))
                        (let ((*packing* (or packing *packing*))
                              (*major* (or major *major*))
                              (*member-path* (cons name *member-path*)))
                          (let ((malign (max (align mtype)
                                             (or align 0)))
                                (msize (let ((*container-offset* next-offset))
                                         (size mtype))))
                            ;; explicit offset must be a multiple of alignment
                            (when offset
                              (assert (zerop (mod offset malign))))
                            ;; member starts at next multiple of
                            ;; its alignment
                            (setf next-offset
                                  (round-to-multiple-of next-offset malign))
                            (push (list (reverse *member-path*)
                                        :offset (+ start next-offset)
                                        :local-offset next-offset
                                        :align malign
                                        :size msize)
                                  *member-offsets*)
                            ;; adjust offset for size of member
                            (incf next-offset msize))))
                   finally (return next-offset))))
      ;; may have padding if alignment is larger than element size
      ;; (either due to std140 rounding or due to 3 element taking up
      ;;  size of 4 element)
      (values (round-to-multiple-of base-size a) a))))

(defun pack-struct (name struct
                    &key (packing :std140) (major :column))
  "Calculate layout, size and alignment of STRUCT with specified
packing and matrix layout. Returns plist of :SIZE :ALIGN and :MEMBERS.
MEMBERS is a list for each member in struct (including nested structs)
of format (path &key offset local-offset align size). Path for a
member is a list of member names to reach the member, starting from
NAME. Offset is the offset in octets from the start of the top level
struct, and local-offset is offset from start of innermost enclosing
struct. Align and size are the alignment and size of that member in
bytes (including any struct/array padding)."
  (let ((*packing* packing)
        (*major* major)
        (*member-path* (list name))
        (*member-offsets* nil))
    (multiple-value-bind (size align) (size struct)
     (list :size size
           :align align
           :members (reverse *member-offsets*)))))


#++
(pack-struct 'foo
             '(:struct
               (s (:struct
                   (b (:bool))
                   (v (:array (:vec (:float 4) 4) 5))
                   (i (:int 4))))
               (cond (:bool)))
             :packing :std140)
#++
(:SIZE 128 :ALIGN 16 :MEMBERS
 (((FOO S B) :OFFSET 0 :LOCAL-OFFSET 0 :ALIGN 4 :SIZE 4)
  ((FOO S V) :OFFSET 16 :LOCAL-OFFSET 16 :ALIGN 16 :SIZE 80)
  ((FOO S I) :OFFSET 96 :LOCAL-OFFSET 96 :ALIGN 4 :SIZE 4)
  ((FOO S) :OFFSET 0 :LOCAL-OFFSET 0 :ALIGN 16 :SIZE 112)
  ((FOO COND) :OFFSET 112 :LOCAL-OFFSET 112 :ALIGN 4 :SIZE 4)))

#++
(pack-struct 'foo
             '(:struct
               (v1 (:vec (:float 4) 3))
               (v1 (:vec (:float 4) 3))
               (f1 (:float 4))
               (f2 (:float 4))
               (av (:array (:vec (:float 4) 3) 1))
               (f3 (:float 4))
               (f4 (:float 4))
               (sv (:struct (v2 (:vec (:float 4) 3))))
               (f5 (:float 4))
               (f6 (:float 4))
               (sv (:struct (av2 (:array (:vec (:float 4) 3) 1))))
               (f7 (:float 4))))

#++
(:SIZE 144 :ALIGN 16 :MEMBERS
 (((FOO V1) :OFFSET 0 :LOCAL-OFFSET 0 :ALIGN 16 :SIZE 12)
  ;; vec3 is aligned to 16 with no padding
  ((FOO V1) :OFFSET 16 :LOCAL-OFFSET 16 :ALIGN 16 :SIZE 12)
  ;; following floats are aligned to 4 immediately after vec3
  ((FOO F1) :OFFSET 28 :LOCAL-OFFSET 28 :ALIGN 4 :SIZE 4)
  ((FOO F2) :OFFSET 32 :LOCAL-OFFSET 32 :ALIGN 4 :SIZE 4)
  ;; array is aligned to 16 and padded to 16
  ((FOO AV) :OFFSET 48 :LOCAL-OFFSET 48 :ALIGN 16 :SIZE 16)
  ;; next float is after padding
  ((FOO F3) :OFFSET 64 :LOCAL-OFFSET 64 :ALIGN 4 :SIZE 4)
  ((FOO F4) :OFFSET 68 :LOCAL-OFFSET 68 :ALIGN 4 :SIZE 4)
  ;; struct is aligned to 16 and padded to 16
  ((FOO SV V2) :OFFSET 80 :LOCAL-OFFSET 0 :ALIGN 16 :SIZE 12)
  ((FOO SV) :OFFSET 80 :LOCAL-OFFSET 80 :ALIGN 16 :SIZE 16)
  ;; next float is after padding
  ((FOO F5) :OFFSET 96 :LOCAL-OFFSET 96 :ALIGN 4 :SIZE 4)
  ((FOO F6) :OFFSET 100 :LOCAL-OFFSET 100 :ALIGN 4 :SIZE 4)
  ;; struct of array is aligned and padded to 16
  ((FOO SV AV2) :OFFSET 112 :LOCAL-OFFSET 0 :ALIGN 16 :SIZE 16)
  ((FOO SV) :OFFSET 112 :LOCAL-OFFSET 112 :ALIGN 16 :SIZE 16)
  ;; following float is after padding
  ((FOO F7) :OFFSET 128 :LOCAL-OFFSET 128 :ALIGN 4 :SIZE 4)))
