(defpackage #:glsl-packing
  (:use :cl)
  (:export #:pack-structs))
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
;;    where size is number of bits, 8,16,32,64 for int/uint, 16,32,64 for float
;;    (8,16,64 may require extensions)
;;    (bool are stored as uint, so can be specified as either (:bool)
;;     or (:uint 32) which might eventually affect whether a hypothetical
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
;;     ELEMENT-TYPE is any type (aside from variable size array,
;;       :struct or :block) or previously defined type name (except :block)
;; struct/block
;;   (:struct () &rest members)
;;   (:block (&key packing major) &rest members)
;;     PACKING specifies default packing of members
;;     MAJOR specifies default layout for matrices in members
;;     MEMBERS is any number of (name type &key offset align major)
;;       lists
;;     NAME is any symbol (or string?) used to identify member in output
;;     TYPE is any of the above types (except :struct or :block)
;;       or previously defined type name (except :block)
;;       (variable size arrays can only be last member of top-level block)
;;     ALIGN is 0 or a power of 2 to specify minimum alignment of
;;        the member in bytes
;;     MAJOR is nil for default matrix layout, or :column, or :row


(defparameter *packing* :std140)
(defparameter *major* :column)
 ;; bind *known-types* to equal hash table (stores name -> definition)
(defvar *known-types*)
;; bind *used-types* to equal hash table (stores (name pack major) -> layout)
(defvar *used-types*)
;; bind *type-dependencies* to equal hash table
;;   (stores (name pack major) -> ((name pack major)*)
;; (used for toplogical sort of output)
(defvar *type-dependencies*)
(defparameter *dump-base-types* nil)
;; bind to equal hash table (stores (name pack major) -> T
(defvar *dumped-types*)
(defvar *output*) ;; bind to nil

(defun lit (n)
  (etypecase n
    (number n)
    ((cons (eql :literal))
     (assert (numberp (third n)))
     (third n))))

(deftype scalar () '(cons (member :bool :int :uint :float)))
(deftype base-type () '(cons (member :bool :int :uint :float :vec :mat)))

(deftype block/buffer/struct () '(cons (member :block :buffer :struct)
                                  (cons list)))

(deftype type-description ()
  '(or block/buffer/struct
    (cons (member :bool :int :uint :float :vec :mat :array))))


(defun round-to-multiple-of (value granularity)
  (* granularity (ceiling value granularity)))

(defun get-type (type &key (errorp t))
  (or (gethash type *known-types*)
      (typecase type
        (type-description
         type)
        (t
         (or ;(gethash type *known-types*)
          (when errorp
            (error "unknown type ~s?" type)))))))

(defgeneric size* (base type))
(defgeneric align* (base type))
(defgeneric stride* (base type))
(defgeneric dump* (base type))

(defmethod stride* (base type)
  ;; only matrices and arrays have stride
  nil)

(defun size (type)
  (or (getf (gethash (list type *packing* *major*) *dumped-types*) :size)
      (getf (gethash type *dumped-types*) :size)
      (size* (car type) type)))

(defun align (type)
  (or (getf (gethash (list type *packing* *major*) *dumped-types*) :align)
      (getf (gethash type *dumped-types*) :align)
      (align* (car type) type)))

(defun stride (type)
  (let* ((d (or (gethash (list type *packing* *major*) *dumped-types*)
                (gethash type *dumped-types*))))
    (if d
        `(,@(when (getf d :stride)
              `(:stride ,(getf d :stride)))
          ,@(when (getf d :matrix-stride)
              `(:matrix-stride ,(getf d :matrix-stride))))
        (stride* (car type) type))))

(defun dump (type)
  (unless (and (consp type)
               (member (second type) '(:std140 :std430))
               (member (third type) '(:row :column)))
    (setf type (list type *packing* *major*)))
  (cond
    ((consp type)
     (unless (nth-value 1 (gethash type *dumped-types*))
       (loop for dep in (gethash type *type-dependencies*)
             do (dump dep))
       (let* ((definition (or (get-type type :errorp nil)
                              (get-type (car type))))
              (dump (dump* (car definition) definition)))
         (push (list type dump) *output*)
         (setf (gethash type *dumped-types*) dump)))
     (gethash type *dumped-types*))))


(defmethod align* (base type)
  ;; default to size of type
  (size type))

(defmethod align* ((base (eql :vec)) type)
  (destructuring-bind (b el n) type
    (declare (ignore b))
    (check-type el scalar)
    (ecase n
      ;; 2 or 4 element vectors are 2 or 4 times alignment of element type
      ((2 4)
       (* n (size el)))
      ;; 3 element vectors are 4 times alignment of element type
      (3 (* 4 (size el))))))

(defmethod align* ((base (eql :mat)) type)
  (destructuring-bind (b el c r) type
    (declare (ignore b))
    (check-type el (cons (eql :float)))
    (ecase *major*
      (:column
       ;; column-major matrix is treated as an array of C vectors of
       ;; size R
       (align `(:array (:vec ,el ,r) ,c)))
      (:row
       ;; row-major matrix is treated as an array of R vectors of
       ;; size C
       (align `(:array (:vec ,el ,c) ,r))))))



(defmethod align* ((base (eql :array)) type)
  (destructuring-bind (b el n &rest r) type
    (declare (ignore b r))
    ;; array of matrices is handles specially:
    (if (typep el '(cons (eql :mat)))
        (destructuring-bind (b mel c r) el
          (declare (ignore b))
          (check-type mel (cons (eql :float)))
          (ecase *major*
            (:column
             ;; array of N column-major matrices is treated as an array of
             ;; (* N C) vectors of size R
             (align `(:array (:vec ,mel ,r) ,(* (lit n) c))))
            (:row
             ;; array of N row-major matrices is treated as an array of
             ;; (* N R) vectors of size C
             (align `(:array (:vec ,mel ,c) ,(* (lit n) r))))))
        (ecase *packing*
          (:std140
           ;; std140 alignment is alignment of elements rounded up
           ;; to a multiple of alignment of vec4
           (round-to-multiple-of (align el)
                                 (align '(:vec (:float 32) 4))))
          (:std430
           ;; std430 removes the rounding to multiple of vec4
           (align el))))))


(defmethod align* ((base (eql :struct)) type)
  (destructuring-bind (b (&key major) &rest members) type
    (declare (ignore b))
    (let* ((*major* (or major *major*))
           (base-align
             (loop for (name mtype . keys) in members
                   maximize
                   (destructuring-bind (&key offset align major)
                       keys
                     (declare (ignore offset align))
                     (let ((*major* (or major *major*)))
                       (align mtype))))))
      (ecase *packing*
        (:std140
         ;; std140 alignment of struct is max of alignments of members,
         ;; rounded up to multiple of vec4
         (round-to-multiple-of base-align (align '(:vec (:float 32) 4))))
        (:std430
         ;; std430 removes the rounding to multiple of vec4
         base-align)))))

(defmethod size* ((base (eql :bool)) type)
  4)

(defmethod size* ((base (eql :int)) type)
  (/ (second type) 8))

(defmethod size* ((base (eql :uint)) type)
  (/ (second type) 8))

(defmethod size* ((base (eql :float)) type)
  (/ (second type) 8))

(defmethod size* ((base (eql :vec)) type)
  (destructuring-bind (b el n) type
    (declare (ignore b))
    (check-type el scalar)
    (* n (size el))))

(defmethod size* ((base (eql :mat)) type)
  (destructuring-bind (b el c r) type
    (declare (ignore b))
    (check-type el scalar)
    (ecase *major*
      (:column
       ;; column-major matrix is treated as an array of C vectors of
       ;; size R
       (size `(:array (:vec ,el ,r) ,c)))
      (:row
       ;; row-major matrix is treated as an array of R vectors of
       ;; size C
       (size `(:array (:vec ,el ,c) ,r))))))


(defmethod stride* ((base (eql :mat)) type)
  (list :matrix-stride (align type)))

(defmethod stride* ((base (eql :array)) type)
  (destructuring-bind (b el n &rest r) type
    (declare (ignore b n r))
    (let* ((a (align type))
           (s (size el)))
      (if (eq (car (get-type el)) :mat)
          ;; for matrix, stride is alignment of array (= alignment of array of
          ;; column/row vector) * # of rows or columns
          (destructuring-bind (b mel c r) el
            (declare (ignore b))
            (check-type mel scalar)
            (ecase *major*
              (:column
               ;; column-major matrix is treated as an array of C
               ;; vectors of size R, so matrix stride is C * vector
               ;; stride (= array alignment)
               (list :stride (* a c) :matrix-stride a))
              (:row
               ;; row-major matrix is treated as an array of R vectors of
               ;; size C, so matrix stride is R * vector
               ;; stride (= array alignment)
               (list :stride (* a r) :matrix-stride a))))
          ;; for scalars or struct, stride is size of element rounded
          ;; up to alignment of array (which differ for 3 element
          ;; vectors and structs)
          (list :stride (round-to-multiple-of s a))))))

(defmethod size* ((base (eql :array)) type)
  (destructuring-bind (b el n &rest r) type
    (declare (ignore b el r))
    ;; matrices are handled specially, but STRIDE take care of that
    (let* ((align (align type))
           (stride (getf (stride type) :stride)))
      ;; may have padding if alignment is larger than element size
      ;; (either due to std140 rounding or due to 3 element taking up
      ;;  size of 4 element)
      (if (eq n '*)
          ;; not sure unsized array should return size of 0 or 1 element?
          0
          (round-to-multiple-of (* stride (lit n)) align)))))

#++
(defmethod size* ((base (eql :struct)) type)
  (destructuring-bind (b (&key major) &rest members) type
    (declare (ignore b))
    (let* ((*major* (or major *major*))
           (a (align type))
           (start 0)
           (base-size
             (loop with next-offset = 0
                   for (name mtype . keys) in members
                   do (destructuring-bind (&key offset align major)
                          keys
                        (when align
                          ;; align must be a power or 2
                          (assert (or (zerop align)
                                      (= 1 (logcount align)))))
                        (let ((*major* (or major *major*))
                              #++(*member-path* (cons name *member-path*)))
                          (let ((malign (max (align mtype)
                                             (or align 0)))
                                (msize (progn
                                        ;let ((*container-offset* next-offset))
                                         (size mtype)))
                                (mstride (stride mtype)))
                            ;; explicit offset must be a multiple of alignment
                            (when offset
                              (assert (zerop (mod offset malign))))
                            ;; member starts at next multiple of
                            ;; its alignment
                            (setf next-offset
                                  (round-to-multiple-of next-offset malign))
                            ;; pushnew is a bit of a hack here, to
                            ;; work around align being called twice
                            ;; causing size to be called twice when
                            ;; arrays contain structs possibly should
                            ;; cache results of align/stride instead?
                            ;; (can't cache size, since it is responsible
                            ;;  for dumping output currently, which changes
                            ;;  for repeated structs)
                            #++(pushnew `(,(reverse *member-path*)
                                          :offset ,(+ start next-offset)
                                          :local-offset ,next-offset
                                          :align ,malign
                                          :size ,msize
                                          ,@mstride)
                                        *member-offsets*
                                        :key 'car :test 'equal)
                            ;; adjust offset for size of member
                            (incf next-offset msize))))
                   finally (return next-offset))))
      ;; may have padding if alignment is larger than element size
      ;; (either due to std140 rounding or due to 3 element taking up
      ;;  size of 4 element)
      (values (round-to-multiple-of base-size a) a))))

(defmethod size* ((base (eql :block)) type)
  ;; same as struct, except allows specifying packing
  (destructuring-bind (b opts &rest members) type
    (declare (ignore b))
    (let ((*packing* (or (getf opts :packing) *packing*)))
      (size (list* :struct (alexandria:remove-from-plist opts :packing)
                   members)))))

(defmethod size* ((base (eql :buffer-block)) type)
  ;; same as struct, except allows specifying packing
  (destructuring-bind (b opts &rest members) type
    (declare (ignore b))
    (let ((*packing* (or (getf opts :packing) *packing*)))
      (size (list* :struct (alexandria:remove-from-plist opts :packing)
                   members)))))

(defmethod dump* (base type)
  (check-type type scalar)
  (list :size (size type) :align (align type) :base-type t))

(defmethod dump* ((base (eql :vec)) type)
  (dump (second type))
  (list :size (size type) :align (align type) :base-type t))

(defmethod dump* ((base (eql :mat)) type)
  (dump (second type))
  (list* :size (size type) :align (align type)
         :base-type t (stride type)))

(defmethod dump* ((base (eql :array)) type)
  (when (typep (second type) '(cons (member :struct :block :buffer-block)))
    (error "Can't store anonymous BLOCK or STRUCT in array."))
  (dump (second type))
  (list* :size (size type) :align (align type) (stride type)))

(defmethod dump* ((base (eql :struct)) type)
  (destructuring-bind (b (&key  &allow-other-keys) &rest members) type
    (declare (ignore b))
    (let ((salign 0 #++(align type))
          (mdumps nil)
          (next-offset 0))
      (setf mdumps
            (loop
              for (name mtype . keys) in members
              append
              (destructuring-bind (&key offset align major)
                  keys
                (when (typep (second type)
                             '(cons (member :struct :block :buffer-block)))
                  (error "Can't store anonymous BLOCK or STRUCT in struct."))
                (when align
                  ;; align must be a power or 2
                  (assert (or (zerop align)
                              (= 1 (logcount align)))))
                (let* ((*major* (or major *major*))
                       (mtype-info (dump (list mtype *packing* *major*)))
                       (malign (max (getf mtype-info :align)
                                    (or align 0)))
                       (msize (getf mtype-info :size))
                       (stride (getf mtype-info :stride))
                       (mstride (getf mtype-info :matrix-stride))
                       #++(mname (getf mtype-info :name))
                       (mmembers (getf mtype-info :members)))
                  (setf salign (max salign malign))
                  ;; explicit offset must be a multiple of alignment
                  (when offset
                    (assert (zerop (mod offset malign))))
                  ;; member starts at next multiple of
                  ;; its alignment
                  (setf next-offset
                        (round-to-multiple-of next-offset malign))
                  (prog1 `((:name ,name
                            :offset ,next-offset
                            :align ,malign
                            :size ,msize
                            :type ,(list mtype *packing* *major*)
                            ,@(when stride
                                `(:stride ,stride))
                            ,@(when mstride
                                `(:matrix-stride ,mstride))
                            ,@ (when mstride
                                 `(:major ,*major*)))
                           ,@(loop for mm in mmembers
                                   collect
                                   (list*
                                    :name (cons name
                                                (alexandria:ensure-list
                                                 (getf mm :name)))
                                    :offset (+ next-offset
                                               (getf mm :offset))
                                    :nested t
                                    (alexandria:remove-from-plist
                                     mm :name :offset :nested))))
                    (incf next-offset msize))))))
      (when (eq *packing* :std140)
        (setf salign (round-to-multiple-of salign
                                           (align '(:vec (:float 32) 4)))))
      `(:size ,(round-to-multiple-of next-offset salign)
        :align ,salign
        ,@(stride type)
        :members ,mdumps))))

(defmethod dump* ((base (eql :block)) type)
  (destructuring-bind (b (&key major packing instance) &rest members) type
    (declare (ignore b members instance))
    (let ((*packing* (or packing *packing*))
          (*major* (or major *major*)))
      (dump* :struct type))))

(defmethod dump* ((base (eql :buffer-block)) type)
  (destructuring-bind (b (&key major packing instance) &rest members) type
    (declare (ignore b members instance))
    (let ((*packing* (or packing *packing*))
          (*major* (or major *major*)))
      (dump* :struct type))))



#++
(defun pack-block (name block
                   &key (packing :std140) (major :column))
  "Calculate layout, size and alignment of BLOCK with specified
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
(defun pack-block (name type))


(defun pack-structs (types &key ROOTS dump-base-types)
  "TYPES is a list of lists (NAME TYPE) which is processed like a
sequence of calls to PACK-STRUCT, except with the ability to refer to
previous types by name. if NAME is :packing or :major, TYPE is
interpreted as new default value for corresponding arguments to
PACK-STRUCT for following definitions. If ROOTS is specified, it
should be a list of block names to use as roots when removing unused
types from output. If DUMP-BASE-TYPES is true, output will include
data for used scalar/vec/mat types types."
  (let ((*packing* *packing*)
        (*major* *major*)
        (*known-types* (make-hash-table :test #'equal))
        (*used-types* (make-hash-table :test #'equal))
        (*type-dependencies* (make-hash-table :test #'equal))
        (*dumped-types* (make-hash-table :test #'equal))
        (*dump-base-types* dump-base-types)
        (*output* nil)
        (blocks nil))
    (loop for (name type) in types
          do (format t "-===== ~s ~s~%" name (if (consp type) (car type) type))
          when (eq name :packing)
            do (setf *packing* type)
          when (eq name :major)
            do (setf *major* type)
          when (gethash name *known-types*)
            do (error "duplicate type name ~s?~%~s -> ~s~%"
                      name (gethash name *known-types*) type)
          when (typep type 'type-description)
            do (setf (gethash name *known-types*) type)
               (format t "added type ~s -> ~s~%" name type)
               (when (and (member (car type) '(:block :buffer-block))
                          (or (not roots)
                              (member name roots :test 'equal)))
                 (when (assoc name blocks :test 'equal)
                   (error "duplicate block name ~s?~%~s -> ~s~%"
                          name
                          (cdr (assoc name blocks :test 'equal))
                          type))
                 (push (list name type) blocks)))
    (setf blocks (nreverse blocks))
    #++(loop for (name type) in blocks
             do (pack-block name type))
    (loop for (name type) in blocks
          for packing = (or (getf (second type) :packing) *packing*)
          for major = (or (getf (second type) :major) *major*)
          do (dump (list name packing major)))
    (nreverse *output*)))

#++
(pack-structs '((:packing :std140)
                ("S" (:struct ()
                      (b (:bool))
                      (v (:array (:vec (:float 32) 4) 5))
                      (i (:int 32))))
                (foo
                 (:block ()
                   (s "S")
                   (cond (:bool))))))

#++
(((:BOOL)
  (:SIZE 4 :ALIGN 4 :BASE-TYPE T))
 ((:FLOAT 4)
  (:SIZE 4 :ALIGN 4 :BASE-TYPE T))
 ((:VEC (:FLOAT 4) 4)
  (:SIZE 16 :ALIGN 16 :BASE-TYPE T))
 ((:ARRAY (:VEC (:FLOAT 4) 4) 5)
  (:SIZE 80 :ALIGN 16 :STRIDE 16))
 ((:INT 4)
  (:SIZE 4 :ALIGN 4 :BASE-TYPE T))
 (("S" :STD140 :COLUMN)
  (:SIZE 112 :ALIGN 16
   :MEMBERS ((:NAME B :OFFSET 0 :ALIGN 4 :SIZE 4)
             (:NAME V :OFFSET 16 :ALIGN 16 :SIZE 80 :STRIDE 16)
             (:NAME I :OFFSET 96 :ALIGN 4 :SIZE 4))))
 ((FOO :STD140 :COLUMN)
  (:SIZE 128 :ALIGN 16
   :MEMBERS ((:NAME S :OFFSET 0 :ALIGN 16 :SIZE 112)
             (:NAME (S B) :OFFSET 0 :ALIGN 4 :SIZE 4)
             (:NAME (S V) :OFFSET 16 :ALIGN 16 :SIZE 80 :STRIDE 16)
             (:NAME (S I) :OFFSET 96 :ALIGN 4 :SIZE 4)
             (:NAME COND :OFFSET 112 :ALIGN 4 :SIZE 4)))))
