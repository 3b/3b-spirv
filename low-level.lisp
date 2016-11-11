(in-package #:3b-spirv)

(defparameter *manual-opcodes* '(spirv-core:ext-inst))

(defparameter *enum-operand-types*
  (alexandria:plist-hash-table
   '(:id-scope :scope
     :id-memory-semantics :memory-access
     :image-operands :image-operands
     :storage-class :storage-class
     :source-language :source-language
     :selection-control :selection-control
     :scope :scope
     :sampler-filter-mode :sampler-filter-mode
     :sampler-addressing-mode :sampler-addressing-mode
     :memory-semantics :memory-semantics
     :memory-model :memory-model
     :memory-access :memory-access
     :loop-control :loop-control
     :linkage-type :linkage-type
     :kernel-profiling-info :kernel-profiling-info
     :kernel-enqueue-flags :kernel-enqueue-flags
     :image-operands :image-operands
     :image-format :image-format
     :image-channel-order :image-channel-order
     :image-channel-data-type :image-channel-data-type
     :group-operation :group-operation
     :function-parameter-attribute :function-parameter-attribute
     :function-control :function-control
     :fp-rounding-mode :fp-rounding-mode
     :fp-fast-math-mode :fp-fast-math-mode
     :execution-model :execution-model
     :execution-mode :execution-mode
     :dim :dim
     :decoration :decoration
     :capability :capability
     :built-in :built-in
     :addressing-model :addressing-model
     :access-qualifier :access-qualifier)))

(defparameter *opcode-sections*
  (alexandria:alist-hash-table
   `((spirv-core:capability 1)
     (spirv-core:extension 2)
     (spirv-core:ext-inst-import 3)
     (spirv-core:memory-model 4) ;; todo: limit to 1
     (spirv-core:entry-point 5)
     (spirv-core:execution-mode 6)
     ;; debug info
     (spirv-core:string 7) ;; 7a
     (spirv-core:source-extension 7)
     (spirv-core:source 7)
     (spirv-core:source-continued 7)
     (spirv-core:name 7.5) ;; 7b
     (spirv-core:member-name 7.5)
     ;; annotations
     (spirv-core:decorate 8)
     (spirv-core:member-decorate 8)
     (spirv-core:group-decorate 8)
     (spirv-core:group-member-decorate 8)
     (spirv-core:decoration-group 8)
     ;; :line is valid starting at 9
     (spirv-core:line 9 10 11)
     ;; types/globals
     (spirv-core:type-void 9)
     (spirv-core:type-bool 9)
     (spirv-core:type-int 9)
     (spirv-core:type-float 9)
     (spirv-core:type-vector 9)
     (spirv-core:type-matrix 9)
     (spirv-core:type-image 9)
     (spirv-core:type-sampler 9)
     (spirv-core:type-sampled-image 9)
     (spirv-core:type-array 9)
     (spirv-core:type-runtime-array 9)
     (spirv-core:type-struct 9)
     (spirv-core:type-opaque 9)
     (spirv-core:type-pointer 9)
     (spirv-core:type-function 9)
     (spirv-core:type-event 9)
     (spirv-core:type-device-event 9)
     (spirv-core:type-reserve-id 9)
     (spirv-core:type-queue 9)
     (spirv-core:type-pipe 9)
     (spirv-core:type-forward-pointer 9)
     (spirv-core:constant-true 9)
     (spirv-core:constant-false 9)
     (spirv-core:constant 9)
     (spirv-core:constant-composite 9)
     (spirv-core:constant-sampler 9)
     (spirv-core:constant-null 9)
     (spirv-core:spec-constant-true 9)
     (spirv-core:spec-constant-false 9)
     (spirv-core:spec-constant 9)
     (spirv-core:spec-constant-composite 9)
     (spirv-core:spec-constant-op 9)
     (spirv-core:variable 9 11) ;; depends on storage class
     (spirv-core:undef 9 11)    ;; should be in 9
     ;; functions declaration/definition
     (spirv-core:function 10 11)
     (spirv-core:function-parameter 10 11)
     (spirv-core:function-end 10 11)
     ;; everything else is function body
     )))

;; vars that should be bound while assembling
(defvar *next-id*) ;; bind to 0
(defvar *name->id*) ;; bind to equal hash table (stores anonymous struct and array types as lists)
(defvar *ext-enums*) ;; bind to eq hash table (maps id to enum map)
;; used for error checking
(defvar *enabled-caps*) ;; bind to eq hash table
(defvar *defined-types*) ;; bind to equal hash table
(defvar *defined-constants*) ;; bind to equal hash table
(defvar *defined-globals*) ;; equal hash table
(defvar *current-section*) ;; bind to 1
;; just for debugging
(defvar *current-form* nil)
(defvar *id-uses*) ;; hash table

(defmacro with-ll-asm-context (() &body body)
  `(let ((*next-id* 0)
         (*name->id* (make-hash-table :test #'equal))
         (*ext-enums* (make-hash-table))
         (*enabled-caps* (make-hash-table))
         (*defined-types* (make-hash-table :test #'equal))
         (*defined-constants* (make-hash-table :test #'equal))
         (*defined-globals* (make-hash-table :test #'equal))
         (*current-section* 1)
         (*id-uses* (make-hash-table))
         (*current-form* nil))
     (with-spirv-output ()
       ,@body)))

(defun translate-symbol-name (s)
  ;; not sure if we should have a default package or not?
  (let ((*package* (or (find-package :3bgl-glsl/cl)
                       (find-package :cl))))
    (format nil "~(~a~)" s)))

(defun use-extension (id string)
  (unless (string= string "GLSL.std.450")
    (error "don't know how to assemble extension ~s?" string))
  (setf (gethash id *ext-enums*) (getf 3b-spirv::*spec* :glsl-opcodes)))

(defun check-section (op)
  ;; section numbers from
  ;; https://www.khronos.org/registry/spir-v/specs/1.1/SPIRV.html#_a_id_logicallayout_a_logical_layout_of_a_module
  (let* ((section (gethash op *opcode-sections* '(11)))
         (min (reduce 'min section)))
    (unless (member *current-section* section)
      (if (< *current-section* min)
          (setf *current-section* min)
          (error "got instruction ~s in section ~s, should be in ~s" op
                 *current-section* section)))))

(defun check-cap (cap)
  (assert (gethash cap *enabled-caps*) ()
          "tried to use capability ~s without enabling it?" cap))

(defun use-type (type &key define)
  #++(when define (format t "define types ~s~%" type))
  (when define
    (let ((lisp-type t))
      (when (consp define)
        (destructuring-bind (op &rest args) define
          (case op
            ;; types we need to know for numeric constants
            (spirv-core:type-int
             ;; allow T/NIL or 1/0 for signed/unsigned
             (let ((base (if (or (and (numberp (second args))
                                      (zerop (second args)))
                                 (second args))
                             'unsigned-byte
                             'signed-byte)))
               (ecase (first args)
                 (8 (check-cap :int8)) ;; opencl only?
                 (16 (check-cap :int16))
                 (32)
                 (64 (check-cap :int64)))
               (setf lisp-type `(,base ,(first args)))))
            (spirv-core:type-float
             (setf lisp-type
                   (ecase (first args)
                     (16 (check-cap :float16)
                      ;; not exactly right, but we might want to distinguish
                      ;; from single-float
                      'short-float)
                     (32 'single-float)
                     (64 (check-cap :float64)
                      'double-float))))
            ;; mark a few as unusual types just to catch trying to
            ;; store numbers to them
            (spirv-core:type-void
             (setf lisp-type 'null))
            (spirv-core:type-bool
             (setf lisp-type :boolean))
            ;; todo: decide if these need any more work
            (spirv-core:type-vector)
            (spirv-core:type-matrix)
            (spirv-core:type-image)
            (spirv-core:type-sampler)
            (spirv-core:type-sampled-image)
            (spirv-core:type-array)
            (spirv-core:type-runtime-array)
            (spirv-core:type-struct)
            (spirv-core:type-opaque)
            (spirv-core:type-pointer)
            (spirv-core:type-function)
            (spirv-core:type-event)
            (spirv-core:type-device-event)
            (spirv-core:type-reserve-id)
            (spirv-core:type-queue)
            (spirv-core:type-pipe)
            (spirv-core:type-forward-pointer))))
      (setf (gethash type *defined-types*) (list (id type) lisp-type))))
  (or (car (gethash type *defined-types*))
      (error "tried to use undefined type ~s?" type)))

(defun use-constant (name &key define)
  (when define
    (when (gethash name *defined-constants*)
      (error "redefining constant ~s? ~s -> ~s~%"
             name (gethash name *defined-constants*)
             define))
    (setf (gethash name *defined-constants*)
          (list* (id name) define)))
  (or (car (gethash name *defined-constants*))
      (error "tried to use undefined constant ~s?" name)))

(defun define-global (name type storage-class)
  (when (gethash name *defined-globals*)
    (error "multiple definitions for variable ~s?~% ~s -> ~s~%"
           name
           (gethash name *defined-globals*)
           (list name type storage-class)))
  (setf (gethash name *defined-globals*) (list (id name) type storage-class))
  (id name))

;; todo: add syntax for explicit register numbers so we can
;;  round trip through disasm and back to binary with minimal changes
;;  (may need to add an extra pass in that case to avoid conflicts?
;;   possibly only when triggered by actual conflict?)
;;  (or store offsets so we can back patch them?)
(defun id (id)
  (let ((i (or (gethash id *name->id*)
               (setf (gethash id *name->id*) (incf *next-id*)))))
    (when *current-form*
      (pushnew *current-form* (gethash i *id-uses*) :test 'equal))
    i))

(defparameter *opcode-functions* (make-hash-table))

(defun add-op (op lambda)
  (setf (gethash op *opcode-functions*)
        (compile nil lambda)))

(defun asm-word (w)
  ;; allow t/nil for 1/0 for booleans
  (when (eq w t) (setf w 1))
  (when (eq w nil) (setf w 0))
  (assert (integerp w))
  ;; assumes w is never negative for unsigned 8/16bit
  (write-word (ldb (byte 32 0) w)))

(defun asm-word64 (w)
  (assert (integerp w))
  (write-word (ldb (byte 32 0) w))
  (write-word (ldb (byte 32 32) w)))

(ieee-floats:make-float-converters encode-float16 decode-float16 5 10 nil)

(defun asm-float16 (w)
  (assert (floatp w))
  (write-word (ldb (byte 32 0) (encode-float16 w))))

(defun asm-float32 (w)
  (assert (floatp w))
  (write-word (ldb (byte 32 0) (ieee-floats:encode-float32 w))))

(defun asm-float64 (w)
  (assert (floatp w))
  (let ((i (ieee-floats:encode-float64 w)))
    (write-word (ldb (byte 32 0) i))
    (write-word (ldb (byte 32 32) i))))

(defun asm-string (s?)
  (let* ((s (etypecase s?
              (symbol (translate-symbol-name s?))
              (string s?)))
         (o (babel:string-to-octets s :encoding :utf-8))
         (l (length o)))
    ;; null terminated, utf8, in order? pad to multiple of 4
    ;;(format t "~&asm string: ~s~%" s)
    (setf o (adjust-array o (* 4 (ceiling (1+ l) 4)) :initial-element 0))
    (loop for i below (length o) by 4
          do (write-word
              (loop for j from i
                    for b below 4
                    sum (ash (aref o j) (* b 8)))))))

(defun asm-id (id)
  #++(format t "asm id ~s~%" id)
  (write-word (id id)))

(defun asm-type (type)
  (let ((id (use-type type)))
    (write-word id)))

(defun lookup-ext (ext inst)
  (let ((h (gethash ext *ext-enums*)))
    (unless h
      (error "tried to use extension ~s without importing it?" ext))
    (getf (gethash inst h) :value)))

(defun lookup-enum (type enum)
  (let* ((values (gethash type (getf 3b-spirv::*spec* :enums))))
    (unless values
      (error "unknown enum type ~s?" type))
    (ecase (car values)
      (:bit
       (setf enum (alexandria:ensure-list enum))
       (loop with result = 0
             for bit in enum
             for value = (gethash bit (second values))
             do (cond
                  ((eq bit :none)
                   ;; if explicitly specifying :none, don't allow any
                   ;; other bits to be set
                   (assert (equalp enum '(:none))))
                  (value
                   (setf result (logior result (ash 1 value))))
                  (t (error "couldn't find ~s in bitfield type ~s?" bit type)))
             finally (return result)))
      (:value
       (let ((v (gethash enum (second values))))
         (unless v
           (error "enum ~s not found in type ~s?" enum type))
         v)))))

(defun @asm-enum (arg-type value)
  (let* ((enum-type (gethash arg-type *enum-operand-types*)))
    (unless enum-type
      (error "couldn't find enum type for arg type ~s?" arg-type))
    `(write-word (lookup-enum ,enum-type ,value))))

(defmacro defop (op lambda-list &body body)
  (let ((value (getf (gethash op (getf 3b-spirv::*spec* :opcodes))
                     :value)))
    `(add-op ',op
             '(lambda ,lambda-list
               (let ((*current-op* ',op))
                 (with-spirv-instruction (,value)
                   ,@body))))))

(defop spirv-core:ext-inst  (dest type ext inst &rest operand-ids)
  (asm-type type)
  (asm-id dest)
  (asm-id ext)
  (let ((inst# (lookup-ext ext inst)))
    (unless inst#
      (error "couldn't find instruction ~s in extension ~s?"
             inst ext))
    (asm-word inst#))
  (loop for id in operand-ids
        do (asm-id id)))

(defun make-opcodes ()
  (labels ((assemble-op (type &optional type-var)
             (let ((enum (gethash type *enum-operand-types*)))
               (cond
                 (enum
                  (@asm-enum type '(pop args)))
                 ((and (consp type) (eq (car type) :?))
                  `(when args
                     ,(assemble-op (second type))))
                 ((and (consp type) (eq (car type) :*))
                  `(loop while args
                         do ,(assemble-op (second type))))
                 ((consp type)
                  (error "todo"))
                 (t
                  (ecase type
                    (:id-ref `(asm-id (pop args)))
                    (:literal-integer
                     '(asm-word (pop args)))
                    (:literal-string
                     '(asm-string (pop args)))
                    (:pair-literal-integer-id-ref
                     `(progn
                        (asm-word (pop args))
                        (asm-id (pop args))))
                    (:pair-id-ref-literal-integer
                     `(progn
                        (asm-id (pop args))
                        (asm-word (pop args))))
                    (:pair-id-ref-id-ref
                     `(progn
                        (asm-id (pop args))
                        (asm-id (pop args))))
                    (:literal-spec-constant-op-integer
                     '(asm-word (getf (gethash (pop args)
                                       (getf 3b-spirv::*spec* :opcodes))
                                 :value)))
                    (:literal-context-dependent-number
                     `(let* ((type-info (gethash ,type-var
                                                 *defined-types*))
                             (lisp-type (second type-info))
                             (raw-value (pop args))
                             (value (coerce raw-value
                                            ;; short-float is probably wrong
                                            ;; size on any implementation
                                            ;; that has it :/
                                            (if (eq lisp-type 'short-float)
                                                'single-float
                                                lisp-type))))
                        (ecase (if (consp lisp-type)
                                   (car lisp-type)
                                   lisp-type)
                          (short-float
                           (asm-float16 value))
                          (single-float
                           (asm-float32 value))
                          (double-float
                           (asm-float64 value))
                          ((signed-byte unsigned-byte)
                           (ecase (second lisp-type)
                             ((8 16 32)
                              (asm-word value))
                             (64 (asm-word64 value)))))))))))))
    (loop
      for (op . info) in (alexandria:hash-table-alist
                          (getf 3b-spirv::*spec* :opcodes))
      unless (member op *manual-opcodes*)
        do #++(format t "~&generating ~s~%" op)
           (destructuring-bind (&key value required-capabilities resultp
                                  operands) info
             #++(format t "~s:~s~%" op (mapcar 'car operands))
             (add-op op
                     `(lambda (,@(when resultp '(dest))
                               ,@(when (eq resultp :typed) '(type))
                               &rest args)
                        ,@(unless operands
                            `((declare (ignore args))))
                        ,@(loop for cap in required-capabilities
                                collect `(setf (gethash ,cap *used-caps*) t))
                        ,@(when (and resultp ;; ??
                                     ;; type-forward-pointer doesn't have dest?
                                     (alexandria:starts-with-subseq
                                      "TYPE-" (symbol-name op)))
                            `((use-type dest
                                        :define (list* ',op
                                                       ,(if operands
                                                            'args
                                                            nil)))))
                        ,@(when (and resultp ;; ??
                                     ;; type-forward-pointer doesn't have dest?
                                     (alexandria:starts-with-subseq
                                      "CONSTANT-" (symbol-name op)))
                            `((use-constant dest
                                            :define (list* ',op
                                                           ,(if operands
                                                                'args
                                                                nil)))))
                        ,@(when (eq op 'spirv-core:name)
                            ;; probably should expand NAME manually
                            ;; allow implicit name from symbol
                            `((when (= (length args) 1)
                                (setf args (list* (car args) args)))))
                        (with-spirv-instruction (,value)
                          ,@(when (eq resultp :typed)
                              `((asm-type type)))
                          ,@(when resultp
                              `((asm-id dest)))
                          ,@(loop for (type name) in operands
                                  collect (assemble-op
                                           type
                                           (when (eq resultp :typed)
                                             'type))))))))))

(make-opcodes)

(defmacro add-op-after (op lambda-list &body body)
  (let ((args (gensym "ARGS")))
    `(setf (gethash ',op *opcode-functions*)
           (list
            (let ((old (gethash ',op *opcode-functions*)))
              (if (consp old) (first old) old))
            (compile nil
                     `(lambda (&rest ,',args)
                        (destructuring-bind ,',lambda-list ,',args
                          ,@',body)))))))

(add-op-after spirv-core:ext-inst-import (dest name)
  (use-extension dest name))

(add-op-after spirv-core:variable (id type storage-class
                                      &optional initial-value)
  (declare (ignore initial-value))
  (define-global id type storage-class))

(defun ll-assemble1 (form)
  #++(format t "~&assemble ~s~%" form)
  (let* ((*current-form* form)
         (op (car form))
         (f (gethash op *opcode-functions*)))
    (check-section op)
    (loop for i in (alexandria:ensure-list f)
          do (apply i (cdr form)))))

(defun ll-assemble (code)
  (let ((*gensym-counter* 0))
    (with-ll-asm-context ()
      (loop for form in code
            do (ll-assemble1 form))
      (format t "~&-------~%")
      (format t "  used ~d IDs~%  caps: ~s~%  types~{~%   ~10s : ~s~}~%"
              *next-id*
              (alexandria:hash-table-keys *enabled-caps*)
              (loop for i in (alexandria:hash-table-keys *defined-types*)
                    collect i
                    collect (gethash i *defined-types*)))
      (format t "  globals~{~%   ~10s : ~s~}~%"
              (loop for i in (alexandria:hash-table-keys *defined-globals*)
                    collect i
                    collect (gethash i *defined-globals*)))
      (format t "  literals~{~%   ~10s : ~s~}~%"
              (loop for i in (alexandria:hash-table-keys *defined-constants*)
                    collect i
                    collect (gethash i *defined-constants*)))
      (format t "  IDs:~%")
      (loop for (a . b) in
                        (sort (alexandria:hash-table-alist *name->id*)
                              '< :key 'cdr)
            do (format t "    ~16s : ~s~@[ t=~s~]~@[ c=~s~]~%" a b
                       (cdr (gethash a *defined-types*))
                       (cdr (gethash a *defined-constants*)))
            #++(loop for f in (reverse (gethash b *id-uses*))
                     do (format t "      ~s~%" f)))
      (finish-spirv (1+ *next-id*)))))


#++
(let ((out
        (ll-assemble
         '((spirv-core:capability :shader)
           (spirv-core:ext-inst-import :glsl "GLSL.std.450")
           (spirv-core:memory-model :logical :glsl-450)
           (spirv-core:entry-point :fragment main main color color1 color2 multiplier)
           (spirv-core:execution-mode main :origin-lower-left)
           (spirv-core:source :glsl 450)
           (spirv-core:name main)
           (spirv-core:name scale)
           (spirv-core:name s "S")
           (spirv-core:member-name s 0 b)
           (spirv-core:member-name s 1 v)
           (spirv-core:member-name s 2 i)

           (spirv-core:name block-name "blockName")
           (spirv-core:member-name block-name 0 s)
           (spirv-core:member-name block-name 1 cond)
           (spirv-core:name || "") ;; anonymous struct in GLSL
           (spirv-core:name color)
           (spirv-core:name color1)
           (spirv-core:name color2)
           (spirv-core:name i)
           (spirv-core:name multiplier)

           (spirv-core:decorate vec4[5] :array-stride 16)

           ;; (member-decorate s b :offset 0) ??
           ;; (would need to have member-name forms which aren't required, so
           ;;  for now just require idex instead of member names)
           (spirv-core:member-decorate s 0 :offset 0)
           (spirv-core:member-decorate s 1 :offset 16)
           (spirv-core:member-decorate s 2 :offset 96)
           (spirv-core:member-decorate block-name 0 :offset 0)
           (spirv-core:member-decorate block-name 1 :offset 112)
           (spirv-core:decorate block-name :block)
           (spirv-core:decorate || :descriptor-set 0)
           (spirv-core:decorate color2 :no-perspective)

           ;; types/constants/globals
           (spirv-core:type-void :void)
           (spirv-core:type-function |void()| :void)
           (spirv-core:type-float :float32 32)
           (spirv-core:type-vector :vec4 :float32 4)
           (spirv-core:type-pointer function/vec4* :function :vec4)
           (spirv-core:constant float32/1.0 :float32 1.0)
           (spirv-core:constant float32/2.0 :float32 2.0)
           (spirv-core:constant-composite vec4#1 :vec4 float32/1.0 float32/1.0 float32/2.0 float32/1.0)
           (spirv-core:type-int :uint32 32 0)
           (spirv-core:constant uint32/5 :uint32 5)
           (spirv-core:type-array vec4[5] :vec4 uint32/5)
           (spirv-core:type-int :int32 32 t)
           (spirv-core:type-struct s :uint32 vec4[5] :int32)
           (spirv-core:type-struct block-name s :uint32)
           (spirv-core:type-pointer uniform/block-name* :uniform block-name)
           (spirv-core:variable || uniform/block-name* :uniform)
           (spirv-core:constant int32/1 :int32 1)
           (spirv-core:type-pointer uniform/uint32* :uniform :uint32)
           (spirv-core:type-bool :bool)
           (spirv-core:constant uint32/0 :uint32 0)
           (spirv-core:type-pointer output/vec4* :output :vec4)
           (spirv-core:variable color output/vec4* :output)
           (spirv-core:type-pointer input/vec4* :input :vec4)
           (spirv-core:variable color1 input/vec4* :input)
           (spirv-core:constant int32/0 :int32 0)
           (spirv-core:constant int32/2 :int32 2)
           (spirv-core:type-pointer uniform/vec4* :uniform :vec4)
           (spirv-core:variable color2 input/vec4* :input)
           (spirv-core:type-pointer function/int32* :function :int32)
           (spirv-core:constant int32/4 :int32 4)
           (spirv-core:variable multiplier input/vec4* :input)

           ;; functions
           (spirv-core:function main :void :none |void()|)
           (spirv-core:label #:@initial-block)
           (spirv-core:variable scale function/vec4* :function)
           (spirv-core:variable i function/int32* :function)
           (spirv-core:store scale vec4#1)
           (spirv-core:access-chain %23 uniform/uint32* || int32/1) ; location of cond
           (spirv-core:load %24 :uint32 %23) ; load 32bit int from cond
           (spirv-core:i-not-equal %27 :bool %24 uint32/0) ; convert to bool
           (spirv-core:selection-merge @29 :none)      ; structured if
           (spirv-core:branch-conditional %27 @28 @41) ; if cond
           (spirv-core:label @28)                      ; then
           (spirv-core:load %34 :vec4 color1)
           (spirv-core:access-chain %38 uniform/vec4* || int32/0 int32/1 int32/2) ; s.v[2]
           (spirv-core:load %39 :vec4 %38)
           (spirv-core:f-add %40 :vec4 %34 %39)
           (spirv-core:store color %40)
           (spirv-core:branch @29)
           (spirv-core:label @41)       ; else
           (spirv-core:load %43 :vec4 color2)
           (spirv-core:ext-inst %44 :vec4 :glsl spirv-glsl450:sqrt %43) ; extended instruction sqrt
           (spirv-core:load %45 :vec4 scale)
           (spirv-core:f-mul %46 :vec4 %44 %45)
           (spirv-core:store color %46)
           (spirv-core:branch @29)
           (spirv-core:label @29)
           (spirv-core:store i int32/0)
           (spirv-core:branch @49)
           (spirv-core:label @49)
           (spirv-core:loop-merge @51 @52 :none) ; structured loop
           (spirv-core:branch @53)
           (spirv-core:label @53)
           (spirv-core:load %54 :int32 i)
           (spirv-core:s-less-than %56 :bool %54 int32/4) ; i < 4
           (spirv-core:branch-conditional %56 @50 @51) ; body or break
           (spirv-core:label @50)                      ; body
           (spirv-core:load %58 :vec4 multiplier)
           (spirv-core:load %59 :vec4 color)
           (spirv-core:f-mul %60 :vec4 %59 %58)
           (spirv-core:store color %60)
           (spirv-core:branch @52)
           (spirv-core:label @52)       ; continue target
           (spirv-core:load %61 :int32 i)
           (spirv-core:i-add %62 :int32 %61 int32/1) ; ++i
           (spirv-core:store i %62)
           (spirv-core:branch @49)      ; loop back
           (spirv-core:label @51)       ; loop merge point
           ;;(spirv-core:bit-reverse %70 :int32 %54) ;; something with caps
           (spirv-core:return)
           (spirv-core:function-end)))))
  (with-open-file (f "/tmp/example.spv"
                     ;; assuming LE host that writes ub32 packed
                     :element-type '(unsigned-byte 32)
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :direction :output)
    (write-sequence out f)))
