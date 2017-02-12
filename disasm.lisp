(in-package #:3b-spirv)

(defparameter *manual-decode-opcodes* '(spirv-core:ext-inst))

(defparameter *decoders* (make-hash-table))

(defvar *decode-names*)
(defvar *decode-member-names*)
(defvar *decode-decorate*)
(defvar *decode-member-decorate*)
(defvar *decode-types*)
(defvar *decode-exts*)

(defmacro with-decoder-context (() &body body)
  `(let ((*decode-names* (make-hash-table))
         (*decode-member-names* (make-hash-table))
         (*decode-decorate* (make-hash-table))
         (*decode-member-decorate* (make-hash-table))
         (*decode-types* (make-hash-table :test 'equal))
         (*decode-exts* (make-hash-table)))
     ,@body))

(defgeneric %decode-op (op words))
(defmethod  %decode-op (op words)
  (let ((f (gethash op *decoders*)))
    (print
     (cond
       ((consp f)
        (let ((r (funcall (first f) words)))
          (apply (second f) (cdr r))
          r))
       (f
        (funcall f words))
       (t
        (list* op words))))))

(defun decode-op (form)
  (%decode-op (car form) (coerce (cdr form)
                                 '(vector (unsigned-byte 32)))))

(defun add-decoder (op lambda)
  (setf (gethash op *decoders*)
        (compile nil lambda)))

(defun lookup-enum* (type bits)
  (let* ((values (gethash type (getf 3b-spirv::*spec* :enums))))
    (unless values
      (error "unknown enum type ~s?" type))
    (ecase (car values)
      (:bit
       (loop for enum being the hash-keys of (second values)
               using (hash-value bit)
             when (logbitp bit bits)
               collect bit into ret
               and do (setf (ldb (byte 1 bit) bits) 0)
             finally (when (plusp bits)
                       (cerror "unknown bits #b~0'32b in bitfield ~s?"
                               bits type))
                     (return ret)))
      (:value
       (let ((v (car (rassoc bits
                             (alexandria:hash-table-alist (second values))))))
         (unless v
           (error "enum value ~s not found in type ~s?" bits type))
         v)))))

(defun @decode-enum (arg-type)
  (let* ((enum-type (gethash arg-type *enum-operand-types*)))
    (unless enum-type
      (error "couldn't find enum type for arg type ~s?" arg-type))
    `((lookup-enum* ,enum-type (read-word)))))

(defun read-string ()
  (let ((octets (make-array 32 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    (loop for w = (read-word)
          while (loop for i below 32 by 8
                      for octet = (ldb (byte 8 i) w)
                      always (plusp octet)
                      do (vector-push-extend octet octets)))
    (or (babel:octets-to-string octets :encoding :utf-8 :errorp nil)
        ;; shouldn't get this, but try to do something useful anyway
        (list (babel:octets-to-string octets :encoding :iso-8859-1)))))

(defun read-id (&optional label)
  (let ((w (read-word)))
    (or (gethash w *decode-names*)
        (setf (gethash w *decode-names*)
              (list (if label :label :id) w)
              #++(make-symbol (format nil "~a~a" prefix w))))))

(defun read-float16 ()
  (decode-float16 (read-word)))

(defun read-float32 ()
  (ieee-floats:decode-float32 (read-word)))

(defun read-float64 ()
  (let ((l (read-word)))
    (setf l (dpb (read-word) (byte 32 32) l))
    (ieee-floats:decode-float64 l)))

(defun read-word64 ()
  (let ((l (read-word)))
    (dpb (read-word) (byte 32 32) l)))

(defun read-type ()
  (let* ((id (read-id))
         (type (car (gethash id *decode-types*))))
    (unless type
      (error "id ~s doesn't name a type?" id))
    ;; just return ID for now, and replace them in a 2nd pass
    id))

(defun add-decoded-type (id type)
  (when (gethash id *decode-types*)
    (assert (equal type (gethash id *decode-types*))))
  (setf (gethash id *decode-types*) type)
  (setf (gethash (car type) *decode-types*) type))

(labels ((decode-arg (type &optional type-var)
           (let ((enum (gethash type *enum-operand-types*)))
             (cond
               (enum
                `((list ,@(@decode-enum type))))
               ((and (consp type) (eq (car type) :?))
                `((when (< (car *spirv-input*)
                           (length (cdr *spirv-input*)))
                    ,@(decode-arg (second type)))))
               ((and (consp type) (eq (car type) :*))
                `((loop while (< (car *spirv-input*)
                                 (length (cdr *spirv-input*)))
                        ,@ (loop for f in (decode-arg (second type))
                                 collect 'append
                                 collect f))))
               ((consp type)
                (error "todo"))
               (t
                `((list
                   ,@(ecase type
                       (:id-ref `((read-id)))
                       (:literal-integer
                        '((read-word)))
                       (:literal-string
                        '((read-string)))
                       (:pair-literal-integer-id-ref
                        `((read-word)
                          (read-id)))
                       (:pair-id-ref-literal-integer
                        `((read-id)
                          (read-word)))
                       (:pair-id-ref-id-ref
                        `((read-id)
                          (read-id)))
                       (:literal-spec-constant-op-integer
                        '((gethash (read-word) *opcodes-by-value*)))
                       (:literal-context-dependent-number
                        `((let* ((type-info (gethash ,type-var *decode-types*))
                                 (lisp-type (second type-info)))
                            (ecase (if (consp lisp-type)
                                       (car lisp-type)
                                       lisp-type)
                              (short-float
                               (read-float16))
                              (single-float
                               (read-float32))
                              (double-float
                               (read-float64))
                              ((signed-byte unsigned-byte)
                               (ecase (second lisp-type)
                                 ((8 16 32)
                                  (read-word))
                                 (64 (read-word64))))))))))))))))
  (defun make-decoders ()
    (loop
      for (op . info) in (alexandria:hash-table-alist
                          (getf 3b-spirv::*spec* :opcodes))
      unless (member op *manual-decode-opcodes*)
        do
           (destructuring-bind (&key value required-capabilities resultp
                                  operands) info
             (declare (ignore value required-capabilities))
             (add-decoder
              op
              (print
               `(lambda (words)
                  (let ((type)
                        (*spirv-input* (cons 0 words)))
                    (declare (ignorable type))
                    ,@(when (eq resultp :typed)
                        `((setf type (read-type))))
                    (list*
                     ',op
                     ,@(when resultp
                         `((read-id)))
                     ,@(when (eq resultp :typed)
                         `(type))
                     (concatenate
                      'list
                      ,@(loop for (type name) in operands
                              append (decode-arg
                                      type
                                      (when (eq resultp :typed)
                                        'type)))))))))))))
(make-decoders)

(defmacro defdec (op () &body body)
  (let ((words (gensym)))
    `(add-decoder ',op
                  '(lambda (,words)
                    (let ((*spirv-input* (cons 0 ,words)))
                      ,@body)))))

(defun lookup-ext* (ext inst#)
  (let ((h (gethash ext *decode-exts*)))
    (unless h
      (error "id ~s doesn't name an extension?" ext))
    (if (eq h :unknown)
        :unknown
        (gethash inst# h))))

(defdec spirv-core:ext-inst ()
  (let* ((type (read-type))
         (dest (read-id))
         (ext (read-id))
         (inst# (read-word))
         (inst (lookup-ext* ext inst#)))
    (cond
      ((not inst)
       (error "couldn't find instruction ~s in extension ~s?"
              inst# ext))
      ((eq inst :unknown)
       (list* 'spirv-core:ext-inst
              dest type ext inst#
              (loop while (< (car *spirv-input*)
                             (length (cdr *spirv-input*)))
                    collect (read-id))))
      (t
       (list* inst
              dest type
              (loop while (< (car *spirv-input*)
                             (length (cdr *spirv-input*)))
                    collect (read-id)))))))

(defmacro add-decode-after (op lambda-list &body body)
  (let ((args (gensym "ARGS")))
    `(setf (gethash ',op *decoders*)
           (list
            (let ((old (gethash ',op *decoders*)))
              (if (consp old) (first old) old))
            (compile nil
                     `(lambda (&rest ,',args)
                        (destructuring-bind ,',lambda-list ,',args
                          ,@',body)))))))

(defun use-decode-extension (id name)
  (if (string= name "GLSL.std.450")
      (setf (gethash id *decode-exts*)
            (alexandria:alist-hash-table
             (loop for k being the hash-keys
                     of (getf 3b-spirv::*spec* :glsl-opcodes)
                       using (hash-value v)
                   collect (cons (getf v :value) k))))
      (setf (gethash id *decode-exts*) :unknown)))

(add-decode-after spirv-core:ext-inst-import (id name)
  (use-decode-extension id name))

(add-decode-after spirv-core:name (id name)
  (unless (string= name (gethash id *decode-names* name))
    (error "multiple names for ID ~s? ~s, ~s~%"
           id (gethash id *decode-names*) name))
  (setf (gethash id *decode-names*) name))

(add-decode-after spirv-core:member-name (id index name)
  (unless (gethash id *decode-member-names*)
    (setf (gethash id *decode-member-names*)
          (make-hash-table)))
  (unless (string= name
                   (gethash index (gethash id *decode-member-names*) name))
    (error "multiple names for member ~s of structure ~s? ~s, ~s~%"
           index (gethash id *decode-names* id)
           (gethash index (gethash id *decode-member-names*))
           name))
  (setf (gethash index (gethash id *decode-member-names*))
        name))

;; not sure if we need these for low-level?
#++
(add-decode-after spirv-core:decorate (id &rest decoration)
  (push decoration (gethash id *decode-decorate*)))

#++
(add-decode-after spirv-core:member-decorate (id index &rest decoration)
  (unless (gethash id *decode-member-decorate*)
    (setf (gethash id *decode-member-decorate*)
          (make-hash-table)))
  (push decoration (gethash index (gethash id *decode-member-decorate*))))



(add-decode-after spirv-core:type-void (id)
  (add-decoded-type id `((:bool) nil)))

(add-decode-after spirv-core:type-bool (id)
  (add-decoded-type id `((:bool) (unsigned-byte 32))))

(add-decode-after spirv-core:type-int (id size signed)
  (add-decoded-type id (if (plusp signed)
                           `((:int ,size) (signed-byte ,size))
                           `((:uint ,size) (unsigned-byte ,size)))))

(add-decode-after spirv-core:type-float (id size)
  (add-decoded-type id `((:float ,size) ,(ecase size
                                           (16 'short-float)
                                           (32 'single-float)
                                           (64 'double-float)))))

(add-decode-after spirv-core:type-vector (id type size)
  (add-decoded-type id `((:vec ,type ,size))))

(add-decode-after spirv-core:type-matrix (id column-type column-count)
  (add-decoded-type id `((:mat ,column-type ,column-count))))

(add-decode-after spirv-core:type-array (id type size)
  (add-decoded-type id `((:array ,type ,size))))

(add-decode-after spirv-core:type-struct (id &rest members)
  (let ((mn (gethash id *decode-member-names*)))
    (add-decoded-type id `((:struct
                            ()
                            ,@(loop for m in members
                                    for i from 0
                                    for n = (or (gethash i mn)
                                                (format nil "m~s" i))
                                    collect (list n m)))))))

(add-decode-after spirv-core:type-pointer (id storage-class type)
  (add-decoded-type id `((:pointer ,type))))

(add-decode-after spirv-core:type-function (id ret &rest args)
  (add-decoded-type id `((:function ,ret ,@args))))

(add-decode-after spirv-core:type-image (&rest r)
  (error "spirv-core:type-function not implemented yet"))
(add-decode-after spirv-core:type-sampler (&rest r)
  (error "spirv-core:type-sampler not implemented yet"))
(add-decode-after spirv-core:type-sampled-image (&rest r)
  (error "spirv-core:type-sampled-image not implemented yet"))
(add-decode-after spirv-core:type-runtime-array (&rest r)
  (error "spirv-core:type-runtime-array not implemented yet"))
(add-decode-after spirv-core:type-opaque (&rest r)
  (error "spirv-core:type-opaque not implemented yet"))
(add-decode-after spirv-core:type-event (&rest r)
  (error "spirv-core:type-event not implemented yet"))
(add-decode-after spirv-core:type-device-event (&rest r)
  (error "spirv-core:type-device-event not implemented yet"))
(add-decode-after spirv-core:type-reserve-id (&rest r)
  (error "spirv-core:type-reserve-id not implemented yet"))
(add-decode-after spirv-core:type-queue (&rest r)
  (error "spirv-core:type-queue not implemented yet"))
(add-decode-after spirv-core:type-pipe (&rest r)
  (error "spirv-core:type-pipe not implemented yet"))
(add-decode-after spirv-core:type-forward-pointer (&rest r)
  (error "spirv-core:type-forward-pointer not implemented yet"))






(defun %disasm (code)
  (with-decoder-context ()
    (list* (first code) ;; keep header for now
           (mapcar 'decode-op (cdr code)))))

;; todo: add option to add "nice" IDs instead of (:id foo)
;;   (use names if possible, expand types where possible)
(defmethod disasm ((v vector))
  (%disasm (read-spirv v)))

(defmethod disasm ((s stream))
  (%disasm (read-spirv s)))

(defmethod disasm ((f string))
  (with-open-file (s f :element-type '(unsigned-byte 32))
    (disasm s)))

(defmethod disasm ((f pathname))
  (with-open-file (s f :element-type '(unsigned-byte 32))
    (disasm s)))


#++
(disasm "/tmp/example-hl2.spv")
#++
(let ((out (ll-assemble
            (cdr (disasm "/tmp/example-hl2.spv")))))
  (with-open-file (f "/tmp/example-hl2d.spv"
                     ;; assuming LE host that writes ub32 packed
                     :element-type '(unsigned-byte 32)
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :direction :output)
    (write-sequence out f)))
