(defpackage #:packing-test2
  (:use :cl))
(in-package #:packing-test2)
(defparameter *glslang-validator*
  (merge-pathnames "src/glslang/build/StandAlone/glslangValidator.exe"
                   (user-homedir-pathname)))

(defun random-type (types &key (limit (length types)) (min 0))
  (make-member (aref types (+ min(random (- limit min)))) types))

(defmethod make-member ((type (eql :bool)) types)
  '(:bool))

(defmethod make-member ((type (eql :int)) types)
  `(:int 32;,(aref #(8 16 32 64) (random 4))
         ))

(defmethod make-member ((type (eql :uint)) types)
  `(:uint 32;,(aref #(8 16 32 64) (random 4))
          ))

(defmethod make-member ((type (eql :float)) types)
  `(:float ,(aref #(16 32 64) (random 3))))

(defmethod make-member ((type (eql :vec)) types)
  `(:vec ,(random-type types :limit 4) ,(+ 2 (random 3))))

(defmethod make-member ((type (eql :mat)) types)
  `(:mat ,(aref #((:float 32) (:float 64)) (random 2))
         ;;(make-member :float types) don't have fp16 ext?
         ,(+ 2 (random 3)) ,(+ 2 (random 3))))

(defmethod make-member ((type (eql :array)) types)
  `(:array ,(random-type types) ,(1+ (random 5))))

(defmethod make-member ((type (eql :struct)) types)
  `(:struct () ,@ (loop for i below (+ 1 (random 16))
                        for n = (format nil "m~a" i)
                        collect (list n (random-type types)))))

(defmethod make-member ((type (eql :block)) types)
  `(:block () ,@ (loop for i below (+ 1 (random 16))
                       for sp = (zerop (random 3))
                       for n = (format nil "m~a" i)
                       collect (list n (random-type types
                                                    :min (if sp 7 0))))))

(defmethod make-member ((type string) types)
  type)


(defun gen-structs (&key (blocks 1))
  (let ((types (make-array 16 :fill-pointer 0 :adjustable t))
        (structs nil))
    (loop for i in '(:bool :int :uint :float :vec :mat :array)
          do (vector-push-extend i types))
    (loop for i below (+ 1 (random 10))
          for n = (format nil "struct~a" i)
          for s = (make-member :struct types)
          do (push (list n s) structs)
             (vector-push-extend n types))
    (loop for i below blocks
          for n = (format nil "Block~a" i)
          for b = (make-member :block types)
          do (setf (getf (second b) :instance)
                   (string-downcase n))
             (when (zerop (random 3))
               (setf (getf (second b) :major)
                     (aref #(:row :column) (random 2))))
             (when (zerop (random 3))
               (setf (getf (second b) :packing)
                     (aref #(:std140 :std430) (random 2))))
             (push (list n b) structs))
    (reverse structs)))


#++
(gen-structs)
#++
(glsl-packing:pack-structs (list* '(:packing :std140)
                                  (gen-structs)))

#++
(defun check-layout (n pack props)
  (let ((l (cadr (assoc n pack :test 'equal :key 'car))))
    (format t "check ~s~%" l)
    (loop for m in (getf l :members)
          for mn =  (format nil "~a.~{~a~^.~}" n
                     (alexandria:ensure-list (getf m :name)))
          for piq = (basecode-piq:get-program-resource
                     program :buffer-variable mn)
          do (format t "look up ~a~%      -> ~s~%      -> ~s~%" mn m piq)
             (when piq
               (assert (= (getf m :offset) (getf piq :offset)))
               (when (getf m :matrix-stride)
                 (assert (eq (getf m :major)
                             (if (zerop (getf piq :is-row-major))
                                 :column :row)))
                 (assert (= (getf m :matrix-stride) (getf piq :matrix-stride)))
                 )
               (when (getf m :stride)
                 (assert (= (getf m :stride) (getf piq :array-stride))))))))

(defun check-layout (n pack mprops)
  (let ((l (cadr (assoc n pack :test 'equal :key 'car))))
    #++(format t "check ~s~%" l)
    (setf (getf l :members)
          (remove-if 'consp (getf l :members) :key (lambda (a) (getf a :name))))
    (loop for m in (getf l :members)
          for i from 0
          for mn =  (format nil "~a.~{~a~^.~}" n
                            (alexandria:ensure-list (getf m :name)))
          for mi = (gethash i mprops)
          do (format t "look up ~a~%      -> ~s~%      -> ~s~%" mn m mi)
             (assert (= (getf m :offset) (getf mi :offset)))
             (when (getf m :matrix-stride)
               (assert (ecase (getf m :major)
                         (:column (getf mi :colmajor))
                         (:row (getf mi :rowmajor))))
               (assert (= (getf m :matrix-stride) (getf mi :matrixstride))))
             (when (getf m :stride)
               (assert (= (getf m :stride) (getf mi :stride))))))
  )

(defun check-layouts (structs pack props)
  (loop with index = (first props)
        with props = (second props)
        for (n (type)) in structs
        for id = (gethash n index)
        for m = (gethash id props)
        when m
          do (check-layout n pack m)))

(defvar *foo* (print (list* (gen-structs))))
#++
(glsl-packing:pack-structs *foo*)

(defun k (s)
  (intern (string-upcase s) :keyword))
(defun parse-output (spv)
  (let ((props (make-hash-table :test #'equal))
        (index (make-hash-table :test #'equal)))
    (ppcre:do-register-groups (('parse-integer id)
                               name
                               ('parse-integer member)
                               ('k prop)
                               ('parse-integer arg))
      ("MemberDecorate (\\d+)\\((\\w+)\\) (\\d+) (\\w+)(?: (\\d+))?" spv)
      (let ((mh (gethash id props)))
        (unless mh
          (setf (gethash id props)
                (setf mh (make-hash-table :test 'equal))))
       (setf (getf (gethash member mh) prop)
             (or arg t)))
      (setf (gethash name index) id)
      #++(format t "~s = ~s.~s = :~s(~s)~%" id name member prop arg))
    (ppcre:do-register-groups (('parse-integer id)
                               ('parse-integer arg))
      ("Decorate (\\d+) ArrayStride (\\d+)" spv)
      (setf (getf (gethash id props) :stride) arg)
      #++(format t "~s = ~s~%" id arg))
    (ppcre:do-register-groups (('parse-integer id) members)
      ("(\\d+)\\((?:\\w+)\\):\\W+TypeStruct (.*)" spv)
      (let ((mids nil))
        (ppcre:do-register-groups (('parse-integer m))
          ("(\\d+)(?:\\(\\w+\\))?" members)
          (push m mids)
          #++(format t "~s = m~s~%" id  m))
        #++(format t "id ~s, members ~s:~% ~s~%"
                id (reverse mids)
                (loop for i in (reverse mids) collect (gethash i props)))
        (loop for m in (nreverse mids)
              for i from 0
              for p = (gethash m props)
              when (consp p)
                do (setf (gethash i (gethash id props))
                         (append p (gethash i (gethash id props)))))))
    (list index props))

)

(defun run-test ()
  (let* ((structs ;*foo*
           (print (setf *foo* (list* ;'(:packing :std140)
                               (gen-structs)))))
         (pack (glsl-packing:pack-structs structs))
         (g (with-output-to-string (*standard-output*)
              (glsl-packing-io::print-structs structs)))
         (shader (concatenate
                  'string
                  "#version 450
//#extension GL_NV_gpu_shader5 : enable
#extension GL_AMD_gpu_shader_half_float : enable
//#extension GL_ARB_gpu_shader_fp64 : enable

out vec4 color;
layout(std140) buffer;
"

                  g
                  "
void main() {
"
                  (with-output-to-string (*standard-output*)
                    (loop for (n type) in structs
                          when (and (consp type)
                                    (eq (car type) :block))
                            do (format t "  ~a.~a;~%"
                                       (or (getf (second type)
                                                 :instance)
                                           n)
                                       (car (third type)))))
                  "  color = vec4(1.0);
}"))
         (err nil)
         (spv (handler-bind
                  ((UIOP/RUN-PROGRAM:SUBPROCESS-ERROR
                     (lambda (e)
                       (setf err t)
                       (continue e))))
                (uiop:with-temporary-file (:stream s :pathname p
                                           :type "frag")
                  (format s "~a~%" (print shader))
                  :close-stream
                  (uiop:run-program (list *glslang-validator*
                                          "-H"
                                          (uiop:native-namestring p))
                                    :output :string)))))
    (when err
      (format t "~&~%!!!!!!! compile failed:~%~%  ~s~%" spv))
    (assert (not err))
    #++(format t "~&~%spv =~%~s~%" spv)
    (check-layouts structs pack (parse-output spv))
    
    ))

#++
(time
 (loop
   repeat 1000
   do
      (run-test)))

