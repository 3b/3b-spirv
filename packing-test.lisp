(defpackage #:packing-test
  (:use :cl))
(in-package #:packing-test)

(defun random-type (types &key (limit (length types)) (min 0))
  (make-member (aref types (+ min(random (- limit min)))) types))

(defmethod make-member ((type (eql :bool)) types)
  '(:bool))

(defmethod make-member ((type (eql :int)) types)
  `(:int (aref #(8 16 32 64) (random 4))))

(defmethod make-member ((type (eql :uint)) types)
  `(:uint ,(aref #(8 16 32 64) (random 4))))

(defmethod make-member ((type (eql :float)) types)
  `(:float ,(aref #(16 32 64) (random 3))))

(defmethod make-member ((type (eql :vec)) types)
  `(:vec ,(random-type types :limit 4) ,(+ 2 (random 3))))

(defmethod make-member ((type (eql :mat)) types)
  `(:mat (:float 32)
         ;; dmat confuses nvidia driver?
         ;;,(aref #((:float 32) (:float 64)) (random 2))
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


(defun check-layout (n pack program)
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

(defun check-layouts (structs pack program)
  (basecode-piq:dump-resources program)

  (loop for (n (type)) in structs
        when (eq type :block)
          do (check-layout n pack program)))

(defvar *foo* (print (list* (gen-structs))))
(glsl-packing:pack-structs *foo*)

(defun run-test ()
  (let* ((structs ;;*foo*
           (print (setf *foo* (list* ;'(:packing :std140)
                                     (gen-structs)))))
         (pack (glsl-packing:pack-structs structs))
         (g (with-output-to-string (*standard-output*)
              (glsl-packing-io::print-structs structs)))
         (p (gl:create-program))
         (vs (gl:create-shader :vertex-shader))
         (fs (gl:create-shader :fragment-shader)))
    (gl:shader-source vs (print
                          "#version 450
in vec4 foo;
void main() {
  gl_Position = foo;
}

"))
    (gl:compile-shader vs)
    (unless (gl:get-shader vs :compile-status)
      (format t "~s shader compile failed: ~s"
              :vertex (gl:get-shader-info-log vs))
      (return-from run-test nil))
    (gl:attach-shader p vs)

    (gl:shader-source fs (print
                          (concatenate
                           'string
                           "#version 450
#extension GL_NV_gpu_shader5 : enable
//#extension GL_AMD_gpu_shader_half_float : enable
#extension GL_ARB_gpu_shader_fp64 : enable

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
}")))
    (gl:compile-shader fs)
    (cond
      ((gl:get-shader fs :compile-status)
       (format t "compile ok~%")
       (gl:attach-shader p fs))
      (t
       (format t "~s shader compile failed: ~s"
               :fragment (gl:get-shader-info-log fs))
       (return-from run-test nil)))

    (gl:link-program p)
    (cond
      ((gl:get-program p :link-status)
       (format t "link ok~%"))
      (t
       (format t "program link failed: ~s"
               (gl:get-program-info-log p))
       (return-from run-test nil)))
    (check-layouts structs pack p)

    (gl:delete-shader fs)
    (gl:delete-shader vs)
    (gl:delete-program p)))

#++
(time
 (glop:with-window (w "foo" 256 256 :x 0 :y 0)
   (loop
     repeat 20
     while (glop:dispatch-events w :blocking nil :on-foo nil)
     do
        (run-test)
        (glop:swap-buffers w))))


