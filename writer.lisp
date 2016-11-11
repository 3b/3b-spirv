(in-package #:3b-spirv)

(defvar *spirv-header-args*)
(defvar *spirv-output*)

(defun magic-number ()
  ;; todo: get this from spec file...
  #x07230203)

(defun write-word (word)
  (check-type word (unsigned-byte 32))
  #++(format t "~8,'0x: write word ~s / #x~8,'0x~%"
          (fill-pointer *spirv-output*) word word)
  (vector-push-extend word *spirv-output*))

(defun header (pos ids &key (version #x00010000)
                         (generator 0))
  (setf (aref *spirv-output* (+ pos 0)) (magic-number))
  (setf (aref *spirv-output* (+ pos 1)) version)
  (setf (aref *spirv-output* (+ pos 2)) generator)
  (setf (aref *spirv-output* (+ pos 3)) ids)
  (setf (aref *spirv-output* (+ pos 4)) 0))

(defun finish-spirv (ids &key)
  (apply #'header 0 ids *spirv-header-args*)
  *spirv-output*)

(defmacro with-spirv-instruction ((op-value) &body body)
  (let ((file-position (gensym "FILE-POSITION")))
    `(let ((,file-position (fill-pointer *spirv-output*)))
       (write-word ,op-value)
       (prog1
           (progn ,@body)
         (let ((size (- (fill-pointer *spirv-output*) ,file-position)))
           (setf (aref *spirv-output* ,file-position)
                 (logior ,op-value (ash size 16))))))))

(defmacro with-spirv-output ((&key (major 1) (minor 0)
                                (generator 0))
                             &body body)
  `(let ((*spirv-output* (make-array 128 :fill-pointer 5
                                         :element-type '(unsigned-byte 32)
                                         :initial-element #x3b3b3b3b))
         (*spirv-header-args* (list :generator ,generator
                                    :version  ,(dpb major (byte 8 16)
                                                    (dpb minor (byte 8 8) 0)))))
     ,@body))
