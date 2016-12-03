(in-package #:3b-spirv)

(defvar *spirv-input*) ;; ub32 stream or (cons index ub32-vector)

(defvar *opcodes-by-value* (make-hash-table))

(loop for op being the hash-keys of (getf *spec* :opcodes) using (hash-value n)
      do (setf (gethash (getf n :value)
                        *opcodes-by-value*) op))

(defun read-word ()
  (etypecase *spirv-input*
    (stream
     (read-byte *spirv-input* nil nil))
    ((cons unsigned-byte vector)
     (assert (array-in-bounds-p (cdr *spirv-input*) (car *spirv-input*)))
     (prog1 (aref (cdr *spirv-input*) (car *spirv-input*))
       (incf (car *spirv-input*))))))

(defun read-header ()
  (let ((h (list :magic (read-word)
                 :version (read-word)
                 :generator (read-word)
                 :ids (read-word)
                 :0 (read-word))))
    (assert (= (getf h :magic) (magic-number)))
    (assert (= (getf h :0) 0))
    h))

(defun read-inst ()
  (let* ((op+size (or (read-word) (return-from read-inst nil)))
         (op (ldb (byte 16 0) op+size))
         (size (ldb (byte 16 16) op+size)))
    (list* (gethash op *opcodes-by-value* op)
           (loop for i from 1 below size
                 collect (read-word)))))

(defun read-body ()
  (loop for i = (read-inst)
        while i
        collect i))


(defmethod read-spirv ((v vector))
  (let ((*spirv-input* (cons 0 v)))
    (list* (read-header)
           (read-body))))

(defmethod read-spirv ((s stream))
  (let ((*spirv-input* s))
    (list* (read-header)
           (read-body))))

#++
(with-open-file (s "/tmp/example-hl2.spv"
                   :element-type '(unsigned-byte 32))
  (read-spirv s))
