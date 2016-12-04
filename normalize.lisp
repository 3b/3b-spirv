(in-package #:3b-spirv)
;; quick hack to 'normalize' a spv file for comparing output between
;; tools with different strategies for assigning IDs and organizing
;; types/constants/etc

(defvar *normalized-id-map*)

(defun remap-id (id)
  (or (gethash id *normalized-id-map*)
      (setf (gethash id *normalized-id-map*) (incf *next-id*))))

(defun remap-op (asm op index &key sort)
  (let ((forms (loop for form in asm
                     when (eq (car form) op)
                       collect form)))
    (when sort
      (stable-sort forms
                   'string-lessp
                   ;; for now just concatenate the fields with a NUL
                   ;; between them and sort normally
                   :key (lambda (a)
                          ;; fixme: do something faster here
                          ;;  (at least memoize it or something)
                          (with-output-to-string (s)
                            (loop for .n in sort
                                  for n = (if (consp .n) (second n) n)
                                  for v = (nth n (cdr a))
                                  for k = (if (consp .n)
                                              (gethash v *normalized-id-map* v)
                                              v)
                                  do (format s "~a~c" k (code-char 0)))))))
    (loop for (nil . args) in forms
          do (remap-id (nth index args)))))


(defun remap-exts (asm)
  (remap-op asm 'spirv-core:ext-inst-import 0 :sort '(1)))

(defun remap-entry-points (asm)
  (remap-op asm 'spirv-core:entry-point 1 :sort '(0 2)))

(defun remap-functions (asm)
  (let ((functions (make-hash-table :test 'equal)))
    (loop for start = (position 'spirv-core:function asm :key 'car :start 0)
            then (position 'spirv-core:function asm :key 'car :start end)
          for end = (when start
                      (position 'spirv-core:function-end asm :key 'car
                                                             :start (1+ start)))
          for fn = (when (and start end)
                     (subseq asm start end))
          while fn
          do (setf (gethash (second (first fn)) functions)
                   fn))
    (flet ((body (fun)
             (loop for form in fun
                   do (loop for x in form
                            when (typep x '(cons (member :label :id)
                                            (cons unsigned-byte)))
                              do (remap-id x)))))
      (loop while (plusp (hash-table-count functions))
            for work = (sort
                        (loop for k in (alexandria:hash-table-keys functions)
                              for r = (gethash k *normalized-id-map*)
                              when r
                                collect (list k r))
                        '< :key 'second)
            do (assert work) ;; todo: handle unused functions
               (loop for (id rid) in work
                     for body = (gethash id functions)
                     do (remhash id functions)
                        (body body))))))

(defun remap-types/etc (asm)
  (let ((forms (make-hash-table :test 'equalp)))
    (loop for form in asm
          for op = (car form)
          when (or (member op '(spirv-core:constant
                                spirv-core:variable))
                   (alexandria:starts-with-subseq "TYPE-" (symbol-name op))
                   (alexandria:starts-with-subseq "CONSTANT-" (symbol-name op)))
            do (setf (gethash (second form) forms) form))
    (loop while (plusp (hash-table-count forms))
          for work = (sort
                      (loop for k in (alexandria:hash-table-keys forms)
                            for r = (gethash k *normalized-id-map*)
                            when r
                              collect (list k r))
                      '< :key 'second)
          do (assert work) ;; todo: handle unused types/vars/constants
             (loop for (id) in work
                   for form = (gethash id forms)
                   do (remhash id forms)
                      (format t "~&remap ~s~%" form)
                      (loop for x in form
                            when (typep x '(cons (member :label :id)
                                            (cons unsigned-byte)))
                              do (remap-id x))))))

(defun final-remap (asm)
  (loop for form in asm
        collect (loop for x in form
                      for r = (gethash x *normalized-id-map*)
                      when r
                        collect (list (car x) r)
                      else
                        collect (if (typep x '(cons (member :label :id)
                                               (cons unsigned-byte)))
                                    (list :? x)
                                    x))))

(defun normalize (spv)
  (let* ((*next-id* 0)
         (*normalized-id-map* (make-hash-table :test 'equal))
         (asm (if (vectorp spv)
                  (disasm spv)
                  spv)))
    (remap-exts asm)
    (remap-entry-points asm)
    (remap-functions asm)
    (remap-types/etc asm)
    (final-remap asm)))

#++
(format t "~%~%~{~s~%~}" (normalize "/tmp/example-hl2.spv"))
#++
(format t "~%~%~{~s~%~}" (normalize "/tmp/sample.spv"))

#++
(let ((a (sort (normalize "/tmp/sample.spv")
               'string< :key 'princ-to-string))
      (b (sort (normalize "/tmp/example-hl2.spv")
               'string< :key 'princ-to-string)))
  (list :a-b (set-difference a b :test 'equalp)
        :b-a (set-difference b a :test 'equalp)))
