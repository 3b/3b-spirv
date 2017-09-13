(in-package #:3b-spirv/hl)

;; seems like these should be UINT, but comparing with something that
;; generates INT so make it configurable for now
(defparameter *access-chain-int-literals* t)


(defparameter *no-auto-constants-ops*
  ;; don't try to convert numbers to constants for these ops, because
  ;; they take literal numbers in the instruction encoding
  '(spirv-core:constant-pipe-storage
    spirv-core:lifetime-stop
    spirv-core:lifetime-start
    spirv-core:member-decorate
    spirv-core:array-length
    spirv-core:constant-sampler
    spirv-core:type-image
    spirv-core:type-matrix
    spirv-core:type-vector
    spirv-core:type-float
    spirv-core:type-int
    spirv-core:line
    spirv-core:member-name
    spirv-core:source
    spirv-core:composite-extract))

;; check for list in 2nd cons to distinguish from automatic names
;; like (:block foo)
(deftype block/buffer/struct () '(cons (member :block :buffer :struct)
                                  (cons list)))
(deftype block/buffer () '(cons (member :block :buffer)
                           (cons list)))
(deftype variable () '(cons (eql :variable)))
(deftype scalar () '(cons (member :bool :int :uint :float)))
(deftype literal () '(cons (eql :literal)))

(defvar *types*)
(defvar *struct-types*)
(defvar *variables*)
(defvar *extensions*)
(defvar *literal-names*) ;; allow alternate names for literals
;(defvar *globals*)
(defvar *entry-points*)
(defvar *current-function*)
(defvar *type-deps*)
(defvar *remapped-types*)
(defvar *dumped-types*)

(defmacro with-hl-asm-context (() &body body)
  `(let ((3b-spirv::*enabled-caps* (make-hash-table :test #'equal))
         (*types* (make-hash-table :test #'equal))
         (*struct-types* (make-hash-table :test #'equal))
         (*variables* (make-hash-table :test #'equal))
         (*literal-names* (make-hash-table :test #'equal))
         (*type-deps* (make-hash-table :test #'equal))
         (*extensions* (make-hash-table :test #'equal))
         (*entry-points* (make-hash-table :test #'equal)))
     ,@body))

(defun avm-type-len (type &optional (axis 0))
  (let ((n (elt type (+ 2 axis))))
    (if (numberp n)
        n
        (progn
          (assert (eq (car n) :literal))
          (third n)))))

(defparameter *in-struct* nil)

(defun composite-literal (type value)
  (when (typep value '(cons (eql :literal)))
    ;; don't wrap things that are already literals
    (return-from composite-literal value))
  (etypecase type
    (scalar
     `(:literal ,type ,value))
    (block/buffer/struct
     `(:literal ,type
                (:composite
                 ,@(loop for (nil mt) in (cddr type)
                         for v in value
                         collect (use-type (composite-literal mt v))))))
    ((cons (member :array :vec))
     (when (eq (car type) :array)
       ;; todo: check # of values for :vec literals
       ;; vec allows things like (vec4 1.0 (vec2 2.0 3.0) 4.0), so
       ;; just relaxing checking on it for now...
       (assert (= (third type) (length value))))
     `(:literal ,type
                (:composite
                 ,@(loop with mt = (second type)
                         for v in value
                         collect (use-type (composite-literal mt v))))))
    ((cons (member :mat))
     (assert (= (third type) (length value)))
     `(:literal ,type
                (:composite
                 ,@(loop with mt = `(:vec ,(second type) ,(fourth type))
                         for v in value
                         collect (use-type (composite-literal mt v))))))))

(defun use-type (type &optional (definition type))
  (when (gethash type *types*)
    (assert (equal definition (gethash type *types*))))
  (setf (gethash type *types*) definition)
  type)

(defun normalize-literals-and-types (x &key force layout (expand-structs t)
                                         keep-literals
                                         the)
  (when (and (numberp x)
             keep-literals)
    (return-from normalize-literals-and-types x))
  (typecase x
    ((or (eql :bool)
         (cons (eql :bool)))
     (let ((bt (if *in-struct* :uint :bool)))
       (use-type (gethash bt glsl-packing:*base-types*))))
    (unsigned-byte
     (use-type '(:uint 32))
     (use-type `(:literal ,(or the '(:uint 32)) ,x)))
    (signed-byte
     (use-type '(:int 32))
     (use-type `(:literal ,(or the '(:int 32)) ,x)))
    (real
     (use-type '(:float 32))
     (use-type `(:literal ,(or the '(:float 32)) ,x)))
    ((cons (eql the))
     (let* ((nt (normalize-literals-and-types (second x) :force t))
            (.nv (mapcar (lambda (x)
                           (normalize-literals-and-types x :the nt))
                         (cddr x)))
            (nv (if (typep nt 'scalar)
                    (first .nv)
                    .nv)))
       (use-type nt)
       (when (and the (not (equal the nt)))
         (error "nested THE? figure out correct way to handle this..."))
       (if (every (lambda (x) (typep x 'literal)) .nv)
           (use-type (composite-literal nt nv))
           (use-type `(the ,nt ,nv)))))
    ((or string symbol)
     (cond
       ((gethash x glsl-packing:*base-types*)
        ;; expand base types (:int, :vec4 etc)
        (use-type (gethash x glsl-packing:*base-types*)))
       ((gethash x *literal-names*)) ;; expand named literals
       (force
        (cond
          ((eq force :variable)
           ;; if we know it should be a literal or variable, make
           ;; sure it exists (fixme: see if variables can actually be used
           ;; for array size? possibly just uniforms? if so check that too)
           (assert (gethash x *variables*))
           (gethash x *variables*))
          ;; if we know this should be a type name, make sure it exists
          (force
           (assert (gethash x *struct-types*))
           (let ((xt (gethash x *struct-types*)))
             (if (and (typep xt 'block/buffer/struct)
                      (not expand-structs))
                 ;; don't expand names of structs, but add layout info
                 (list* (first xt) x layout)
                 (normalize-literals-and-types xt
                                               :force force
                                               :layout layout
                                               :expand-structs
                                               expand-structs))))))
       ;;otherwise leave named types, labels, temporary IDs etc as-is
       (t x)))
    (block/buffer/struct
     (assert (or (not layout)
                 (not (second x))
                 (equal (second x) layout)))
     (let ((*in-struct* t))
       (list* (first x)
              (or layout (second x))
              (loop for m in (cddr x)
                    for (mn mt) = m
                    collect (list* mn
                                   (normalize-literals-and-types
                                    mt :force t :layout (or layout (second x))
                                    :expand-structs expand-structs)
                                   ;; preserve extra args
                                   (cddr m))))))
    ((cons (eql :array))
     (let* ((bt (normalize-literals-and-types (second x)
                                              :force t
                                              :layout layout
                                              :expand-structs expand-structs))
            #++(xt (if expand-structs
                    bt
                    (normalize-literals-and-types (second x)
                                                  :force t
                                                  :layout layout
                                                  :expand-structs t))))
       (list* (first x)
              bt
              ;; todo: figure out if there can be calculated values here
              ;; (and if so, how they work)
              (normalize-literals-and-types (third x)) ;; literal or variable
              layout)))
    ((cons (member :vec))
     (list* (first x)
            (normalize-literals-and-types (second x) :force t)
            (cddr x)))
    ((cons (member :mat))
     ;; define the column type
     (use-type `(:vec ,(second x) ,(third x)) :force t)
     (list* (first x)
            (normalize-literals-and-types (second x) :force t)
            (cddr x)))
    (scalar ;; already normalized
     x)
    (t
     (when force
       (error "couldn't normalize type ~s?" x))
     x)))

;;; passes:

;; pass 1
;;   normalize types and literals in source, calculate ftypes
;;     expand type descriptions to glsl-packing input form like
;;    (:array (:vec (:float 32) 4) (:literal (:uint 32) 5)) also add
;;    :stdXX0, :row/:column, and :block/buffer-block as applicable for
;;    uniform/buffer blocks
;;   accumulate type refs, literals
;;   accumulate list of blocks that need layout calculated.
;;   add pointer types to variables
;;   add struct type for blocks
;;   expand references to variables to use pointer types
;;
;;  need to figure out syntax for specifying layout of blocks and
;;    block members, as well as other decorations on members (in
;;    particular stuff like flat on input blocks)

;; pass 1.5: calculate layouts for blocks

;; pass 2: generate low-level asm
;;   names = figure out if we should dump expanded types or description
;;      used in code for 'name' of otherwise unnamed types?
;;      could have multiple names for same type though, so using
;;      expanded name for now. (possibly should add some concise format
;;      for expanded name at some point?)
;;   decorations:
;;      array stride for array types in pack output
;;      block/buffer-block for uniform blocks
;;      member offsets/strides for uniform blocks
;;      member decorations (for all blocks, not just uniform blocks)
;;      global decorations (noperspective, etc)
;;   types/globals/literals
;;      topo sort types/literals
;;      dump definitions
;;   functions
;;      add function def
;;      add initial label if needed
;;      dump code, expanding local defs
;;      add endfunction


(defparameter *current-form* nil)

(defgeneric pass1 (op args))

(defmethod pass1 :before (op args)
  (format t "pass1: ~s ~s~%" op args))

(defmethod pass1 (op args)
  (when (or (alexandria:starts-with-subseq "TYPE-" (symbol-name op))
            (alexandria:starts-with-subseq "CONSTANT-" (symbol-name op)))
    ;; todo: handle type/constant ops
    ;;  need to keep track of manually defined constants/types so we don't
    ;;  duplicate them, and can use them as needed from implicit references
    (error "explicit type/constant ops not handled correctly yet..."))
  ;; handle literals in default case since it applies to most
  ;; instructions, and just have a list of ops to skip
  (let ((a (loop for i in args
                 collect (normalize-literals-and-types
                          i
                          :keep-literals (member op *no-auto-constants-ops*)))))
    (unless (equal a (cdr *current-form*))
      (setf *current-form* (cons op a)))))

(defmacro defpass1 (op lambda-list &body body)
  (let ((args (gensym "ARGS")))
    `(defmethod pass1 ((op (eql ',op)) ,args)
       (destructuring-bind ,lambda-list ,args
         ,@body))))


(defpass1 spirv-core:capability (cap)
  (setf (gethash cap 3b-spirv::*enabled-caps*) cap))

(defpass1 capabilities (&rest caps)
  (loop for c in caps
        do (setf (gethash c 3b-spirv::*enabled-caps*) c)))

(defpass1 spirv-core:extension (name)
  (setf (gethash name *extensions*) name))

(defpass1 spirv-core:ext-inst-import (id name)
  (setf (gethash id *extensions*) name))

(defpass1 spirv-core:entry-point (model id name &rest io-ids)
  (when (gethash id *entry-points*)
    (error "duplicate entry-point instruction? id=~s~%~s~%-> ~s"
           id (gethash id *entry-points*)
           (list model id name io-ids)))
  (setf (gethash id *entry-points*) (list model id name io-ids)))

(defpass1 defstruct (id &rest members)
  (let ((type (normalize-literals-and-types (list* :struct nil members))))
    ;; don't mark the type used, will get marked by users since they
    ;; may need to add layout info
    #++(use-type id type)
    #++(loop for (nil mt) in (cddr type)
             do (use-type mt))
    (unless (equal members (gethash id *struct-types* members))
      (error "redefined struct type ~s from ~s to ~s?"
             id members (gethash id *struct-types*)))
    (setf (gethash id *struct-types*) type)
    ;; not sure we actually need to update this, since will probably
    ;; dump it directly from *types* or whatever, but might as well...
    (setf *current-form*
          (list* 'defstruct id (cddr type)))))

(defun use-struct (name type layout)
  (etypecase type
    (block/buffer/struct
     (loop for (nil mt) in (cddr type)
           for (nil xt) in (cddr (normalize-literals-and-types
                                  type :force t :layout layout))
           unless (equal xt mt)
             do (use-struct mt xt layout)
           else
             do (use-type mt xt))
     (use-type name type))
    ((cons (eql :array))
     (let* ((base (second type))
            (x (normalize-literals-and-types base :force t :layout layout
                                                  :expand-structs t)))
       (if (equal base x)
           (use-type type)
           (use-struct base x layout))))
    (t
     (use-type type))))

(defun pass1/variable (storage-class id type decorate default
                       &key local layout)
  (when layout
    ;; normalize LAYOUT (:stdXX0 first, then :row/:column)
    (setf layout (alexandria:ensure-list layout))
    (assert (<= 1 (length layout) 2))
    (loop for i in layout
          do (assert (member i '(:std140 :std430
                                 :row :column :row-major :column-major))))
    (setf layout (subst :row :row-major layout))
    (setf layout (subst :column :column-major layout))
    (if (member (first layout) '(:row :column))
        (if (= (length layout) 1)
            (setf layout (list* :std140 layout))
            (setf layout (reverse layout)))
        (when (= (length layout) 1)
          (setf layout (list (first layout) :column)))))
  ;; we access variables through pointer types, so create it, and if
  ;; we are defining a block, create a struct type for that
  (let* ((base-type (normalize-literals-and-types type
                                                  :force t :layout layout
                                                  :expand-structs nil))
         (struct (when (typep type 'block/buffer)
                   (list* (car type) id layout)))
         (pointer (list :pointer storage-class (or struct base-type))))
    ;; todo: decide if defstruct types are valid for use as variable
    ;; if so, decide if they should be treated as an interface block
    ;; or not?
    (when struct           ;; define interface block types
      (assert (not local)) ;; locals can't define a block
      (loop for (nil mt) in (cddr base-type)
            for (nil xt) in (cddr (normalize-literals-and-types
                                   type :force t :layout layout))
            do (format t "~s: ~s ~s~%" id mt xt)
            unless (equal mt xt)
              do (use-struct mt xt layout)
            else
              do (use-type mt xt))
      (setf type base-type)
      #++(use-type type)
      (use-type struct type))
    (use-type pointer struct)
    (setf type pointer)
    (setf (gethash id *variables*)
          (print
           (list :variable storage-class :id id :type type :decorate decorate
                 :layout layout)))
    (when default
      (setf default (composite-literal base-type default)))
    (setf *current-form*
          (list* (car *current-form*) id type
                 :decorate decorate :default default
                 (when layout (list :layout layout))))))


(defpass1 literal (id type value)
  (setf (gethash id *literal-names*)
        (normalize-literals-and-types `(the ,type ,value)))
  (setf *current-form* nil))

(defpass1 input (id type &key decorate default)
  (pass1/variable :input id type decorate default))

(defpass1 output (id type &key decorate default)
  (pass1/variable :output id type decorate default))

(defpass1 uniform (id type &key decorate default (layout '(:std140 :column)))
  (when (typep type '(cons (eql :struct)))
    ;; make sure we have a :block instead of a :struct
    ;; (possibly should complain if we get :buffer-block here?
    (setf *current-form* (copy-list *current-form*))
    (setf (third *current-form*) (copy-list (third *current-form*)))
    (setf (car (third *current-form*)) :block))
  (pass1/variable :uniform id type decorate default :layout layout))

(defpass1 buffer (id type &key decorate default (layout '(:std140 :column)))
  (when (typep type '(cons (member :block :struct)))
    ;; make sure we have a :buffer-block instead of a :struct or :block
    (setf *current-form* (copy-list *current-form*))
    (setf (third *current-form*) (copy-list (third *current-form*)))
    (setf (car (third *current-form*)) :buffer-block))
  (pass1/variable :buffer id type decorate default :layout layout))

(defpass1 local (id type &key decorate default)
  (pass1/variable :function id type decorate default :local t))

(defpass1 defun (name lambda-list &body body)
  ;; todo : verify lambda list against entry-point def (probably only
  ;;   matters for opencl?)
  (let ((*current-function* name)
        (all-declarations nil))
    ;; extract declarations, combine into single DECLARE form
    (loop while (and body (consp (car body)) (eql (caar body) 'declare))
          do (let ((declarations (pop body)))
               (loop for dec in (cdr declarations)
                     for (declaration . args) = dec
                     ;; fixme: decide correct package for LAYOUT, etc
                     when (or (member declaration '(values  type
                                                    :layout :function-control))
                              ;; allow base type as shortcut for TYPE
                              (gethash declaration glsl-packing:*base-types*))
                       do (if (gethash declaration glsl-packing:*base-types*)
                              ;; expand TYPE shortcuts
                              (push (list* 'type dec) all-declarations)
                              (push dec all-declarations))
                     else
                       do (error "declaration ~s not implemented yet...~% ~s"
                                 declaration declarations))))
    ;; normalize type declarations and add ftype declaration
    (let* ((normalized-declarations)
           (ptypes (loop with h = (make-hash-table :test #'equal)
                         for d in all-declarations
                         when (eq (car d) 'type)
                           do (let ((nt (normalize-literals-and-types
                                         (second d) :force t)))
                                (loop for v in (cddr d)
                                      do (when (gethash v h)
                                           (if (equalp nt (gethash v h))
                                               (warn "duplicate type~
 declarations for ~s in function ~s?" v name)
                                               (error "duplicate type~
 declarations for ~s in function ~s: ~s->~s" v name (gethash v h) nt)))
                                         (setf (gethash v h) nt))
                                (use-type nt)
                                (push (list* 'type nt (cddr d))
                                      normalized-declarations))
                         else do (push d normalized-declarations)
                         finally (return h)))
           (ftype (list* :function
                         (normalize-literals-and-types
                          (or (second (assoc 'values all-declarations))
                              ':void)
                          :force t)
                         (loop for p in lambda-list
                               for type = (gethash p ptypes)
                               unless type
                                 do (error "no type declared for parameter ~s of function ~s?" p name)
                               collect type))))
      (use-type ftype)
      (setf all-declarations (list* (list 'ftype ftype)
                                    (nreverse normalized-declarations))))

    ;; build new form and process the body
    (setf *current-form*
          `(defun ,name ,lambda-list
             ;; always include a single declare form for easier
             ;; parsing in 2nd pass, since we modify defun form anyway
             (declare ,@all-declarations)
             ,@(first-pass body)))))

(defpass1 spirv-core:access-chain (id type var &rest indices)
  ;; not sure how much to simplify this, could have a separate version
  ;; without a TYPE parameter and always calculate it automatically?
  ;; for now, just allowing :auto, dest-type or (:pointer * dest-type)
  ;; for base dest-type, (:pointer * dest-type) will be defined and
  ;; used. for :auto, type will be extracted from type of VAR and
  ;; indices and used to define pointer type
  (let* ((v (gethash var *variables*))
         (ptype (getf v :type))
         (sc (when (and (consp ptype) (eq (car ptype) :pointer))
               (second ptype)))
         (btype (when (and (consp ptype) (eq (car ptype) :pointer))
                  (gethash (third ptype) *types*)))
         (new-indices (copy-list indices)))
    ;; validate indices, add any literals used as indices, and calculate
    ;; a type for result in case one wasn't specified
    (cond
      ((not v)
       (error "undefined variable ~s in access-chain" var))
      ((not ptype)
       (error "~s isn't a variable in access-chain" var))
      ((not sc)
       (error "~s is type ~s, expected pointer type in access-chain"
              var ptype))
      ((not btype)
       (error "couldn't find type ~s pointed to by ~s?" var (third ptype))))
    (loop for index in indices
          for i from 0
          while index
          do (format t "check index ~d=~d, btype = ~s~%" i index btype)
          when (typep index 'number)
            do (let ((l `(:literal ,(if *access-chain-int-literals*
                                        '(:int 32)
                                        '(:uint 32))
                                   ,index)))
                 (unless (typep index 'unsigned-byte)
                   (error "literal index ~s must be non-negative integers in access-chain" index))
                 (use-type l)
                 (setf (nth i new-indices) l))
          else
            do (let ((a (or (gethash index *types*) index)))
                 ;; if using a named constant, it must be defined already
                 ;; for now..
                 (if (and (consp a)
                          (eq (car a) :literal))
                     (setf index (third a))
                     (setf index a)))
          do ;; expand struct/block names
             (unless btype
               (break "broke" *types*))
             (when (and (typep btype '(cons (member :struct :block :buffer)))
                        (not (typep btype 'block/buffer/struct)))
               (format t "expand ~s to ~s~%" btype (gethash btype *types*))
               (setf btype (gethash btype *types*)))
             (etypecase btype
               (block/buffer/struct
                (unless (typep index 'unsigned-byte)
                  (error "must use constant integral index for struct members in access-chain~% got index ~s for ~s" index btype))
                (let ((m (elt (cddr btype) index)))
                  (unless m
                    (error "member index ~s out of range for type ~s"
                           index btype))
                  ;; we don't expand struct type names in struct/array types
                  ;; so need to expand them here
                  (format t "~s ~s ~s ~%" index m
                          (gethash (second m) *types*))
                  (setf btype (gethash (second m) *types*))))
               ((cons (member :array :vec))
                (when (and (numberp index)
                           (not (< -1 index (avm-type-len btype))))
                  (error "index ~s out of range for ~s~%" index btype))
                ;; we don't expand struct type names in struct/array types
                ;; so need to expand them here
                (setf btype (gethash (second btype) *types*)))
               ((cons (eql :mat))
                ;; matrix is handled specially since it is treated
                ;; as a set of vectors
                (when (and (numberp index)
                           (not (< -1 index (avm-type-len btype))))
                  (error "index ~s out of range for ~s~%" index btype))
                (setf btype (list :vec (second btype) (fourth btype))))))
    (typecase type
      ((cons (eql :pointer))
       ;; just make sure it is expanded
       (setf type (normalize-literals-and-types type :force t)))
      ((eql :auto) ;; figure out a type automatically
       (setf type (list :pointer sc btype)))
      (t ;; we have a non-pointer type, wrap it in a pointer to that type
       (setf type (list :pointer sc
                        (normalize-literals-and-types type :force t)))))
    ;; mark the dest type as used
    (use-type type)
    ;; and update the form if needed
    (let ((new (list* (first *current-form*)
                      id type var new-indices)))
      (unless (equal *current-form* new)
        (setf *current-form* new)))))

(defmethod pass1 :around ((op (eql 'spirv-core:composite-construct)) args)
  (destructuring-bind (name type &rest values) args
    (let ((nv (mapcar 'normalize-literals-and-types values)))
      (cond
        ;; all arguments are literals, convert to a constant
        ;; fixme: make sure this catches named constants too
        ((every (lambda (x) (typep x '(cons (eql :literal)))) nv)
         (loop for l in nv
               do (use-type (second nv)))
         ;; add name as an alias for the literal
         (let* ((l (composite-literal (normalize-literals-and-types type) nv)))
           #+=(format t "~&cc ~s ->~% ~s~% @ ~s (~s)->   ~s~%" values
                      nv
                      type (normalize-literals-and-types type)
                      l)
           (use-type l)
           (setf (gethash name *literal-names*) l))
         ;; and drop the instruction since it will be created as a
         ;; constant instead
         (setf *current-form* nil))
        ;; non-constant arguments, handle normally
        (t (call-next-method))))))

(defun first-pass (code)
  (loop for form in code
        for (op . args) = form
        ;; give functions a chance to update the form (mostly for
        ;;   replacing literals with IDs so we don't need to duplicate
        ;;   work in 2nd pass)
        ;; (possibly should pass it directly, or let functions just
        ;;   rebuild it from op and args instead of making it optional?
        collect (let ((*current-form* form))
                  (pass1 op args)
                  *current-form*)))



(defgeneric pass2 (op args))

(defmethod pass2 (op args)
  ;; second-pass expects a list of forms, so wrap form in list by
  ;; default
  (list *current-form*))

(defmacro defpass2 (op lambda-list &body body)
  (let ((args (gensym "ARGS")))
    `(defmethod pass2 ((op (eql ',op)) ,args)
       (destructuring-bind ,lambda-list ,args
         ,@body))))

(defun %second-pass (code)
  (loop for form in code
        for (op . args) = form
        ;; 2nd pass can return multiple forms (for example a type
        ;;  definition might need to include a bunch of types it
        ;;  depends on, and defun returns whole body) so pass2 always
        ;;  returns a list of forms
        append (let ((*current-form* form))
                 (pass2 op args))))

(defpass2 defun (name lambda-list &body body)
  (assert (eq (caar body) 'declare))
  (let* ((decl (cdar body))
         (values (cdr (assoc 'values decl))))
    `((spirv-core:function ,name
                           ,(normalize-literals-and-types
                             (or (car values) ':void) :force t)
                           ,(cadr (assoc 'function-control decl))
                           ,(cadr (assoc 'ftype decl)))
      ,@(loop for d in (cdddr (assoc 'ftype decl))
              for n in lambda-list
              collect `(spirv-core:function-parameter ,d ,n))
      ,(if (eql (caar body) 'spirv-core:label)
           (car body)
           `(spirv-core:label ,(gensym "FUNCTION-BODY")))
      ,@ (%second-pass (cdr body))
      ,@ (unless (eql (caar (last body)) 'spirv-core:function-end)
           `((spirv-core:function-end))))))


(defun sort-types-for-dump (types)
  (labels ((r (x)
             (format t "~&r ~s" x)
             (or (and (not (numberp x))
                      (gethash x *dumped-types*))
                 (let ((d (etypecase x
                            ((or number
                                 (eql :void)
                                 (cons (member :void :bool :int :uint :float)))
                             0)
                            ((cons (eql :composite))
                             (reduce 'max (mapcar #'r (cdr x))))
                            ((cons (member :vec :literal))
                             (1+ (max (r (second x))
                                      (r (third x)))))
                            ((cons (member :mat))
                             (1+ (max (r `(:vec ,(second x) ,(third x)))
                                      (r (fourth x)))))

                            ((cons (member :array))
                             (1+ (max (r (second x))
                                      (r (third x)))))
                            ((cons (member :pointer))
                             (1+ (r (third x))))
                            ((cons (member :variable))
                             ;; variables handled separately
                             :variable)
                            (block/buffer/struct
                             (1+ (reduce 'max
                                         (loop for (nil m) in (cddr x)
                                               collect (r m)))))
                            ((cons (member :function))
                             (1+ (reduce 'max (mapcar #'r (cdr x)))))
                            ;; implicit struct/block names
                            ((cons (member :struct :block :buffer))
                             (1+ (r (gethash x *types*)))))))
                   ;; probably should make a separate place to store results
                   ;; but just putting in dumped hash under integers 0..N
                   (format t "~s  ==>~s~%" x d)
                   (unless (typep x '(or number
                                      (cons (member :composite))))
                     (pushnew x (gethash d *dumped-types*)
                              :test 'equalp)
                     (setf (gethash x *dumped-types*) d))
                   d))))
    (let ((max (loop for k being the hash-keys of types using (hash-value v)
                     for d = (r k)
                     when (numberp d)
                       maximize d)))
      (loop for i upto max
            do (setf (gethash i *dumped-types*)
                     (print (sort (print (gethash i *dumped-types*))
                                  'string< :key 'prin1-to-string))))
      (loop for i upto max
            for dump = (gethash i *dumped-types*)
            append (loop for x in dump
                         do (format t "~&~2,'0d: ~s~%" i x)
                         collect x)))))

(defun translate-for-packing (x)
  (typecase x
    (block/buffer/struct
     (let* ((tmp (copy-list x))
            (opts (second x))
            (packing)
            (major))
       (setf (second tmp)
             (loop for o in opts
                   when (member o '(:std140 :std430))
                     do (setf packing o)
                   else when (member o '(:row :column))
                          do (setf major o)
                   else collect o))
       (setf (getf (second tmp) :major) major)
       (setf (getf (second tmp) :packing) packing)
       tmp))
    #++
    ((cons (eql :array))
     (?))
    (t x)))

(defun second-pass (code)
  (let ((orig-code code)
        (blocks nil)
        (structs nil)
        (pack nil)
        (variables (alexandria:hash-table-keys *variables*))
        #++(*remapped-types* (make-hash-table :test #'equal)))
    (declare (ignorable orig-code))
    #++(loop for (n . type) in (alexandria:hash-table-alist glsl-packing:*base-types*)
             do (setf (gethash n *remapped-types*) type))
    (loop
      for (n . type) in (alexandria:hash-table-alist *types*)
      do (format t "~s~%   ~s~%" n type)
      do (typecase type
           (block/buffer
            (push n blocks))
           ((cons (eql :struct))
            (push n structs))
           (variable
            (push n variables))))

    (macrolet ((section (num (&rest ops))
                 (declare (ignorable num))
                 (let ((prefixes (remove-if-not 'stringp ops)))
                   `(progn
                      (format t "2nd pass, section ~s:~%" ,num)
                      (%second-pass
                       (loop
                         while (and
                                code
                                (or (member (caar code)
                                            ',(remove-if 'stringp ops))
                                    ,@(loop
                                        for p in prefixes
                                        collect `(alexandria:starts-with-subseq
                                                  ,p (symbol-name (caar code))))))
                         collect (pop code)))))))
      (remove
       nil
       (append
        (section 1 (capabilties spirv-core:capability))
        (section 2 (spirv-core:extension))
        (section 3 (spirv-core:ext-inst-import))
        (section 4 (spirv-core:memory-model))
        (section 5 (spirv-core:entry-point))
        (section 6 (spirv-core:execution-mode))
        (let ((s7 (section 7 (spirv-core:source-extension
                              spirv-core:source
                              spirv-core:source-continued
                              spirv-core:name
                              spirv-core:member-name)))
              (dumped (make-hash-table :test #'equal)))
          ;; possibly should just forbid mixing low-level API for
          ;; names, decorations and type definitions with HL API,
          ;; since trying to avoid duplications of conflicts is messy?
          (loop for (o n) in s7
                when (eq o 'spirv-core:name)
                  do (setf (gethash n dumped) t)
                when (eq o 'spirv-core:member-name)
                  ;; assuming for now that if we see member-name, we
                  ;; don't need to dump struct name
                  do (setf (gethash n dumped) t))
          (append
           s7
           (loop
             for (n . type) in (sort (alexandria:hash-table-alist *types*)
                                     'string<
                                     :key 'princ-to-string)
             unless (gethash n dumped)
               collect `(spirv-core:name ,n ,n)
               and when (typep type 'block/buffer/struct)
                     append (loop for i from 0
                                  for m in (cddr type)
                                  collect `(spirv-core:member-name ,n ,i ,(car m))))))
        (let* ((pack-index (make-hash-table :test #'equal))
               (to-pack (loop for i in (append (reverse structs)
                                               (reverse blocks))
                              for x = (gethash i *types*)
                              ;; todo: handle multiple names for same type?
                              do (format t "i:~s x=~s~%" i x)
                              unless (eq i x)
                                do (assert (not (gethash x pack-index)))
                                   (setf (gethash x pack-index) i)
                                and collect (list i (translate-for-packing x)))))
          (format t "pack = ~s~%" to-pack)
          (setf pack (glsl-packing:pack-structs to-pack))

          (format t "packed = ~s~%" pack)
          (append
           (section 8 (spirv-core:decorate
                       spirv-core:member-decorate
                       spirv-core:group-decorate
                       spirv-core:group-member-decorate
                       spirv-core:decoration-group))
           (loop for (packed-name info) in pack
                 for (n . layout) = (print packed-name)
                 for orig = (gethash n pack-index)
                 do (format t "n ~s, packed-name ~s~%" n packed-name)
                 when (typep n '(cons (member :block :buffer)))
                   collect `(spirv-core:decorate
                             ;; add block/buffer-block decoration
                             ,n
                             ,(if (eq (car n) :block)
                                  :block
                                  :buffer-block))
                 when (getf info :stride)
                   collect `(spirv-core:decorate
                             ,n
                             :array-stride ,(getf info :stride))
                 when (getf info :members)
                   append (loop for m in (remove-if ;; skip nested members
                                          'consp
                                          (getf info :members)
                                          :key 'second)
                                for i from 0
                                collect `(spirv-core:member-decorate
                                          ,(or orig n)
                                          ,i
                                          :offset ,(getf m :offset))
                                when (getf m :matrix-stride)
                                  collect `(spirv-core:member-decorate
                                            ,(or orig n)
                                            ,i
                                            :matrix-stride
                                            ,(getf m :matrix-stride))
                                  and collect `(spirv-core:member-decorate
                                                ,(or orig n)
                                                ,i
                                                ,(ecase (getf m :major)
                                                   (:row :row-major)
                                                   (:column :col-major)))))
           (loop for (o . r) in code
                 when (and (member o '(input output uniform buffer))
                           (getf (gethash (first r) *variables*) :decorate))
                   append (loop with n = (first r)
                                ;; allow single decoration with no args
                                ;; outside list
                                for d in (alexandria:ensure-list
                                          (getf (gethash n *variables*)
                                                :decorate))
                                collect `(spirv-core:decorate
                                          ,n
                                          ;; allow :foo or (:foo arg)
                                          ;; for each decoration
                                          ,@ (alexandria:ensure-list d))))))
        (let* ((*dumped-types* (make-hash-table :test #'equal))
               (types (sort-types-for-dump *types*)))
          ;; dump anonymous types and structs (blocks are handled by
          ;; normal pass2 methods)
          #++(break "foo" :*types *types*
                          :*dumped-types *dumped-types*
                          :types types
                          :structs struct-names
                          :pack pack)
          (append
           (loop for x in types
                 do (format t "~&dump ~s~%" x)
                 collect (print
                          (etypecase x
                            ((or (eql :void)
                                 (cons (eql :void)))
                             `(spirv-core:type-void ,x))
                            ((cons (eql :bool))
                             `(spirv-core:type-bool ,x))
                            ((cons (eql :int))
                             `(spirv-core:type-int ,x ,(second x) t))
                            ((cons (eql :uint))
                             `(spirv-core:type-int ,x ,(second x) nil))
                            ((cons (eql :float))
                             `(spirv-core:type-float ,x ,(second x)))
                            ((cons (eql :vec))
                             `(spirv-core:type-vector ,x ,@(cdr x)))
                            ((cons (eql :mat))
                             `(spirv-core:type-matrix ,x ,@(cdr x)))
                            ((cons (eql :literal))
                             (if (typep (third x) '(cons (eql :composite)))
                                 `(spirv-core:constant-composite
                                   ,x ,(second x)
                                   ,@ (cdr (third x)))
                                 `(spirv-core:constant ,x ,@(cdr x))))
                            ((cons (eql :pointer))
                             `(spirv-core:type-pointer ,x ,@(cdr x)))
                            ((cons (eql :array))
                             `(spirv-core:type-array ,x ,@(cdr x)))
                            (block/buffer/struct
                             nil)
                            ((cons (member :struct :block :buffer))
                             `(spirv-core:type-struct
                               ,x ,@(mapcar 'second
                                            (cddr (gethash x *types*)))))

                            ((cons (eql :function))
                             `(spirv-core:type-function ,x ,@(cdr x))))))
           (section 9 (spirv-core:variable
                       spirv-core:undef
                       spirv-core:line
                       "TYPE-" "CONSTANT-" "SPEC-"
                       defstruct input output uniform buffer))))
        (%second-pass code))))))

(defpass2 defstruct (name &rest members)
  (declare (ignore name members))
  nil)

(defpass2 input (name type &key default decorate)
  (declare (ignore decorate type))
  `((spirv-core:variable ,name ,(getf (gethash name *variables*) :type)
                         :input ,@(when default (list default)))))

(defpass2 output (name type &key default decorate)
  (declare (ignore decorate type))
  `((spirv-core:variable ,name ,(getf (gethash name *variables*) :type)
                         :output ,@(when default (list default)))))

(defpass2 uniform(name type &key default decorate layout)
  (declare (ignore decorate type layout))
  `((spirv-core:variable ,name , (getf (gethash name *variables*) :type)
                         :uniform ,@(when default (list default)))))

(defpass2 buffer (name type &key default decorate layout)
  (declare (ignore decorate type layout))
  `((spirv-core:variable ,name ,(getf (gethash name *variables*) :type)
                         :buffer ,@(when default (list default)))))

(defpass2 local (name type &key default decorate)
  (declare (ignore decorate))
  `((spirv-core:variable ,name ,type
                         :function ,@(when default (list default)))))



(defun assemble (code)
  (handler-bind
      ((3b-spirv::cap-not-enabled
         (lambda (c) (declare (ignorable c))
           (format t "~&%%%%%enabling cap ~s~%" (3b-spirv::cap c))
           (invoke-restart '3b-spirv::add-caps))))
   (with-hl-asm-context ()
     (setf code (first-pass code))
     (let ((tt (alexandria:hash-table-alist *types*)))
       (format t "literals=~{~s=> ~s~%~^        ~}"
               (alexandria:alist-plist
                (remove :literal tt
                        :test-not 'eql
                        :key (lambda (a) (and (consp (car a)) (caar a))))))
       (format t "types=~{~s=> ~s~%~^      ~}"
               (alexandria:alist-plist
                (remove :literal tt
                        :key (lambda (a) (and (consp (car a)) (caar a)))))))
     (format t "code1=~s~%" code)
     (setf code (second-pass code))
     (format t "code2=~{~s~^~%      ~}~%" code)
     (list *types* *type-deps* *entry-points* code)
     (3b-spirv::ll-assemble code))))


(defun assemble-to-file (filename code)
  (let ((out (assemble code)))
    (with-open-file (f filename
                       ;; assuming LE host that writes ub32 packed
                       :element-type '(unsigned-byte 32)
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :direction :output)
      (write-sequence out f))
    (print out)
    nil))

#++
(assemble '((literal foo :int 0)
            (spirv-core:store i foo)
            (literal bar :int 0)
            (spirv-core:store i bar)))

#++
(assemble-to-file
 "/tmp/example-hl2.spv"
 '( ;; (capabilities &rest caps) ;; 1x
   (spirv-core:capability :shader)
   ;; (import-extension ID "name") ;; 0 or more
   (spirv-core:ext-inst-import :glsl "GLSL.std.450")
   ;; (memory-model addressing-model memory-model) ;; 1x
   (spirv-core:memory-model :logical :glsl-450)
   ;; (entry-point execution-model id "name" &rest interface-IDs)
   (spirv-core:entry-point :fragment main "main" color color1 color2 multiplier)
   ;; (execution-mode id execution-mode &rest args)
   (spirv-core:execution-mode main :origin-lower-left)
   ;; (source source-language version-number &optional filename source)
   (spirv-core:source :glsl 450)
   ;; (defstruct name-and-options &rest slot-definitions)
   (defstruct s
     (b :bool)
     (v (:array :vec4 5))
     (i :int))
   ;; (input name type &key decorate) same for output,uniform
   (input color1 :vec4)
   (input multiplier :vec4)
   (input color2 :vec4 :decorate (:no-perspective))
   (output color :vec4)
   (uniform block-name (:block () (s s) (cond :bool))
    :decorate ((:descriptor-set 0)))
   (defun main ()
     (declare (values))
     (local scale :vec4 #+:default (1.0 1.0 2.0 1.0))
     (local i :int  #+:default 0)
     (spirv-core:store scale (the :vec4 (1.0 1.0 2.0 1.0)))
     ;; %xx would probably be gensyms in practice
     ;; using @xx for labels to make them easier to distinguish
     ;; for human readers but has no effect on assembler
     (spirv-core:access-chain %23 :auto block-name 1) ; location of cond
     (spirv-core:load %24 :uint %23)    ; load 32bit int from cond
     (spirv-core:i-not-equal %27 :bool %24 (the :uint 0)) ; convert to bool
     (spirv-core:selection-merge @29 :none)         ; structured if
     (spirv-core:branch-conditional %27 @28 @41)    ; if cond
     (spirv-core:label @28)                         ; then
     (spirv-core:load %34 :vec4 color1)
     (spirv-core:access-chain %38 :auto block-name 0 1 2) ; s.v[2]
     (spirv-core:load %39 :vec4 %38)
     (spirv-core:f-add %40 :vec4 %34 %39)
     (spirv-core:store color %40)
     (spirv-core:branch @29)
     (spirv-core:label @41)             ; else
     (spirv-core:load %43 :vec4 color2)
     (spirv-glsl450:sqrt %44 :vec4 %43) ; extended instruction sqrt
     (spirv-core:load %45 :vec4 scale)
     (spirv-core:f-mul %46 :vec4 %44 %45)
     (spirv-core:store color %46)
     (spirv-core:branch @29)
     (spirv-core:label @29)
     (spirv-core:store i (the :int 0))
     (spirv-core:branch @49)
     (spirv-core:label @49)
     (spirv-core:loop-merge @51 @52 :none) ; structured loop
     (spirv-core:branch @53)
     (spirv-core:label @53)
     (spirv-core:load %54 :int i)
     (literal %l4 :int 4) ;; alternate syntax for literals
     (spirv-core:s-less-than %56 :bool %54 %l4) ; i < 4
     (spirv-core:branch-conditional %56 @50 @51) ; body or break
     (spirv-core:label @50)                      ; body
     (spirv-core:load %58 :vec4 multiplier)
     (spirv-core:load %59 :vec4 color)
     (spirv-core:f-mul %60 :vec4 %59 %58)
     (spirv-core:store color %60)
     (spirv-core:branch @52)
     (spirv-core:label @52)             ; continue target
     (spirv-core:load %61 :int i)
     (spirv-core:i-add %62 :int %61 (the :int 1)) ; ++i
     (spirv-core:store i %62)
     (spirv-core:branch @49)            ; loop back
     (spirv-core:label @51)             ; loop merge point
     ;;(spirv-core:bit-reverse %70 :int %54) ;; something with caps
     (spirv-core:return))))
