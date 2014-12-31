(in-package #:lgame)

;;;
;;; macros
;;;

(defmacro do-hash ((k v table) &body body)
  "Look through entries of hash table `table', binding `k' to the key and `v'
to the value"
  `(maphash #'(lambda (,k ,v)
                (declare (ignorable ,k ,v))
                ,@body) ,table))

(defmacro with-cstruct-slots ((vars ptr type) &body body)
  "Create local symbol macros for each var in `vars' to reference foreign slots
in `ptr' of `type'. Similar to `with-slots'.  Each var can be of the form:
slot-name - in which case slot-name will be bound to the value of the slot or:
\(:pointer slot-name) - in which case slot-name will be bound to the pointer to
that slot. This is like cffi:with-foreign-slots except it calculates slot
offsets at compile time."
  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var ,ptr))
       (symbol-macrolet
           ,(loop :for var :in vars
               :collect
               (if (listp var)
                   (if (eq (first var) :pointer)
                       `(,(second var) (cffi:inc-pointer
                                        ,ptr-var
                                        ,(cffi:foreign-slot-offset
                                          type (second var))))
                       (error
                        "Malformed slot specification ~a"
                        var))
                   `(,var (cffi:mem-ref
                           (cffi:inc-pointer ,ptr-var
                                             ,(cffi:foreign-slot-offset
                                               type var))
                           ,(cffi:foreign-slot-type type var)))))
         ,@body))))


