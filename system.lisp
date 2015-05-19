(in-package #:lgame)

;;;
;;; class
;;;

(defclass =system= ()
  ())

(defmethod initialize-instance :after ((system =system=) &key)
  "Sets the special variable associated with this instance."
  (setf (symbol-value (type-of system)) system))

(defmethod deinitialize-instance ((system =system=))
  "Called when a system is stopped. By default unsets the special variable
associated with this instance."
  (setf (symbol-value (type-of system)) nil))

(defmacro define-system (class-name superclasses &body body)
  "Use exactly like 'defclass.' This is the same as defining a class normally,
but also does some necessary bookkeeping for a system class (add =system= as a
superclass, register the class as a system class to load on start, define a
special variable for easy access)."
  `(progn
     (defclass ,class-name (,@superclasses =system=)
       ,@body)
     (defvar ,class-name nil)
     (eval-when (:load-toplevel :execute)
       (register-system-class ',class-name))))



;;;
;;; system management
;;;

(defvar *system-classes* ()
  "Systems to create on game start.")

(defvar *systems* (make-hash-table)
  "Class name -> instance table for currently running systems.")

(defmacro do-systems ((var) &body body)
  "(do-systems (var) ...) iterates over systems, binding var to the system
currently being visited."
  (let ((class-var (gensym)))
    `(maphash #'(lambda (,class-var ,var)
                  (declare (ignore ,class-var))
                  ,@body)
              *systems*)))


(defvar *game-running* nil)

(defun register-system-class (class)
  (pushnew class *system-classes*)
  (when *game-running*
    (start-system class)))

(defun clear-system-classes ()
  (setf *system-classes* nil))


(defmacro get-system (class)
  `(gethash ,class *systems*))

(defun start-system (class)
  (assert (find class *system-classes*))
  (unless (get-system class)
    (setf (get-system class) (make-instance class))))

(defun stop-system (system &optional class)
  (let ((class (or class (type-of system))))
    (remhash class *systems*)
    (deinitialize-instance system)))

(defun restart-system (system)
  (let ((class (type-of system)))
    (stop-system system)
    (start-system class)))


(defun start-systems ()
  (mapc #'start-system *system-classes*))

(defun stop-systems ()
  (do-systems (s) (stop-system s))
  (clrhash *systems*))                  ; just in case

(defun restart-systems ()
  (stop-systems)
  (start-systems))



;;;
;;; basic events
;;;

(defmethod update ((system =system=) dt)
  )

(defmethod draw ((system =system=))
  )


