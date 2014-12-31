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
     (eval-when (:load-toplevel :execute) ; TODO: is this right?
       (defvar ,class-name nil)
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


(defun register-system-class (class)
  (pushnew class *system-classes*)
  (unless (zerop (hash-table-size *systems*))
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
  (maphash #'(lambda (c s) (stop-system s c)) *systems*))

(defun restart-systems ()
  (stop-systems)
  (start-systems))



;;;
;;; basic events
;;;

(defgeneric update (system dt)
  (:documentation "Step a system forward by dt.")
  (:method ((system =system=) dt)
    (declare (ignore dt))))

(defgeneric draw (system)
  (:documentation "Draw a system to the screen.")
  (:method ((system =system=))))


