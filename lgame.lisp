;;;; lgame.lisp

(in-package #:lgame)

;;;
;;; ideas/todo:
;;;
;;;    - an abstract 'system dependencies' mechanism
;;;      - you can establish relationships of the form 'system x depends on
;;;        y in z,' for example system gui depends on text in initialization,
;;;        or system sprite depends on transform in entity ownership
;;;      - you can make queries of the form "get ordered list of systems in
;;;        z," for example, "get systems in initialization order"
;;;      - you can ask what a system depends on in z, for example while adding
;;;        an entity to an entity system
;;;
;;;    - don't use initialize-instance for logic side-effects stuff
;;;      - might want to do memory pool stuff later
;;;      - if you fix the above make sure to forward cell initargs and change
;;;        the name of the deinit counterpart
;;;



;;;
;;; math
;;;

;;; vec2

(defun vec2 (x y) (vector x y))

(defun vec2-x (v) (aref v 0))
(defun vec2-y (v) (aref v 1))



;;;
;;; entity
;;; 

(defvar *curr-entity* 0)

(defun generate-entity ()
  (incf *curr-entity*))                 ; TODO: better entity id generation

(defmacro create-entity (&body body)
  "Generate an entity and add it to systems. 'body' is like in 'add-to-systems.'"
  `(add-to-systems (generate-entity) ,@body))

(defun destroy-entity (entity)
  (dolist (system *systems*)             ; TODO: keep track of which
    (remove-from-system system entity))) ;       entity-systems an entity is in?



;;;
;;; system
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
  "Use exactly like 'defclass.' This is the same as defining a class
normally, but also does some necessary bookkeeping for a system class (add
=system= as a superclass, register the class as a system class to load on
start, define a special variable for easy access."
  `(progn
     (defclass ,class-name (,@superclasses =system=)
       ,@body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,class-name nil)
       (register-system-class ',class-name))))


(defvar *system-classes* ()
  "Systems to create on game start.")

(defvar *systems* (make-hash-table)
  "Class name -> instance table for currently running systems.")


(defun register-system-class (class)
  (pushnew class *system-classes*))

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


(defmacro do-systems ((var) &body body)
  "(do-systems (var) ...) iterates over systems, binding var to the
system currently being visited."
  (let ((class-var (gensym)))
    `(maphash #'(lambda (,class-var ,var)
                  (declare (ignore ,class-var))
                  ,@body)
              *systems*)))



;;;
;;; entity-system
;;; 

(defclass entity-cell ()
  ((entity
    :initform (error "Entity not specified for entity-cell.")
    :initarg :entity
    :reader entity
    :documentation "The entity for which this cell holds data.")))

(defmethod deinitialize-instance ((cell entity-cell))
   "Called when a cell is discarded, for example when its entity
is removed from the system. By default this does nothing.")


(defclass =entity-system= (=system=)
  ((cell-class
    :initform (error "Cell class not specified for an entity-system.")
    :documentation "Name of class to use for values in entity -> cell hash
table. Must be a subclass of entity-cell.")
   (table
    :initform (make-hash-table :test 'equal)
    :accessor table
    :documentation "Entity -> cell hash table.")))

(defgeneric add-to-system (system entity &rest args)
  (:documentation "Add entity to an entity-system. By default simply creates
a cell giving it the initializer arguments and adds it to the table, doing
nothing if the entity is already in the system.")
  (:method ((system =entity-system=) entity &rest args)
    (with-slots (table cell-class) system
      (unless (gethash entity table)
        (setf (gethash entity table)
              (apply #'make-instance
                     (list* cell-class :entity entity args)))))))

(defgeneric remove-from-system (system entity)
  (:documentation "Remove entity from an entity-system. By default removes
the cell from the table and calls deinitialize-instance on it, doing nothing
if the entity is not in the system.")
  (:method ((system =entity-system=) entity)
    (with-slots (table) system
      (let ((cell (gethash entity table)))
        (when cell
          (remhash entity table)
          (deinitialize-instance cell))))))

(defmacro add-to-systems (entity &body body)
  "Add an entity to multiple systems as described by 'body' of the form,
    (system1 args) (system2 args) ...
For example,
    (add-to-systems e (=transform= :pos (vec2 3 4)) (=enemy= :health 3))"
  (let ((entity-var (gensym)))
    `(let ((,entity-var ,entity))
       ,@(mapcar
          #'(lambda (form)
              `(add-to-system ,(first form) ,entity-var ,@(rest form)))
          body)
       ,entity-var)))

(defun cell (system entity)
  "Get the cell for an entity in an entity-system, nil if not found."
  (gethash entity (table system)))

(defmacro prop (system accessor entity)
  "Invoke an accessor on a cell for an entity in an entity-system. Setf-able."
  `(,accessor (cell ,system ,entity)))



;;;
;;; basic systems
;;;

(defmacro do-hash ((k v table) &body body)
  `(maphash #'(lambda (,k ,v) ,@body) ,table))


;;; events

(defgeneric update (system dt)
  (:documentation "Step a system forward by dt.")
  (:method ((system =system=) dt)))

(defgeneric draw (system)
  (:documentation "Draw a system to the screen.")
  (:method ((system =system=))))


;;; transform

(defclass transform (entity-cell)
  ((pos :initform (vec2 0 0) :initarg :pos :accessor pos
    :documentation "Position of the entity in parent-space.")
   (rot :initform 0 :initarg :rot :accessor rot
    :documentation "Rotation of the entity in parent-space.")
   (scale :initform (vec2 1 1) :initarg :scale :accessor scale
    :documentation "Scale of the entity in parent-space.")))

(define-system =transform= (=entity-system=)
  ((cell-class :initform 'transform)))


;;; sprite

(defclass sprite (entity-cell)
  ((color :initform '(1 0 0 1) :initarg :color :accessor color)))

(define-system =sprite= (=entity-system=)
  ((cell-class :initform 'sprite)))

(defmethod draw ((system =sprite=))
  (do-hash (entity cell (table system))
    (gl:with-pushed-matrix
      (with-slots (pos rot scale) (cell =transform= entity)
        (gl:translate (vec2-x pos) (vec2-y pos) 0)
        (gl:rotate rot 0 0 -1)
        (gl:scale (vec2-x scale) (vec2-y scale) 1)
        (destructuring-bind (r g b a) (color cell)
          (gl:color r g b a))
        (gl:rect -0.5 -0.5 0.5 0.5)))))


;;; rotator

(defclass rotator (entity-cell)
  ((rate :initform 0.02 :initarg :rate :accessor rate)))

(define-system =rotator= (=entity-system=)
  ((cell-class :initform 'rotator)))

(defmethod update ((system =rotator=) dt)
  (do-hash (entity cell (table system))
    (declare (ignore cell))
    (incf (prop =transform= rot entity)
          (* dt (prop =rotator= rate entity)))))



;;;
;;; game
;;;

(defun update-swank ()
  "Handle REPL requests."
  (continuable
   (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))

(defmacro continuable (&body body)
  "Allow continuing execution from errors."
  `(restart-case
       (progn ,@body)
        (continue () :report "Continue")))

(defun update-game ()
  (do-systems (system)
    (update system 0.02)))

(defun draw-game ()
  (gl:clear :color-buffer-bit)
  (do-systems (system)
    (draw system))
  (gl:flush)
  (sdl:update-display))

(defun run-game ()
  (sdl:with-init (sdl:sdl-init-video)
    (sdl:window 640 480 :flags sdl:sdl-opengl)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             (update-swank)
             (continuable
               (update-game)
               (draw-game))))))


