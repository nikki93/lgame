(in-package #:lgame)

;;;
;;; entity-cell
;;; 

(defclass entity-cell ()
  ((entity
    :initform (error "Entity not specified for entity-cell.")
    :initarg :entity
    :reader entity
    :documentation "The entity for which this cell holds data.")))

(defmethod deinitialize-instance ((cell entity-cell))
  "Called when a cell is discarded, for example when its entity
is removed from the system. By default this does nothing."
  nil)

(defmacro define-entity-cell (class-name superclasses &body body)
  "Use exactly like 'defclass.' This is the same as defining a class normally,
but also does some necessary bookkeeping for a cell class (add entity-cell as a
base class)."
  `(defclass ,class-name (,@superclasses entity-cell)
     ,@body))



;;;
;;; entity-system
;;; 

;;; class

(defclass =entity-system= (=system=)
  ((cell-class
    :initform (error "Cell class not specified for an entity-system.")
    :documentation "Name of class to use for values in entity -> cell hash
table. Must be a subclass of entity-cell.")
   (table
    :initform (make-hash-table :test 'equal)
    :accessor table
    :documentation "Entity -> cell hash table.")))

(defmacro define-entity-system (class-name superclasses
                                cell-class &optional slots
                                &body rest)
  "Like 'define-system,' except you define the cell class inline and it adds
=entity-system= as a base class."
  (let ((cell-class-var (gensym)))
    `(let ((,cell-class-var ,cell-class))
       (define-system ,class-name (,@superclasses =entity-system=)
         (,@slots (cell-class :initform ,cell-class-var))
         ,@rest))))


;;; interface

(defgeneric add-to-system (system entity &rest args)
  (:documentation "Add entity to an entity-system. By default simply creates a
cell giving it the initializer arguments and adds it to the table, doing nothing
if the entity is already in the system.")
  (:method ((system =entity-system=) entity &rest args)
    (with-slots (table cell-class) system
      (unless (gethash entity table)
        (setf (gethash entity table)
              (apply #'make-instance
                     (list* cell-class :entity entity args)))))))

(defgeneric remove-from-system (system entity)
  (:documentation "Remove entity from an entity-system. By default removes the
cell from the table and calls deinitialize-instance on it, doing nothing if the
entity is not in the system.")
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
    (add-to-systems e (=transform= :pos (vec2 3 4)) (=enemy= :health 3))
Returns the entity."
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


