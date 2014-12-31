(in-package #:lgame)

;;;
;;; create/destroy
;;; 

(defvar *curr-entity* 0)

(defun generate-entity ()
  (incf *curr-entity*))                 ; TODO: better entity id generation

(defmacro create-entity (&body body)
  "Generate an entity and add it to systems. 'body' is like in
'add-to-systems.'"
  `(add-to-systems (generate-entity) ,@body))

(defun destroy-entity (entity)
  (do-systems (system)                   ; TODO: maintain which entity-systems
    (remove-from-system system entity))) ;       an entity is in?


