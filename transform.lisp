(in-package #:lgame)

;;;
;;; class
;;;

(define-entity-system =transform= ()
    (define-entity-cell transform ()
      ((pos :initform (vec2 0 0) :initarg :pos :accessor pos)
       (rot :initform 0 :initarg :rot :accessor rot)
       (scale :initform (vec2 1 1) :initarg :scale :accessor scale))))



;;;
;;; interface
;;;

(defmethod world-matrix ((system =transform=) ent)
  (let ((cell (cell system ent)))
    (assert cell)
    (transform->mat3 (scale cell) (rot cell) (pos cell))))


