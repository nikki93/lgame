(in-package #:lgame)

;;;
;;; class
;;;

(define-entity-system =sprite= ()
    (define-entity-cell sprite ()
      ((color :initform '(1 0 0 1) :initarg :color :accessor color))))



;;;
;;; events
;;;

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


