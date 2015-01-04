(in-package #:lgame)

;;;
;;; =rotator=
;;;

(define-entity-system =rotator= ()
    (define-entity-cell rotator ()
      ((rate :initform 0.02 :initarg :rate :accessor rate))))

(defmethod update ((rotator rotator) dt)
  (incf (rot (=transform= (entity rotator))) (* 2 dt (rate rotator))))



;;;
;;; =oscillator=
;;;

(define-entity-system =oscillator= ()
    (define-entity-cell oscillator ()
      ((rate :initarg :rate :accessor rate :initform 0.5)
       (amp :initarg :amp :accessor amp :initform 1)))
    ((elapsed :initform 0 :accessor elapsed)))

(defmethod update ((oscillator oscillator) dt)
  (with-slots (entity rate amp) oscillator
    (setf (vec2-y (pos (=transform= entity)))
          (* amp (sin (* 2 PI rate (elapsed =oscillator=)))))))

(defmethod update :before ((system =oscillator=) dt)
  (incf (elapsed =oscillator=) dt))



;;;
;;; scenes
;;;

(defparameter *player* nil)
(defparameter *enemy* nil)

(defun test-basic ()
  (setf *player*
        (create-entity (=transform= :scale (vec2 2 2))
                       (=sprite=)
                       (=rotator= :rate (/ PI 2))))

  (add-to-systems *player* (=oscillator= :rate 0.5))

  (setf *enemy*
        (create-entity (=transform= :pos (vec2 -0.5 0))
                       (=sprite=)
                       (=rotator= :rate PI)
                       (=oscillator= :rate 1))))


(defparameter *boxes* nil)

(defun test-huge (n)
  (labels ((symrand ()
             (- (* 2 (random 1.0)) 1)))

    (setf *boxes* ())
    (dotimes (i n)
      (push
       (create-entity (=transform= :pos (vec2 (* 8 (symrand)) (* 8 (symrand))))
                      (=sprite= :texcell (vec2 (* (random 2) 32) 64))
                      (=rotator= :rate (* PI (symrand)))
                      (=oscillator= :rate (symrand) :amp (* 8 (symrand))))
       *boxes*))))
