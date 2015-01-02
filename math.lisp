(in-package #:lgame)

;;;
;;; vec2
;;;

(defun vec2 (x y)
  (let ((v (make-array 2 :element-type 'single-float)))
    (setf (aref v 0) (float x) (aref v 1) (float y))
    v))

(defun vec2-x (v) (aref v 0))
(defun (setf vec2-x) (val v) (setf (aref v 0) (float val 1.0)))
(defun vec2-y (v) (aref v 1))
(defun (setf vec2-y) (val v) (setf (aref v 1) (float val 1.0)))



;;;
;;; mat3
;;;

(defun mat3 (m00 m01 m02
             m10 m11 m12
             m20 m21 m22)
  (let ((m (make-array '(3 3) :element-type 'single-float)))
    (setf (mat3-ref m 0 0) m00 (mat3-ref m 1 0) m10 (mat3-ref m 2 0) m20
          (mat3-ref m 0 1) m01 (mat3-ref m 1 1) m11 (mat3-ref m 2 1) m21
          (mat3-ref m 0 2) m02 (mat3-ref m 1 2) m12 (mat3-ref m 2 2) m22)
    m))

(defun mat3-ref (m i j) (aref m i j))
(defun (setf mat3-ref) (val m i j) (setf (aref m i j) (float val 1.0)))

(defun transform->mat3 (scale rot trans)
  "Return a matrix representing the given scaling (a vec2), rotation (an angle)
and translation (a vec2) applied in that order."
  (mat3 (* (vec2-x scale) (cos rot)) (* (vec2-x scale) (sin rot)) 0
        (* (vec2-y scale) (- (sin rot))) (* (vec2-y scale) (cos rot)) 0
        (vec2-x trans) (vec2-y trans) 1))


