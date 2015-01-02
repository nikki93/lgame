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


