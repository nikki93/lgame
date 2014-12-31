(in-package #:lgame)

;;;
;;; vec2
;;;

(defun vec2 (x y) (vector x y))

(defun vec2-x (v) (aref v 0))
(defun (setf vec2-x) (val v) (setf (aref v 0) val))
(defun vec2-y (v) (aref v 1))
(defun (setf vec2-y) (val v) (setf (aref v 1) val))


