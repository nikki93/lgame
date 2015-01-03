(in-package #:lgame)

(declaim (optimize (space 0) (speed 3)))

;;;
;;; vec2
;;;

;;; type

(deftype vec2 () '(simple-array single-float (2)))


;;; constructor

(defmacro vec2 (x y)
  (let ((v (gensym)))
    `(let ((,v (make-array 2 :element-type 'single-float)))
       (psetf (vec2-x ,v) (float ,x 1.0) (vec2-y ,v) (float ,y 1.0))
       ,v)))


;;; component access

(defmacro vec2-x (v) `(aref ,v 0))
(defmacro vec2-y (v) `(aref ,v 1))
(defsetf vec2-x (v) (val) `(setf (aref ,v 0) (float ,val 1.0)))
(defsetf vec2-y (v) (val) `(setf (aref ,v 1) (float ,val 1.0)))


;;; add/subtract, incf/decf

(declaim (inline vec2-add vec2-sub vec2-incf vec2-decf))
(declaim (ftype (function (vec2 vec2) vec2)
                vec2-add vec2-sub vec2-incf vec2-decf))

(defun vec2-add (u v)
  (vec2 (+ (vec2-x u) (vec2-x v)) (+ (vec2-y u) (vec2-y v))))
(defun vec2-sub (u v)
  (vec2 (- (vec2-x u) (vec2-x v)) (- (vec2-y u) (vec2-y v))))
(defun vec2-incf (v inc)
  (incf (vec2-x v) (vec2-x inc)) (incf (vec2-y v) (vec2-y inc)) v)
(defun vec2-decf (v dec)
  (decf (vec2-x v) (vec2-x dec)) (decf (vec2-y v) (vec2-y dec)) v)



;;;
;;; mat3
;;;

;;; type

(deftype mat3 () '(simple-array single-float (3 3)))


;;; constructor

(defmacro mat3 (m00 m01 m02
                m10 m11 m12
                m20 m21 m22)
  (let ((m (gensym)))
    `(let ((,m (make-array '(3 3) :element-type 'single-float)))
       (psetf
        (mat3-ref ,m 0 0) ,m00 (mat3-ref ,m 1 0) ,m10 (mat3-ref ,m 2 0) ,m20
        (mat3-ref ,m 0 1) ,m01 (mat3-ref ,m 1 1) ,m11 (mat3-ref ,m 2 1) ,m21
        (mat3-ref ,m 0 2) ,m02 (mat3-ref ,m 1 2) ,m12 (mat3-ref ,m 2 2) ,m22)
       ,m)))


;;; component access

(defmacro mat3-ref (m i j) `(aref ,m ,i ,j))
(defsetf mat3-ref (m i j) (val) `(setf (aref ,m ,i ,j) (float ,val 1.0)))


;;; transforms

(declaim (inline transform->mat3)
         (ftype (function (vec2 number vec2) mat3) transform->mat3))
(defun transform->mat3 (scale rot trans)
  "Return a matrix representing the given scaling (a vec2), rotation (an angle)
and translation (a vec2) applied in that order."
  (let ((rot (float rot 1.0)))
    (mat3 (* (vec2-x scale) (cos rot)) (* (vec2-x scale) (sin rot)) 0
          (* (vec2-y scale) (- (sin rot))) (* (vec2-y scale) (cos rot)) 0
          (vec2-x trans) (vec2-y trans) 1)))


