(in-package #:lgame)

;;;
;;; types
;;;

(define-entity-system =sprite= ()
    (define-entity-cell sprite ()
      ((color :initform '(1 0 0 1) :initarg :color :accessor color)))
    ((program :initform (load-program "sprite.vert"
                                      "sprite.geom"
                                      "sprite.frag"))
     (vao :initform (gl:gen-vertex-array))
     (vbo :initform (car (gl:gen-buffers 1)))
     (vbo-map)))

(cffi:defcstruct gl-sprite
  (pos :float :count 2)
  (cell :float :count 2)
  (size :float :count 2))



;;;
;;; init/deinit
;;;

(defparameter *nsprites* 50000)

(defmethod initialize-instance :after ((system =sprite=) &key)
  ;; init GL stuff
  (with-slots (program vao vbo vbo-map) system
    (gl:use-program program)
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (bind-vertex-attribs program '(:struct gl-sprite))
    (%gl:buffer-data :array-buffer
                     (* *nsprites* (cffi:foreign-type-size
                                    '(:struct gl-sprite)))
                     (cffi:null-pointer)
                     :stream-draw)
    (setf vbo-map (gl:map-buffer :array-buffer :write-only)))) 

(defmethod deinitialize-instance :before ((system =sprite=))
  ;; deinit GL stuff
  (with-slots (program vao vbo vbo-map) system
    (gl:delete-buffers (list vbo))
    (gl:delete-vertex-arrays (list vao))
    (gl:delete-program program)))



;;;
;;; events
;;;

(defmethod update ((system =sprite=) dt)
  ;; update VBO data
  (with-slots (vbo-map) system
    (let ((i 0))
      (do-hash (entity cell (table system))
        (with-cstruct-slots (((:pointer pos) (:pointer cell) (:pointer size))
                             (cffi:mem-aptr vbo-map '(:struct gl-sprite) i)
                             (:struct gl-sprite))
          (with-slots ((transform-pos pos) rot scale) (cell =transform= entity)
            (setf (cffi:mem-aref pos :float 0)
                  (coerce (vec2-x transform-pos) 'float))
            (setf (cffi:mem-aref pos :float 1)
                  (coerce (vec2-y transform-pos) 'float))))
        (incf i)))))

(defmethod draw ((system =sprite=))
  (with-slots (program vao vbo) system
    (gl:use-program program)
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:draw-arrays :points 0 (hash-table-count (table system)))))


