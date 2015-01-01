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
     (vbo-capacity :initform 0)
     (vbo-map :initform nil)))

(cffi:defcstruct gl-sprite
  (pos :float :count 2)
  (cell :float :count 2)
  (size :float :count 2))



;;;
;;; utils
;;;

(defun bind-gl-stuff (system)
  (with-slots (program vao vbo) system
    (gl:use-program program)
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)))

(defun next-pow-2 (n)
  (do ((p 1 (ash p 1)))
      ((<= n p) p)))

(defun resize-vbo (system new-size)
  (with-slots (vbo-map vbo-capacity) system
    (unless (<= (/ vbo-capacity 4) new-size vbo-capacity)
      (setf vbo-capacity (max 16 (next-pow-2 new-size)))
      (bind-gl-stuff system)
      (when vbo-map (gl:unmap-buffer :array-buffer))
      (%gl:buffer-data :array-buffer
                       (* vbo-capacity (cffi:foreign-type-size
                                        '(:struct gl-sprite)))
                       (cffi:null-pointer)
                       :stream-draw)
      (setf vbo-map (gl:map-buffer :array-buffer :write-only)))))



;;;
;;; init/deinit
;;;

(defmethod initialize-instance :after ((system =sprite=) &key)
  (with-slots (program vbo-map) system
    (bind-gl-stuff system)
    (bind-vertex-attribs program '(:struct gl-sprite)))) 

(defmethod deinitialize-instance :before ((system =sprite=))
  (with-slots (program vao vbo) system
    (gl:delete-buffers (list vbo))
    (gl:delete-vertex-arrays (list vao))
    (when (gl:is-program program) (gl:delete-program program))))



;;;
;;; events
;;;

(defmethod update ((system =sprite=) dt)
  (with-slots (table vbo-map) system
    (resize-vbo system (hash-table-count table))
    (let ((i 0))
      (do-hash (entity cell table)
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
  (bind-gl-stuff system)
  (gl:draw-arrays :points 0 (hash-table-count (table system))))


