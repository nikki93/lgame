(in-package #:lgame)

;;;
;;; types
;;;

(define-entity-system =sprite= ()
    (define-entity-cell sprite ()
      ((texcell :initform (vec2  0 64) :initarg :texcell :accessor texcell)
       (texsize :initform (vec2 32 32) :initarg :texsize :accessor texsize)))
    ((program :initform (load-program "sprite.vert"
                                      "sprite.geom"
                                      "sprite.frag"))
     (vao :initform (gl:gen-vertex-array))
     (vbo :initform (car (gl:gen-buffers 1)))
     (vbo-capacity :initform 0)
     (vbo-map :initform nil)
     (atlas-tex :initform (image->texture "atlas.png"))))

(cffi:defcstruct gl-sprite
  (wmat0 :float :count 3)               ; need to split up matrix into columns
  (wmat1 :float :count 3)               ; to bind as shader vertex attribute
  (wmat2 :float :count 3)
  (texcell :float :count 2)
  (texsize :float :count 2))



;;;
;;; utils
;;;

(defun bind-gl-stuff (system)
  (with-slots (program vao vbo atlas-tex) system
    (gl:use-program program)
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:bind-texture :texture-2d atlas-tex)))

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
  (bind-gl-stuff system)
  (with-slots (program) system
   (bind-vertex-attribs program '(:struct gl-sprite))
   (gl:uniformi (gl:get-uniform-location program "tex0") 0)
   (gl:uniformf (gl:get-uniform-location program "atlas_size")
                (gl:get-tex-level-parameter :texture-2d 0 :texture-width)
                (gl:get-tex-level-parameter :texture-2d 0 :texture-height))))

(defmethod deinitialize-instance :before ((system =sprite=))
  (with-slots (program vao vbo atlas-tex) system
    (gl:delete-textures (list atlas-tex))
    (gl:delete-buffers (list vbo))
    (gl:delete-vertex-arrays (list vao))
    (when (gl:is-program program) (gl:delete-program program))))



;;;
;;; events
;;;

(defun update-vbo-data (system)
  (with-slots (table vbo-map) system
    (resize-vbo system (hash-table-count table))
    (let ((i 0))
      (do-hash (entity cell table)
        (with-cstruct-slots (((:pointer wmat0) (:pointer wmat1) (:pointer wmat2)
                              (:pointer texcell) (:pointer texsize))
                             (cffi:mem-aptr vbo-map '(:struct gl-sprite) i)
                             (:struct gl-sprite))
          (let ((wmat (world-matrix (=transform= entity))))
            (setf (cffi:mem-aref wmat0 :float 0) (mat3-ref wmat 0 0))
            (setf (cffi:mem-aref wmat0 :float 1) (mat3-ref wmat 0 1))
            (setf (cffi:mem-aref wmat0 :float 2) (mat3-ref wmat 0 2))
            (setf (cffi:mem-aref wmat1 :float 0) (mat3-ref wmat 1 0))
            (setf (cffi:mem-aref wmat1 :float 1) (mat3-ref wmat 1 1))
            (setf (cffi:mem-aref wmat1 :float 2) (mat3-ref wmat 1 2))
            (setf (cffi:mem-aref wmat2 :float 0) (mat3-ref wmat 2 0))
            (setf (cffi:mem-aref wmat2 :float 1) (mat3-ref wmat 2 1))
            (setf (cffi:mem-aref wmat2 :float 2) (mat3-ref wmat 2 2)))
          (with-slots ((spr-texcell texcell) (spr-texsize texsize)) cell
            (setf (cffi:mem-aref texcell :float 0) (vec2-x spr-texcell))
            (setf (cffi:mem-aref texcell :float 1) (vec2-y spr-texcell))
            (setf (cffi:mem-aref texsize :float 0) (vec2-x spr-texsize))
            (setf (cffi:mem-aref texsize :float 1) (vec2-y spr-texsize))))
        (incf i)))))

(defmethod draw ((system =sprite=))
  (update-vbo-data system)
  (bind-gl-stuff system)
  (gl:draw-arrays :points 0 (hash-table-count (table system))))


