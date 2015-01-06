(in-package #:lgame)


;;;
;;; shader utilities
;;;

(defun load-shader (pathspec)
  "Load a shader given a path. Shader type is guessed from filename."
  (let* ((path (resolve-filename pathspec))
         (ext (subseq path (- (length path) 4)))
         (type (cond ((equal ext "vert") :vertex-shader)
                     ((equal ext "frag") :fragment-shader)
                     ((equal ext "geom") :geometry-shader))))
    (with-open-file (stream path)
      (let ((string (make-string (file-length stream)))
            (shader (gl:create-shader type)))
        (read-sequence string stream)
        (gl:shader-source shader string)
        (gl:compile-shader shader)
        (unless (gl:get-shader shader :compile-status)
          (error (format nil "Failed to compile shader '~a':~%~a"
                         path (gl:get-shader-info-log shader))))
        shader))))

(defun load-program (&rest args)
  "Link a program given shader paths. Shader types are guessed from filenames."
  (let ((shaders (mapcar #'load-shader args)))
    (unwind-protect
         (let ((program (gl:create-program)))
           (dolist (shader shaders)
             (gl:attach-shader program shader))
           (gl:link-program program)
           program)
      (dolist (shader shaders)
        (gl:delete-shader shader)))))

(defun bind-vertex-attribs (program struct &optional slot-vars)
  "Helper for binding VAO vertex attributes. `struct' is a cffi struct, usually
defined with cffi:defstruct and named as `(:struct struct-name). `slot-vars' is
either a list of slot symbols or a list of `(slot var)' cons cells where `slot'
is the symbol naming the slot in the struct and `var' is the string name of the
variable in the program."
  (dolist (spec (or slot-vars (cffi:foreign-slot-names struct)))
    (let* ((slot (if (listp spec) (car spec) spec))
           (var (if (listp spec) (cdr spec) (format nil "~(~a~)" spec)))
           (location (gl:get-attrib-location program var)))
      (when (< -1 location)
        (gl:vertex-attrib-pointer location
                                  (cffi:foreign-slot-count struct slot)
                                  (cffi:foreign-slot-type struct slot)
                                  nil
                                  (cffi:foreign-type-size struct)
                                  (cffi:foreign-slot-offset struct slot))
        (gl:enable-vertex-attrib-array location)))))



;;;
;;; texture utilities
;;;

(defun image->texture (pathspec &key texture-min-filter texture-mag-filter)
  "Read an image file into an OpenGL texture. Returns texture, width, height. "
  (let* ((path (resolve-filename pathspec))
         (image (png-read:read-png-file path))
         (data (png-read:image-data image))
         (width (array-dimension data 1))
         (height (array-dimension data 0))
         (image-data (make-array (* width height 4)
                                 :element-type '(unsigned-byte 8)
                                 :displaced-to data))
         (texture (car (gl:gen-textures 1))))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter
                      (or texture-min-filter :nearest))
    (gl:tex-parameter :texture-2d :texture-mag-filter
                      (or texture-mag-filter :nearest))
    (gl:tex-image-2d :texture-2d 0 :rgba
                     width height 0
                     :rgba :unsigned-byte image-data)
    (values texture width height)))


