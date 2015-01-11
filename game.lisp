(in-package #:lgame)

;;;
;;; live coding utils
;;;

(defmacro continuable (&body body)
  "Allow continuing execution from errors."
  `(restart-case (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
  "Handle REPL requests."
  #+swank
  (continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))

(defmacro with-main (&body body)
  `(sdl2:make-this-thread-main
    (lambda ()
      #+sbcl (sb-int:with-float-traps-masked (:invalid) ,@body)
      #-sbcl ,@body)))



;;;
;;; interface
;;;

(defun quit-game ()
  (sdl2:push-event :quit))



;;;
;;; main procedures
;;;

(defvar *window* nil)

(defun init-game ()
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:disable :depth-test)
  (gl:clear-color 0.95 0.95 0.95 1)
  (restart-systems))

(defun deinit-game ()
  (stop-systems))

(let ((nframes 0) (last-fps 0))
  (defun update-fps (period dt)
    (incf nframes)
    (incf last-fps dt)
    (when (<= period last-fps)
      (format t "~&fps: ~a~%" (/ nframes last-fps))
      (finish-output nil)
      (setf nframes 0 last-fps 0))))

(let ((last-time))
  (defun update-game ()
    (let ((curr-time (get-internal-real-time)))
      (when last-time
        (let ((dt (coerce (/ (- curr-time last-time)
                             internal-time-units-per-second) 'float)))
          (do-systems (system)
            (update system dt))
          (update-fps 5 dt)))
      (setf last-time curr-time))))

(defun draw-game ()
  (gl:clear :color-buffer-bit)
  (do-systems (system)
    (draw system))
  (gl:flush)
  (sdl2:gl-swap-window *window*))

(defun run-game (&key (x :centered) (y :centered) (w 800) (h 600))
  (with-main
    (sdl2:with-init (:everything)
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 2)
      (sdl2:gl-set-attr :context-profile-mask
                        sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)
      (setf cl-opengl-bindings::*gl-get-proc-address*
            #'sdl2::gl-get-proc-address)
      (sdl2:with-window (*window* :title "lgame" :x x :y y :w w :h h
                                  :flags '(:shown :opengl))
        (sdl2:with-gl-context (gl-context *window*)
          (let ((*game-running* t))
            (init-game)
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:idle ()
                     (update-swank)
                     (continuable
                       (update-game)
                       (draw-game))))
            (deinit-game)))))))


