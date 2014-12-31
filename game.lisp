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
  (continuable
    (let ((connection (or swank::*emacs-connection*
                          (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t)))))

(defmacro with-main (&body body)
  `(sdl2:make-this-thread-main
    (lambda ()
      (sb-int:with-float-traps-masked (:invalid)
        ,@body))))



;;;
;;; main procedures
;;;

(defparameter *window* nil)

(defun init-game ()
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

(defun run-game ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (*window* :flags '(:shown :opengl))
      (sdl2:set-window-position *window* 634 53)
      (sdl2:with-gl-context (gl-context *window*)
        (init-game)
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)
          (:idle ()
                 (update-swank)
                 (continuable
                   (update-game)
                   (draw-game))))
        (deinit-game)))))


