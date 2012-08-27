(require 'org.io.joystick)
(use-package 'org.io.joystick)

(defparameter *display* (xlib:open-default-display))
(defparameter *screen* (first (xlib:display-roots *display*)))
(defparameter *win* (xlib:create-window :parent (xlib:screen-root *screen*)
                                        :width 200 :x 10 :y 10
                                        :height 150))
;;(defvar *black* (xlib:screen-black-pixel *screen*))
(defparameter *black* (xlib:create-gcontext
                 :drawable (xlib:screen-root *screen*)
                 :foreground (xlib:alloc-color
                              (xlib:window-colormap (xlib:screen-root *screen*))
                              (symbol-name 'BLACK))
                 :background (xlib:screen-black-pixel *screen*)))

;;(defvar *white* (xlib:screen-white-pixel *screen*))
(defparameter *white* (xlib:create-gcontext
                 :drawable (xlib:screen-root *screen*)
                 :foreground (xlib:alloc-color
                              (xlib:window-colormap (xlib:screen-root *screen*))
                              (symbol-name 'WHITE))
                 :background (xlib:screen-black-pixel *screen*)))

(defparameter *js* (org.io.joystick:make-stick "/dev/input/js0"))

(defun update ()
  (update-stick *js*)
  (xlib:draw-rectangle *win* *white* 0 0 200 150 t)
  (xlib:display-force-output *display*)
  (xlib:draw-rectangle *win* *black* 10 10 100 100)
  (let ((x-offset (round (+ 60 (* 50 (aref (axes *js*) 0)))))
	(y-offset (round (+ 60 (* 50 (aref (axes *js*) 1)))))
	(twist (+ 60 (round (* 50 (aref (axes *js*) 3)))))
	(throttle (+ 60 (round (* 50 (aref (axes *js*) 2))))))
    (xlib:draw-line *win* *black* x-offset (- y-offset 5) x-offset (+ y-offset 5))
    (xlib:draw-line *win* *black* (- x-offset 5) y-offset (+ x-offset 5) y-offset)
    (xlib:draw-line *win* *black* twist 120 twist 130)
    (xlib:draw-line *win* *black* 120 throttle 130 throttle)
    (loop for n from 0
	  for bitt across (buttons *js*)
	  do (multiple-value-bind (y x)
		 ( truncate n 4)
	       (let ((x (+ 135 (* 10 x)))
		     (y (+ 10 (* 10 y))))
		 (xlib:draw-arc *win* *black* x y 6 6 0 (* 2 pi) (not (zerop bitt))))))
    )
  (xlib:display-force-output *display*))

(defun start()
  (progn
    (xlib:map-window *win*)
    ;;(continous-poll *js*)
    (loop for n below 1000 do
         (progn
           (update)
           (sleep 0.1)))
    (xlib:destroy-window *win*)))


