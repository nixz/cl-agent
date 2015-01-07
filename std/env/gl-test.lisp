;;;; --------------------------------------------------------------------------
;;;; @file   gl-test.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Thu Apr 22 03:11:23 2010
;;;;
;;;; @brief 
;;;; --------------------------------------------------------------------------

(in-package #:std.env)

;;(defun test (function)
(defun window (function &key (host "localhost") (display 1) (width 200) (height 200))
  (let* ((display (open-default-display));(open-display host :display display))
;;         (screen (display-default-screen display))
         (screen (first (display-roots display)))
         (root (screen-root screen))
         ctx
         (width (+ (xlib:screen-width screen) 2))
         (height (+ (xlib:screen-height screen) 2)))
    (unwind-protect
         (progn
           ;;; Inform the server about us.
           (glx::client-info display)
           (let* ((visual (glx:choose-visual screen '(:glx-rgba
                                                      (:glx-red-size 1)
                                                      (:glx-green-size 1)
                                                      (:glx-blue-size 1)
                                                      :glx-double-buffer)))
                  (colormap (create-colormap (glx:visual-id visual) root))
                  (window (create-window :parent root
                                         :x -2 :y -2 :width width :height height
                                         :class :input-output
                                         :background (screen-black-pixel screen)
                                         :border (screen-black-pixel screen)
                                         :visual (glx:visual-id visual)
                                         :depth 24
                                         :colormap colormap
                                         :override-redirect :on
                                         ;;:event-mask '(:structure-notify :exposure)
                                         :event-mask nil))
                  (gc (create-gcontext :foreground (screen-white-pixel screen)
                                       :background (screen-black-pixel screen)
                                       :drawable window
                                       :font (open-font display "fixed"))))
             (set-wm-properties window
                                :name "glx-test"
                                :resource-class "glx-test"
                                :command (list "glx-test")
                                :x -2 :y -2 :width width :height height
                                :min-width width :min-height height
                                :initial-state :normal)
             (setf ctx (glx:create-context screen (glx:visual-id visual)))
             (map-window window)
             (glx:make-current window ctx)
             (funcall function display window)
             (unmap-window window)
             (free-gcontext gc)))      
      (when ctx (glx:destroy-context ctx))
      (close-display display))))

;; (defun window(function &key (host "localhost") (display 1) (width 200) (height 200))
;;   (let* ((display (open-default-display))
;;          (screen (display-default-screen display))
;;          (root (screen-root screen))
;;          ctx)
;;     (unwind-protect
;;          (progn
;;            ;;; Inform the server about us.
;;            (glx::client-info display)
;;            (let* ((visual (glx:choose-visual screen '(:glx-rgba
;;                                                       (:glx-red-size 1)
;;                                                       (:glx-green-size 1)
;;                                                       (:glx-blue-size 1)
;;                                                       :glx-double-buffer)))
;;                   (colormap (create-colormap (glx:visual-id visual) root))
;;                   (window (create-window :parent root
;;                                          :x 0 :y 0 :width width :height height
;;                                          :class :input-output
;;                                          :background (screen-black-pixel screen)
;;                                          :border (screen-black-pixel screen)
;;                                          :visual (glx:visual-id visual)
;;                                          :depth 24
;;                                          :colormap colormap
;;                                          :event-mask '(:structure-notify :exposure)))
;;                   (gc (create-gcontext :foreground (screen-white-pixel screen)
;;                                        :background (screen-black-pixel screen)
;;                                        :drawable window
;;                                        :font (open-font display "fixed"))))
;;              (set-wm-properties window
;;                                 :name "glx-test"
;;                                 :resource-class "glx-test"
;;                                 :command (list "glx-test")
;;                                 :x 0 :y 0 :width width :height height
;;                                 :min-width width :min-height height
;;                                 :initial-state :normal)
;;              (setf ctx (glx:create-context screen (glx:visual-id visual)))
;;              (map-window window)
;;              (glx:make-current window ctx)
;;              (funcall function display window)
;;              (unmap-window window)
;;              (free-gcontext gc)))  
;;       (when ctx (glx:destroy-context ctx))
;;       (close-display display))))


;;; Tests


(defun no-floats (display window)
  (declare (ignore display window))
  (xgl:color-3s #x7fff #x7fff 0)
  (xgl:begin xgl:+polygon+)
  (xgl:vertex-2s 0 0)
  (xgl:vertex-2s 1 0)
  (xgl:vertex-2s 1 1)
  (xgl:vertex-2s 0 1)
  (xgl:end)
  (glx:swap-buffers)
  (sleep 5))


(defun anim (display window)
  (declare (ignore display window))
  (xgl:ortho 0.0d0 1.0d0 0.0d0 1.0d0 -1.0d0 1.0d0)
  (xgl:clear-color 0.0s0 0.0s0 0.0s0 0.0s0)
  (xgl:line-width 2.0s0)
  (loop
     repeat 361
     for angle upfrom 0.0s0 by 1.0s0
     do (progn
          (xgl:clear xgl:+color-buffer-bit+)
          (xgl:push-matrix)
          (xgl:translate-f 0.5s0 0.5s0 0.0s0)
          (xgl:rotate-f angle 0.0s0 0.0s0 1.0s0)
          (xgl:translate-f -0.5s0 -0.5s0 0.0s0)
          (xgl:begin xgl:+polygon+ #-(and) xgl:+line-loop+)
          (xgl:color-3ub 255 0 0)
          (xgl:vertex-2f 0.25s0 0.25s0)
          (xgl:color-3ub 0 255 0)
          (xgl:vertex-2f 0.75s0 0.25s0)
          (xgl:color-3ub 0 0 255)
          (xgl:vertex-2f 0.75s0 0.75s0)
          (xgl:color-3ub 255 255 255)
          (xgl:vertex-2f 0.25s0 0.75s0)
          (xgl:end)
          (xgl:pop-matrix)
          (glx:swap-buffers)
          (sleep 0.02)))
  (sleep 3))


(defun anim/list (display window)
  (declare (ignore display window))
  (xgl:ortho 0.0d0 1.0d0 0.0d0 1.0d0 -1.0d0 1.0d0)
  (xgl:clear-color 0.0s0 0.0s0 0.0s0 0.0s0)
  (let ((list (xgl:gen-lists 1)))
    (xgl:new-list list xgl:+compile+)
    (xgl:begin xgl:+polygon+)
    (xgl:color-3ub 255 0 0)
    (xgl:vertex-2f 0.25s0 0.25s0)
    (xgl:color-3ub 0 255 0)
    (xgl:vertex-2f 0.75s0 0.25s0)
    (xgl:color-3ub 0 0 255)
    (xgl:vertex-2f 0.75s0 0.75s0)
    (xgl:color-3ub 255 255 255)
    (xgl:vertex-2f 0.25s0 0.75s0)
    (xgl:end)
    (glx:render)
    (xgl:end-list)

    (loop
       repeat 361
       for angle upfrom 0.0s0 by 1.0s0
       do (progn
            (xgl:clear xgl:+color-buffer-bit+)
            (xgl:push-matrix)
            (xgl:rotate-f angle 0.0s0 0.0s0 1.0s0)
            (xgl:call-list list)
            (xgl:pop-matrix)
            (glx:swap-buffers)
            (sleep 0.02))))
  
  (sleep 3))


;;; glxgears

(defconstant +pi+ (coerce pi 'single-float))
(declaim (type single-float +pi+))


(defun gear (inner-radius outer-radius width teeth tooth-depth)
  (let ((r0 inner-radius)
        (r1 (/ (- outer-radius tooth-depth) 2.0s0))
        (r2 (/ (+ outer-radius tooth-depth) 2.0s0))
        (da (/ (* 2.0s0 +pi+) teeth 4.0s0)))
    (xgl:shade-model xgl:+flat+)
    (xgl:normal-3f 0.0s0 0.0s0 1.0s0)

    ;; Front face.
    (xgl:begin xgl:+quad-strip+)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0 +pi+) teeth)))
        (declare (type single-float angle))
        (xgl:vertex-3f (* r0 (cos angle))
                      (* r0 (sin angle))
                      (* width 0.5s0))
        (xgl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (when (< i teeth)
          (xgl:vertex-3f (* r0 (cos angle))
                        (* r0 (sin angle))
                        (* width 0.5s0))
          (xgl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width 0.5s0)))))
    (xgl:end)


    ;; Draw front sides of teeth.
    (xgl:begin xgl:+quads+)
    (setf da (/ (* 2.0s0 +pi+) teeth 4.0s0))
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (xgl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (xgl:vertex-3f (* r2 (cos (+ angle da)))
                      (* r2 (sin (+ angle da)))
                      (* width 0.5s0))
        (xgl:vertex-3f (* r2 (cos (+ angle (* 2 da))))
                      (* r2 (sin (+ angle (* 2 da))))
                      (* width 0.5s0))
        (xgl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                      (* r1 (sin (+ angle (* 3 da))))
                      (* width 0.5s0))))
    (xgl:end)

    (xgl:normal-3f 0.0s0 0.0s0 -1.0s0)
                 
    ;; Draw back face.
    (xgl:begin xgl:+quad-strip+)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (xgl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))
        (xgl:vertex-3f (* r0 (cos angle))
                      (* r0 (sin angle))
                      (* width -0.5s0))
        (when (< i teeth)
          (xgl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width -0.5s0))
          (xgl:vertex-3f (* r0 (cos angle))
                        (* r0 (sin angle))
                        (* width 0.5s0)))))
    (xgl:end)

    ;; Draw back sides of teeth.
    (xgl:begin xgl:+quads+)
    (setf da (/ (* 2.0s0 +pi+) teeth 4.0s0))
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (xgl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                      (* r1 (sin (+ angle (* 3 da))))
                      (* width -0.5s0))
        (xgl:vertex-3f (* r2 (cos (+ angle (* 2 da))))
                      (* r2 (sin (+ angle (* 2 da))))
                      (* width -0.5s0))
        (xgl:vertex-3f (* r2 (cos (+ angle da)))
                      (* r2 (sin (+ angle da)))
                      (* width -0.5s0))
        (xgl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))))
    (xgl:end)

    ;; Draw outward faces of teeth.
    (xgl:begin xgl:+quad-strip+)
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (xgl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (xgl:vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))
        (let* ((u (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
               (v (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
               (len (sqrt (+ (* u u) (* v v)))))
          (setf u (/ u len)
                v (/ v len))
          (xgl:normal-3f v u 0.0s0)
          (xgl:vertex-3f (* r2 (cos (+ angle da)))
                        (* r2 (sin (+ angle da)))
                        (* width 0.5s0))
          (xgl:vertex-3f (* r2 (cos (+ angle da)))
                        (* r2 (sin (+ angle da)))
                        (* width -0.5s0))
          (xgl:normal-3f (cos angle) (sin angle) 0.0s0)
          (xgl:vertex-3f (* r2 (cos (+ angle (* 2 da))))
                        (* r2 (sin (+ angle (* 2 da))))
                        (* width 0.5s0))
          (xgl:vertex-3f (* r2 (cos (+ angle (* 2 da))))
                        (* r2 (sin (+ angle (* 2 da))))
                        (* width -0.5s0))
          (setf u (- (* r1 (cos (+ angle (* 3 da)))) (* r2 (cos (+ angle (* 2 da)))))
                v (- (* r1 (sin (+ angle (* 3 da)))) (* r2 (sin (+ angle (* 2 da))))))
          (xgl:normal-3f v (- u) 0.0s0)
          (xgl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width 0.5s0))
          (xgl:vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width -0.5s0))
          (xgl:normal-3f (cos angle) (sin angle) 0.0s0))))

    (xgl:vertex-3f (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5s0))
    (xgl:vertex-3f (* r1 (cos 0)) (* r1 (sin 0)) (* width -0.5s0))

    (xgl:end)

    (xgl:shade-model xgl:+smooth+)
                 
    ;; Draw inside radius cylinder.
    (xgl:begin xgl:+quad-strip+)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (xgl:normal-3f (- (cos angle)) (- (sin angle)) 0.0s0)
        (xgl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5s0))
        (xgl:vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5s0))))
    (xgl:end)))


(defun draw (gear-1 gear-2 gear-3 view-rotx view-roty view-rotz angle)
  (xgl:clear (logior xgl:+color-buffer-bit+ xgl:+depth-buffer-bit+))

  (xgl:push-matrix)
  (xgl:rotate-f view-rotx 1.0s0 0.0s0 0.0s0)
  (xgl:rotate-f view-roty 0.0s0 1.0s0 0.0s0)
  (xgl:rotate-f view-rotz 0.0s0 0.0s0 1.0s0)

  (xgl:push-matrix)
  (xgl:translate-f -3.0s0 -2.0s0 0.0s0)
  (xgl:rotate-f angle 0.0s0 0.0s0 1.0s0)
  (xgl:call-list gear-1)
  (xgl:pop-matrix)

  (xgl:push-matrix)
  (xgl:translate-f 3.1s0 -2.0s0 0.0s0)
  (xgl:rotate-f (- (* angle -2.0s0) 9.0s0) 0.0s0 0.0s0 1.0s0)
  (xgl:call-list gear-2)
  (xgl:pop-matrix)

  (xgl:push-matrix)
  (xgl:translate-f -3.1s0 4.2s0 0.0s0)
  (xgl:rotate-f (- (* angle -2.s0) 25.0s0) 0.0s0 0.0s0 1.0s0)
  (xgl:call-list gear-3)
  (xgl:pop-matrix)

  (xgl:pop-matrix))


(defun reshape (width height)
  (xgl:viewport 0 0 width height)
  (let ((h (coerce (/ height width) 'double-float)))
    (xgl:matrix-mode xgl:+projection+)
    (xgl:load-identity)
    (xgl:frustum -1.0d0 1.0d0 (- h) h 5.0d0 60.0d0))

  (xgl:matrix-mode xgl:+modelview+)
  (xgl:load-identity)
  (xgl:translate-f 0.0s0 0.0s0 -40.0s0))

             
(defun init ()
  (let (gear-1 gear-2 gear-3)
    ;;(xgl:light-fv xgl:+light0+ xgl:+position+ '(5.0s0 5.0s0 10.0s0 0.0s0))
    ;;(xgl:enable xgl:+cull-face+)
    ;;(xgl:enable xgl:+lighting+)
    ;;(xgl:enable xgl:+light0+)
    ;;(xgl:enable xgl:+depth-test+)

    ;; Make the gears.
    (setf gear-1 (xgl:gen-lists 1))
    (xgl:new-list gear-1 xgl:+compile+)
    (xgl:material-fv xgl:+front+ xgl:+ambient-and-diffuse+ '(0.8s0 0.1s0 0.0s0 1.0s0))
    (gear 1.0s0 4.0s0 1.0s0 20 0.7s0)
    (xgl:end-list)

    (setf gear-2 (xgl:gen-lists 1))
    (xgl:new-list gear-2 xgl:+compile+)
    (xgl:material-fv xgl:+front+ xgl:+ambient-and-diffuse+ '(0.0s0 0.8s0 0.2s0 1.0s0))
    (gear 0.5s0 2.0s0 2.0s0 10 0.7s0)
    (xgl:end-list)

    (setf gear-3 (xgl:gen-lists 1))
    (xgl:new-list gear-3 xgl:+compile+)
    (xgl:material-fv xgl:+front+ xgl:+ambient-and-diffuse+ '(0.2s0 0.2s0 1.0s0 1.0s0))
    (gear 1.3s0 2.0s0 0.5s0 10 0.7s0)
    (xgl:end-list)

    ;;(xgl:enable xgl:+normalize+)

    (values gear-1 gear-2 gear-3)))


(defun gears* (display window)
  (declare (ignore display window))

  (xgl:enable xgl:+cull-face+)
  (xgl:enable xgl:+lighting+)
  (xgl:enable xgl:+light0+)
  (xgl:enable xgl:+normalize+)
  (xgl:enable xgl:+depth-test+)

  (reshape 300 300)

  ;;(xgl:light-fv xgl:+light0+ xgl:+position+ #(5.0s0 5.0s0 10.0s0 0.0s0))

  (let (list)
    (declare (ignore list))
    #-(and)
    (progn
      (setf list (xgl:gen-lists 1))
      (xgl:new-list list xgl:+compile+)
      ;;(xgl:material-fv xgl:+front+ xgl:+ambient-and-diffuse+ '(0.8s0 0.1s0 0.0s0 1.0s0))
      (gear 1.0s0 4.0s0 1.0s0 20 0.7s0)
      (glx:render)
      (xgl:end-list))


    (loop
       ;;for angle from 0.0s0 below 361.0s0 by 1.0s0
       with angle single-float = 0.0s0
       with dt = 0.004s0
       repeat 2500
       do (progn

            (incf angle (* 70.0s0 dt))       ; 70 degrees per second
            (when (< 3600.0s0 angle)
              (decf angle 3600.0s0))

            (xgl:clear (logior xgl:+color-buffer-bit+ xgl:+depth-buffer-bit+))

            (xgl:push-matrix)
            (xgl:rotate-f 20.0s0 0.0s0 1.0s0 0.0s0)


            (xgl:push-matrix)
            (xgl:translate-f -3.0s0 -2.0s0 0.0s0)
            (xgl:rotate-f angle 0.0s0 0.0s0 1.0s0)
            (xgl:material-fv xgl:+front+ xgl:+ambient-and-diffuse+ '(0.8s0 0.1s0 0.0s0 1.0s0))
            (gear 1.0s0 4.0s0 1.0s0 20 0.7s0)
            (xgl:pop-matrix)

            
            (xgl:push-matrix)
            (xgl:translate-f 3.1s0 -2.0s0 0.0s0)
            (xgl:rotate-f (- (* angle -2.0s0) 9.0s0) 0.0s0 0.0s0 1.0s0)
            (xgl:material-fv xgl:+front+ xgl:+ambient-and-diffuse+ '(0.0s0 0.8s0 0.2s0 1.0s0))
            (gear 0.5s0 2.0s0 2.0s0 10 0.7s0)
            (xgl:pop-matrix)


            (xgl:push-matrix)
            (xgl:translate-f -3.1s0 4.2s0 0.0s0)
            (xgl:rotate-f (- (* angle -2.s0) 25.0s0) 0.0s0 0.0s0 1.0s0)
            (xgl:material-fv xgl:+front+ xgl:+ambient-and-diffuse+ '(0.2s0 0.2s0 1.0s0 1.0s0))
            (gear 1.3s0 2.0s0 0.5s0 10 0.7s0)
            (xgl:pop-matrix)


            (xgl:pop-matrix)

            (glx:swap-buffers)
            ;;(sleep 0.025)
            )))
  

  ;;(sleep 3)
  )


(defun gears (display window)
  (declare (ignore window))
  (let ((view-rotx 20.0s0)
        (view-roty 30.0s0)
        (view-rotz 0.0s0)
        (angle 0.0s0)
        (frames 0)
        (dt 0.004s0)                ; *** This is dynamically adjusted
        ;;(t-rot-0 -1.0d0)
        ;;(t-rate-0 -1.d0)
        gear-1 gear-2 gear-3)

    (multiple-value-setq (gear-1 gear-2 gear-3)
      (init))

    (loop
       (event-case (display :timeout 0.01 :force-output-p t)
         (configure-notify (width height)
                           (reshape width height)
                           t)
         (key-press (code)
                    (format t "Key pressed: ~S~%" code)
                    (return-from gears t)))

       (incf angle (* 70.0s0 dt))       ; 70 degrees per second
       (when (< 3600.0s0 angle)
         (decf angle 3600.0s0))

       (draw gear-1 gear-2 gear-3 view-rotx view-roty view-rotz angle)
       (glx:swap-buffers)
       
       (incf frames)

       ;; FPS calculation goes here
       )))
