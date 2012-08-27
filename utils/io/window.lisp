;;;; --------------------------------------------------------------------------
;;;; @file   window.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Thu Apr 22 03:14:30 2010
;;;;
;;;; @brief 
;;;; --------------------------------------------------------------------------

(in-package #:org.io)

(defclass  window ()
  ((display :initarg :display 
         :initform (error ":display must be specified")
         :reader display 
         :allocation :instance
         :documentation "The default display for the window")
   (screen :initarg :screen 
         :initform (error ":screen must be specified")
         :reader screen 
         :allocation :instance
         :documentation "The default screen for the window")
   (root :initarg :root 
         :initform (error ":root must be specified")
         :reader root 
         :allocation :instance
         :documentation "the screen root")
   (width :initarg :width 
         :initform (error ":width must be specified")
         :accessor width 
         :allocation :instance
         :documentation "The width of the window")
   (height :initarg :height 
         :initform (error ":height must be specified")
         :accessor height 
         :allocation :instance
         :documentation "The height of the window")
   (win :initarg :window 
         :initform (error ":window must be specified")
         :reader window 
         :allocation :instance
         :documentation "The created window")
   (visual :initarg :visual 
         :initform (error ":visual must be specified")
         :reader visual 
         :allocation :instance
         :documentation "The visual of the screen?")
   (colormap :initarg :colormap 
         :initform (error ":colormap must be specified")
         :reader colormap 
         :allocation :instance
         :documentation "Create the colormap based on visual")
   (gc :initarg :gc 
         :initform (error ":gc must be specified")
         :reader gc 
         :allocation :instance
         :documentation "The gl context")
   (ctx :initarg :ctx 
         :initform (error ":ctx must be specified")
         :reader ctx
         :allocation :instance
         :documentation "ctx?"))
  (:documentation "The glx window class"))

;; cursor code from
;; http://vintage-digital.com/hefner/hacks/busy-cursor-restart-process.lisp
(defparameter *my-bitmap*
  (make-array '(16 16) :element-type 'bit :initial-contents
	      '( #*1111111111100000
		 #*1111111111100000
		 #*0100000001000000
		 #*0100000001000000
		 #*0010000010000000
		 #*0011111110000000
		 #*0001000100000000
		 #*0000101000000000
		 #*0000101000000000
		 #*0001010100000000
		 #*0010010010000000
		 #*0010010010000000
		 #*0100101001000000
		 #*0101000101000000
		 #*0111111111000000
		 #*1111111111100000 )))

(defun my-cursor (display screen)
  (declare (optimize (debug 3)
		     (safety 3)))
  (let* ((image  (xlib:create-image  :width 16 :height 16 :depth 1 :data *my-bitmap*))
	 (source (xlib:create-pixmap :width 16 :height 16 :depth 1
				     :drawable (xlib:screen-root screen)))
	 (mask   (xlib:create-pixmap :width 16 :height 16 :depth 1
				     :drawable (xlib:screen-root screen)))	 
	 (gc-1 (xlib:create-gcontext :drawable source))
	 (gc-2 (xlib:create-gcontext :drawable mask :foreground 1 :background 0)))
    (unwind-protect
	(progn
	  (xlib:put-image source gc-1 image :x 0 :y 0 :width 16 :height 16 :bitmap-p T)
	  (xlib:put-image  mask  gc-2 image :x 0 :y 0 :width 16 :height 16 :bitmap-p T)	  
	  (xlib:create-cursor :source source
			      :mask mask
			      :x 0 :y 0
			      :foreground (xlib:make-color :blue 1.0 :green 1.0 :red 1.0)
			      :background (xlib:make-color :blue 0.0 :green 0.0 :red 0.0)))
     (xlib:free-gcontext gc-1)
     (xlib:free-gcontext gc-2)
     (xlib:free-pixmap source)
     (xlib:free-pixmap mask))))  

(defconstant water
  '((O  0.000 0.000 0.000)
    (H -0.900 0.000 0.000)
    (H  0.000 1.000 0.000)))

(defconstant ethanol
  '((C -0.426  -0.115  -0.147)
    (O -0.599   1.244  -0.481)
    (H -0.750  -0.738  -0.981)
    (H -1.022  -0.351   0.735)
    (H -1.642   1.434  -0.689)
    (C  1.047  -0.383   0.147)
    (H  1.370   0.240   0.981)
    (H  1.642  -0.147  -0.735)
    (H  1.180  -1.434   0.405)))



;; Variables/Constants

(defconstant spin-speed 5)
(defvar slices 40)    ; number of hori/verti slices on spheres
(defvar model-type 'solid)
(defvar curr-mol)
(defvar view-rotx 20)
(defvar view-roty 30)
(defvar view-rotz 0)
(defvar show-light-source nil)
(defvar light-theta 0)
(defvar light-phi 0)
(defvar light-dist 5)
(defconstant light-spin-speed .0872664625) ; 5 degrees in radians
(defvar light-r .8)
(defvar light-g .8)
(defvar light-b .8)
(defconstant light-colour-vel .1)
(defvar walk-mode nil)

(defun draw-light-source ()
  (gl:with-pushed-matrix
    (gl:material :front :ambient (vector light-r light-g light-b 1))

    (gl:translate (* light-dist (cos light-theta) (sin light-phi))
                  (* light-dist (sin light-theta) (sin light-phi))
                  (* light-dist (cos light-phi)))

    (glut:solid-sphere 0.1 20 20)))

(defun draw-atom (element x y z)
  (gl:with-pushed-matrix
    (gl:rotate view-rotx 1 0 0)
    (gl:rotate view-roty 0 1 0)
    (gl:rotate view-rotz 0 0 1)

    (gl:material :front :ambient-and-diffuse
      (case element
        ((H) #(0.8 0.8 0.8 1))
        ((O) #(0.8 0.1 0.0 1))
        ((C) #(0.2 0.2 0.2 1))))

    (gl:translate x y z)
    (funcall
      (if (eq model-type 'wire) #'glut:wire-sphere #'glut:solid-sphere)
      (case element
        ((H) 0.7)
        ((O) 1.0)
        ((C) 1.2))
      slices slices)))

(defun mol-view (width height display window)
  (declare (ignore display window))
  ;; (gl:viewport 0 0 width height)
  ;; (gl:matrix-mode :projection)
  ;; (gl:load-identity)
  ;; (let ((h (/ height width)))
  ;;   (gl:frustum -1 1 (- h) h 9 50))
  ;; (gl:matrix-mode :modelview)
  ;; (gl:load-identity)
  ;; (gl:translate 0 0 -40)
  ;; draw
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:light :light0 :position (vector (* light-dist (cos light-theta) (sin light-phi))
                                      (* light-dist (sin light-theta) (sin light-phi))
                                      (* light-dist (cos light-phi))
                                      0))
  (gl:light :light0 :diffuse (vector light-r light-g light-b 1))
  (gl:enable :cull-face :lighting :light0 :depth-test)
  (if show-light-source (draw-light-source))
  (dolist (a ethanol)
    (apply #'draw-atom a))
  (glx:swap-buffers))

;; (defun draw (life-time width height display window)
;;   (mol-view width height display window)
;;   (sleep life-time))

(defun draw (life-time width height display window)
  (format t "~&~a ~a ~a ~a ~a ~%" life-time width height display window)
  (glut:wire-sphere 1.0 10 10)
  (sleep life-time))

(defun make-window(&key
                   (x -2)(y -2)
                   (width 0 width-supplied-p)
                   (height 0 height-supplied-p))
  (let* ((display (open-default-display))
         (screen (first (display-roots display)))
         (root (screen-root screen))
         (cursor (my-cursor display screen))
         ctx)
    (unless width-supplied-p
      (setf width (+ (xlib:screen-width screen) 2)))
    (unless height-supplied-p
      (setf height (+ (xlib:screen-height screen) 2)))
    ;; Inform the server about us.
    (glx::client-info display)
    (let* ((visual (glx:choose-visual screen '(:glx-rgba
                                               (:glx-red-size 1)
                                               (:glx-green-size 1)
                                               (:glx-blue-size 1)
                                               :glx-double-buffer)))
           (colormap (create-colormap (glx:visual-id visual) root))
           (window (create-window :parent root
                                  :x x :y y
                                  :width width :height height
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
                         :x x :y y
                         :width width :height height
                         :min-width width :min-height height
                         :initial-state :normal)
      (setf (window-cursor window) cursor)
      (setf ctx (glx:create-context screen (glx:visual-id visual)))
      (map-window window)
      (glx:make-current window ctx)
      (make-instance 'window
                     :display display
                     :screen screen
                     :root root
                     :width width
                     :height height
                     :visual visual
                     :colormap colormap
                     :window window
                     :gc gc
                     :ctx ctx))))

(defmethod close ((w window) &rest rest)
  (progn
    (xlib:unmap-window (window w))
    (xlib:free-gcontext (gc w))
    (glx:destroy-context (ctx w))
    (xlib:destroy-window (window w))
    (xlib:close-display (display w))))



;(funcall function display window)       
  


