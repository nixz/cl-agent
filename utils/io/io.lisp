;;;; --------------------------------------------------------------------------
;;;; @file   io.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Sat Apr 24 23:23:17 2010
;;;;
;;;; @brief 
;;;; --------------------------------------------------------------------------


(require 'clx)
(defpackage #:org.io.window)
(in-package #:org.io.window)

(defclass  screen ()
  ((display :initarg :display 
         :initform (error ":display must be specified")
         :reader display 
         :allocation :instance
         :documentation "The default display for the window"))
   ;; (screen :initarg :screen 
   ;;       :initform (error ":screen must be specified")
   ;;       :reader screen 
   ;;       :allocation :instance
   ;;       :documentation "The default screen for the window")
   ;; (root :initarg :root 
   ;;       :initform (error ":root must be specified")
   ;;       :reader root 
   ;;       :allocation :instance
   ;;       :documentation "the screen root")
   ;; (width :initarg :width 
   ;;       :initform (error ":width must be specified")
   ;;       :accessor width 
   ;;       :allocation :instance
   ;;       :documentation "The width of the window")
   ;; (height :initarg :height 
   ;;       :initform (error ":height must be specified")
   ;;       :accessor height 
   ;;       :allocation :instance
   ;;       :documentation "The height of the window")
   ;; (win :initarg :window 
   ;;       :initform (error ":window must be specified")
   ;;       :reader window 
   ;;       :allocation :instance
   ;;       :documentation "The created window")
   ;; (visual :initarg :visual 
   ;;       :initform (error ":visual must be specified")
   ;;       :reader visual 
   ;;       :allocation :instance
   ;;       :documentation "The visual of the screen?")
   ;; (colormap :initarg :colormap 
   ;;       :initform (error ":colormap must be specified")
   ;;       :reader colormap 
   ;;       :allocation :instance
   ;;       :documentation "Create the colormap based on visual")
   ;; (gc :initarg :gc 
   ;;       :initform (error ":gc must be specified")
   ;;       :reader gc 
   ;;       :allocation :instance
   ;;       :documentation "The gl context")
   ;; (ctx :initarg :ctx 
   ;;       :initform (error ":ctx must be specified")
   ;;       :reader ctx
   ;;       :allocation :instance
   ;;       :documentation "ctx?"))
  (:documentation "The glx window class"))

(defclass  window ()
  ((test :initarg :test 
         :initform (error ":test must be specified")
         :accessor test 
         :allocation :instance
         :documentation "test")
   
)
  (:documentation "something"))


(defun make-window()
  (let* ((display (open-default-display))
         (screen (first (display-roots display)))
         (root (screen-root screen))
         ctx
         (width (+ (xlib:screen-width screen) 2))
         (height (+ (xlib:screen-height screen) 2)))
    ;; Inform the server about us.
    (glx::client-info display)
    (let* ((visual (glx:choose-visual screen '(:glx-rgba
                                               (:glx-red-size 1)
                                               (:glx-green-size 1)
                                               (:glx-blue-size 1)
                                               :glx-double-buffer)))
           (colormap (create-colormap (glx:visual-id visual) root))
           (window (create-window :parent root
                                  :x -2 :y -2
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
                         :x -2 :y -2
                         :width width :height height
                         :min-width width :min-height height
                         :initial-state :normal)
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

(defmethod close ((w window))
  (progn
    (glx:destroy-context (ctx w))
    (xlib:unmap-window (window w))
    (xlib:free-gcontext (gc w))
    (xlib:destroy-window (window w))
    (xlib:close-display (display w))))



;(funcall function display window)       
  




