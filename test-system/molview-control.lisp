;; Doug Hoyte, March 2007
;; Assignment 2: Molecule Viewer
;; COSC414 with Dr. Alan Paeth, UBC Okanagan

;; To protect your rights, this program is distributed under
;; the terms and conditions of the GNU GPL, http://gnu.org

;; For power I decided to use Common Lisp instead of C.

;; This should be ANSI Common Lisp code that uses the OpenGL
;; libraries through the cl-opengl bindings.

;; Tested with SBCL 1.0.3 and the most recent cl-opengl bindings
;; available here: http://common-lisp.net/project/cl-opengl/

;; Usage:
;; $ sbcl --load molview.lisp
;; ...
;; * (molview ethanol)
;; or
;; * (molview (with-open-file ...))

;; Commands:
;; x/X - Rotate molecule around X axis
;; y/Y - Rotate molecule around Y axis
;; z/Z - Rotate molecule around Z axis
;; t/T - Rotate light source around theta
;; p/P - Rotate light source around phi
;; r/R - Increase/decrease red light component
;; g/G - Increase/decrease green light component
;; b/B - Increase/decrease blue light component
;; s/S - Increase/decrease number of sphere slices
;; w   - Toggle "random walk" frame-rate testing mode
;; m   - Toggle solid sphere vs wire sphere
;; l   - Toggle showing light source


;; We depend on cl-glut (which depends on cl-opengl and cffi)

(require :cl-glut)



;; Hard-coded molecules

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
(defparameter *MV* nil)
;; (defparameter *PR* nil)
(defparameter *MODEL* nil)
(defparameter *left* nil)
(defparameter *right* nil)
(defparameter *bottom* nil)
(defparameter *top* nil)
(defparameter *near* nil)
(defparameter *far* nil)

;; Main function

;;macro for fork
(defmacro fork (&rest code)
  `(list ,@(loop for var in code collect `(sb-thread:make-thread #'(lambda () ,var)))))

;;; macro for join
(defmacro join (thread-list)
  `(mapcar #'sb-thread:join-thread ,thread-list))

(defun node1() t)
(defun node2() t)
(defun node3() t)
(defun node4() t)

(defclass molicule (glut:window)
  ()
  (:default-initargs :title "Doug Hoyte - Molecule Viewer"
                     :width 500 :height 500 :game-mode t :mode '(:double :rgb :depth)))

(defmethod initialize-instance :after((win molicule) &key)
  (join
   (fork
    (defenv node1 (:host "dell7cn41f1" :port 9001) "")
    (defenv node2 (:host "dell7cn41f1" :port 9002) "")
    (defenv node3 (:host "dell7cn41f1" :port 9003) "")
    (defenv node4 (:host "dell7cn41f1" :port 9004) "")))
  (join
   (fork
    (node1 (init ethanol))
    (node2 (init ethanol))
    (node3 (init ethanol))
    (node4 (init ethanol))))
  (join
   (fork
    (node1 (draw))
    (node2 (draw))
    (node3 (draw))
    (node4 (draw)))))
  
  
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

(defmethod glut:display ((window molicule))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:light :light0 :position (vector (* light-dist (cos light-theta) (sin light-phi))
                                      (* light-dist (sin light-theta) (sin light-phi))
                                      (* light-dist (cos light-phi))
                                      0))
  (gl:light :light0 :diffuse (vector light-r light-g light-b 1))
  (gl:enable :cull-face :lighting :light0 :depth-test)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum *left* *right* *bottom* *top* *near* *far*)
  (gl:matrix-mode :modelview)
  (gl:load-transpose-matrix *MV*)
  (if show-light-source (draw-light-source))
  (dolist (a *MODEL*)
    (apply #'draw-atom a))
  (glut:swap-buffers)
  (join
   (fork
    (node1 (draw))
    (node2 (draw))
    (node3 (draw))
    (node4 (draw)))))

;; (defmethod glut:reshape ((w molicule) width height)
;;   (gl:viewport 0 0 width height)
;;   (gl:matrix-mode :projection)
;;   (gl:load-identity)
;;   (let ((h (/ height width)))
;;     (gl:frustum -1 1 (- h) h 9 50))
;;   (gl:matrix-mode :modelview)
;;   (gl:load-identity)
;;   (gl:translate 0 0 -40))

(defmethod glut:reshape ((w molicule) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum *left* *right* *bottom* *top* *near* *far*)
  (gl:matrix-mode :modelview)
  (gl:load-transpose-matrix *MV*))


(defun set-projection(l r b top n f)
  (setf *left*   l
        *right*  r
        *bottom* b
        *top*    top
        *near*   n
        *far*    f))

(defun set-modelview(a b c d
                     e f g h
                     i j k l
                     m n o p)
  (declare (type single-float
                 a b c d
                 e f g h
                 i j k l
                 m n o p))
  (setf *MV* (make-array 16
                         :element-type 'single-float
                         :initial-contents (list a b c d
                                                 e f g h
                                                 i j k l
                                                 m n o p))))


;; Input methods

(defmacro lim-between (sym bot top)
  `(setq ,sym (max ,bot (min ,top ,sym))))


(defmethod glut:keyboard ((window molicule) key x y)
  (declare (ignore x y))
  (case key
    (#\x (incf view-rotx spin-speed)
         (join (fork
                (node1 (incf view-rotx spin-speed))
                (node2 (incf view-rotx spin-speed))
                (node3 (incf view-rotx spin-speed))
                (node4 (incf view-rotx spin-speed)))))
    (#\X (decf view-rotx spin-speed)
         (join (fork
                (node1 (decf view-rotx spin-speed))
                (node2 (decf view-rotx spin-speed))
                (node3 (decf view-rotx spin-speed))
                (node4 (decf view-rotx spin-speed)))))
    (#\y (incf view-roty spin-speed)
         (join (fork
                (node1 (incf view-roty spin-speed))
                (node2 (incf view-roty spin-speed))
                (node3 (incf view-roty spin-speed))
                (node4 (incf view-roty spin-speed)))))
    (#\Y (decf view-roty spin-speed)
         (join (fork
                (node1 (decf view-roty spin-speed))
                (node2 (decf view-roty spin-speed))
                (node3 (decf view-roty spin-speed))
                (node4 (decf view-roty spin-speed)))))
    (#\z (incf view-rotz spin-speed)
         (join (fork
                (node1 (incf view-rotz spin-speed))
                (node2 (incf view-rotz spin-speed))
                (node3 (incf view-rotz spin-speed))
                (node4 (incf view-rotz spin-speed)))))
    (#\Z (decf view-rotz spin-speed)
         (join(fork
               (node1 (decf view-rotz spin-speed))
               (node2 (decf view-rotz spin-speed))
               (node3 (decf view-rotz spin-speed))
               (node4 (decf view-rotz spin-speed)))))

    (#\t (incf light-theta light-spin-speed)
         (join(fork
               (node1 (incf light-theta light-spin-speed))
               (node2 (incf light-theta light-spin-speed))
               (node3 (incf light-theta light-spin-speed))
               (node4 (incf light-theta light-spin-speed)))))
    
    (#\T (decf light-theta light-spin-speed)
         (join(fork
               (node1 (decf light-theta light-spin-speed))
               (node2 (decf light-theta light-spin-speed))
               (node3 (decf light-theta light-spin-speed))
               (node4 (decf light-theta light-spin-speed)))))
    (#\p (incf light-phi light-spin-speed)
         (join (fork
                (node1 (incf light-phi light-spin-speed))
                (node2 (incf light-phi light-spin-speed))
                (node3 (incf light-phi light-spin-speed))
                (node4 (incf light-phi light-spin-speed)))))
    (#\P (decf light-phi light-spin-speed)
         (join (fork
                (node1 (decf light-phi light-spin-speed))
                (node2 (decf light-phi light-spin-speed))
                (node3 (decf light-phi light-spin-speed))
                (node4 (decf light-phi light-spin-speed)))))

    (#\r (incf light-r light-colour-vel))
    (#\R (decf light-r light-colour-vel))
    (#\g (incf light-g light-colour-vel))
    (#\G (decf light-g light-colour-vel))
    (#\b (incf light-b light-colour-vel))
    (#\B (decf light-b light-colour-vel))

    (#\m (setq model-type (if (eq model-type 'wire) 'solid 'wire))
         (join (fork
                (node1 (setq model-type (if (eq model-type 'wire) 'solid 'wire)))
                (node2 (setq model-type (if (eq model-type 'wire) 'solid 'wire)))
                (node3 (setq model-type (if (eq model-type 'wire) 'solid 'wire)))
                (node4 (setq model-type (if (eq model-type 'wire) 'solid 'wire))))))

    (#\s (incf slices))
    (#\S (decf slices))
    (#\q (glut:destroy-current-window)
         (join (fork
                (node1 (glut:destroy-current-window))
                (node2 (glut:destroy-current-window))
                (node3 (glut:destroy-current-window))
                (node4 (glut:destroy-current-window))))
         (return-from glut:keyboard))
    
    (#\l (setq show-light-source (not show-light-source))
         (join (fork
                (node1 (setq show-light-source (not show-light-source)))
                (node2 (setq show-light-source (not show-light-source)))
                (node3 (setq show-light-source (not show-light-source)))
                (node4 (setq show-light-source (not show-light-source))))))
    
    (#\w (if walk-mode
             (glut:disable-event window :idle)
             (glut:enable-event window :idle))
         (setq walk-mode (not walk-mode))))
    
    
  (lim-between light-r 0 1)
  (lim-between light-g 0 1)
  (lim-between light-b 0 1)

  (lim-between slices 1 100)
  
  (glut:post-redisplay)
  (join
   (fork
    (node1 (draw))
    (node2 (draw))
    (node3 (draw))
    (node4 (draw)))))


(defvar origclick)
(defvar origrot)

(defmethod glut:mouse ((window molicule) button state x y)
  (if (eq button :left-button)
    (if (eq state :down)
      (progn (setf origrot (list view-rotx view-roty))
             (setf origclick (list x y)))
      (setf origclick ()))))

(defmethod glut:motion ((window molicule) x y)
  (setf view-rotx (+ (car origrot) (- y (cadr origclick))))
  (setf view-roty (+ (cadr origrot) (- x (car origclick))))
  (glut:post-redisplay))


(defun random-interval (bot top)
  (+ (* (- top bot) (/ (random 100000) 100000.0)) bot))

(defvar view-rotx-vel 0)
(defvar view-roty-vel 0)
(defvar view-rotz-vel 0)

(defvar last-update 0)
(defvar counter 0)

(defmethod glut:idle ((window molicule))
  (if walk-mode
    (progn
      (incf counter)

      (if (< (+ last-update internal-time-units-per-second) (get-internal-real-time))
        (progn
          (format t "~a frames per second with ~a slices.~%" counter slices)
          (setq counter 0)
          (setq last-update (get-internal-real-time))))

      (incf view-rotx-vel (random-interval -.1 .1))
      (incf view-roty-vel (random-interval -.1 .1))
      (incf view-rotz-vel (random-interval -.1 .1))
      (lim-between view-rotx-vel -2 2)
      (lim-between view-roty-vel -2 2)
      (lim-between view-rotz-vel -2 2)
      (incf view-rotx view-rotx-vel)
      (incf view-roty view-roty-vel)
      (incf view-rotz view-rotz-vel)

      (incf light-r (random-interval -.02 .02))
      (incf light-g (random-interval -.02 .02))
      (incf light-b (random-interval -.02 .02))
      (lim-between light-r 0 1)
      (lim-between light-g 0 1)
      (lim-between light-b 0 1)))
  (glut:post-redisplay))

(defun molicule-view(mol)
  (setf *MODEL* mol)
  (setf glut:*run-main-loop-after-display* nil)
  (set-projection -1 1 -1 1 9 50)
  (set-modelview 1.0 0.0 0.0 0.0
                 0.0 1.0 0.0 0.0
                 0.0 0.0 1.0 -40.0
                 0.0 0.0 0.0 1.0)
  (glut:display-window (make-instance 'molicule))
  (glut:set-cursor :cursor-none)
  (glut:main-loop))

(defun init (mol)
  (setf *MODEL* mol)
  (setf glut:*run-main-loop-after-display* nil)
  (set-projection -1 1 -1 1 9 50)
  (set-modelview 1.0 0.0 0.0 0.0
                 0.0 1.0 0.0 0.0
                 0.0 0.0 1.0 -40.0
                 0.0 0.0 0.0 1.0)
  (glut:display-window (make-instance 'molicule))
  (glut:set-cursor :cursor-none)
  t)

;; (setf *MODEL* water)
;; (setf *MODEL* ethanol)
;;node11
;; (set-projection -0.0476562 0.00628125  -0.0225 0.018125 .1 10000)
;;node12
;; (set-projection -0.00628125 0.0476562 -0.0225 0.018125 .1 10000)
;;node13
;; (set-projection -0.047625 0.0063125  -0.0225 0.018125 .1 10000)
;;node14
;; (set-projection -0.0063125 0.047625   -0.0225 0.018125 .1 10000)



(defun draw()
  (glut:post-redisplay)
  (glut:main-loop-event)
  t)

;; (defun draw()
;;   (glut:main-loop-event))
;; Our molecule viewer class
