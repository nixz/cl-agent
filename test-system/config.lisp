;; ssh -N cdgw -L localhost:9000:master:9999

;;macro for fork
(defmacro fork (&rest code)
  `(list ,@(loop for var in code collect `(sb-thread:make-thread #'(lambda () ,var)))))

;;; macro for join
(defmacro join (thread-list)
  `(mapcar #'sb-thread:join-thread ,thread-list))


(defparameter *head-pos* '((1 0 0 0)
                           (0 1 0 1.82)
                           (0 0 1 0)
                           (0 0 0 1)))
(defparameter *ipd* 0.064)
(defparameter *eye-offset* (/ *ipd* 2))

(defclass  screen ()
  ((ll :initarg :ll 
         :initform (error ":ll must be specified")
         :reader ll
         :allocation :instance
         :documentation "lower left corner")
   (lr   :initarg :lr 
         :initform (error ":lr must be specified")
         :reader lr
         :allocation :instance
         :documentation "lower right corner")
   (ur :initarg :ur 
         :initform (error ":ur must be specified")
         :reader ur 
         :allocation :instance
         :documentation "upper right")
   (ul :initarg :ul 
         :initform (error ":ul must be specified")
         :reader ul 
         :allocation :instance
         :documentation "upper left")
   (o2s  :accessor o2s 
         :allocation :instance
         :documentation "orign to screen")
   (o2r  :accessor o2r 
         :allocation :instance
         :documentation "origin to right")
   (o2l  :accessor o2l 
         :allocation :instance
         :documentation "origin to left")
   (o2t  :accessor o2t 
         :allocation :instance
         :documentation "origin to top")
   (o2b  :accessor o2b 
         :allocation :instance
         :documentation "origin to bottom")
   (eye :initarg :eye 
         :initform (error ":eye must be specified")
         :reader eye 
         :allocation :instance
         :documentation ":left or :right")
   (eye-pos :accessor eye-pos 
         :allocation :instance
         :documentation "matrix with the eye position"))
   

   (:documentation "the screen configuration"))

(defun x (v) (elt v 0))
(defun y (v) (elt v 1))
(defun z (v) (elt v 2))

(defmethod initialize-instance :after ((this screen) &key)
  (setf (o2s this) (- (z (ll this)))
        (o2r this) (x (lr this))
        (o2l this) (- (x (ll this)))
        (o2t this) (y (ur this))
        (o2b this) (- (y (lr this)))
        (eye-pos this) (matrix-apply 'eval
                                     (matrix-multiply *head-pos*
                                                      (eye-matrix this)))))

(defmethod eye-matrix ((this screen))
  (let ((eye-offset nil)
        (eye-matrix nil))
    (cond
      ((eq (eye this) :left)
       (setf eye-offset (- *eye-offset*)))
      ((eq (eye this) :right)
       (setf eye-offset *eye-offset*)))
    (setf eye-matrix (list (list 1 0 0 eye-offset)
                           (list 0 1 0 0)
                           (list 0 0 1 0)
                           (list 0 0 0 1)))
    eye-matrix))



(defparameter *screen1*
  (make-instance 'screen
                 :eye :left
                 :ll (vector -1.525 1.08 -3.2)
                 :lr (vector 0.201 1.08 -3.2)
                 :ur (vector 0.201 2.38 -3.2)
                 :ul (vector -1.525 2.38 -3.2)))

(defparameter *screen2*
  (make-instance 'screen
                 :eye :left
                 :ll (vector -0.201 1.08 -3.2)
                 :lr (vector 1.525 1.08 -3.2)
                 :ur (vector 1.525 2.38 -3.2)
                 :ul (vector -0.201 2.38 -3.2)))

(defparameter *screen3*
  (make-instance 'screen
                 :eye :right
                 :ll (vector -1.524 1.08 -3.2)
                 :lr (vector 0.202 1.08 -3.2)
                 :ur (vector 0.202 2.38 -3.2)
                 :ul (vector -1.524 2.38 -3.2)))

(defparameter *screen4*
  (make-instance 'screen
                 :eye :right
                 :ll (vector -0.202 1.08 -3.2)
                 :lr (vector 1.524 1.08 -3.2)
                 :ur (vector 1.524 2.38 -3.2)
                 :ul (vector -0.202 2.38 -3.2)))




;; Define environments
(join
 (fork
  (defenv node1 (:host "dell7cn41f1" :port 9001) "")
  (defenv node2 (:host "dell7cn41f1" :port 9002) "")
  (defenv node3 (:host "dell7cn41f1" :port 9003) "")
  (defenv node4 (:host "dell7cn41f1" :port 9004) "")))

;; init all to ethanol
(join
 (fork
  (node1 (init ethanol))
  (node2 (init ethanol))
  (node3 (init ethanol))
  (node4 (init ethanol))))

;; init modelview and projection matrix
(join
 (fork
  (node1
    (progn
      (set-projection -0.0466563 0.00728125 -0.0225 0.018125 0.1 10000)
      (set-modelview 1.0 0.0 0.0 0.032
                     0.0 1.0 0.0 -1.8
                     0.0 0.0 1.0 0.0 
                     0.0 0.0 0.0 1.0)))
  (node2
    (progn 
     (set-projection -0.00528125 0.0486562 -0.0225 0.018125 0.1 10000)
      (set-modelview 1.0 0.0 0.0 0.032
                     0.0 1.0 0.0 -1.8
                     0.0 0.0 1.0 0.0 
                     0.0 0.0 0.0 1.0)))
  (node3
    (progn
      (set-projection -0.048625 0.0053125 -0.0225 0.018125 0.1 10000)
      (set-modelview 1.0 0.0 0.0 -0.032
                     0.0 1.0 0.0 -1.8
                     0.0 0.0 1.0 0.0 
                     0.0 0.0 0.0 1.0)))
  (node4
    (progn
      (set-projection -0.0073125 0.046625 -0.0225 0.018125 0.1 10000)
      (set-modelview 1.0 0.0 0.0 -0.032
                     0.0 1.0 0.0 -1.8
                     0.0 0.0 1.0 0.0 
                     0.0 0.0 0.0 1.0)))))


;; draw all

(join
 (fork
  (node1 (draw))
  (node2 (draw))
  (node3 (draw))
  (node4 (draw))))

(loop until some condition
   ;; Read input
   (join
    (fork
     (setf ((h00 h01 h02 h03)
            (h10 h11 h12 h13)
            (h20 h21 h22 h23)
            (h30 h31 h32 h33)) (read-vrpn-head))
     (setf ((w00 w01 w02 w03)
            (w10 w11 w12 w13)
            (w20 w21 w22 w23)
            (w30 w31 w32 w33)) (read-vrpn-wand))))
     ;; Print output
     
     (join
      (fork
       (route1(node1
                (window(draw ((h00 h01 h02 h03)
                              (h10 h11 h12 h13)
                              (h20 h21 h22 h23)
                              (h30 h31 h32 h33))
                             ((w00 w01 w02 w03)
                              (w10 w11 w12 w13)
                              (w20 w21 w22 w23)
                              (w30 w31 w32 w33))))))
       (route2(node2
                (window(draw ((h00 h01 h02 h03)
                              (h10 h11 h12 h13)
                              (h20 h21 h22 h23)
                              (h30 h31 h32 h33))
                             ((w00 w01 w02 w03)
                              (w10 w11 w12 w13)
                              (w20 w21 w22 w23)
                              (w30 w31 w32 w33))))))
       (route3(node3
                (window(draw ((h00 h01 h02 h03)
                              (h10 h11 h12 h13)
                              (h20 h21 h22 h23)
                              (h30 h31 h32 h33))
                             ((w00 w01 w02 w03)
                              (w10 w11 w12 w13)
                              (w20 w21 w22 w23)
                              (w30 w31 w32 w33))))))
       (route4(node4
                (window(draw ((h00 h01 h02 h03)
                              (h10 h11 h12 h13)
                              (h20 h21 h22 h23)
                              (h30 h31 h32 h33))
                             ((w00 w01 w02 w03)
                              (w10 w11 w12 w13)
                              (w20 w21 w22 w23)
                              (w30 w31 w32 w33)))))))))

   
(join
 (fork
  (route1 (node1 (close-gl-context)))
  (route2 (node2 (close-gl-context)))
  (route3 (node3 (close-gl-context)))
  (route4 (node4 (close-gl-context)))))

                                  

(join(fork(route1(node1(window 'anim)))
          (route2(node2(window 'anim)))
          (route3(node3(window 'anim)))
          (route4(node4(window 'anim)))
          (newton(window 'anim))
          (einstein(window 'anim))
          (vet(window 'anim))))


(defclass matrix ()
  ((rows :initarg :rows
         :initform (error ":rows must be specified")
         :reader matrix-rows)
   (cols :initarg :cols
         :initform (error ":cols must be specified")
         :reader matrix-cols)
   (data :initarg :data
         :accessor matrix-data)))


(setf scene identity)


(setf base-scene


;;;;-----------------------------------------------------------------model-view
(setf head-screen (m* head-base
                      (m* base-screen)))

(setf head-eye.r (transform :rt-component head-scene
                            :tx-component (transform
                                           :data ((1 0 0 (/ *ipd* 2))
                                                  (0 1 0 0)
                                                  (0 0 1 0)
                                                  (0 0 0 1)))))

(setf head-eye.l (transfrom :rt-component head-scene
                            :tx-component (transform
                                           :data ((1 0 0 (- (/ *ipd* 2)))
                                                  (0 1 0 0)
                                                  (0 0 1 0)
                                                  (0 0 0 1)))))

(setf eye.r-head (inverse eye.r-head))
(setf eye.l-head (inverse eye.l-head))

(setf eye.r-scene (m* eye.r-head
                      (m* head-base
                          (m* base-world
                              (m* world scene)))))

(setf eye.l-scene (m* eye.l-head
                      (m* head-base
                          (m* base-world
                              (m* world scene)))))


;;;;-----------------------------------------------------------------projection
(setf projection (m* 
(defmatrix J
    ((1 0 0)
     (0 1 0)
     (0 0 1)))

(defmethod initialize-instance :after ((m matrix) &key generator)
  (assert (< 0 (matrix-rows m))
          nil
          ":rows must  be >0")
  (assert (< 0 (matrix-cols m))
          nil
          ":cols must  be >0")
  (if (slot-boundp m 'data)
      (progn
        (assert (= (length (matrix-data m))
                   (* (matrix-rows m)
                      (matrix-cols m)))
                nil
                ":data dimension should be ~d."
                (* (matrix-rows m) (matrix-cols m)))
        (assert (not generator)
                nil
                ":data and :generator may not be specified at the same time."))
      (if (functionp generator)
          (progn
            (setf (matrix-data m)
                  (make-array (* (matrix-rows m)(matrix-cols m))
                              :element-type 'single-float))
            (dotimes (i (matrix-rows m) m)
              (dotimes (j (matrix-cols m))
                (setf (matrix-at m i j)
                      (funcall generator i j)))))
          (progn
            (setf (matrix-data m)
                  (make-array (* (matrix-rows m) (matrix-cols m))
                              :element-type 'single-float
                              :initial-element 0.0))
            m))))

