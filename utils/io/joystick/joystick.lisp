;; code from Gamelib library

(in-package #:org.io.joystick)

(defconstant +axis-event+ #x02)
(defconstant +button-event+ #x01)
(defconstant +init-event+ #x80)

(defclass  joystick ()
  ((device :initarg :device
         :initform (error ":device must be specified")
         :reader device
         :allocation :instance
         :documentation "about-slot")
   (event-stream :initarg :event-stream
         :initform (error ":event-stream must be specified")
         :accessor event-stream
         :allocation :instance
         :documentation "about-slot")
   (axes :initarg :axes
         :initform (make-array 8 :element-type 'single-float)
         :accessor axes
         :allocation :instance
         :documentation "about-slot")
   (internal-buttons :initarg :internal-buttons 
         :initform (make-array 24 :element-type 'bit)
         :accessor internal-buttons 
         :allocation :instance
         :documentation "about-slot")
   (internal-axes :initarg :internal-axes 
         :initform (make-array 8 :element-type '(signed-byte 16))
         :accessor internal-axes 
         :allocation :instance
         :documentation "about-slot")
   (buttons :initarg :buttons 
         :initform (make-array 24 :element-type 'bit)
         :accessor buttons 
         :allocation :instance
         :documentation "about-slot")
   (terminate :initarg :terminate 
         :initform nil
         :accessor terminate 
         :allocation :instance
         :documentation "about-slot")
   (lock :initarg :lock 
         :initform #+sb-thread (sb-thread:make-mutex) #-sb-thread nil
         :reader lock 
         :allocation :instance
         :documentation "about-slot")
   (polled :initarg :polled 
         :initform nil
         :accessor polled 
         :allocation :instance
         :documentation "about-slot")
   (data :initarg :data 
         :initform (make-array 8 :element-type '(unsigned-byte 8))
         :reader data 
         :allocation :instance
         :documentation "about-slot")
   (transfer-funs :initarg :transfer-funs 
         :initform (make-array 8 :initial-element #'simple-transfer)
         :reader transfer-funs 
         :allocation :instance
         :documentation "about-slot"))
  (:documentation "The joystick class used to read values out of
  joystick device"))




;; (defclass joystick ()
;;   ((device :reader device
;;           :initarg :device)
;;    (event-stream :accessor event-stream
;;           :initarg :event-stream)
;;    (axes :accessor axes
;;           :initarg :axes)
;;    (internal-buttons :accessor internal-buttons
;;           :initarg :internal-buttons)
;;    (internal-axes :accessor internal-axes
;;           :initarg :internal-axes)
;;    (buttons :accessor buttons
;;           :initarg :buttons)
;;    (terminate :accessor terminate
;;           :initarg :terminate)
;;    (lock :reader lock
;;          :initarg :lock)
;;    (polled :accessor polled
;;          :initarg :polled)
;;    (data :reader data :initarg :data)
;;    (transfer-funs :reader transfer-funs :initarg :transfer-funs) 
;;    )
;;   (:default-initargs :axes (make-array 8 :element-type 'single-float)
;; 		     :internal-axes (make-array
;; 				     8 :element-type '(signed-byte 16))
;; 		     :buttons (make-array 24 :element-type 'bit)
;; 		     :internal-buttons (make-array 24 :element-type 'bit)
;; 		     :lock #+sb-thread (sb-thread:make-mutex) #-sb-thread nil
;; 		     :terminate nil
;; 		     :polled nil
;; 		     :data (make-array 8 :element-type '(unsigned-byte 8))
;; 		     :transfer-fun (make-array 8 :initial-element #'simple-transfer)
;; 		     ))

(defun make-stick (device)
  (let ((event-stream (open device :element-type '(unsigned-byte 8))))
    (let ((device (make-instance 'joystick :device device :event-stream event-stream)))
      device)))

(defun lock-stick (joystick)
  #+sb-thread
  (sb-thread:get-mutex (lock joystick)))

(defun unlock-stick (joystick)
  #+sb-thread
  (sb-thread:release-mutex (lock joystick)))

(defun gobble (vector start end &optional signed)
  (loop for n from start below end
	for mult = 1 then (* mult 256)
	with value = 0
	do (setf value (+ value (* mult (aref vector n))))
	finally (if signed
		    (let ((max (expt 2 (* 8 (- end start)))))
		      (if (>= value (/ max 2))
				  (return (- value max))
				(return value)))
			  (return value))))

(defun read-stick (joystick)
  (let ((bytes (read-sequence (data joystick) (event-stream joystick))))
    (when (= bytes 8)
      (let ((data (data joystick)))
	(let (;(ms (gobble data 0 4))
	      (offset (gobble data 4 6 t))
	      (event-type (aref data 6))
	      (index (aref data 7)))
	  (when (polled joystick)
	    (lock-stick joystick))
	  (cond ((= +axis-event+ (logand event-type +axis-event+))
		 (setf (aref (internal-axes joystick) index) offset))
		((= +button-event+ (logand event-type +button-event+))
		 (setf (aref (internal-buttons joystick) index)
		       (if (zerop offset) 0 1))))
	  (when (polled joystick)
	    (unlock-stick joystick)))))))

(defun update-stick (joystick)
  (unless (polled joystick)
    (read-stick joystick))
  (when (polled joystick)
    (lock-stick joystick))
  (loop for raw across (internal-axes joystick)
	for transfer across (transfer-funs joystick)
	for ix from 0
	do (setf (aref (axes joystick) ix) (funcall transfer raw)))
  (loop for bit across (internal-buttons joystick)
	for ix from 0
	do (setf (aref (buttons joystick) ix) bit))
  (when (polled joystick)
    (unlock-stick joystick)))

#+sb-thread
(defun continous-poll (joystick)
  (unless (polled joystick)
    (setf (terminate joystick) nil)
    (setf (polled joystick) t)
    (sb-thread:make-thread (lambda ()
			     (loop until (terminate joystick)
				   do (read-stick joystick))
			     (setf (polled joystick) nil)))))

#+sb-thread
(defun terminate-thread (joystick)
  (lock-stick joystick)
  (setf (terminate joystick) t)
  (unlock-stick joystick))

(defun set-transfer (joystick axis function)
  (setf (aref (transfer-funs joystick) axis) function))
