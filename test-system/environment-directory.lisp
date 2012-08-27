;;;; --------------------------------------------------------------------------
;;;; @file   environment-directory.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Mon Feb 15 19:39:10 2010
;;;;
;;;; @brief 
;;;; --------------------------------------------------------------------------

(defclass  env-dir ()
  ((dir  :initform (make-hash-table)
         :accessor env-dir
         :allocation  :instance
         :documentation "Directory containing all the available environments"))
  (:documentation "Used to register and locate environments"))



(defmethod register ((dir env-dir) (e env))
  (setf (gethash (env-name e) (env-dir dir)) (env-dir-entry e)))

(defmethod deregister ((dir env-dir) (e env))
  (setf (gethash (env-name e) (env-dir dir)) nil))

(defmethod modify ((dir env-dir) (e env))
  (setf (gethash (env-name e) (env-dir dir)) (env-dir-entry e)))

(defmethod find-env ((dir env-dir) (e env))
  (gethash (env-name e) (env-dir dir)))



(defmethod env-register ((dir env-dir) name locator)
  (setf (gethash name (env-dir dir)) locator))

(defmethod env-search ((dir env-dir) name)
  (gethash name (env-dir dir)))

(defun make-env-dir()
  (make-instance 'env-dir))

(defparameter *env* (make-env-dir))

(env-register *env* 'newton "apple")
(env-register *env* 'einstein "ball")
(env-register *env* 'hawking "cat")
