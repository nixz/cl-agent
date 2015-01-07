(in-package #:std.env)

;;------------------------------------------------------------------------class
(defclass  env-dir ()
  ((dir  :initform (make-hash-table :test 'EQUAL)
         :accessor env-dir
         :allocation  :class
         :documentation "Directory containing all the available environments"))
  (:documentation "Used to register and locate environments"))

;;-----------------------------------------------------------------------method
(defmethod register ((dir env-dir) (e env-dir-entry))
  (let ((name (string-upcase (env-name e))))
    (multiple-value-bind (entry entry-p) (gethash name (env-dir dir))
      (unless entry-p 
        (setf (gethash name (env-dir dir)) e)))))

;; (defmethod register ((dir env-dir) symbol)
;;   (let ((name (string-upcase symbol)))
;;     (multiple-value-bind (entry entry-p) (gethash name (env-dir dir))
;;       (unless entry-p 
;;         (setf (gethash name (env-dir dir)) "register")))))

;;-----------------------------------------------------------------------method
(defmethod deregister ((dir env-dir) symbol)
  (let ((name (string-upcase symbol)))
    (multiple-value-bind (entry entry-p) (gethash name (env-dir dir))
      (when entry-p 
        (remhash name (env-dir dir))))))

;;-----------------------------------------------------------------------method
(defmethod modify ((dir env-dir) (e env-dir-entry))
  (let ((name (string-upcase (env-name e))))
    (multiple-value-bind (entry entry-p) (gethash name (env-dir dir))
      (when entry-p
        (setf (gethash (env-name e) (env-dir dir)) e)))))

;; (defmethod modify ((dir env-dir) symbol)
;;   (let ((name (string-upcase symbol)))
;;     (multiple-value-bind (entry entry-p) (gethash name (env-dir dir))
;;       (when entry-p
;;         (setf (gethash name (env-dir dir)) "modify")))))


;;-----------------------------------------------------------------------method
(defmethod lookup ((dir env-dir) symbol)
  (let ((name (string-upcase symbol)))
    (multiple-value-bind (entry entry-p) (gethash name (env-dir dir))
    (when entry-p 
      entry))))

;;-------------------------------------------------------------------------make
(defun make-env-dir()
  (make-instance 'env-dir))



