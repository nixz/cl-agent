(in-package #:std.service)

;;------------------------------------------------------------------------class
(defclass  service-dir ()
  ((dir  :initform (make-hash-table :test 'EQUAL)
         :accessor service-dir
         :allocation  :class
         :documentation "Directory containing all the available serviceironments"))
  (:documentation "Used to register and locate serviceironments"))

;;-----------------------------------------------------------------------method
(defmethod register ((dir service-dir) (e service-dir-entry))
  (let ((name (string-upcase (service-name e))))
    (multiple-value-bind (entry entry-p) (gethash name (service-dir dir))
      (unless entry-p 
        (setf (gethash name (service-dir dir)) e)))))

;; (defmethod register ((dir service-dir) symbol)
;;   (let ((name (string-upcase symbol)))
;;     (multiple-value-bind (entry entry-p) (gethash name (service-dir dir))
;;       (unless entry-p 
;;         (setf (gethash name (service-dir dir)) "register")))))

;;-----------------------------------------------------------------------method
(defmethod deregister ((dir service-dir) symbol)
  (let ((name (string-upcase symbol)))
    (multiple-value-bind (entry entry-p) (gethash name (service-dir dir))
      (when entry-p 
        (remhash name (service-dir dir))))))

;;-----------------------------------------------------------------------method
(defmethod modify ((dir service-dir) (e service-dir-entry))
  (let ((name (string-upcase (service-name e))))
    (multiple-value-bind (entry entry-p) (gethash name (service-dir dir))
      (when entry-p
        (setf (gethash (service-name e) (service-dir dir)) e)))))

;; (defmethod modify ((dir service-dir) symbol)
;;   (let ((name (string-upcase symbol)))
;;     (multiple-value-bind (entry entry-p) (gethash name (service-dir dir))
;;       (when entry-p
;;         (setf (gethash name (service-dir dir)) "modify")))))


;;-----------------------------------------------------------------------method
(defmethod lookup ((dir service-dir) symbol)
  (let ((name (string-upcase symbol)))
    (multiple-value-bind (entry entry-p) (gethash name (service-dir dir))
    (when entry-p 
      entry))))

;;-------------------------------------------------------------------------make
(defun make-service-dir()
  (make-instance 'service-dir))



