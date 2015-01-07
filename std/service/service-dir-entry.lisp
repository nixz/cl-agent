(in-package #:std.service)

;;------------------------------------------------------------------------class
(defclass  service-dir-entry ()
  ((name :initarg :name
         :initform (error ":name should be specified")
         :accessor service-name
         :allocation  :instance
         :documentation "A unique identifier of a particular service.")
   (type :initarg :type
         :initform (error ":type should be specified")
         :accessor service-type
         :allocation  :instance
         :documentation "A key-value tuple describing the type of a service.")
   (locator :initarg :locator
         :initform (error ":locator must be specified")
         :accessor service-locator 
         :allocation  :instance
         :documentation "A service-locator consists of the set of
         service-location-descriptions used to access a service.")
   (connection :initarg :connection
         :initform (error ":connection must be specified")
         :accessor connection
         :allocation  :instance
         :documentation "TCP socket connection class")
   (test :initarg :test
         :initform (error ":test must be specified")
         :accessor test
         :allocation :instance
         :documentation "Test to be performed after connection")
   (attributes :initarg :attributes
         :accessor service-attributes
         :allocation :instance
         :documentation "A set of properties associated with a service by
         inclusion in its service-directory-entry."))
  (:documentation "A composite entity containing the service-name,
  service-locator, and service-type of a service."))

;;------------------------------------------------------------------------make
(defun make-service-dir-entry(&key name type locator connection test attributes)
  (make-instance 'service-dir-entry
                 :name name
                 :type type
                 :locator locator
                 :attributes attributes
                 :connection connection
                 :test test))
