(in-package #:org.fipa.std.env)

;;------------------------------------------------------------------------class
(defclass  env-dir-entry ()
  ((name :initarg :name
         :initform (error ":name should be specified")
         :accessor env-name
         :allocation  :instance
         :documentation "A unique identifier of a particular env.")
   (type :initarg :type
         :initform (error ":type should be specified")
         :accessor env-type
         :allocation  :instance
         :documentation "A key-value tuple describing the type of a env.")
   (locator :initarg :locator
         :initform (error ":locator must be specified")
         :accessor env-locator 
         :allocation  :instance
         :documentation "A env-locator consists of the set of
         env-location-descriptions used to access a env.")
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
         :accessor env-attributes
         :allocation :instance
         :documentation "A set of properties associated with a env by
         inclusion in its env-directory-entry."))
  (:documentation "A composite entity containing the env-name,
  env-locator, and env-type of a env."))

;;------------------------------------------------------------------------make
(defun make-env-dir-entry(&key name type locator connection test attributes)
  (make-instance 'env-dir-entry
                 :name name
                 :type type
                 :locator locator
                 :attributes attributes
                 :connection connection
                 :test test))
