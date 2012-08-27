(in-package #:org.fipa.std.service)

;;------------------------------------------------------------------------class
(defclass  service-location-description ()
  ((signature-type :initarg :signature-type
         :initform (error ":signature-type must be specified")
         :accessor signature-type
         :allocation :instance
         :documentation "A key-value tuple describing the type of
         service-signature.")
   (signature :initarg :signature
         :initform (error ":signature must be specified")
         :accessor service-signature
         :allocation  :instance
         :documentation "A identifier that describes the binding
         signature for a service.")
   (address :initarg :address
         :initform (error ":address should be specified")
         :accessor service-address
         :allocation :instance
         :documentation "A service-type specific string containing
         transport addressing information."))
  (:documentation "A key-value-tuple containing a signature-type a
  service-signature and service-address."))

;;-------------------------------------------------------------------------make
(defun make-service-location-description (&key signature-type signature address)
  "A key-value-tuple containing a signature-type a service-signature and service-address."
  (make-instance 'service-location-description
                 :signature-type signature-type
                 :signature signature
                 :address address)
)
