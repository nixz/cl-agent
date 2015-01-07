(in-package #:std.env)

;;------------------------------------------------------------------------class
(defclass  env-location-description ()
  ((signature-type :initarg :signature-type
         :initform (error ":signature-type must be specified")
         :accessor signature-type
         :allocation :instance
         :documentation "A key-value tuple describing the type of
         service-signature.")
   (signature :initarg :signature
         :initform (error ":signature must be specified")
         :accessor env-signature
         :allocation  :instance
         :documentation "A identifier that describes the binding
         signature for a env.")
   (address :initarg :address
         :initform (error ":address should be specified")
         :accessor env-address
         :allocation :instance
         :documentation "A env-type specific string containing
         transport addressing information."))
  (:documentation "A key-value-tuple containing a signature-type a
  env-signature and env-address."))

;;-------------------------------------------------------------------------make
(defun make-env-location-description (&key signature-type signature address)
  "A key-value-tuple containing a signature-type a env-signature and env-address."
  (make-instance 'env-location-description
                 :signature-type signature-type
                 :signature signature
                 :address address))
