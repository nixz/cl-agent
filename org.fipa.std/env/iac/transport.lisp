(in-package #:org.fipa.std.env.iac)

;; abstract class (concrete class MUST inherit)
(defclass  transport ()
  ((type :initarg :transport-type
         :initform (error ":transport-type should be specified")
         :accessor transport-type
         :allocation :instance
         :documentation "A transport-type describes the type of transport associated with a transport-specific-address.")
   (specific-address :initarg :transport-address
         :initform (error ":transport-address should be specified")
         :accessor transport-specific-address
         :allocation  :instance
         :documentation "A transport address specific to a given transport-type")
   (specific-properties :initarg :transport-properties
         :accessor transport-specific-properties
         :allocation :instance
         :documentation "A set of transport-specific-property"))
  (:documentation "A transport is a particular data delivery service supported by a given iac."))

;; abstract class (concrete class MAY to inherit)
(defclass transport-specific-property()
  ()
  (:documentation "A transport-specific-property is a property associated with a transport-type."))
