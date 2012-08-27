(in-package #:org.fipa.std.agent)

(defclass  agent ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name
         :allocation :instance
         :documentation "Name of the agent")
   (locator :initarg :locator
         :initform (error ":locator must be specified")
         :accessor agent-locator
         :type agent-locator
         :allocation :instance
         :documentation "An agent-locator consists of the set of transport-descriptions used to communicate with an agent.")
   (attributes :initarg :attributes
         :accessor attributes
         :allocation :instance
         :documentation "List of attributed in the agent"))
(:documentation "A computational process that implements the autonomous, communicating functionality of an application."))

(defclass  agent-attrib ()
  ()
  (:documentation "A set of properties associated with an agent by inclusion in its agent-directory-entry."))


