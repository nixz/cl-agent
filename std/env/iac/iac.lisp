(in-package #:org.fipa.std.env.iac)

(defclass  iac ()
  ((bindings :initarg :bindings
         :accessor agent-bindings
         :allocation  :class
         :documentation "The collection of agents currently bound"))
  (:documentation  "A service that supports the sending and receiving of transport-messages between agents."))

(defmethod bind-transport((tx iac) (desc transport-description)))
(defmethod unbind-transport((tx iac) (desc transport-description)))
(defmethod send-message((tx iac) (msg transport-message)))
(defmethod deliver-message((tx iac) (desc transport-description) (msg transport-description)))


(defclass  transport-description (transport)
  ()
  (:documentation "A transport-description is a self describing structure containing a transport-type, a transport-specific-address and zero or more transport-specific-properties."))

(defclass  agent-locator ()
  ((descriptions :initarg initial-argument
         :initform initial-form
         :accessor descriptions
         :allocation  :instance
         :documentation "a set of transport-description"))
  (:documentation "An agent-locator consists of the set of transport-descriptions used to communicate with an agent."))

