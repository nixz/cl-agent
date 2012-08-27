(in-package #:org.fipa.std.service.msg-transport-service)

(defclass  agent-locator ()
  ((descriptions :initarg initial-argument
         :initform initial-form
         :accessor descriptions
         :allocation  :instance
         :documentation "a set of transport-description"))
  (:documentation "An agent-locator consists of the set of
  transport-descriptions used to communicate with an agent."))

