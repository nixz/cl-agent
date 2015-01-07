(in-package #:std.service.msg-transport-service)

(defclass  msg-transport-service ()
  ((bindings :initarg :bindings
         :accessor agent-bindings
         :allocation  :class
         :documentation "The collection of agents currently bound"))
  (:documentation "A service that supports the sending and receiving of
  transport-messages between agents."))

(defmethod bind-transport((tx msg-transport-service) 
                          (desc transport-description))
  )
(defmethod unbind-transport((tx msg-transport-service) 
                            (desc transport-description))
  )
(defmethod send-message((tx msg-transport-service) 
                        (msg transport-message))
  )
(defmethod deliver-message((tx msg-transport-service) 
                           (desc transport-description) 
                           (msg transport-description))
  )
