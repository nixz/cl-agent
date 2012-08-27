(in-package #:org.fipa.std.service)

(defclass  agent-dir (service)
  ((dir :initform (make-hash-table)
        :accessor agent-dir
        :allocation :instance
        :documentation "The list of all the agents in the system"))
  (:documentation "A service providing a shared information repository in which agent-dir-entries may be stored and queried" ))

(defclass  agent-dir-entry ()
  ((name :initarg :name
         :initform (error ":name of the agent should be specified")
         :accessor name
         :allocation :instance
         :documentation "Name of the agent to which it refers")
   (attributes :initarg :attributes
         :initform (error "List of attributes is optional")
         :accessor attributes
         :allocation :instance
         :documentation "List of attributed in the agent")
   (locator :initarg :locator
         :initform (error ":locator which is a list of one or more
         transport-descriptions must be specified")
         :accessor agent-locator
         :allocation :instance
         :documentation "Contains a list of one or more
         transport-descriptions"))
  (:documentation "A composite entity containing the name, agent-locator, and agent-attributes of an agent."))

(defmethod agent-dir-entry((a agent))
  (make-instance 'agent-dir-entry :name (name a) :attributes (attributes a) :locator (agent-locator a))

(defmethod register ((dir agent-dir) (a agent))
  (setf (gethash (agent-name a) (agent-dir dir))
        (agent-dir-entry a)))

(defmethod deregister ((dir agent-dir) (a agent))
  (setf (gethash (agent-name a) (agent-dir dir))
        nil))

(defmethod modify ((dir agent-dir) (a agent))
  (setf (gethash (agent-name a) (agent-dir dir))
        (agent-dir-entry a)))

(defmethod lookup ((dir agent-dir) (a agent))
  (gethash (agent-name a) (agent-dir dir)))
