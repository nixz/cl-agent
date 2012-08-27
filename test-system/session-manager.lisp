
(defclass  repl-session ()
  ((id :initarg :id
         :initform (error ":id must be specified")
         :reader id
         :allocation :instance
         :documentation "Session id")   
   (socket :initarg :socket
         :initform (error ":socket must be specified")
         :reader socket
         :allocation :instance
         :documentation "socket")
   (stream :initarg :stream
         :initform (error ":stream must be specified")
         :accessor stream
         :allocation :instance
         :documentation "the stream which the thread reads form")
   (thread :initarg :thread
         :initform (error ":thread must be specified")
         :reader repl-session-thread
         :allocation  :instance
         :documentation "about-slot"))
  (:documentation "A repl thread which is bound to the socket."))

(defclass  session-manager ()
  ((server :initarg :server
         :initform (error ":server should be provided")
         :reader server
         :allocation  :class
         :documentation "The server which starts repl connections")
   (session :initform (make-array 5 :fill-pointer 0 :adjustable t :element-type 'repl-session)
         :accessor session
         :allocation :class
         :documentation "A list of all the sessions"))
  (:documentation "contains a list of all the sessions"))

(defmethod register-session((this session-manager) (current-session repl-session))
  (dotimes (i (length (session this)))
    (when (equal (id current-session)
                   (id (elt (session this) i)))
      (format t "Cannot register session")
      (return nil)))
  (vector-push session (session this)))

(defmethod deregister-session((this session-manager) id)
  (dotimes (i (length (session this)))
    (when (equal id (id (elt (session this) i)))
      (remove (elt (session this) i))
      (return t)))
  (return nil)))
    
(defmethod initialize-instance :after ((this session-manager) &key)
  (
