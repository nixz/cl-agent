(in-package #:std.env)

;;------------------------------------------------------------------------class
(defclass  connection ()
  ((socket :initform (make-instance 'sb-bsd-sockets:inet-socket
                                    :type :stream
                                    :protocol :tcp)
        :reader connection-socket
        :allocation :instance
        :documentation "sb-bsd-socket object")
  (stream  :initarg initial-argument
        :accessor connection-stream
        :allocation :instance
        :documentation "about-slot"))
  (:documentation "Tcp connection class. It opens a socket and has a
  stream reading and writing to the socket"))

;;------------------------------------------------------------------------cstor
(defmethod initialize-instance :after ((con connection) &key host (port 9999))
    (socket-connect (connection-socket con)
                    (nslookup host)
                    port)
    (setf (connection-stream con)
          (socket-make-stream (connection-socket con)
                              :input t :output t
                              :element-type 'character
                              :buffering :none)))

;;------------------------------------------------------------------------dstor
(defmethod release((con connection))
  (socket-close (connection-socket con)))

;;-------------------------------------------------------------------------make
(defun make-connection(&key host (port 9999))
  (make-instance 'connection :host host :port port))

