;;;; --------------------------------------------------------------------------
;;;; @file   repl-server.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Wed Mar 10 13:30:42 2010
;;;;
;;;; @brief 
;;;; --------------------------------------------------------------------------

(in-package #:cl-user)

(defpackage #:repl-server
  (:nicknames #:server)
  (:use #:cl  #:sb-bsd-sockets #:sb-ext)
  (:export
   ;; Data
   #:with-repl-server
   #:accept
   #:nslookup))

(in-package repl-server)


(defclass  repl-server ()
  ((host :initarg :host
         :initform (read-line
                    (process-output
                     (run-program "/bin/hostname"
                                  nil
                                  :output :stream
                                  :wait nil)))
         :reader host
         :allocation :instance
         :documentation "The name of the host.")
   (port :initarg :port
         :initform (error ":port needs to be provided")
         :reader port
         :allocation :instance
         :documentation "The port at which the repl can be connected")
   (socket :initform (make-instance 'inet-socket
                                    :type :stream
                                    :protocol :tcp)
         :reader socket
         :allocation  :instance
         :documentation "the main socket which is listening"))
  (:documentation "The repl-server class starts at a predefined port
  and waits for clients to connect to it. There is
  a (loop(print(eval(read) waiting to server streams of commands at
  the ports."))

(defmethod initialize-instance :after ((this repl-server) &key)
  "Constructor for repl-server"
  (handler-case
      (progn
        (sb-bsd-sockets:socket-bind (socket this) (nslookup (host this)) (port this))
        (sb-bsd-sockets:socket-listen (socket this) 5))
    (address-in-use-error ()
      (format t "address ~A : ~A is already in use" (host this) (port this))
      (force-output)
      (quit))))


(defmethod accept ((this repl-server))
  "Accepts new connectios and returns the connection socket"
  (sb-bsd-sockets:socket-accept (socket this)))

    
(defun nslookup (hostname)
  "Performs a DNS look up for HOSTNAME and returns the address as a
   four element array, suitable for socket-connect.  If HOSTNAME is
   not found, a host-not-found-error condition is thrown."
  (if hostname
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name hostname))
      nil))

(defmacro with-repl-server ((name &key (port 9999)) &body forms)
  `(let (,name)
     (unwind-protect
          (progn
            (setq ,name  (make-instance 'repl-server :port ,port))
            ,@forms)
       (when ,name
         (progn
           (sb-bsd-sockets:socket-close (socket ,name))
           (sb-ext:gc :full t))))))

;; (with-repl-server (server)
;;   (setf connection (accept server)))

;; (repl-server::with-repl-server (server-connect)
;;   (print
;;    (read
;;     (sb-bsd-sockets:socket-make-stream (repl-server::accept server-connect) 
;;                         :input t 
;;                         :output t 
;;                         :element-type 'character 
;;                         :buffering :none))))

  
  
