(defun nslookup (hostname)
  "Performs a DNS look up for HOSTNAME and returns the address as a
   four element array, suitable for socket-connect.  If HOSTNAME is
   not found, a host-not-found-error condition is thrown."
  (if hostname
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name hostname))
      nil))

(defun get-host-name()
  (read-line
   (process-output
    (run-program "/bin/hostname"
                 nil
                 :output :stream
                 :wait nil))))

(defun repl (stream)
    (unwind-protect
         (progn
           (setq *standard-input* stream
                 *standard-output* stream)
           (loop
              (print (handler-case (eval (read))
                          (error (condition) (list 'error condition))))))
      ;;         (loop (print (eval (read stream)) stream))
      (quit)))


;;; macro for fork
(defmacro fork (&rest code)
  `(list ,@(loop for var in code collect `(sb-thread:make-thread #'(lambda () ,var)))))

;;; macro for join
(defmacro join (thread-list)
  `(mapcar #'sb-thread:join-thread ,thread-list))

;; (defclass  connection ()
;;   ((client :initform (make-hash-table :test 'EQUAL)
;;          :accessor client
;;          :allocation :class
;;          :documentation "The hash map containing the list of all the
;;          sockets connected to a particular server"))
;;   (:documentation "The connection class has the list of all connection
;;   from to various servers"))

(defparameter *connections* (make-hash-table :test 'EQUAL))

(defstruct connection
  (socket 
  stream)
  
(defun name (host &optional (port 9999))
  "Given a string with the name of the host and the port (integer),
this function returns the concatenated string of the form host-port"
  (concatenate 'string host "-" (write-to-string port)))

(defun registered? (host &optional (port  9999))
  (multiple-value-bind (stream, stream-p) (gethash host *connections* )
    
                        
(defun connect (host &optional (port 9999))
  "Interface method to *connections*. It creates a connection socket
+ a stream to the socket and registers the connection with
*connections*"
  (let ((con (make-connection)))
    (sb-bsd-sockets:socket-connect (connection-socket con)
                                   (nslookup host)
                                   port)
    (setf (connection-stream con)
          (sb-bsd-sockets:socket-make-stream (connection-socket con)
                                             :input t :output t
                                             :element-type 'character
                                             :buffering :none))
    (unless (registered? host)
      (setf (gethash (name host port) *connections*) con))))

(defun disconnect (host &optional (port 9999))
  
                      
(defun host-stream (host &optional (port 9999))
  (unless (gethash (name host port) *connections*)))
    (unless stream (progn
                     (connect host port)
                     (setf stream (host-stream host port))))
    (stream)))

(defmacro defcon (name &optional &keys host (port 9999))
  )

(defmacro host ((host &optional (port 9999))  &body body)
  "Usage: (host (\"host-name\" [port]) body). This macro opens a
  stream to the host (if not present) and executes the body at the end
  and returns the response"
  (let ((stream (host-stream host port)))
    `(progn
       (print ',@body ,stream)
       (read ,stream))))

;; (host ("newton")
;;     (+ 1 2 3))

;;(defmacro defun+ ((parameters)  &body body) )

(defun server (&key (port 9999))
  "This is the start of the environment. The environment comes up at
defaut port 9999. It waits for connections and opens up repl
connections and waits for various inputs."
  (let (socket (count 0))
    (unwind-protect
         (progn
           (setq socket (make-instance 'sb-bsd-sockets:inet-socket
                                       :type :stream
                                       :protocol :tcp))
           (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
           (sb-bsd-sockets:socket-bind socket (nslookup (get-host-name)) port)
           (sb-bsd-sockets:socket-listen socket 5)
           (loop
              (let (session pid)
                (setq session (sb-bsd-sockets:socket-accept socket))
                (setq pid (sb-posix:fork))
                (cond
                  ((zerop pid) (progn
                                 (sb-bsd-sockets:socket-close socket)
                                 (repl  (sb-bsd-sockets:socket-make-stream session
                                                                           :input t :output t
                                                                           :element-type 'character
                                                                           :buffering :none))))
                  ((plusp pid) (progn
                                 (sb-bsd-sockets:socket-close session)
                                 (setf count (+ count 1))
                                 (format t "~&Count = ~a ~%" count)))
                  (t           (error "Something went wrong while forking."))))))
      (quit))))

(defun router (&key (port 9998))
  "This is the start of the environment. The environment comes up at
defaut port 9999. It waits for connections and opens up repl
connections and waits for various inputs."
  (let (socket (count 0))
    (unwind-protect
         (progn
           (setq socket (make-instance 'sb-bsd-sockets:inet-socket
                                       :type :stream
                                       :protocol :tcp))
           (sb-bsd-sockets:socket-bind socket (nslookup (get-host-name)) port)
           (sb-bsd-sockets:socket-listen socket 5)
           (loop
              (let (session pid)
                (setq session (sb-bsd-sockets:socket-accept socket))
                (setq pid (sb-posix:fork))
                (cond
                  ((zerop pid) (progn
                                 (sb-bsd-sockets:socket-close socket)
                                 (route  (sb-bsd-sockets:socket-make-stream session
                                                                           :input t :output t
                                                                           :element-type 'character
                                                                           :buffering :none))))
                  ((plusp pid) (progn
                                 (sb-bsd-sockets:socket-close session)
                                 (setf count (+ count 1))
                                 (format t "~&Count = ~a ~%" count)))
                  (t           (error "Something went wrong while forking."))))))
      (quit))))

;; (ignore-errors (+ 1 2 3) (- 1 3))

(defun send-user-input (stream)
  (loop
     (print (read *standard-input*) stream)
     (force-output)
     (print (read stream) *standard-output*)
     (force-output)))


(defun router-send(name)
  (let ((input (sb-bsd-sockets:socket-make-stream (route-pair-in (gethash name *route-table*))
                                           :input t :output nil
                                           :element-type 'character
                                           :buffering :none))
        (output (sb-bsd-sockets:socket-make-stream (route-pair-in (gethash name *route-table*))
                                           :input nil :output t
                                           :element-type 'character
                                           :buffering :none)))
    (print (read input) output)))


(defun router-recv(name)
  (let ((input (sb-bsd-sockets:socket-make-stream (route-pair-out (gethash name *route-table*))
                                           :input t :output nil
                                           :element-type 'character
                                           :buffering :none))
        (output (sb-bsd-sockets:socket-make-stream (route-pair-in (gethash name *route-table*))
                                           :input nil :output t
                                           :element-type 'character
                                           :buffering :none)))
    (print (read input) output)))

(defun router (name)
  (let* ((in (sb-bsd-sockets:socket-accept (route-pair-in (gethash name *route-table*)))))
    (sb-bsd-sockets:socket-close (route-pair-in (gethash name *route-table*)))
    (setf (route-pair-in (gethash name *route-table*)) in)
    (unwind-protect
         (loop
            (router-send name)
            (force-output)
            (router-recv name)
            (force-output))
      (print '(error "Router Connection Closed..."))
      (when in (sb-bsd-sockets:socket-close in))
      (when output (sb-bsd-sockets:socket-close out-sock)))))
  
  ;; (connect node1-sock :host cdgw (route :host node1))
  ;; (connect node2-sock :host cdgw (route :host node2))
  ;;         (connect node3-sock :host cdgw (route :host node3))
          
  ;;         (route :host node2)

(defstruct route-pair
  (in (make-instance 'sb-bsd-sockets:inet-socket
                     :type :stream
                     :protocol :tcp))
  (out (make-instance 'sb-bsd-sockets:inet-socket
                      :type :stream
                      :protocol :tcp)))

(defparameter *route-table* (make-hash-table :test 'EQUAL))


(defun route-name(host port)
  "Given a string with the name of the host and the port (integer),
this function returns the concatenated string of the form host-port"
  (concatenate 'string host "-" (write-to-string port)))
                  
(defun route (&key (host "127.0.0.1" host-supplied-p) (port 9999))
  (unless host-supplied-p (error ":host must be specified"))
  (let ((name (route-name host port))
        ip port)
    (progn
      (setf (gethash name *route-table*) (make-route-pair))
      (sb-bsd-sockets:socket-bind
       (route-pair-in (gethash name *route-table*)) (nslookup (get-host-name)) 0)
      (sb-bsd-sockets:socket-listen
       (route-pair-in (gethash name *route-table*)) 1)
      (sb-bsd-sockets:socket-connect
       (route-pair-out (gethash name *route-table*)) (nslookup host) port)
      (sleep 5)
      (fork (router name))
      (setf (values ip port)
            (sb-bsd-sockets:socket-name
             (route-pair-in (gethash name *route-table*))))
      (print port))))
    
    

(defun client (&key (server "newton") (port 9999))
  (let (socket)
    (unwind-protect
         (progn
           (setq socket (make-instance 'sb-bsd-sockets:inet-socket
                                       :type :stream
                                       :protocol :tcp))
           (sb-bsd-sockets:socket-connect socket (nslookup server) port)
           (send-user-input (sb-bsd-sockets:socket-make-stream socket
                                                               :input t :output t
                                                               :element-type 'character
                                                               :buffering :none)))
      (when socket (progn
                     (print '(error "Connection Closed..."))
                     (sb-bsd-sockets:socket-close socket))))))
                            

(defun test () "this is a test")
