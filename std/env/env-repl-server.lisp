(in-package #:std.env)

;;--------------------------------------------------------------------------fun
(defun nslookup (hostname)
  "Performs a DNS look up for HOSTNAME and returns the address as a
   four element array, suitable for socket-connect.  If HOSTNAME is
   not found, a host-not-found-error condition is thrown."
  (if hostname
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name hostname))
      nil))

;;--------------------------------------------------------------------------fun
(defun get-host-name()
  "Based on the linux program hostname. Get the name of the host by
running the command."
  (read-line
   (sb-ext:process-output
    (sb-ext:run-program "/bin/hostname"
                        nil
                        :output :stream
                        :wait nil))))

;;--------------------------------------------------------------------------fun
(defun repl (stream)
  (unwind-protect
       (progn
         (setq *standard-input* stream
               *standard-output* stream)
         (loop
            ;;(print (handler-case (write-to-string (eval (read)))
            (print (handler-case (eval (read))                     
                     (error (condition) (list 'error condition))))))
    ;;         (loop (print (eval (read stream)) stream))
    (cl-user::quit)))

;;--------------------------------------------------------------------------fun
(defun repl-server (&key (port 9999))
  "This is the start of the environment. The environment comes up at
defaut port 9999. It waits for connections and opens up repl
connections and waits for various inputs."
  (let (socket (count 0))
    (unwind-protect
         (progn
           (let ((socket (make-instance 'inet-socket
                                       :type :stream
                                       :protocol :tcp)))
             (setf (sockopt-reuse-address socket) t)
             (socket-bind socket (nslookup (get-host-name)) port)
             (socket-listen socket 5)
           (loop
              (let (session pid)
                (setq session (socket-accept socket))
                (setq pid (sb-posix:fork))
                (cond
                  ((zerop pid) (progn
                                 (socket-close socket)
                                 (repl  (socket-make-stream session
                                                            :input t :output t
                                                            :element-type 'character
                                                            :buffering :none))))
                  ((plusp pid) (progn
                                 (socket-close session)
                                 (setf count (+ count 1))
                                 (format t "~&Count = ~a ~%" count)))
                  (t           (error "Something went wrong while forking."))))))
      (quit))))

;; ;;--------------------------------------------------------------------------fun
;; (defun repl-server (&key (port 9999))
;;   "This is the start of the environment. The environment comes up at
;; defaut port 9999. It waits for connections and opens up repl
;; connections and waits for various inputs."
;;   (let (socket (count 0))
;;     (unwind-protect
;;          (progn
;;            (setq socket (make-instance 'inet-socket
;;                                        :type :stream
;;                                        :protocol :tcp))
;;            (setf (sockopt-reuse-address socket) t)
;;            (socket-bind socket (nslookup (get-host-name)) port)
;;            (socket-listen socket 5)
;;            (loop
;;               (let (session pid)
;;                 (setq session (socket-accept socket))
;;                 (setq pid (sb-posix:fork))
;;                 (cond
;;                   ((zerop pid) (progn
;;                                  (socket-close socket)
;;                                  (repl  (socket-make-stream session
;;                                                             :input t :output t
;;                                                             :element-type 'character
;;                                                             :buffering :none))))
;;                   ((plusp pid) (progn
;;                                  (socket-close session)
;;                                  (setf count (+ count 1))
;;                                  (format t "~&Count = ~a ~%" count)))
;;                   (t           (error "Something went wrong while forking."))))))
;;       (quit))))
