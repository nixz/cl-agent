;; Snellman, <jsnell@iki.fi>. http://jsnell.iki.fi/tmp/echo-server.lisp
(require :sb-bsd-sockets)

(defpackage echo-server
  (:use :cl :sb-bsd-sockets))

(in-package echo-server)

(defvar *port* 7001)

(defun make-echoer (stream id disconnector)
  (lambda (_)
    (declare (ignore _))
    (handler-case
        (let ((line (read-line stream)))
          (setf line (subseq line 0 (1- (length line))))
          (cond ((string= line "(quit)")
                 (funcall disconnector))
                (t
                 (format t "~a: ~a~%" id line)
                 (format stream "~a: ~a~%" id line)
                 (force-output stream))))
      (end-of-file ()
        (funcall disconnector)))))

(defun make-echoer (stream id disconnector)
  (lambda (_)
    (declare (ignore _))
    (handler-case
        (progn
          (print (eval (read stream)) stream )
          (finish-output stream))
      (end-of-file ()
        (funcall disconnector)))))

(defun make-disconnector (socket id)
  (lambda ()
    (let ((fd (socket-file-descriptor socket)))
      (format t "~a: closing~%" id)
      (sb-impl::invalidate-descriptor fd)
      (socket-close socket))))

(defun serve (socket id)
  (let ((stream (socket-make-stream socket
                                    :output t
                                    :input t
                                    :element-type 'character
                                    :buffering :full));:none))
        (fd (socket-file-descriptor socket)))
    (sb-impl::add-fd-handler fd
                             :input
                             (make-echoer stream
                                          id
                                          (make-disconnector socket id)))))

(defun echo-server (&optional (port *port*))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        (counter 0))
    (socket-bind socket #(127 0 0 1) port)
    (socket-listen socket 5)
    (sb-impl::add-fd-handler (socket-file-descriptor socket)
                             :input
                             (lambda (_)
                               (declare (ignore _))
                               (incf counter)
                               (format t "Accepted client ~A~%" counter)
                               (serve (socket-accept socket) counter)))))

;; #+sb-thread
;; ;; (sb-thread:make-thread (lambda ()
;; ;;                          (echo-server)
;; ;;                          (loop
;; ;;                             (sb-impl::serve-all-events))))
;; #-sb-thread
;; (echo-server)
(defun server ()
  (echo-server)
  (loop
  (sb-sys:serve-all-events)))

(defun nslookup (hostname)
  "Performs a DNS look up for HOSTNAME and returns the address as a
   four element array, suitable for socket-connect.  If HOSTNAME is
   not found, a host-not-found-error condition is thrown."
  (if hostname
      (host-ent-address (get-host-by-name hostname))
      nil))

(defun thread-server()
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-bind socket 
