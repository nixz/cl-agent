;; http://community.livejournal.com/lisp/38356.html

(require 'asdf)
(require 'usocket)

(defconstant +address+ "127.0.0.1"
  "Listening address")

(defconstant +port+ 9300
  "Listening port")

(defconstant +backlog+ 2
  "Queue length")

(defun start-rp-listener ()
  "Start the server, run accept loop."
  (let ((sock (usocket:socket-listen +address+ +port+
                                     :backlog +backlog+
                                     :reuseaddress t)))
    (labels ((rep-loop (s)
              (let ((line (read-line s nil)))
                (if (stringp line)
                  (progn
                    (format s "~A~%" (eval (read-from-string line)))
                    (force-output s)
                    (rep-loop s)))))
             (accept-connection (s)
              (let* ((lsock (usocket:socket-accept s))
                     (cstream (usocket:socket-stream lsock)))
                (sb-thread:make-thread
                  (lambda ()
                    (unwind-protect
                      (rep-loop cstream)
                      (progn
                        (close cstream)
                        (usocket:socket-close lsock))))))
              (accept-connection s)))
      (unwind-protect
        (accept-connection sock)
        (usocket:socket-close sock)))))
