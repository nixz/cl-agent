;;; This file tests the LISTEN functionality of sockets.  According to
;;; ANSI, CL:LISTEN:
;;;
;;;   Returns true if there is a character immediately available from
;;;   input-stream; otherwise, returns false. On a non-interactive
;;;   input-stream, listen returns true except when at end of
;;;   file[1]. If an end of file is encountered, listen returns false.
;;;
;;; If the test was for SBCL, why is there all this OpenMCL stuff?  So
;;; I could verify the behavior I expect against another
;;; implementation.

(defpackage #:nocandy-user
  (:use #:common-lisp))

(in-package #:nocandy-user)

;;;; SBCL and OpenMCL portability

;;; This stuff is mostly taken from SLIME, specifically, swank-sbcl
;;; and swank-openmcl.

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-bsd-sockets)
  (require 'sb-introspect)
  (require 'sb-posix))

;; setup-server (418)
(defun spawn (fn &key name)
  #+sbcl (sb-thread:make-thread fn :name name)
  #+openmcl (ccl:process-run-function (or name "Anonymous (nocandy)") fn))

#+sbcl
(defun resolve-hostname (name)
  (car (sb-bsd-sockets:host-ent-addresses
        (sb-bsd-sockets:get-host-by-name name))))

;; setup-server (418)
(defun create-socket (host port)
  #+sbcl (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
				      :type :stream
				      :protocol :tcp)))
	   (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
	   (sb-bsd-sockets:socket-bind socket (resolve-hostname host) port)
	   (sb-bsd-sockets:socket-listen socket 5)
	   socket)
  #+openmcl (ccl:make-socket :connect :passive :local-port port 
			     :local-host host :reuse-address t))

;; This is not in swank.
(defun connect-to-server (host port &key
			       (external-format :iso-latin-1-unix)
			       (buffering :full))
  #+sbcl (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
				      :type :stream :protocol :tcp)))
	   (sb-bsd-sockets:socket-connect
	    socket (resolve-hostname host) port)
	   (make-socket-io-stream socket external-format buffering))
  #+openmcl (declare (ignore external-format buffering))
  #+openmcl (ccl:make-socket :remote-host host :remote-port port))

#+sbcl
(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case 
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

#+sbcl
(defun find-external-format (coding-system)
  (ecase coding-system
    (:iso-latin-1-unix :iso-8859-1)
    (:utf-8-unix :utf-8)
    (:euc-jp-unix :euc-jp)))

#+sbcl
(defun make-socket-io-stream (socket external-format buffering)
  (let ((ef (find-external-format external-format)))
    (sb-bsd-sockets:socket-make-stream socket
                                       :output t
                                       :input t
                                       :element-type 'character
                                       :buffering buffering
                                       #+sb-unicode :external-format 
                                       #+sb-unicode ef
                                       )))

;; accept-authenticated-connection (444)
(defun accept-connection (socket &key 
			  (external-format :iso-latin-1-unix)
			  (buffering :full))
  #+sbcl (make-socket-io-stream (accept socket) external-format buffering)
  #+openmcl (declare (ignore buffering))
  #+openmcl (progn
	      (assert (eq external-format :iso-latin-1-unix))
	      (ccl:accept-connection socket :wait t)))

#+sbcl
(defun socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor socket))
    (file-stream (sb-sys:fd-stream-fd socket))))

(defun close-socket (socket)
  #+sbcl (progn
	   (sb-sys:invalidate-descriptor (socket-fd socket))
	   (sb-bsd-sockets:socket-close socket))
  #+openmcl (close socket))

#-(or sbcl openmcl)
(dolist (proc '(spawn create-socket accept-connection
		connect-to-server close-socket))
  (setf (symbol-function proc)
	  (lambda (&rest ignore)
	    (declare (ignore ignore))
	    (error "~S not implemented" proc))))

(defparameter *loopback-interface* "127.0.0.1")

(defparameter *test-port*	;2793 here
  (reduce #'+ "sbcl-listen-socket-test.lisp" :key #'char-code))

(defun test-socket-cl-listen ()
  (let ((server (create-socket *loopback-interface* *test-port*))
	connection service spin-lock)
    (unwind-protect
      (progn
	(spawn (lambda ()		;client thread
		 (setf connection (connect-to-server *loopback-interface*
						     *test-port*))
		 (format connection "s~%")
		 (close connection)
		 (setf spin-lock t)))
	(setf service (accept-connection server))
	(assert (equal (read-char service nil) #\s))
	(read-char service nil)
	(loop until spin-lock) ;why I'm not a threads kind of guy
	(assert (progn 1 (eq nil (listen service))))
	(read-char service nil)
	(assert (progn 2 (eq nil (listen service)))))
      (when service (close service))
      (close-socket server))))

;;; line 1590 in fd-stream-misc-routine in src/code/fd-stream.lisp
;;; (0.9.10) is the underlying select(2) call for listen, if I followed
;;; the code correctly.
;;;
;;; delete assert 1 -- 2 succeeds
