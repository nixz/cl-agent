;;;;---------------------------------------------------------------------------
;;;; agent.lisp
;;;;---------------------------------------------------------------------------

(defpackage #:agent
  (:use #:cl #:sb-thread)
  (:export :agent))


(defclass agent()
  ((name  :initarg :name
          :initform (error ":name should be provided")
          :reader agent-name)
   (brain :initarg :brain
          :initform "brain"
          :accessor agent-brain)
   (todo  :initarg :todo
          :initform (error ":todo should be specified")
          :accessor agent-todo)))
   
  
(defun agent()
  "agent function")

(defmethod test (a agent)
  "Hello what is this I say")

(defmethod initialize-instance :before ((a agent))
  )



(defmacro defagent(name :function ))

(defagent alfred (a b)
  :function (lambda () ))



(defagent brenda (:type MECHANIC-TYPE
                        :role "handles special-order cars"))

(defagent calvin ()
  :type WELDER
  :role "is a welding robot")

(defagent dashiel ()
  :type CONTROLLER-TYPE
  :role )


(setf alfred (make-agent :type MECHANIC
                         :role "Handles regular-order cars"))
(setf brenda (make-agent :type MECHANIC
                         :role "handles special-order cars"))
(setf calvin (make-agent :type WELDING-TYPE
                         :role "welding robot"))
(setf dashiel (make-agent :type CONTROLLER
                          :role "coordinating program controlling the plant"))

(agent agent-name (#:cl #:sb-thread)
       " documetnt about agent"
  (iface int-name-0 (paraameters)
         "docmentation about interface"
         (body))
  (iface int-name-1 (parameters)
         "documentation about interface"
         (body))
  (class class-na
  
(alfred)

(defmacro defagent (name b)
  `(defun ,name (,b)))

(defagent agent0 ())
              
(defclass agent0 ((a :accessor a)()))
(defun agent0() t)


(defun thread0 (a b )
  (+ a b))

(defun thread1 (a b )
  (- a b))

(defun thread2 (a b )
  (* a b))


(setf th1 (sb-thread:make-thread #'thread0 2 3)
(setf th2 (sb-thread:make-thread #'thread1 3 2))
(sb-thread:make-thread #'(lambda () 4))


(dolist (var list-form)
  body)

(defmacro with-gensyms(num)
  (dotimes (var num)
    `(var,var (gensym))))
  

  (loop for n in num collect `(var-,n (gensym))))

(defmacro sym (var)
  `(var-,var (gensym)))

(defmacro sym-list (&rest code)
  (let (loop for n from 1 to num collect `(gensym)))


(sym-list (coerce (list-length '(a b c d e f)) 'float))
          
(sym-list 5.0)

(defun nikhil ()
  "this is nikhil")

(defun ll (&rest code)
    (do ((sym-list () (append (list (gensym))) 
         (i init-form step-form)
         (var2 init-form step-form))  
        ((< i (list-length code) return-value)
         (body)))))
         
(defun ll (&rest code)
  (let ((sym-list ())
        (result ()))
    (do ((i 0 (1+ i))
         (thread code (rest thread)))
        ((>= i (list-length code)) result)
      (setf sym-list (append (list (gensym)) sym-list))
      (setf (first sym-list) (make-thread #'(lambda() (eval (first thread)))))
      (setf result (append result (list (join-thread (first sym-list))))))))

(defparameter a 10)
(find-symbol a)


;;;----------------------------------------------------------------------------
;;; Some functions to test the programs in the following form
;;;----------------------------------------------------------------------------

(defun A (m n) 
  (cond ((= 0 m) (+ n 1))
        ((and (> m 0)
              (= 0 n)) (A (- m 1) 1))
        ((and (> m 0)
              (> n 0)) (A (- m 1) (A m (- n 1))))
        (t (error "input error"))))

(defun s (i j)
  (let* ((a (if (< i j)
                (- j i)
                (- i j)))
         (b (/ (+ a 51) 52)))
    (* (- 1 b) (A a a))))


