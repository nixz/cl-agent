;;;;---------------------------------------------------------------------------
;;;; utils.lisp
;;;;---------------------------------------------------------------------------

;;;macro to define parallel forks and join
(defmacro ll(op &rest code)
  `(,op ,@(loop for var in code collect `(sb-thread:join-thread (sb-thread:make-thread #'(lambda () ,var))))))

;;; macro for fork
(defmacro fork (&rest code)
  `(list ,@(loop for var in code collect `(sb-thread:make-thread #'(lambda () ,var)))))

;;; macro for join
(defmacro join (thread-list)
  `(mapcar #'sb-thread:join-thread ,thread-list))

;;; Useage:
;; (ll list (* 1 2) (* 3 4) (* 5 6))
;; (join (fork (* 1 2) (* 3 4) (* 5 6)))
;; (time (* (join-thread (make-thread #'(lambda () (+ 2 4))))
;;          (join-thread (make-thread #'(lambda () (- 2 4))))
;;          (join-thread (make-thread #'(lambda () (* 2 4))))
;;          (join-thread (make-thread #'(lambda () (/ 2 4))))))

;;; Thanks to Mikael Jansson for syntatic sugar hash-map
(defun hash (&rest pairs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key h) value))
    h))

;;; Macro version
(defmacro hash (&rest pairs)
  `(let ((h (make-hash-table :test 'equal)))
     ,@(loop for (key value) on pairs by #'cddr collect `(setf (gethash ,key h) ,value))
       h))

;;; Symbol version
(defmacro @# (&rest pairs)
  `(let ((h (make-hash-table :test 'equal)))
     ,@(loop for (key value) on pairs by #'cddr collect `(setf (gethash ,key h) ,value))
       h))

(defmacro {} (&rest pairs)
  `(let ((h (make-hash-table :test 'equal)))
     ,@(loop for (key value) on pairs by #'cddr collect `(setf (gethash ,key h) ,value))
       h))

(defmacro def# (name &rest pairs)
  `(progn
     (defparameter ,name ({} ,@pairs))
     (defun ,name (key) (gethash key ,name))))


(newton (agent0 ()))

;;; from http://frank.kank.net/essays/hash.html {:key1 "something" :key2 "something-else"}
(defun read-separator (str)
 (let
  ((*readtable* (copy-readtable *readtable* nil)))
  (set-macro-character #\, (lambda (stream char)
                            (declare (ignore char) (ignore stream))
                            'break))
  (read str nil)))

(set-macro-character 
 #\{ (lambda (str char)
       (declare (ignore char))
       (let  ((*readtable* (copy-readtable *readtable* nil))
              (keep-going t))
         (set-macro-character 
          #\} (lambda (stream char)
                (declare (ignore char) (ignore stream))
                (setf keep-going nil))
          (set-syntax-from-char  #\, #\Space)
          (let ((pairs (loop
                          for key = (read str nil nil t)
                          while keep-going
                          for value = (read str nil nil t)
                          collect (list key value)))
                (retn (gensym)))
            `(let ((,retn (make-hash-table :test #'equal)))
               ,@(mapcar (lambda (pair)
                           `(setf (gethash ,(car pair) ,retn) ,(cadr pair)))
                         pairs)
               ,retn))))))

(set-pprint-dispatch 'hash-table
 (lambda (str ht)
  (format str "{~{~{~S => ~S~}~^, ~}}"
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
         collect (list key value)))))


;;; Useage:
;; (hash "apple" 1 "ball" 2 "cat" 300)
;; (setf a (@# "apple" 1 "ball" 2 "cat" 300))
(setf a ({} "apple" 1 "ball" 2 "cat" 300))
;; (setf a {"apple"=>1, "ball"=>2, "cat"=>3})
(setf a {"apple" => 1 , "ball" => 2 , "cat"=>3})
(setf a {"test" => 9 , 'foo => 3})
(setf a {'a "apple" 'b "ball"})
(def# a "apple" 10 "ball" 20 "cat" 30)

;; setf* used to set multiple values into the list (setf* (a b) (10 20))
(defmacro setf* (to-list from-list)
  `(multiple-value-setq ,to-list (values ,@from-list)))
