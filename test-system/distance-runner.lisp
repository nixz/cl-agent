(defmacro fork (&rest code)
  `(list ,@(loop for var in code collect `(make-thread #'(lambda () ,var)))))

;;; macro for join
(defmacro join (thread-list)
  `(mapcar #'join-thread ,thread-list))

(defun distance-runner(n)
