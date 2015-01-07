(in-package #:std.env)

;;--------------------------------------------------------------------------mac
(defmacro make-env-macro(host)
  `(defmacro ,host (&body body)
    (let ((stream (connection-stream (connection (lookup (make-env-dir) ',host)))))
      `(progn
         (print ',@body ,stream)
         (read ,stream)))))

;;--------------------------------------------------------------------------mac
(defmacro env (&key name type locator connection test attributes)
  (let* ((dir (make-env-dir))
         (host (intern (string-upcase name)))
         (old-dir-entry (lookup dir name)))
    (if old-dir-entry
        (progn
          (release (connection old-dir-entry))
          `(modify (make-env-dir)
                   (make-env-dir-entry :name (string-upcase ,name)
                                       :type ,type
                                       :locator (loop for i in ,locator collect
                                                     (apply 'make-env-location-description i))
                                       :connection (apply 'make-connection ,connection)
                                       :test ,test
                                       :attributes ,attributes)))
        (progn
          `(progn
             (make-env-macro ,host)
             (register (make-env-dir)
                     (make-env-dir-entry :name (string-upcase ,name)
                                         :type ,type
                                         :locator (loop for i in ,locator collect
                                                     (apply 'make-env-location-description i))
                                         :connection (apply 'make-connection ,connection)
                                         :test ,test
                                         :attributes ,attributes)))))))


;;--------------------------------------------------------------------------mac
(defmacro defenv (name (&key (host nil host-supplied-p) (port 9999)) &body body)
  (let ((me (string-upcase name))
        (host-name (if host-supplied-p
                       (string-upcase host)
                       (string-upcase name))))
    `(env
         :name ,me
         :type "host"
         :locator '((:signature ,(concatenate 'string
                                              "com.lite3d."
                                              (string-downcase (string me)))
                     :signature-type '(cdgw &rest body)
                     :address '(:host ,host-name :port ,port)))
         :connection '(:host ,host-name :port ,port)
         :test ,(concatenate 'string "connected-to-" me)
         :attributes ,@body)))

;; (defclass  env-root ()
;;   ((msg-transport :initarg :msg-transport
;;          :initform 
;;          :accessor
;;          :reader
;;          :writer
;;          :type
;;          :allocation  :class
;;          :documentation "Access to the message trasnport service")
   
;; )))
;;   (:documentation "A set of env-directory-entries."))

;; (defun env-root ()
;;   (make-instance 'env-root))



;; (env
;;  '((:name (guid "msg-transport")
;;     :type (:key pair :note "hey")
;;     :locator '((:signature "std.service"
;;                 :signature-type '(foo object &key name)
;;                 :address "localhost"))))
;; (env
;;  '((:name (guid "agent-dir")
;;     :type (:key pair :another thing)
;;     :locator '((:signatrue "std.service"
;;                 :signature-type '(foo object &key name)
;;                 :address "localhost")))))
;; (env
;;  '((:name (guid "service-dir")
;;     :type (:key pair :ok yup)
;;     :locator '((:signatrue "std.service"
;;                 :signature-type '(foo object &key name)
;;                 :address "localhost")))))

;; (env-root
;;  '((msg-transport)
;;    (agent-dir)
;;    (service-dir)))
        

;; (defenv einstein() "done")
;; (einstein (+ 1 2 3))
;; (REGISTER (MAKE-ENV-DIR)
;;           (MAKE-ENV-DIR-ENTRY :NAME (STRING-UPCASE "EINSTEIN") :TYPE "host"
;;                               :LOCATOR
;;                               (LOOP FOR I IN '((:SIGNATURE
;;                                                 "com.lite3d.einstein"
;;                                                 :SIGNATURE-TYPE
;;                                                 '(CDGW &REST BODY) :ADDRESS
;;                                                 '(:HOST "EINSTEIN" :PORT
;;                                                   9999)))
;;                                  COLLECT (APPLY
;;                                           'MAKE-ENV-LOCATION-DESCRIPTION I))
;;                               :CONNECTION
;;                               (APPLY 'MAKE-CONNECTION
;;                                      '(:HOST "EINSTEIN" :PORT 9999))
;;                               :TEST "connected-to-EINSTEIN" :ATTRIBUTES "done"))
;; (REGISTER (MAKE-ENV-DIR)
;;           (MAKE-ENV-DIR-ENTRY :NAME (STRING-UPCASE "EINSTEIN") :TYPE "host"
;;                     :LOCATOR
;;                     (LOOP FOR I IN '((:SIGNATURE
;;                                       "com.lite3d.einstein"
;;                                       :SIGNATURE-TYPE
;;                                       '(CDGW &REST BODY) :ADDRESS
;;                                       '(:HOST "EINSTEIN" :PORT
;;                                         9999)))
;;                        COLLECT (APPLY
;;                                 'MAKE-ENV-LOCATION-DESCRIPTION I))
;;                     :CONNECTION
;;                     (APPLY 'MAKE-CONNECTION
;;                            '(:HOST "EINSTEIN" :PORT 9999))
;;                     :TEST "connected-to-EINSTEIN" :ATTRIBUTES "done"))

;; (LOOP FOR I IN '((:SIGNATURE
;;                   "com.lite3d.einstein"
;;                   :SIGNATURE-TYPE
;;                   '(CDGW &REST BODY) :ADDRESS
;;                   '(:HOST "EINSTEIN" :PORT
;;                     9999)))
;;    COLLECT (APPLY
;;             'MAKE-ENV-LOCATION-DESCRIPTION I))

;; (APPLY 'MAKE-CONNECTION
;;        '(:HOST "EINSTEIN" :PORT 9999))

;; (ENV :NAME "EINSTEIN" :TYPE "host" :LOCATOR
;;      ((:SIGNATURE "com.lite3d.einstein" :SIGNATURE-TYPE '(CDGW &REST BODY)
;;         :ADDRESS '(:HOST "EINSTEIN" :PORT 9999)))
;;      :CONNECTION (:HOST "EINSTEIN" :PORT 9999) :TEST "connected-to-EINSTEIN"
;;      :ATTRIBUTES '(" "))

;; (defparameter *client* (apply 'std.env::make-connection '(:HOST "EINSTEIN" :PORT 9999)))
;; (einstein (+ 1 2))
;; Every time we make/create an enviroment we essentially register an env-dir-entry with the env-dir.
;;

;; (register (env-dir)
;;           (make-env-dir-entry
;;            :name (intern "einstein")
;;            :type "dummy"
;;            :locator (list (make-env-location-description
;;                            :signature-type "dummy"
;;                            :signature "dummy"
;;                            :address "dummy")
;;                           (make-env-location-description
;;                            :signature-type "dummy"
;;                            :signature "dummy"
;;                            :address "dummy"))
;;            :connection (make-connection :host "einstein")
;;            :attributes "dummmy"))

;;(make-env 'einstein)

;; (einstein (+ 1 2 3))

;; (register (env-dir)
;;           (make-env-dir-entry
;;            :name (intern "einstein")
;;            :type "dummy"
;;            :locator (list (make-env-location-description
;;                            :signature-type "dummy"
;;                            :signature "dummy"
;;                            :address "dummy")
;;                           (make-env-location-description
;;                            :signature-type "dummy"
;;                            :signature "dummy"
;;                            :address "dummy"))
;;            :connection (make-connection :host "einstein")
;;            :attributes "dummmy"))

;; sample useage pattern

;; (env cdgw (:port 9999)
;;   (env node1() "node1")
;;   (env node2() "node2")
;;   (env node3() "node3")
;;   (env node4() "node4"))




