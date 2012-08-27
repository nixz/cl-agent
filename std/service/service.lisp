(in-package #:org.fipa.std.service)

(defclass  service ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor service-name
         :allocation :instance
         :documentation "A unique identifier of a particular service.")
   (address :initarg :address
         :initform (error ":address must be specified")
         :accessor service-address
         :allocation :instance
         :documentation "A service-type specific string containing transport addressing information.")
   (type :initarg :type
         :initform (error ":type must be specified")
         :accessor service-type
         :allocation :instance
         :documentation )
   (locator :initarg :locator
         :initform (error ":locator must be specified")
         :accessor service-locator
         :allocation :instance
         :documentation "A identifier that describes the binding signature for a service.")
   (signature :initarg :signature
         :initform (error ":signature must be specified")
         :accessor service-signature
         :allocation :instance
         :documentation "about-slot")
   (attributes :initarg :attributes
         :accessor service-attributes
         :allocation :instance
         :documentation "A set of properties associated with a service by inclusion in its service-directory-entry.")
   (root :initarg :root
         :initform (error ":root must be specified")
         :accessor service-root
         :allocation :instance
         :documentation "A set of service-directory-entries.")
   (dir :initarg :dir
         :initform (error ":dir must be specified")
         :accessor service-dir
         :allocation :instance
         :documentation "A directory service for registering and discovering services.")
   (dir-entry :initarg :dir-entry
         :initform (error ":dir-entry must be specified")
         :accessor service-dir-entry
         :allocation :instance
         :documentation "A composite entity containing the service-name, service-locator, and service-type of a service.")
   (location-description :initarg :location-description
         :initform (error ":location-description must be specified")
         :accessor service-location-description
         :allocation :instance
         :documentation "A key-value-tuple containing a signature-type a service-signature and service-address."))
  (:documentation  "A service provided for agents and other services."))


;; ;;--------------------------------------------------------------------------mac
;; (defmacro make-service-macro(host)
;;   `(defmacro ,host (&body body)
;;     (let ((stream (connection-stream (connection (lookup (make-service-dir) ',host)))))
;;       `(progn
;;          (print ',@body ,stream)
;;          (read ,stream)))))

;; ;;--------------------------------------------------------------------------mac
;; (defmacro service (&key name type locator connection test attributes)
;;   (let* ((dir (make-service-dir))
;;          (host (intern (string-upcase name)))
;;          (old-dir-entry (lookup dir name)))
;;     (if old-dir-entry
;;         (progn
;;           (release (connection old-dir-entry))
;;           `(modify (make-service-dir)
;;                    (make-service-dir-entry :name (string-upcase ,name)
;;                                        :type ,type
;;                                        :locator (loop for i in ,locator collect
;;                                                      (apply 'make-service-location-description i))
;;                                        :connection (apply 'make-connection ,connection)
;;                                        :test ,test
;;                                        :attributes ,attributes)))
;;         (progn
;;           `(progn
;;              (make-service-macro ,host)
;;              (register (make-service-dir)
;;                      (make-service-dir-entry :name (string-upcase ,name)
;;                                          :type ,type
;;                                          :locator (loop for i in ,locator collect
;;                                                      (apply 'make-service-location-description i))
;;                                          :connection (apply 'make-connection ,connection)
;;                                          :test ,test
;;                                          :attributes ,attributes)))))))


;; ;;--------------------------------------------------------------------------mac
;; (defmacro defservice (name (&key (host nil host-supplied-p) (port 9999)) &body body)
;;   (let ((me (string-upcase name))
;;         (host-name (if host-supplied-p
;;                        (string-upcase host)
;;                        (string-upcase name))))
;;     `(service
;;          :name ,me
;;          :type "host"
;;          :locator '((:signature ,(concatenate 'string
;;                                               "com.lite3d."
;;                                               (string-downcase (string me)))
;;                      :signature-type '(cdgw &rest body)
;;                      :address '(:host ,host-name :port ,port)))
;;          :connection '(:host ,host-name :port ,port)
;;          :test ,(concatenate 'string "connected-to-" me)
;;          :attributes ,@body)))

;; (defclass  service-root ()
;;   ((msg-transport :initarg :msg-transport
;;          :initform 
;;          :accessor
;;          :reader
;;          :writer
;;          :type
;;          :allocation  :class
;;          :documentation "Access to the message trasnport service")
   
;; )))
;;   (:documentation "A set of service-directory-entries."))

;; (defun service-root ()
;;   (make-instance 'service-root))



;; (service
;;  '((:name (guid "msg-transport")
;;     :type (:key pair :note "hey")
;;     :locator '((:signature "org.fipa.std.service"
;;                 :signature-type '(foo object &key name)
;;                 :address "localhost"))))
;; (service
;;  '((:name (guid "agent-dir")
;;     :type (:key pair :another thing)
;;     :locator '((:signatrue "org.fipa.std.service"
;;                 :signature-type '(foo object &key name)
;;                 :address "localhost")))))
;; (service
;;  '((:name (guid "service-dir")
;;     :type (:key pair :ok yup)
;;     :locator '((:signatrue "org.fipa.std.service"
;;                 :signature-type '(foo object &key name)
;;                 :address "localhost")))))

;; (service-root
;;  '((msg-transport)
;;    (agent-dir)
;;    (service-dir)))
        

;; (defservice einstein() "done")
;; (einstein (+ 1 2 3))
;; (REGISTER (MAKE-SERVICE-DIR)
;;           (MAKE-SERVICE-DIR-ENTRY :NAME (STRING-UPCASE "EINSTEIN") :TYPE "host"
;;                               :LOCATOR
;;                               (LOOP FOR I IN '((:SIGNATURE
;;                                                 "com.lite3d.einstein"
;;                                                 :SIGNATURE-TYPE
;;                                                 '(CDGW &REST BODY) :ADDRESS
;;                                                 '(:HOST "EINSTEIN" :PORT
;;                                                   9999)))
;;                                  COLLECT (APPLY
;;                                           'MAKE-SERVICE-LOCATION-DESCRIPTION I))
;;                               :CONNECTION
;;                               (APPLY 'MAKE-CONNECTION
;;                                      '(:HOST "EINSTEIN" :PORT 9999))
;;                               :TEST "connected-to-EINSTEIN" :ATTRIBUTES "done"))
;; (REGISTER (MAKE-SERVICE-DIR)
;;           (MAKE-SERVICE-DIR-ENTRY :NAME (STRING-UPCASE "EINSTEIN") :TYPE "host"
;;                     :LOCATOR
;;                     (LOOP FOR I IN '((:SIGNATURE
;;                                       "com.lite3d.einstein"
;;                                       :SIGNATURE-TYPE
;;                                       '(CDGW &REST BODY) :ADDRESS
;;                                       '(:HOST "EINSTEIN" :PORT
;;                                         9999)))
;;                        COLLECT (APPLY
;;                                 'MAKE-SERVICE-LOCATION-DESCRIPTION I))
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
;;             'MAKE-SERVICE-LOCATION-DESCRIPTION I))

;; (APPLY 'MAKE-CONNECTION
;;        '(:HOST "EINSTEIN" :PORT 9999))

;; (SERVICE :NAME "EINSTEIN" :TYPE "host" :LOCATOR
;;      ((:SIGNATURE "com.lite3d.einstein" :SIGNATURE-TYPE '(CDGW &REST BODY)
;;         :ADDRESS '(:HOST "EINSTEIN" :PORT 9999)))
;;      :CONNECTION (:HOST "EINSTEIN" :PORT 9999) :TEST "connected-to-EINSTEIN"
;;      :ATTRIBUTES '(" "))

;; (defparameter *client* (apply 'org.fipa.std.service::make-connection '(:HOST "EINSTEIN" :PORT 9999)))
;; (einstein (+ 1 2))
;; Every time we make/create an serviceiroment we essentially register an service-dir-entry with the service-dir.
;;

;; (register (service-dir)
;;           (make-service-dir-entry
;;            :name (intern "einstein")
;;            :type "dummy"
;;            :locator (list (make-service-location-description
;;                            :signature-type "dummy"
;;                            :signature "dummy"
;;                            :address "dummy")
;;                           (make-service-location-description
;;                            :signature-type "dummy"
;;                            :signature "dummy"
;;                            :address "dummy"))
;;            :connection (make-connection :host "einstein")
;;            :attributes "dummmy"))

;;(make-service 'einstein)

;; (einstein (+ 1 2 3))

;; (register (service-dir)
;;           (make-service-dir-entry
;;            :name (intern "einstein")
;;            :type "dummy"
;;            :locator (list (make-service-location-description
;;                            :signature-type "dummy"
;;                            :signature "dummy"
;;                            :address "dummy")
;;                           (make-service-location-description
;;                            :signature-type "dummy"
;;                            :signature "dummy"
;;                            :address "dummy"))
;;            :connection (make-connection :host "einstein")
;;            :attributes "dummmy"))

;; sample useage pattern

;; (service cdgw (:port 9999)
;;   (service node1() "node1")
;;   (service node2() "node2")
;;   (service node3() "node3")
;;   (service node4() "node4"))




