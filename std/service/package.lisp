(in-package #:cl-user)

(defpackage #:org.fipa.std.service
  (:nicknames #:service)
  (:use #:cl
        #:org.fipa.std.service.msg-transport-service)
  (:export #:service
           #:service-name
           #:service-address
           #:service-type
           #:service-locator
           #:service-signature
           #:service-attributes
           #:service-root
           #:service-dir
           #:service-dir-entry
           #:service-location-description
           #:agent-dir
           #:agent-dir-entry
           #:register
           #:deregister
           #:modify
           #:lookup))