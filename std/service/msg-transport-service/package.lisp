(in-package #:cl-user)
(defpackage #:std.service.msg-transport-service
  (:nicknames #:mts)
  (:use #:cl)
  (:export  #:msg-transport-service
            #:agent-locator
            #:transport
            #:transport-description
            #:transport-type
            #:transport-specific-address
            #:transport-specific-property))
