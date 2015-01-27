(in-package #:cl-user)

(defpackage #:cl-agent
  (:use #:cl
        #:org))

(defpackage #:org
  (:use #:cl
        #:org.fipa))

(defpackage #:org.fipa
  (:use #:cl
        #:org.fipa.std))


(defpackage #:org.fipa.std.acl
  (:nicknames #:std-acl)
  (:use #:cl)
  (:export
   #:acl
   #:performative
   #:sender
   #:receiver
   #:reply-to
   #:content
   #:language
   #:encoding
   #:ontology
   #:protocol
   #:conversation-id
   #:reply
   #:in-reply-to
   #:reply-by))

(defpackage #:org.fipa.std.agent
  (:nicknames #:std-agent)
  (:use #:cl)
  (:export
   ;; Data
   #:agent
   #:agent-attrib))

(defpackage #:org.fipa.std.env
  (:use #:cl 
        #:org.fipa.std.env.iac
        #:sb-bsd-sockets)
;        #:xlib)
  (:export #:defenv
           #:repl-server
           #:env
           #:env-name
           #:env-address
           #:env-type
           #:env-locator
           #:env-signature
           #:env-attributes
           #:env-root
           #:env-dir
           #:env-dir-entry
           #:env-location-description
           #:agent-dir
           #:agent-dir-entry
           #:register
           #:deregister
           #:modify
           #:lookup))
;;   #:window
;;   #:anim))

(defpackage #:org.fipa.std.env.iac
  (:nicknames #:std-iac)
  (:use #:cl)
  (:export
   #:iac-service
   #:agent-locator
   #:transport-description
   #:transport
   #:transport-type
   #:transport-specific-address
   #:transport-specific-property))


(defpackage #:org.fipa.std.msg
  (:nicknames #:fipa-msg)
  (:use #:cl)
  (:export
   ;; Data
   #:msg
   #:content
   #:content-language
   #:ontology))

(defpackage #:org.fipa.std.transport-msg
  (:nicknames #:std-transport-msg)
  (:use #:cl)
  (:export
   ;; Data
   #:transport-msg
   #:payload
   #:envelope))

;; from gamelib library
(defpackage #:org.io
  (:use #:common-lisp #:gl #:xlib)
  (:shadow #:window)
  (:export #:make-window
           #:close))

;; from gamelib library
(defpackage #:org.io.joystick
  (:use :common-lisp)
  (:export #:update-stick
           #:make-stick
           #:simple-transfer
           #:square-transfer
           #:set-transfer
           #:axes
           #:buttons
           #+sb-thread #:continous-poll
           #+sb-thread #:terminate-poll))

(defpackage #:org.math.matrix
  (:use #:cl)
  (:export #:calculate-column-width
           #:correct-near-zero
           #:matrix-add
           #:matrix-apply
           #:matrix-augment
           #:matrix-column
           #:matrix-columns
           #:matrix-columnspace
           #:matrix-count-zeros
           #:matrix-determinant
           #:matrix-eigenvalues
           #:matrix-element
           #:matrix-gauss-jordan
           #:matrix-gaussian
           #:matrix-gaussian2
           #:matrix-identity
           #:matrix-inverse
           #:matrix-invertiblep
           #:matrix-lu
           #:matrix-minor
           #:matrix-multiply
           #:matrix-normalize
           #:matrix-nullspace
           #:matrix-ones
           #:matrix-orthogonalp
           #:matrix-qr
           #:matrix-random
           #:matrix-rank
           #:matrix-ref
           #:matrix-row
           #:matrix-row-swap
           #:matrix-rows
           #:matrix-rref
           #:matrix-split
           #:matrix-squarep
           #:matrix-symmetricp
           #:matrix-transpose
           #:matrix-upper-triangularp
           #:matrix-zero
           #:matrixp
           #:print-matrix
           #:print-matrix-four-subspaces
           #:standard-basis-row))
