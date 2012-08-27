(in-package #:org.fipa.std.transport-msg)

(defclass  transport-msg ()
  ((payload :initarg :payload
         :initform (error ":payload must be provided")
         :accessor payload
         :allocation :instance
         :documentation "A message encoded in a manner suitable for inclusion in a transport-message.")
   (envelope :initarg :envelope
         :initform (error ":envelope must be specified")
         :accessor envelope
         :allocation :instance
         :documentation "That part of a transport-message containing information about how to send the message to the intended recipient(s). May also include additional information about the message encoding, encryption, etc."))
  (:documentation "The object conveyed from agent to agent. It contains the transport-description for the sender and receiver or receivers, together with a payload containing the message."))

(defun transport-msg (&key payload envelope)
  (make-instance 'transport-msg :payload payload :envelope envelope))

(defclass  payload ()
  ((data :initarg :data
         :initform (error ":data is required")
         :accessor payload-data
         :allocation  :instance
         :documentation "data that is contained in the payload"))
  (:documentation "A message encoded in a manner suitable for inclusion in a transport-message."))


(defclass  envelope ()
  ((transport-descriptions :initarg :transport-description
         :initform (error "transport descriptions must be specified")
         :accessor transport-descriptions
         :allocation  :instance
         :documentation "describes the type of transport to be used")
   (validity-data :initarg :validity-datainitial-argument
         :accessor validity-data
         :allocation  :instance
         :documentation "such as security keys for message validation")
   (security-data :initarg :security-data
         :accessor security-data
         :allocation  :instance
         :documentation "such as security keys for message encryption and decryption")
   (rouring-data :initarg :routing-data
         :accessor routing-data
         :allocation  :instance
         :documentation "contains routing information")
   (encoding-rep :initarg :encoding-rep
         :initform (error ":encoding-rep has to be specified")
         :accessor encoding-rep
         :allocation :instance
         :documentation "the encoding representation for the payload being transfered"))
  (:documentation "That part of a transport-message containing information about how to send the message to the intended recipient(s). May also include additional information about the message encoding, encryption, etc."))
