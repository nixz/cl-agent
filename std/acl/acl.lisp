(in-package #:org.fipa.std.acl

(defclass  acl ()
  ((performative :initarg :performative
         :initform (error ":performative must be specified")
         :accessor performative
         :allocation  :instance
         :documentation "Denotes the type of the communicative act of
         the ACL message")
   (sender :initarg :sender
         :initform nil
         :accessor sender
         :allocation  :instance
         :documentation "Denotes the identity of the sender of the
         message, that is, the name of the agent of the communicative
         act.")
   (receiver :initarg :receiver
         :initform nil
         :accessor receiver
         :allocation  :instance
         :documentation "Denotes the identity of the intended
         recipients of the message.")
   (reply-to :initarg :reply-to
         :initform nil
         :accessor reply-to
         :documentation "This parameter indicates that subsequent
         messages in this conversation thread are to be directed to
         the agent named in the reply-to parameter, instead of to the
         agent named in the sender parameter.")
   (content :initarg :content
         :initform nil content
         :allocation  :instance
         :documentation"Denotes the content of the message;
         equivalently denotes the object of the action. The meaning of
         the content of any ACL message is intended to be interpreted
         by the receiver of the message. This is particularly relevant
         for instance when referring to referential expressions, whose
         interpretation might be different for the sender and the
         receiver.")
   (language :initarg :language
         :initform nil
         :accessor language
         :allocation  :instance
         :documentation "Denotes the language in which the content
         parameter is expressed.")
   (encoding :initarg :encoding
         :initform nil
         :accessor encoding
         :allocation  :instance
         :documentation "Denotes the specific encoding of the content
         language expression.")
   (ontology :initarg :ontology
         :initform nil
         :accessor ontology
         :allocation  :instance
         :documentation "Denotes the ontology(s) used to give a
         meaning to the symbols in the content expression.")
   (protocol :initarg :protocol
         :initform nil
         :accessor protocol
         :allocation  :instance
         :documentation "Denotes the interaction protocol that the
         sending agent is employing with this ACL message.")
   (conversation-id :initarg :conversation-id
         :initform nil
         :accessor conversation-id
         :allocation  :instance
         :documentation "Introduces an expression (a conversation
         identifier) which is used to identify the ongoing sequence of
         communicative acts that together form a conversation.")
   (reply-with :initarg :reply-with
         :initform nil
         :accessor reply-with
         :allocation  :instance
         :documentation "Introduces an expression that will be used by
         the responding agent to identify this message.")
   (in-reply-to :initarg :in-reply-to
         :initform nil
         :accessor in-reply-to
         :allocation  :instance
         :documentation "Denotes an expression that references an
         earlier action to which this message is a reply.")
   (reply-by :initarg :reply-by
         :initform nil
         :accessor reply-by
         :allocation  :instance
         :documentation "Denotes a time and/or date expression which
         indicates the latest time by which the sending agent would
         like to receive a reply."))
  (:documentation "Agent Communication Language: A language with a
  precisely defined syntax semantics and pragmatics, which is the
  basis of communication between independently designed and developed
  agents."))

