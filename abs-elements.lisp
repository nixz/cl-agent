;; environment
;; -----------
;; (service                      "A service provided for agents and other services."                                            org.fipa.std.env Mandatory)
;; (service-name                 "A unique identifier of a particular service."                                             org.fipa.std.service.service-name Mandatory)
;; (service-address              "A service-type specific string containing transport addressing information."              org.fipa.std.service.service-address Mandatory)
;; (service-type                 "A key-value tuple describing the type of a service."                                      org.fipa.std.service.service-type Mandatory)
;; (service-locator              "A service-locator consists of the set of service-location-descriptions used to access a service." org.fipa.std.service.service-locator Mandatory)
;; (service-signature            "A identifier that describes the binding signature for a service."                         org.fipa.std.service.service-signature Mandatory)
;; (service-attributes           "A set of properties associated with a service by inclusion in its service-directory-entry."   org.fipa.std.service.service-attributes Optional)

;; (service-root                 "A set of service-directory-entries."                                                      org.fipa.std.service.service-root Mandatory)
;; (service-dir                  "A directory service for registering and discovering services."                            org.fipa.std.service.service-dir Mandatory)
;; (service-dir-entry            "A composite entity containing the service-name, service-locator, and service-type of a service."      org.fipa.std.service.service-dir.service-directory-entry Mandatory)

;; (service-location-description "A key-value-tuple containing a signature-type a service-signature and service-address."       org.fipa.std.service.service-location-description Mandatory)

(Action-status             "A status indication delivered by a service showing the success or failure of an action."                            org.fipa.std.service.action-status Mandatory)

;; (agent-dir                 "A service providing a shared information repository in which agent-dir-entries may be stored and queried"       org.fipa.std.service.agent-dir Mandatory)
;; (agent-dir-entry           "A composite entity containing the name, agent-locator, and agent-attributes of an agent."                       org.fipa.std.service.agent-dir.agent-dir-entry Mandatory)




(Encoding-service          "A service that encodes a message to and from a payload."                                                        org.fipa.std.service.encoding-service Mandatory)

(Explanation               "An encoding of the reason for a particular action-status."                                                      org.fipa.std.service.explanation Optional)

(Signature-type            "A key-value tuple describing the type of service-signature."                                                        org.fipa.std.service.signature-type)

;; (iac         "A service that supports the sending and receiving of transport-messages between agents."                         org.fipa.std.service.iac Mandatory)
;; (agent-locator             "An agent-locator consists of the set of transport-descriptions used to communicate with an agent." org.fipa.std.service.iac.agent-locator Mandatory)
;; (transport                 "A transport is a particular data delivery service supported by a given iac."                       org.fipa.std.service.iac.transport Mandatory)
;; (transport-type            "A transport-type describes the type of transport associated with a transport-specific-address."    org.fipa.std.service.iac.transport-type Mandatory)
;; (transport-specific-address "A transport address specific to a given transport-type"                                           org.fipa.std.service.iac.transport-specific-address Mandatory)
;; (transport-specific-property "A transport-specific-property is a property associated with a transport-type."                   org.fipa.std.service.iac.transport-specific-property Optional)
;; (transport-description     "A transport-description is a self describing structure containing a transport-type, a transport-specific-address and zero or more transport-specific-properties."
;;                                                                                                                                org.fipa.std.service.iac.transport-description Mandatory)



;; agent
;; -----
;; (agent           "A computational process that implements the autonomous, communicating functionality of an application." org.fipa.std.agent Mandatory)
;; (agent-attrib "A set of properties associated with an agent by inclusion in its agent-directory-entry."                   org.fipa.std.agent.agent-attrib Optional)

;; acl
;; ---
;; (acl "A language with a precisely defined syntax semantics and pragmatics, which is the basis of communication between independently designed and developed agents." org.fipa.std.acl Mandatory)
;; ACL standard [FIPA00061]
;; ------------------------
;; (performative "Denotes the type of the communicative act of the ACL message" See [FIPA00037])
;; (sender       "Denotes the identity of the sender of the message, that is, the name of the agent of the communicative act.")
;; (receiver     "Denotes the identity of the intended recipients of the message.")
;; (reply-to "This parameter indicates that subsequent messages in this conversation thread are to be directed to the agent named in the reply-to parameter, instead of to the agent named in the sender parameter.")
;; (content  "Denotes the content of the message; equivalently denotes the object of the action. The meaning of the content of any ACL message is intended to be interpreted by the receiver of the message. This is particularly relevant for instance when referring to referential expressions, whose interpretation might be different for the sender and the receiver.")
;; (language "Denotes the language in which the content parameter is expressed." See [FIPA00007])
;; (encoding "Denotes the specific encoding of the content language expression." See [FIPA00007])
;; (ontology "Denotes the ontology(s) used to give a meaning to the symbols in the content expression.")
;; (protocol "Denotes the interaction protocol that the sending agent is employing with this ACL message." See [FIPA00025])
;; (conversation-id "Introduces an expression (a conversation identifier) which is used to identify the ongoing sequence of communicative acts that together form a conversation.")
;; (reply-with "Introduces an expression that will be used by the responding agent to identify this message.")
;; (in-reply-to "Denotes an expression that references an earlier action to which this message is a reply.")
;; (reply-by "Denotes a time and/or date expression which indicates the latest time by which the sending agent would like to receive a reply.")



agent-name
----------
(Agent-name "An opaque, non-forgeable token that uniquely identifies an agent." org.fipa.std.agent-name Mandatory)

message
--------
(Message          "A unit of communication between two agents. A message is expressed in an agent-communication-language, and encoded in an encoding-representation." org.fipa.std.message Mandatory)
(Content          "Content is that part of a message (communicative act) that represents the domain dependent component of the communication."                        org.fipa.std.message.content Mandatory)
(Content-language "A language used to express the content of a communication between agents."                                                                         org.fipa.std.message.content-language Mandatory)
(Ontology         "A set of symbols together with an associated interpretation that may be shared by a community of agents or software. An ontology includes a vocabulary of symbols referring to objects in the subject domain, as well as symbols referring to relationships that may be evident in the domain."                                                                                                                                org.fipa.std.message.ontology Optional)

encoding-service
----------------
(Encoding-representation "A way of representing an abstract syntax in a particular concrete syntax. Examples of possible representations are XML, FIPA Strings, and serialized Java objects." org.fipa.std.encoding-service.encoding-representation Mandatory)

;; transport-message
;; -----------------
;; (transport-msg "The object conveyed from agent to agent. It contains the transport-description for the sender and receiver or receivers, together with a payload containing the message."                        org.fipa.std.transport-msg Mandatory)
;; (envelope "That part of a transport-msg containing information about how to send the message to the intended recipient(s). May also include additional information about the message encoding, encryption, etc." org.fipa.std.transport-msg.envelope Mandatory)
;; (payload  "A message encoded in a manner suitable for inclusion in a transport-msg."                                                                                                                             org.fipa.std.transport-msg.payload Mandatory)

