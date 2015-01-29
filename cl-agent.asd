;;;;===========================================================================
 ;; @file   cl-agent.asd
 ;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
 ;; @date   Wed Mar 24 03:43:33 2010
 ;;
 ;; @brief This package is an implementation of FIPA abstract architecture.
;;;;===========================================================================

(defpackage #:cl-agent-asd (:use #:asdf #:cl))
(in-package :cl-agent-asd)

(defsystem cl-agent
  :name        "cl-agent"
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :maintainer  "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "BSD"
  :description "This package is an implementatio of FIPA abstract architecture"
  :depends-on ("cl-opengl" "clx")
  :components
  ((:module "std"
            :components
            ((:module "env"
                      :components
                      ((:file "package")
                       (:file "env-repl-server"          :depends-on ("package"))
                       (:file "env-location-description" :depends-on ("package"))
                       (:file "env-repl-con"             :depends-on ("env-repl-server"))
                       (:file "env-dir-entry"            :depends-on ("env-location-description"
                                                                      "env-repl-con"))
                       (:file "env-dir"                  :depends-on ("env-dir-entry"))
                       (:file "env"                      :depends-on ("env-dir"))))))))
