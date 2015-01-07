;;;;===========================================================================
 ;; @file   std.env.asd
 ;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
 ;; @date   Fri Apr 16 07:29:42 2010
 ;;
 ;; @brief
;;;;===========================================================================

(defpackage #:std.env-asd (:use #:asdf #:cl))
(in-package :std.env-asd)

(defsystem std.env
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :maintainer  "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "GPL"
  :description ""
;  :depends-on ("clx")
  :components ((:file "package")
 ;              (:file "gl-test" :depends-on ("package"))
               (:file "env-repl-server":depends-on ("package"))
               (:file "env-location-description":depends-on ("package"))
               (:file "env-repl-con":depends-on ("env-repl-server"))
               (:file "env-dir-entry":depends-on ("env-location-description"
                                                  "env-repl-con"))
               (:file "env-dir" :depends-on ("env-dir-entry"))
               (:file "env" :depends-on ("env-dir"))))
