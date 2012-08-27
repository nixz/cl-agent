;;;;===========================================================================
 ;; @file   org.asd
 ;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
 ;; @date   Mon Mar 22 09:56:40 2010
 ;; 
 ;; @brief 
;;;;===========================================================================        

(defpackage #:org-asd (:use #:asdf #:cl))
(in-package :org-asd)

(defsystem org
  :name        "org"
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :maintainer  "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "GPL"
  :description ""
  :depends-on ("org.fipa" "org.math" "org.io.joystick")
  :components ((:file "package")))
               ;; (:system "fipa"
               ;;          :components ((:file "package")))
               ;; (:system "math"
               ;;          :components ((:file "package")
               ;;                       (:file "matrix" :depends-on ("package"))))))


