;;;;===========================================================================
 ;; @file   org.io.asd
 ;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
 ;; @date   Mon Apr 19 11:10:37 2010
 ;; 
 ;; @brief 
;;;;===========================================================================        

(defpackage #:org.io-asd (:use #:asdf #:cl))
(in-package :org.io-asd)

(defsystem org.io
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :maintainer  "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "GPL"
  :description ""
  :depends-on ("cl-opengl" "clx" "org.io.joystick")
  :components ((:file "package")
               (:file "window" :depends-on ("package"))))
