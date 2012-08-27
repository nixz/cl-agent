;;;;===========================================================================
 ;; @file   org.io.joystick.asd
 ;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
 ;; @date   Mon Apr 19 10:43:59 2010
 ;; 
 ;; @brief 
;;;;===========================================================================        

(defpackage #:org.io.joystick-asd (:use #:asdf #:cl))
(in-package :org.io.joystick-asd)

(defsystem org.io.joystick
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :maintainer  "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "GPL"
  :description ""
  :components ((:file "package")
               (:file "stick-transfers" :depends-on ("package"))
               (:file "joystick" :depends-on ("package" "stick-transfers"))))

