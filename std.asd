;;;;===========================================================================
 ;; @file   org.fipa.std.asd
 ;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
 ;; @date   Mon Mar 22 09:40:58 2010
 ;; 
 ;; @brief 
;;;;===========================================================================        

(defpackage #:std-asd (:use #:asdf #:cl))
(in-package :std-asd)

(defsystem org.fipa.std
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :maintainer  "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "GPL"
  :description " "
  :depends-on ("std.env")
  :components ((:file "package")))

