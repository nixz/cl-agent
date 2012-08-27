;;;;===========================================================================
 ;; @file   org.fipa.asd
 ;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
 ;; @date   Tue Feb 23 02:51:05 2010
 ;; 
 ;; @brief 
;;;;===========================================================================        

(defpackage #:org.fipa-system (:use #:asdf #:cl))
(in-package :org.fipa-system)

(defsystem org.fipa
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :maintainer  "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "GPL"
  :description ""
  :depends-on ("org.fipa.std")
  :components ((:file "package")))
