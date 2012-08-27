;;;;===========================================================================
 ;; @file   org.math.asd
 ;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
 ;; @date   Fri Apr 16 06:31:56 2010
 ;; 
 ;; @brief 
;;;;===========================================================================        

(defpackage #:org.math-asd (:use #:asdf #:cl))
(in-package :org.math-asd)

(defsystem org.math
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :maintainer  "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "GPL"
  :description ""
  :components ((:file "package")
               (:file "matrix" :depends-on ("package"))))
