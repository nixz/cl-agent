(use-package :cffi)

(define-foreign-library libssl
    (:unix (:or "/usr/lib/libssl.so" "libssl.so"))
    (t (:default "libssl.so")))

(use-foreign-library libssl)

(cffi:defcfun ("SHA1" SHA1) :int
  (d :string)
  (n :int)
  (md :int))

