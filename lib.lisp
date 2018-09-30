(in-package #:cl-nng)

(cffi:define-foreign-library libnng
  (unix (:or "linbnng.so.1.0.0" "libnng.so.1" "libnng"))
  (t (:default "libnng")))

(use-foreign-library libnng)

