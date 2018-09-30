;;;; package.lisp

(defpackage #:cl-nng
  (:use #:cffi #:cl)
  (:nicknames #:nng)
  (:shadow #:close)
  (:export
   #:version)) 

(defpackage #:nng-cffi
  (:use #:cffi #:cl)
  (:shadow #:close #:listen #:random)
)
