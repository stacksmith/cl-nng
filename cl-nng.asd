;;;; cl-nng.asd
(asdf:load-system '#:cffi-grovel)
(asdf:defsystem #:cl-nng
  :description "Describe cl-nng here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria :cffi :cffi-libffi)
  :components ((:file "package")
	     
	       ;;	       (:file "c-api")
	      
	       ;;(cffi-grovel:grovel-file "nng-grovel" )
	       (:file "lib")
	       (:file "cl-nng")
	       (:file "nng-cffi")
	       (:file "nng-http")
	       (:file "nng-main")

	       (:file "demo/http-client/http-client")

	       ))
