;;;; cl-nng.lisp

(in-package #:cl-nng)

(define-condition nng-error (error)
  ((errno :initarg errno :reader errno)))
;;==============================================================================
;; Functions that return an errno are wrapped here, generating an error.
;;
(defmacro check (form)
  `(let ((errno ,form))
     (when (plusp errno)
       (error 'nng-error :errno errno))
     errno))
(export 'check)
;;==============================================================================
;; Allocators in nng follow the same pattern.  A foreign pointer is allocated
;; and filled in by the nng_xxx_alloc foreign function.
;;
;; Using prefix and name create an allocator.  For instance,
;; (allocator http req url) will generate
;; (defun req-alloc (url)
;;    (with-foreign-object (ptr :pointer)
;;        (check (nng::%http-req-alloc ptr url))
;;        (mem-ref ptr :pointer)))
(defmacro def-alloc-prim (lisp-name c-name &rest args)
  `(progn
     (defun ,lisp-name ,args
       (with-foreign-object (ptr :pointer)
	 (check (,c-name ptr ,@args))
	 (mem-ref ptr :pointer)))
     (export ',lisp-name)))
(export 'def-alloc-prim)
#||
(defmacro def-alloc ((subsystem name &optional suffix)  (&rest args))
  (let ((lisp-name  (concatenate 'string (symbol-name name) "-ALLOC")))
    (when suffix
      (setf lisp-name
	    (concatenate 'string lisp-name "-" (symbol-name suffix))))
    (let* ((binding-name 
	    (concatenate 'string "%" (symbol-name subsystem) "-" lisp-name))
	   (lisp-sym (intern lisp-name))
	   (binding-sym (find-symbol binding-name :nng))) 
      (unless binding-sym
	(error "~A: no C binding named ~A" lisp-name binding-name))
      `(progn
	 (defun ,lisp-sym ,args
	   (with-foreign-object (ptr :pointer)
	     (check (,binding-sym ptr ,@args))
	     (mem-ref ptr :pointer)))
	 (export ',lisp-sym)))))
||#
(defmacro def-alloc ((subsystem &key suffix (name1 "ALLOC"))  (&rest args))
  (let ((lisp-name  (concatenate 'string (symbol-name subsystem) "-" name1)))
    (when suffix
      (setf lisp-name
	    (concatenate 'string lisp-name "-" (symbol-name suffix))))
    (let* ((binding-name 
	    (concatenate 'string "%" lisp-name))
	   (lisp-sym (intern lisp-name))
	   (binding-sym (find-symbol binding-name :nng-cffi))) 
      (unless binding-sym
	(error "~A: no C binding named ~A" lisp-name binding-name))
      `(def-alloc-prim ,lisp-sym ,binding-sym ,@args)
)))

(export 'def-alloc)

;; regular, unchecked
(defmacro def-plain ((subsystem name) (&rest args))
  (let* ((lisp-name (concatenate 'string (symbol-name subsystem)
				 "-" (symbol-name name)))
	 (binding-name (concatenate ))
	 (lisp-sym (intern topic-name))
	 (binding-sym (find-symbol binding-name :nng-cffi)))
    (unless binding-sym
      (error "~A: no C binding named ~A" topic-name binding-name))
    `(progn
       (declaim (inline ,lisp-sym))
       (defun ,lisp-sym ,args
	 (,binding-sym ,@args))
       (export ',lisp-sym))))
(export 'def-plain)
;; regular, unchecked
(defmacro def-check ((subsystem topic name) (&rest args))
  (let* ((topic-name  (concatenate 'string (symbol-name topic)
				  "-"(symbol-name name) ))
	 (binding-name 
	   (concatenate 'string "%" (symbol-name subsystem) "-" topic-name))
	 (lisp-sym (intern topic-name))
	 (binding-sym (find-symbol binding-name :nng-cffi)))
    (unless binding-sym
      (error "~A: no C binding named ~A" topic-name binding-name))
    `(progn
       ;;(declaim (inline ,lisp-sym))
       (defun ,lisp-sym ,args
	 (check (,binding-sym ,@args)))
       (export ',lisp-sym))))

(export 'def-check)
(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (print p)
    (or (not p) (= 0 p))))


(defmacro def ((lname &key (cname lname)(kind nil)) &rest args)
  "Define an higher-level nng binding, wrapping cffi binding of same name.
:KIND may be:
- nil, in which case a simple binding is created;
- ALLOC, allocating a stack pointer on stack and passing as first arg;
- CHECK, wrapping the call in a return value check"
  (let* ((lstring (symbol-name lname))
	 (binding
	  (or (find-symbol lstring :nng-cffi)
	      (error "defnng: no C binding named ~A in nng-cffi." cname ))))
    (unless kind
      (let ((p (mismatch  "-ALLOC" lstring :from-end T)))
	(when (or (not p) (= 0 p))
	  (setf kind 'ALLOC))))
    
    (case kind
      (ALLOC `(progn
		(defun ,lname ,args
		  (with-foreign-object (ptr :pointer)
		    (check (,binding ptr ,@args))
		    (mem-ref ptr :pointer)))
		(export ',lname)))
      (CHECK `(progn
		;;(declaim (inline ,lisp-sym))
		(defun ,lname ,args
		  (check (,binding ,@args)))
		(export ',lname)))
      (T `(progn
	    (declaim (inline ,lname))
	    (defun ,lname ,args
	      (,binding ,@args))
	    (export ',lname))))))




