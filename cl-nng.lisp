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

(defmacro def ((lname &key cname (kind nil)) &rest args)
  "Define an higher-level nng binding, wrapping cffi binding of same name.
:KIND may be:
- nil, in which case a simple binding is created;
- ALLOC, allocating a stack pointer on stack and passing as first arg;
- CHECK, wrapping the call in a return value check"
  (let* ((lstring (symbol-name lname))
	 (cstring (or cname (concatenate 'string "%" lstring)))
	 (binding
	  (or (find-symbol cstring :nng)
	      (error "defnng: no C binding named ~A in library." cstring ))))
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




