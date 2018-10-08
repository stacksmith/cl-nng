;;;; cl-nng.lisp

(in-package #:cl-nng)

(define-condition nng-error (error)
  ((errno :initarg :errno :reader errno)))
;;==============================================================================
;; Functions that return an errno are wrapped here, generating an error.
;;
(defmacro check (form)
  `(let ((errno ,form))
     (when (plusp errno)
       (format t "~%NNG-ERROR ~A ~A" errno (strerror errno))
       (error (make-condition 'nng-error :errno errno)))
     errno))
(export 'check)
;;==============================================================================
;; DEF
;;
;; A convenience defining macro.  Creates an exported definition for a lisp
;; binding, optionally with an error check, or an "ALLOC", which (also checks)
;; allocates an nng object using a stack temporary pointer and returns its val.
;;
;; If not alloc or check, creates a thunk to the %xxx binding as an inline.
;;

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
      (SOCKET `(progn
		(defun ,lname ,args
		  (with-foreign-object (psocket :uint32)
		    (check (,binding psocket ,@args))
		    (mem-ref psocket :uint32)))
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
;;==============================================================================
;; DEF-GETOPT
;;
;; A common pattern is a group of getopt functions, which are similar for
;; different kinds of objects.  CAT parameter is an nng protocol designator
;; (e.g. pipe); NAME is the getopt object such as PTR, and CFFITYPE is the
;; corresponding CFFI type, duh.  The above generates a binding named
;;  (DEFUN PIPE-GETOPT-PTR (PIPE OPT) ...
;; The expansion allocates a pointer on the stack, calls %pipe-getopt-ptr
;; filling in the pointer, and returns it.


(defmacro def-getopt (cat name cffitype)
  (let* ((lispname (concatenate 'string (symbol-name cat)
				"-GETOPT-" (symbol-name name)))
	 (lispsym (intern lispname))
	 (cname (concatenate 'string "%" lispname))
	 (csym (intern cname)))
    `(progn
       (defun ,lispsym (,cat opt)
	 (with-foreign-object (data ,cffitype)
	   (check (,csym ,cat opt data))
	   (mem-ref data ,cffitype)))
       (export ',lispsym))))


