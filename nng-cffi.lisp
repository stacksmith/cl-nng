(in-package #:nng)

(cl:defmacro defanonenum (cl:&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(alexandria:define-constant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  ;; I am sure there is a more graceful way to do this.  size_t seems to be
  ;; equal to the size of a pointer for 32 or 64 bits...
  (if (= 4 (foreign-type-size :pointer))
      (defctype size-t :uint32)
      (defctype size-t :uint64))
  ;; The rest is swig-generated crap
  
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                      rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\-)
                       (helper (cl:cdr lst) '- (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here
(defmacro defconst (name &rest rest)
  `(progn
     (alexandria:define-constant ,name ,@rest)
     (export ',name)))

(defconst MAJOR-VERSION 1)

(defconst MINOR-VERSION 0)

(defconst PATCH-VERSION 1)

(defconst RELEASE-SUFFIX "" :test 'equal)

(defconst MAXADDRLEN 128)

#||(cffi:defcstruct ctx     (id :uint32))
(cffi:defcstruct dialer  (id :uint32))
(cffi:defcstruct listener(id :uint32))
(cffi:defcstruct pipe	 (id :uint32))
(cffi:defcstruct socket	(id :uint32))
||#
(defctype socket :uint32)
(defctype ctx    :uint32)
(defctype dialer :uint32)
(defctype listener :uint32)
(defctype pipe   :uint32)

(cffi:defcstruct sockaddr-inproc
	(sa-family :uint16)
	(sa-name :char :count 128))

(cffi:defcstruct sockaddr-path
	(sa-family :uint16)
	(sa-path :char :count 128))

(cffi:defcstruct sockaddr-in6
	(sa-family :uint16)
	(sa-port :uint16)
	(sa-addr :uint8 :count 16))

(cffi:defcstruct sockaddr-in
	(sa-family :uint16)
	(sa-port :uint16)
	(sa-addr :uint32))

(cffi:defcstruct sockaddr-zt
	(sa-family :uint16)
	(sa-nwid   :uint64)
	(sa-nodeid :uint64)
	(sa-port   :uint32))

(cffi:defcunion sockaddr
	(s-family :uint16)
	(s-ipc sockaddr-path)
	(s-inproc sockaddr-inproc)
	(s-in6 sockaddr-in6)
	(s-in sockaddr-in)
	(s-zt sockaddr-zt))

(cffi:defcenum sockaddr-family
	(:NNG-AF-UNSPEC #.0)
	(:NNG-AF-INPROC #.1)
	(:NNG-AF-IPC #.2)
	(:NNG-AF-INET #.3)
	(:NNG-AF-INET6 #.4)
	(:NNG-AF-ZT #.5))

(cffi:defcstruct iov
	(iov-buf :pointer)
	(iov-len size-t))

(defconst DURATION-INFINITE -1)
(defconst DURATION-DEFAULT -2)
(defconst DURATION-ZERO 0)

(cffi:defcfun ("nng_fini" %fini) :void)

(cffi:defcfun ("nng_close" %close) :int
  (arg0 socket))

(cffi:defcfun ("nng_socket_id" %socket-id) :int
  (arg0 socket))

(cffi:defcfun ("nng_closeall" %closeall) :void)

(cffi:defcfun ("nng_setopt" %setopt) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_setopt_bool" %setopt-bool) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_setopt_int" %setopt-int) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("nng_setopt_ms" %setopt-ms) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_setopt_size" %setopt-size) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 size-t))

(cffi:defcfun ("nng_setopt_uint64" %setopt-uint64) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :uint64))

(cffi:defcfun ("nng_setopt_string" %setopt-string) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_setopt_ptr" %setopt-ptr) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_getopt" %getopt) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 :pointer))

(cffi:defcfun ("nng_getopt_bool" %getopt-bool) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_getopt_int" %getopt-int) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_getopt_ms" %getopt-ms) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_getopt_size" %getopt-size) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_getopt_uint64" %getopt-uint64) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_getopt_ptr" %getopt-ptr) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcenum pipe-ev
	:NNG-PIPE-EV-ADD-PRE
	:NNG-PIPE-EV-ADD-POST
	:NNG-PIPE-EV-REM-POST
	:NNG-PIPE-EV-NUM)

(cffi:defcfun ("nng_pipe_notify" %pipe-notify) :int
  (arg0 socket)
  (arg1 :int)
  (callback :pointer)
  (arg3 :pointer))

(cffi:defcfun ("nng_getopt_string" %getopt-string) :int
  (arg0 socket)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listen" %listen) :int
  (arg0 :uint32) ;***
  (arg1 :string)
  (arg2 :pointer)
  (arg3 :int))

(cffi:defcfun ("nng_dial" %dial) :int
  (arg0 :uint32)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 :int))

(cffi:defcfun ("nng_dialer_create" %dialer-create) :int
  (arg0 :pointer)
  (arg1 socket)
  (arg2 :string))

(cffi:defcfun ("nng_listener_create" %listener-create) :int
  (arg0 :pointer)
  (arg1 socket)
  (arg2 :string))

(cffi:defcfun ("nng_dialer_start" %dialer-start) :int
  (arg0 dialer)
  (arg1 :int))

(cffi:defcfun ("nng_listener_start" %listener-start) :int
  (arg0 listener)
  (arg1 :int))

(cffi:defcfun ("nng_dialer_close" %dialer-close) :int
  (arg0 dialer))

(cffi:defcfun ("nng_listener_close" %listener-close) :int
  (arg0 listener))

(cffi:defcfun ("nng_dialer_id" %dialer-id) :int
  (arg0 dialer))

(cffi:defcfun ("nng_listener_id" %listener-id) :int
  (arg0 listener))

(cffi:defcfun ("nng_dialer_setopt" %dialer-setopt) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_dialer_setopt_bool" %dialer-setopt-bool) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_setopt_int" %dialer-setopt-int) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("nng_dialer_setopt_ms" %dialer-setopt-ms) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_setopt_size" %dialer-setopt-size) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 size-t))

(cffi:defcfun ("nng_dialer_setopt_uint64" %dialer-setopt-uint64) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :uint64))

(cffi:defcfun ("nng_dialer_setopt_ptr" %dialer-setopt-ptr) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_setopt_string" %dialer-setopt-string) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_dialer_getopt" %dialer-getopt) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_dialer_getopt_bool" %dialer-getopt-bool) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_getopt_int" %dialer-getopt-int) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_getopt_ms" %dialer-getopt-ms) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_getopt_size" %dialer-getopt-size) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 size-t))

(cffi:defcfun ("nng_dialer_getopt_sockaddr" %dialer-getopt-sockaddr) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_getopt_uint64" %dialer-getopt-uint64) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_getopt_ptr" %dialer-getopt-ptr) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_dialer_getopt_string" %dialer-getopt-string) :int
  (arg0 dialer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_setopt" %listener-setopt) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_listener_setopt_bool" %listener-setopt-bool) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_setopt_int" %listener-setopt-int) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("nng_listener_setopt_ms" %listener-setopt-ms) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_setopt_size" %listener-setopt-size) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 size-t))

(cffi:defcfun ("nng_listener_setopt_uint64" %listener-setopt-uint64) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :uint64))

(cffi:defcfun ("nng_listener_setopt_ptr" %listener-setopt-ptr) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_setopt_string" %listener-setopt-string) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_listener_getopt" %listener-getopt) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_listener_getopt_bool" %listener-getopt-bool) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_getopt_int" %listener-getopt-int) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_getopt_ms" %listener-getopt-ms) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_getopt_size" %listener-getopt-size) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 size-t))

(cffi:defcfun ("nng_listener_getopt_sockaddr" %listener-getopt-sockaddr) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_getopt_uint64" %listener-getopt-uint64) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_getopt_ptr" %listener-getopt-ptr) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_listener_getopt_string" %listener-getopt-string) :int
  (arg0 listener)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_strerror" %strerror) :string
  (arg0 :int))

(cffi:defcfun ("nng_send" %send) :int
  (arg0 socket)
  (arg1 :pointer)
  (arg2 size-t)
  (arg3 :int))

(cffi:defcfun ("nng_recv" %recv) :int
  (arg0 :uint32);; ***socket
  (arg1 :pointer)
  (arg2 (:pointer size-t))
  (arg3 :int))

(cffi:defcfun ("nng_sendmsg" %sendmsg) :int
  (arg0 socket)
  (arg1 :pointer)
  (arg2 :int))

(cffi:defcfun ("nng_recvmsg" %recvmsg) :int
  (arg0 socket)
  (arg1 :pointer)
  (arg2 :int))

(cffi:defcfun ("nng_send_aio" %send-aio) :void
  (arg0 socket)
  (arg1 :pointer))

(cffi:defcfun ("nng_recv_aio" %recv-aio) :void
  (arg0 socket)
  (arg1 :pointer))

(cffi:defcfun ("nng_ctx_open" %ctx-open) :int
  (arg0 :pointer)
  (arg1 socket))

(cffi:defcfun ("nng_ctx_close" %ctx-close) :int
  (arg0 ctx))

(cffi:defcfun ("nng_ctx_id" %ctx-id) :int
  (arg0 ctx))

(cffi:defcfun ("nng_ctx_recv" %ctx-recv) :void
  (arg0 ctx)
  (arg1 :pointer))

(cffi:defcfun ("nng_ctx_send" %ctx-send) :void
  (arg0 ctx)
  (arg1 :pointer))

(cffi:defcfun ("nng_ctx_getopt" %ctx-getopt) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_ctx_getopt_bool" %ctx-getopt-bool) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_ctx_getopt_int" %ctx-getopt-int) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_ctx_getopt_ms" %ctx-getopt-ms) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_ctx_getopt_size" %ctx-getopt-size) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 size-t))

(cffi:defcfun ("nng_ctx_setopt" %ctx-setopt) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_ctx_setopt_bool" %ctx-setopt-bool) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_ctx_setopt_int" %ctx-setopt-int) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("nng_ctx_setopt_ms" %ctx-setopt-ms) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_ctx_setopt_size" %ctx-setopt-size) :int
  (arg0 ctx)
  (arg1 :string)
  (arg2 size-t))

(cffi:defcfun ("nng_alloc" %alloc) :pointer
  (arg0 size-t))

(cffi:defcfun ("nng_free" %free) :void
  (arg0 :pointer)
  (arg1 size-t))

(cffi:defcfun ("nng_strdup" %strdup) :string
  (arg0 :string))

(cffi:defcfun ("nng_strfree" %strfree) :void
  (arg0 :string))

(cffi:defcfun ("nng_aio_alloc" %aio-alloc) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("nng_aio_free" %aio-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_aio_stop" %aio-stop) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_aio_result" %aio-result) :int
  (arg0 :pointer))

(cffi:defcfun ("nng_aio_count" %aio-count) size-t
  (arg0 :pointer))

(cffi:defcfun ("nng_aio_cancel" %aio-cancel) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_aio_abort" %aio-abort) :void
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("nng_aio_wait" %aio-wait) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_aio_set_msg" %aio-set-msg) :void
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_aio_get_msg" %aio-get-msg) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_aio_set_input" %aio-set-input) :int
  (arg0 :pointer)
  (arg1 :unsigned-int)
  (arg2 :pointer))

(cffi:defcfun ("nng_aio_get_input" %aio-get-input) :pointer
  (arg0 :pointer)
  (arg1 :unsigned-int))

(cffi:defcfun ("nng_aio_set_output" %aio-set-output) :int
  (arg0 :pointer)
  (arg1 :unsigned-int)
  (arg2 :pointer))

(cffi:defcfun ("nng_aio_get_output" %aio-get-output) :pointer
  (arg0 :pointer)
  (arg1 :unsigned-int))

(cffi:defcfun ("nng_aio_set_timeout" %aio-set-timeout) :void
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_aio_set_iov" %aio-set-iov) :int
  (arg0 :pointer)
  (arg1 :unsigned-int)
  (arg2 :pointer))

(cffi:defcfun ("nng_aio_begin" %aio-begin) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_aio_finish" %aio-finish) :void
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("nng_aio_defer" %aio-defer) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("nng_sleep_aio" %sleep-aio) :void
  (arg0 :int32)
  (arg1 :pointer))

(cffi:defcfun ("nng_msg_alloc" %msg-alloc) :int
  (arg0 :pointer)
  (arg1 size-t))

(cffi:defcfun ("nng_msg_free" %msg-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_msg_realloc" %msg-realloc) :int
  (arg0 :pointer)
  (arg1 size-t))

(cffi:defcfun ("nng_msg_header" %msg-header) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_msg_header_len" %msg-header-len) size-t
  (arg0 :pointer))

(cffi:defcfun ("nng_msg_body" %msg-body) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_msg_len" %msg-len) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_msg_append" %msg-append) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_msg_insert" %msg-insert) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_msg_trim" %msg-trim) :int
  (arg0 :pointer)
  (arg1 size-t))

(cffi:defcfun ("nng_msg_chop" %msg-chop) :int
  (arg0 :pointer)
  (arg1 size-t))

(cffi:defcfun ("nng_msg_header_append" %msg-header-append) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_msg_header_insert" %msg-header-insert) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_msg_header_trim" %msg-header-trim) :int
  (arg0 :pointer)
  (arg1 size-t))

(cffi:defcfun ("nng_msg_header_chop" %msg-header-chop) :int
  (arg0 :pointer)
  (arg1 size-t))

(cffi:defcfun ("nng_msg_header_append_u16" %msg-header-append-u16) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_msg_header_append_u32" %msg-header-append-u32) :int
  (arg0 :pointer)
  (arg1 :uint32))

(cffi:defcfun ("nng_msg_header_append_u64" %msg-header-append-u64) :int
  (arg0 :pointer)
  (arg1 :uint64))

(cffi:defcfun ("nng_msg_header_insert_u16" %msg-header-insert-u16) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_msg_header_insert_u32" %msg-header-insert-u32) :int
  (arg0 :pointer)
  (arg1 :uint32))

(cffi:defcfun ("nng_msg_header_insert_u64" %msg-header-insert-u64) :int
  (arg0 :pointer)
  (arg1 :uint64))

(cffi:defcfun ("nng_msg_header_chop_u16" %msg-header-chop-u16) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_msg_header_chop_u32" %msg-header-chop-u32) :int
  (arg0 :pointer)
  (arg1 :uint32))

(cffi:defcfun ("nng_msg_header_chop_u64" %msg-header-chop-u64) :int
  (arg0 :pointer)
  (arg1 :uint64))

(cffi:defcfun ("nng_msg_header_trim_u16" %msg-header-trim-u16) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_msg_header_trim_u32" %msg-header-trim-u32) :int
  (arg0 :pointer)
  (arg1 :uint32))

(cffi:defcfun ("nng_msg_header_trim_u64" %msg-header-trim-u64) :int
  (arg0 :pointer)
  (arg1 :uint64))

(cffi:defcfun ("nng_msg_append_u16" %msg-append-u16) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_msg_append_u32" %msg-append-u32) :int
  (arg0 :pointer)
  (arg1 :uint32))

(cffi:defcfun ("nng_msg_append_u64" %msg-append-u64) :int
  (arg0 :pointer)
  (arg1 :uint64))

(cffi:defcfun ("nng_msg_insert_u16" %msg-insert-u16) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_msg_insert_u32" %msg-insert-u32) :int
  (arg0 :pointer)
  (arg1 :uint32))

(cffi:defcfun ("nng_msg_insert_u64" %msg-insert-u64) :int
  (arg0 :pointer)
  (arg1 :uint64))

(cffi:defcfun ("nng_msg_chop_u16" %msg-chop-u16) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_msg_chop_u32" %msg-chop-u32) :int
  (arg0 :pointer)
  (arg1 :uint32))

(cffi:defcfun ("nng_msg_chop_u64" %msg-chop-u64) :int
  (arg0 :pointer)
  (arg1 :uint64))

(cffi:defcfun ("nng_msg_trim_u16" %msg-trim-u16) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_msg_trim_u32" %msg-trim-u32) :int
  (arg0 :pointer)
  (arg1 :uint32))

(cffi:defcfun ("nng_msg_trim_u64" %msg-trim-u64) :int
  (arg0 :pointer)
  (arg1 :uint64))

(cffi:defcfun ("nng_msg_dup" %msg-dup) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_msg_clear" %msg-clear) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_msg_header_clear" %msg-header-clear) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_msg_set_pipe" %msg-set-pipe) :void
  (arg0 :pointer)
  (arg1 pipe))

(cffi:defcfun ("nng_msg_get_pipe" %msg-get-pipe) pipe
  (arg0 :pointer))

(cffi:defcfun ("nng_msg_getopt" %msg-getopt) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_pipe_getopt" %pipe-getopt) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t))

(cffi:defcfun ("nng_pipe_getopt_bool" %pipe-getopt-bool) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_pipe_getopt_int" %pipe-getopt-int) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_pipe_getopt_ms" %pipe-getopt-ms) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_pipe_getopt_size" %pipe-getopt-size) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 size-t))

(cffi:defcfun ("nng_pipe_getopt_sockaddr" %pipe-getopt-sockaddr) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_pipe_getopt_uint64" %pipe-getopt-uint64) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 :uint64))

(cffi:defcfun ("nng_pipe_getopt_ptr" %pipe-getopt-ptr) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_pipe_getopt_string" %pipe-getopt-string) :int
  (arg0 pipe)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_pipe_close" %pipe-close) :int
  (arg0 pipe))

(cffi:defcfun ("nng_pipe_id" %pipe-id) :int
  (arg0 pipe))

(cffi:defcfun ("nng_pipe_socket" %pipe-socket) socket
  (arg0 pipe))

(cffi:defcfun ("nng_pipe_dialer" %pipe-dialer) dialer
  (arg0 pipe))

(cffi:defcfun ("nng_pipe_listener" %pipe-listener) listener
  (arg0 pipe))

(cffi:defcenum flag-enum
	(:NNG-FLAG-ALLOC #.1)
	(:NNG-FLAG-NONBLOCK #.2))

(defconst OPT-SOCKNAME "socket_name" :test 'equal)

(defconst OPT-RAW "raw" :test 'equal)

(defconst OPT-PROTO "protocol" :test 'equal)

(defconst OPT-PROTONAME "protocol_name" :test 'equal)

(defconst OPT-PEER "peer" :test 'equal)

(defconst OPT-PEERNAME "peer_name" :test 'equal)

(defconst OPT-RECVBUF "recv_buffer" :test 'equal)

(defconst OPT-SENDBUF "send_buffer" :test 'equal)

(defconst OPT-RECVFD "recv_fd" :test 'equal)

(defconst OPT-SENDFD "send_fd" :test 'equal)

(defconst OPT-RECVTIMEO "recv_timeout" :test 'equal)

(defconst OPT-SENDTIMEO "send_timeout" :test 'equal)

(defconst OPT-LOCADDR "local_address" :test 'equal)

(defconst OPT-REMADDR "remote_address" :test 'equal)

(defconst OPT-URL "url" :test 'equal)

(defconst OPT-MAXTTL "ttl_max" :test 'equal)

(defconst OPT-RECVMAXSZ "recv_size_max" :test 'equal)

(defconst OPT-RECONNMINT "reconnect_time_min" :test 'equal)

(defconst OPT-RECONNMAXT "reconnect_time_max" :test 'equal)

(defconst OPT-TLS-CONFIG "tls_config" :test 'equal)

(defconst OPT-TLS-AUTH-MODE "tls_authmode" :test 'equal)

(defconst OPT-TLS-CERT-KEY-FILE "tls_cert_key_file" :test 'equal)

(defconst OPT-TLS-CA-FILE "tls_ca_file" :test 'equal)

(defconst OPT-TLS-SERVER-NAME "tls_server_name" :test 'equal)

(defconst OPT-TLS-VERIFIED "tls_verified" :test 'equal)

(defconst OPT-TCP-NODELAY "tcp_nodelay" :test 'equal)

(defconst OPT-TCP-KEEPALIVE "tcp_keepalive" :test 'equal)

(cffi:defcfun ("nng_stats_get" %stats-get) :int
  (arg0 :pointer))

(cffi:defcfun ("nng_stats_free" %stats-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_stats_dump" %stats-dump) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_stat_next" %stat-next) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_stat_child" %stat-child) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_stat_name" %stat-name) :string
  (arg0 :pointer))

(cffi:defcfun ("nng_stat_type" %stat-type) :int
  (arg0 :pointer))

(cffi:defcenum stat-type-enum
	(:NNG-STAT-SCOPE #.0)
	(:NNG-STAT-LEVEL #.1)
	(:NNG-STAT-COUNTER #.2)
	(:NNG-STAT-STRING #.3)
	(:NNG-STAT-BOOLEAN #.4)
	(:NNG-STAT-ID #.5))

(cffi:defcfun ("nng_stat_unit" %stat-unit) :int
  (arg0 :pointer))

(cffi:defcenum unit-enum
	(:NNG-UNIT-NONE #.0)
	(:NNG-UNIT-BYTES #.1)
	(:NNG-UNIT-MESSAGES #.2)
	(:NNG-UNIT-MILLIS #.3)
	(:NNG-UNIT-EVENTS #.4))

(cffi:defcfun ("nng_stat_value" %stat-value) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_stat_string" %stat-string) :string
  (arg0 :pointer))

(cffi:defcfun ("nng_stat_desc" %stat-desc) :string
  (arg0 :pointer))

(cffi:defcfun ("nng_stat_timestamp" %stat-timestamp) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_device" %device) :int
  (arg0 socket)
  (arg1 socket))

(cffi:defcenum errno-enum
	(:NNG-EINTR #.1)
	(:NNG-ENOMEM #.2)
	(:NNG-EINVAL #.3)
	(:NNG-EBUSY #.4)
	(:NNG-ETIMEDOUT #.5)
	(:NNG-ECONNREFUSED #.6)
	(:NNG-ECLOSED #.7)
	(:NNG-EAGAIN #.8)
	(:NNG-ENOTSUP #.9)
	(:NNG-EADDRINUSE #.10)
	(:NNG-ESTATE #.11)
	(:NNG-ENOENT #.12)
	(:NNG-EPROTO #.13)
	(:NNG-EUNREACHABLE #.14)
	(:NNG-EADDRINVAL #.15)
	(:NNG-EPERM #.16)
	(:NNG-EMSGSIZE #.17)
	(:NNG-ECONNABORTED #.18)
	(:NNG-ECONNRESET #.19)
	(:NNG-ECANCELED #.20)
	(:NNG-ENOFILES #.21)
	(:NNG-ENOSPC #.22)
	(:NNG-EEXIST #.23)
	(:NNG-EREADONLY #.24)
	(:NNG-EWRITEONLY #.25)
	(:NNG-ECRYPTO #.26)
	(:NNG-EPEERAUTH #.27)
	(:NNG-ENOARG #.28)
	(:NNG-EAMBIGUOUS #.29)
	(:NNG-EBADTYPE #.30)
	(:NNG-EINTERNAL #.1000)
	(:NNG-ESYSERR #.#x10000000)
	(:NNG-ETRANERR #.#x20000000))

(cffi:defcstruct url
	(u-rawurl :string)
	(u-scheme :string)
	(u-userinfo :string)
	(u-host :string)
	(u-hostname :string)
	(u-port :string)
	(u-path :string)
	(u-query :string)
	(u-fragment :string)
	(u-requri :string))

(cffi:defcfun ("nng_url_parse" %url-parse) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_url_free" %url-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_url_clone" %url-clone) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_version" %version) :string)

(cffi:defcenum http-status
	(:NNG-HTTP-STATUS-CONTINUE #.100)
	(:NNG-HTTP-STATUS-SWITCHING #.101)
	(:NNG-HTTP-STATUS-PROCESSING #.102)
	(:NNG-HTTP-STATUS-OK #.200)
	(:NNG-HTTP-STATUS-CREATED #.201)
	(:NNG-HTTP-STATUS-ACCEPTED #.202)
	(:NNG-HTTP-STATUS-NOT-AUTHORITATIVE #.203)
	(:NNG-HTTP-STATUS-NO-CONTENT #.204)
	(:NNG-HTTP-STATUS-RESET-CONTENT #.205)
	(:NNG-HTTP-STATUS-PARTIAL-CONTENT #.206)
	(:NNG-HTTP-STATUS-MULTI-STATUS #.207)
	(:NNG-HTTP-STATUS-ALREADY-REPORTED #.208)
	(:NNG-HTTP-STATUS-IM-USED #.226)
	(:NNG-HTTP-STATUS-MULTIPLE-CHOICES #.300)
	(:NNG-HTTP-STATUS-STATUS-MOVED-PERMANENTLY #.301)
	(:NNG-HTTP-STATUS-FOUND #.302)
	(:NNG-HTTP-STATUS-SEE-OTHER #.303)
	(:NNG-HTTP-STATUS-NOT-MODIFIED #.304)
	(:NNG-HTTP-STATUS-USE-PROXY #.305)
	(:NNG-HTTP-STATUS-TEMPORARY-REDIRECT #.307)
	(:NNG-HTTP-STATUS-PERMANENT-REDIRECT #.308)
	(:NNG-HTTP-STATUS-BAD-REQUEST #.400)
	(:NNG-HTTP-STATUS-UNAUTHORIZED #.401)
	(:NNG-HTTP-STATUS-PAYMENT-REQUIRED #.402)
	(:NNG-HTTP-STATUS-FORBIDDEN #.403)
	(:NNG-HTTP-STATUS-NOT-FOUND #.404)
	(:NNG-HTTP-STATUS-METHOD-NOT-ALLOWED #.405)
	(:NNG-HTTP-STATUS-NOT-ACCEPTABLE #.406)
	(:NNG-HTTP-STATUS-PROXY-AUTH-REQUIRED #.407)
	(:NNG-HTTP-STATUS-REQUEST-TIMEOUT #.408)
	(:NNG-HTTP-STATUS-CONFLICT #.409)
	(:NNG-HTTP-STATUS-GONE #.410)
	(:NNG-HTTP-STATUS-LENGTH-REQUIRED #.411)
	(:NNG-HTTP-STATUS-PRECONDITION-FAILED #.412)
	(:NNG-HTTP-STATUS-PAYLOAD-TOO-LARGE #.413)
	(:NNG-HTTP-STATUS-ENTITY-TOO-LONG #.414)
	(:NNG-HTTP-STATUS-UNSUPPORTED-MEDIA-TYPE #.415)
	(:NNG-HTTP-STATUS-RANGE-NOT-SATISFIABLE #.416)
	(:NNG-HTTP-STATUS-EXPECTATION-FAILED #.417)
	(:NNG-HTTP-STATUS-TEAPOT #.418)
	(:NNG-HTTP-STATUS-UNPROCESSABLE-ENTITY #.422)
	(:NNG-HTTP-STATUS-LOCKED #.423)
	(:NNG-HTTP-STATUS-FAILED-DEPENDENCY #.424)
	(:NNG-HTTP-STATUS-UPGRADE-REQUIRED #.426)
	(:NNG-HTTP-STATUS-PRECONDITION-REQUIRED #.428)
	(:NNG-HTTP-STATUS-TOO-MANY-REQUESTS #.429)
	(:NNG-HTTP-STATUS-HEADERS-TOO-LARGE #.431)
	(:NNG-HTTP-STATUS-UNAVAIL-LEGAL-REASONS #.451)
	(:NNG-HTTP-STATUS-INTERNAL-SERVER-ERROR #.500)
	(:NNG-HTTP-STATUS-NOT-IMPLEMENTED #.501)
	(:NNG-HTTP-STATUS-BAD-GATEWAY #.502)
	(:NNG-HTTP-STATUS-SERVICE-UNAVAILABLE #.503)
	(:NNG-HTTP-STATUS-GATEWAY-TIMEOUT #.504)
	(:NNG-HTTP-STATUS-HTTP-VERSION-NOT-SUPP #.505)
	(:NNG-HTTP-STATUS-VARIANT-ALSO-NEGOTIATES #.506)
	(:NNG-HTTP-STATUS-INSUFFICIENT-STORAGE #.507)
	(:NNG-HTTP-STATUS-LOOP-DETECTED #.508)
	(:NNG-HTTP-STATUS-NOT-EXTENDED #.510)
	(:NNG-HTTP-STATUS-NETWORK-AUTH-REQUIRED #.511))

;;==============================================================================
;; HTTP REQ
;;
(cffi:defcfun ("nng_http_req_add_header" %http-req-add-header) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

;;-----------
(cffi:defcfun ("nng_http_req_alloc" %http-req-alloc) :int
  (arg0 :pointer)
  (arg1 :pointer))

;;-----------
(cffi:defcfun ("nng_http_req_free" %http-req-free) :void
  (req :pointer))
;;-----------
(cffi:defcfun ("nng_http_req_get_header" %http-req-get-header) :string
  (req :pointer)
  (key :string))
;;-----------
(cffi:defcfun ("nng_http_req_get_method" %http-req-get-method) :string
  (req :pointer))
;;-----------
(cffi:defcfun ("nng_http_req_get_uri" %http-req-get-uri) :string
  (req :pointer))
;;-----------
(cffi:defcfun ("nng_http_req_get_version" %http-req-get-version) :string
  (req :pointer))
;;-----------
(cffi:defcfun ("nng_http_req_set_header" %http-req-set-header) :int
  (req :pointer)
  (key :string)
  (val :string))
;;-----------
(cffi:defcfun ("nng_http_req_set_data" %http-req-set-data) :int
  (req :pointer)
  (body :pointer)
  (size size-t))

(cffi:defcfun ("nng_http_req_del_header" %http-req-del-header) :int
  (arg0 :pointer)
  (arg1 :string))


(cffi:defcfun ("nng_http_req_set_method" %http-req-set-method) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_http_req_set_version" %http-req-set-version) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_http_req_set_uri" %http-req-set-uri) :int
  (arg0 :pointer)
  (arg1 :string))



(cffi:defcfun ("nng_http_req_copy_data" %http-req-copy-data) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_http_req_get_data" %http-req-get-data) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_http_res_alloc" %http-res-alloc) :int
  (arg0 :pointer))

(cffi:defcfun ("nng_http_res_alloc_error" %http-res-alloc-error) :int
  (arg0 :pointer)
  (arg1 :uint16))

(cffi:defcfun ("nng_http_res_free" %http-res-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_http_res_get_status" %http-res-get-status) :uint16
  (arg0 :pointer))

(cffi:defcfun ("nng_http_res_set_status" %http-res-set-status) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_res_get_reason" %http-res-get-reason) :string
  (arg0 :pointer))

(cffi:defcfun ("nng_http_res_set_reason" %http-res-set-reason) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_http_res_set_header" %http-res-set-header) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_http_res_add_header" %http-res-add-header) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_http_res_del_header" %http-res-del-header) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_http_res_get_header" %http-res-get-header) :string
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_http_res_set_version" %http-res-set-version) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_http_res_get_version" %http-res-get-version) :string
  (arg0 :pointer))

(cffi:defcfun ("nng_http_res_get_data" %http-res-get-data) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_http_res_set_data" %http-res-set-data) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_http_res_copy_data" %http-res-copy-data) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 size-t))

(cffi:defcfun ("nng_http_conn_close" %http-conn-close) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_http_conn_read" %http-conn-read) :void
  (arg0 :pointer)
  (arg1 :pointer))


(cffi:defcfun ("nng_http_conn_read_all" %http-conn-read-all) :void
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_conn_write" %http-conn-write) :void
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_conn_write_all" %http-conn-write-all) :void
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_conn_write_req" %http-conn-write-req) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("nng_http_conn_write_res" %http-conn-write-res) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("nng_http_conn_read_req" %http-conn-read-req) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("nng_http_conn_read_res" %http-conn-read-res) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))


(cffi:defcfun ("nng_http_conn_transact" %http-conn-transact) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer)
  (arg3 :pointer))


(cffi:defcfun ("nng_http_req_reset" %http-req-reset) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_http_res_reset" %http-res-reset) :void
  (arg0 :pointer))

;;

(cffi:defcfun ("nng_http_handler_alloc" %http-handler-alloc) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("nng_http_handler_free" %http-handler-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_http_handler_alloc_file" %http-handler-alloc-file) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_http_handler_alloc_static" %http-handler-alloc-static) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :pointer)
  (arg3 size-t)
  (arg4 :string))

(cffi:defcfun ("nng_http_handler_alloc_directory" %http-handler-alloc-directory) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_http_handler_set_method" %http-handler-set-method) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_http_handler_set_host" %http-handler-set-host) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_http_handler_set_tree" %http-handler-set-tree) :int
  (arg0 :pointer))

(cffi:defcfun ("nng_http_handler_set_data" %http-handler-set-data) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("nng_http_handler_get_data" %http-handler-get-data) :pointer
  (arg0 :pointer))

(cffi:defcfun ("nng_http_server_hold" %http-server-hold) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_server_release" %http-server-release) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_http_server_start" %http-server-start) :int
  (arg0 :pointer))

(cffi:defcfun ("nng_http_server_stop" %http-server-stop) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_http_server_add_handler" %http-server-add-handler) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_server_del_handler" %http-server-del-handler) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_server_set_tls" %http-server-set-tls) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_server_get_tls" %http-server-get-tls) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_server_set_error_page" %http-server-set-error-page) :int
  (arg0 :pointer)
  (arg1 :uint16)
  (arg2 :string))

(cffi:defcfun ("nng_http_server_set_error_file" %http-server-set-error-file) :int
  (arg0 :pointer)
  (arg1 :uint16)
  (arg2 :string))

(cffi:defcfun ("nng_http_server_res_error" %http-server-res-error) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_hijack" %http-hijack) :int
  (arg0 :pointer))

(cffi:defcfun ("nng_http_client_alloc" %http-client-alloc) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_client_free" %http-client-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_http_client_set_tls" %http-client-set-tls) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_client_get_tls" %http-client-get-tls) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_http_client_connect" %http-client-connect) :void
  (arg0 :pointer)
  (arg1 :pointer))


(cffi:defcfun ("nng_http_client_transact" %http-client-transact) :void
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer)
  (arg3 :pointer))

(cffi:defcenum tls-mode
	(:NNG-TLS-MODE-CLIENT #.0)
	(:NNG-TLS-MODE-SERVER #.1))

(cffi:defcenum tls-auth-mode
	(:NNG-TLS-AUTH-MODE-NONE #.0)
	(:NNG-TLS-AUTH-MODE-OPTIONAL #.1)
	(:NNG-TLS-AUTH-MODE-REQUIRED #.2))

(cffi:defcfun ("nng_tls_config_alloc" %tls-config-alloc) :int
  (arg0 :pointer)
  (arg1 tls-mode))

(cffi:defcfun ("nng_tls_config_free" %tls-config-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_tls_config_server_name" %tls-config-server-name) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_tls_config_ca_chain" %tls-config-ca-chain) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_tls_config_own_cert" %tls-config-own-cert) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string)
  (arg3 :string))

(cffi:defcfun ("nng_tls_config_key" %tls-config-key) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("nng_tls_config_pass" %tls-config-pass) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_tls_config_auth_mode" %tls-config-auth-mode) :int
  (arg0 :pointer)
  (arg1 tls-auth-mode))

(cffi:defcfun ("nng_tls_config_ca_file" %tls-config-ca-file) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("nng_tls_config_cert_key_file" %tls-config-cert-key-file) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(cffi:defcfun ("nng_clock" %clock) :uint64)

(cffi:defcfun ("nng_msleep" %msleep) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_thread_create" %thread-create) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("nng_thread_destroy" %thread-destroy) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_mtx_alloc" %mtx-alloc) :int
  (arg0 :pointer))

(cffi:defcfun ("nng_mtx_free" %mtx-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_mtx_lock" %mtx-lock) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_mtx_unlock" %mtx-unlock) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_cv_alloc" %cv-alloc) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_cv_free" %cv-free) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_cv_wait" %cv-wait) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_cv_until" %cv-until) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("nng_cv_wake" %cv-wake) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_cv_wake1" %cv-wake1) :void
  (arg0 :pointer))

(cffi:defcfun ("nng_random" %random) :pointer)

(cffi:defcstruct optspec
	(o-name :string)
	(o-short :int)
	(o-val :int)
	(o-arg :pointer))

(cffi:defcfun ("nng_opts_parse" %opts-parse) :int
  (argc :int)
  (argv :pointer)
  (opts :pointer)
  (val :pointer)
  (optarg :pointer)
  (optidx :pointer))

;; Protocols
(cffi:defcfun ("nng_pair0_open"     %pair0-open) :int (socket :pointer))
(cffi:defcfun ("nng_pair0_open_raw" %pair0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_bus0_open"      %bus0-open) :int (socket :pointer))
(cffi:defcfun ("nng_bus0_open_raw"  %bus0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_pair1_open"     %pair1-open) :int (socket :pointer))
(cffi:defcfun ("nng_pair1_open_raw" %pair1-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_pull0_open"     %pull0-open) :int (socket :pointer))
(cffi:defcfun ("nng_pull0_open_raw" %pull0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_push0_open"     %push0-open) :int (socket :pointer))
(cffi:defcfun ("nng_push0_open_raw" %push0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_pub0_open"      %pub0-open) :int (socket :pointer))
(cffi:defcfun ("nng_pub0_open_raw"  %pub0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_sub0_open"      %sub0-open) :int (socket :pointer))
(cffi:defcfun ("nng_sub0_open_raw"  %sub0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_req0_open"      %req0-open) :int (socket :pointer))
(cffi:defcfun ("nng_req0_open_raw"  %req0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_rep0_open"      %rep0-open) :int (socket :pointer))
(cffi:defcfun ("nng_rep0_open_raw"  %rep0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_surveyor0_open"      %surveyor0-open) :int (socket :pointer))
(cffi:defcfun ("nng_surveyor0_open_raw"  %surveyor0-open-raw) :int (socket :pointer))
(cffi:defcfun ("nng_respondent0_open"     %respondent0-open) :int (socket :pointer))
(cffi:defcfun ("nng_respondent0_open_raw" %respondent0-open-raw) :int (socket :pointer))


(cffi:defcfun ("nni_base64_encode" %base64-encode) :int
  (in :pointer)
  (in-len size-t)
  (out :pointer)
  (out-len size-t))

(cffi:defcfun ("nni_base64_decode" %base64-decode) :int
  (in :pointer)
  (in-len size-t)
  (out :pointer)
  (out-len size-t))

;; Protocol registration.  NOTE: no %
(cffi:defcfun ("nng_ipc_register" ipc-register) :int)
(cffi:defcfun ("nng_inproc_register" inproc-register) :int)
(cffi:defcfun ("nng_tcp_register" tcp-register) :int)
(cffi:defcfun ("nng_tls_register" tls-register) :int)

(cffi:defcfun ("nng_ws_register" ws-register) :int)
(cffi:defcfun ("nng_wss_register" wss-register) :int)
(cffi:defcfun ("nng_zt_register" zt-register) :int)
