;;;; package.lisp
(in-package #:nng)


(defmacro with-iov ((iov len &key buf (extra 0)) &body body)
  `(let ((len ,len))
     (with-foreign-object (,iov '(:struct iov))
       (with-foreign-slots ((iov-buf
			     iov-len) ,iov (:struct iov))
	 (setf iov-buf (or ,buf (foreign-alloc :char :count (+ ,extra len) ))
	       iov-len len)
	 ,@body))))


(defmacro iov-buf (iov)
  `(with-foreign-slots ((iov-buf) ,iov (:struct iov))
     iov-buf))

(defmacro iov-len (iov)
  `(with-foreign-slots ((iov-len) ,iov (:struct iov))
     iov-len))

(defun iov-make(bytes &key buf (extra 0))
  (let ((iov (foreign-alloc '(:struct iov))))
    (with-foreign-slots ((iov-buf  iov-len) iov
			 (:struct iov))
      (setf iov-buf (or buf (foreign-alloc :char :count (+ extra bytes)))
	    iov-len bytes))
    iov))

;;===========================================================================
;; Raw allocator.  You track the size...
;;
(def (alloc) size)
(def (free) ptr size)


;;===========================================================================
;; AIO (Async IO) uses aio structures for each instance.  aios contain a
;; C callback function, which in turn calls a Lisp function.  To accomplish
;; this, we create closures:

(defun aio-default-func (arg)
  (declare (ignore arg)))
;;
(defun aio-alloc (&optional (func #'aio-default-func)  (arg (null-pointer)))
  (defcallback cb :void ((arg :pointer))
    (funcall func arg))
  (with-foreign-object (ptr :pointer)
    (check (%aio-alloc ptr (callback cb) arg))
    (mem-ref ptr :pointer)))


(def (aio-abort) aio err)
;; Make this manually, because we want some useful defaults.
#||
(progn
 (defun aio-alloc (&optional (func (null-pointer)) (arg (null-pointer)))
   (with-foreign-object (ptr :pointer)
     (check (%aio-alloc ptr callback arg))
     (MEM-REF PTR :POINTER)))
 (EXPORT 'AIO-ALLOC))
||#
;;(def (aio-alloc :kind alloc) &optional (callback (cffi::null-pointer)) (arg 0)) ;;*
(def (aio-cancel) aio)
(def (aio-count) aio)
(def (aio-finish) aio err)
(def (aio-free) aio)
(def (aio-get-input) aio index)
(def (aio-get-msg) aio)
(def (aio-get-output) aio index)
(def (aio-result :kind check) aio)
(def (aio-set-input) aio index param)
(def (aio-set-iov) aio niov iov)
(def (aio-set-msg) aio msg)
(def (aio-set-output) aio index result)
(def (aio-set-timeout) aio timeout)
(def (aio-stop) aio)
(def (aio-wait) aio)


;;(def (bus0-open :kind check) socket)
;;(def (bus0-open-raw :kind check) socket)
(def (close) socket)

(def (ctx-close :kind check) ctx)
(def (ctx-getopt :kind check) ctx opt val valszp)
(def-getopt ctx bool :int)
(def-getopt ctx int  :int)
(def-getopt ctx ms   :int32)
(def-getopt ctx size  'size-t)
;;(def-getopt ctx string :string)
;;(def-getopt ctx uint64 :uint64)

;;(def (ctx-getopt-string :kind check) ctx opt strp)
;;(def (ctx-getopt-uint64 :kind check) ctx opt u65p)

(def (ctx-id) ctx)

(defun ctx-open (socket)
  (with-foreign-object (ctxp :pointer)
    (check (%ctx-open ctxp socket))
    (mem-ref ctxp :int)))

(def (ctx-recv) ctx aio)
(def (ctx-send) ctx aio)
(def (ctx-setopt-bool :kind check) ctx opt bvalp)
(def (ctx-setopt-int  :kind check) ctx opt ivalp)
(def (ctx-setopt-ms   :kind check) ctx opt durp)
(def (ctx-setopt-size :kind check) ctx opt zp)
;;(def (ctx-setopt-string :kind check) ctx opt str)
;;(def (ctx-setopt-uint64 :kind check) ctx opt u64)

(def (device :kind check) s1 s2)

;;------------------------------------------------------------
(defun dial (socket url &optional (flags 0))
  (with-foreign-object (pdialer :uint32)
    (check (%dial socket url pdialer flags))
    (mem-ref pdialer :uint32)))
 
(def (dialer-close) dialer)
(def (dialer-create :kind alloc) socket url)
(def (dialer-getopt :kind check) dialer opt val valszp)


(def-getopt dialer bool :int)
(def-getopt dialer int  :int)
(def-getopt dialer ms   :int32)
(def-getopt dialer ptr :pointer)
(def-getopt dialer size  'size-t)
(def-getopt dialer sockaddr 'sockaddr )
(def-getopt dialer string :string)
(def-getopt dialer uint64 :uint64)

(def (dialer-id) dialer)
(def (dialer-setopt      :kind check) dialer opt val valsz)
(def (dialer-setopt-bool :kind check) dialer opt bval)
(def (dialer-setopt-int  :kind check) dialer opt ival)
(def (dialer-setopt-ms   :kind check) dialer opt dur)
(def (dialer-setopt-ptr  :kind check) dialer opt ptr)
(def (dialer-setopt-size :kind check) dialer opt zp)
(def (dialer-setopt-string :kind check) dialer opt str)
(def (dialer-setopt-uint64 :kind check) dialer opt u64)

(def (dialer-start :kind check) dialer flags)

;;(def (inproc-register :kind check))
;;(def (ipc-register :kind check))

(defun listen (socket url &key (flags 0))
  (with-foreign-object (plistener :uint32)
    (check (%listen socket url plistener flags))
    (mem-ref plistener :uint32)))

(def (listener-close :kind check) listener)
(def (listener-create :kind alloc) socket url)
(def-getopt listener bool :int)
(def-getopt listener int  :int)
(def-getopt listener ms   :int32)
(def-getopt listener ptr :pointer)
(def-getopt listener size  'size-t)
(def-getopt listener sockaddr 'sockaddr )
(def-getopt listener string :string)
(def-getopt listener uint64 :uint64)

(def (listener-id :kind check) listener)
(def (listener-start :kind check) listener flags)

(def (listener-setopt      :kind check) dialer opt val valsz)
(def (listener-setopt-bool  :kind check) dialer opt bval)
(def (listener-setopt-int  :kind check) dialer opt ival)
(def (listener-setopt-ms   :kind check) dialer opt dur)
(def (listener-setopt-ptr  :kind check) dialer opt ptr)
(def (listener-setopt-size :kind check) dialer opt zp)
(def (listener-setopt-string :kind check) dialer opt str)
(def (listener-setopt-uint64 :kind check) dialer opt u64)

(def (msg-alloc :kind alloc) size)
(def (msg-append :kind check) msg val size)
(def (msg-append-u32 :kind check) msg u32)
(def (msg-body) msg)
(def (msg-chop :kind check) msg size)
(def (msg-chop-u32 :kind check) msg pu32)
(def (msg-clear) msg)
(def (msg-dup :kind alloc) orig)
(def (msg-free ) msg)
(def (msg-get-pipe) msg)

(def (msg-header) msg)
(def (msg-header-append :kind check) msg val size)
(def (msg-header-append-u32 :kind check) msg u32)
(def (msg-header-chop :kind check) msg  size)
(def (msg-header-chop-u32 :kind check) msg u32)
(def (msg-header-clear) msg)
(def (msg-header-insert :kind check) msg val size)
(def (msg-header-insert-u32 :kind check) msg u32)
(def (msg-header-len) msg );*
(def (msg-header-trim :kind check) msg  size)
(def (msg-header-trim-u32 :kind check) msg u32)

(def (msg-insert :kind check) msg val size)
(def (msg-insert-u32 :kind check) msg u32)
(def (msg-len) msg );*
(def (msg-realloc :kind check) msg size)
(def (msg-set-pipe) msg pipe)
(def (msg-trim :kind check) msg  size)
(def (msg-trim-u32 :kind check) msg u32)

(def (pipe-close) pipe)
(def (pipe-dialer) pipe)
(def-getopt pipe bool :int)
(def-getopt pipe int  :int)
(def-getopt pipe ms   :int32)
(def-getopt pipe ptr :pointer)
(def-getopt pipe size  'size-t)
(def-getopt pipe sockaddr 'sockaddr )
(def-getopt pipe string :string)
(def-getopt pipe uint64 :uint64)

(def (pipe-id) pipe)
(def (pipe-listener) pipe)
(def (pipe-notify :kind check) socket ev cb arg)
(def (pipe-socket) pipe)

;;(def (recv :kind check) socket data sizep flags)
;;==========================================================================
;; RECV
;;
;; We use zero-copy mode exclusively (since we dont' keep C buffers
;; anyway).  We return the buffer and the length
(defun recv (socket &key nonblock)
  "Return values: result, buf, size."
  (with-foreign-object (size 'size-t)
    (with-foreign-object (buf :pointer)
      (let ((flag (if nonblock 3 1)))
	(let ((result (check (%recv socket buf size flag))))
	  (values result (mem-ref buf :pointer) (mem-ref size 'size-t)))))))
;;==========================================================================
;; SEND
;;
(defun send (socket buf size &key nonblock free)
  "Send buf of size.  If nonblock, return immediately; if free, system will
free the buffer (must allocate with nng!)"
  (let ((flag (+ (if free 1 0) (if nonblock 2 0))))
    (check (%send socket buf size flag))))

(def (recv-aio) socket aio)

(defun recvmsg (socket &optional (flag 0))
  (with-foreign-object (ptr :pointer)
    (check (%recvmsg socket ptr flag))
    (mem-ref ptr :pointer)))

(def (send-aio) socket aio)

(def (sendmsg :kind check) socket msg flags )

(def (sleep-aio) msec aio)
(def (socket-id) socket)
(def (strdup) src)
(def (strerror) error)
(def (strfree) str)

;;(def (tcp-register :kind check))
;;(def (tls-register :kind check))
(def (url-clone :kind check) dup orig)
(def (url-free) url)
(def (url-parse :kind alloc) url-string)
(def (version))
;;(def (ws-register :kind check))
;;(def (wss-register :kind check))
;;(def (zt-register :kind check))



;; Supplemental
(def (clock))

(def (cv-alloc :kind alloc) mtx)
(def (cv-free)  cv)
(def (cv-until :kind check) cv when)
(def (cv-wait) cv)
(def (cv-wake) cv)
(def (cv-wake1) cv)
(def (msleep) msec)

(def (mtx-alloc :kind alloc) )
(def (mtx-free) mtx)
(def (mtx-lock) mtx)
(def (mtx-unlock) mtx)

;;(def (opts-parse))
;;(def (random))

;; protocol
(def (pair0-open     :kind socket))
(def (pair0-open-raw :kind socket))

(def (bus0-open     :kind socket))
(def (bus0-open-raw :kind socket))

(def (pair1-open     :kind socket))
(def (pair1-open-raw :kind socket))

(def (pull0-open     :kind socket) )
(def (pull0-open-raw :kind socket))

;; TODO: these must alloc!
;;(def (push0-open     :kind socket))
(def (push0-open     :kind socket) )
(def (push0-open-raw :kind socket))

(def (pub0-open     :kind socket))
(def (pub0-open-raw :kind socket))

(def (sub0-open     :kind socket))
(def (sub0-open-raw :kind socket))

(def (req0-open     :kind socket))
(def (req0-open-raw :kind socket))

(def (rep0-open     :kind socket))
(def (rep0-open-raw :kind socket))

(def (surveyor0-open     :kind socket))
(def (surveyor0-open-raw :kind socket))

(def (respondent0-open     :kind socket))
(def (respondent0-open-raw :kind socket))


;;======================================================================
;; Thread create
;;
;;  The callback function calls Lisp at thread entry.  A single function
;; is used, calling via a single vecotr.  This is ok since it is used
;; only once at entry.
;;
(defun thread-create (func &optional (arg (null-pointer)))
  "Create a thread, passing it a Lisp func and optionally, a pointer
to an argument"
  (defcallback cb :void ((arg :pointer))
    (funcall func arg))
  (with-foreign-object (ptr :pointer)
    (check (%thread-create  ptr (callback cb) arg))
    (mem-ref ptr :pointer)))
(export 'thead-create)

(def (thread-destroy) thread)

#||
(defun thproc (arg)
  (declare (ignore arg))
  (loop for i from 0 to 10 do
       (format t "~%~A..."i) (sleep 1)))

(defun ttt ()
  (thread-create #'thproc ))
||#
