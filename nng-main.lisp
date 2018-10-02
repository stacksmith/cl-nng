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



;; URL


(def (aio-abort) aio err)
;; Make this manually, because we want some useful defaults.

(PROGN
 (DEFUN AIO-ALLOC (&OPTIONAL (CALLBACK (NULL-POINTER)) (ARG (null-pointer)))
   (WITH-FOREIGN-OBJECT (PTR :POINTER)
     (CHECK
      (%AIO-ALLOC PTR CALLBACK ARG))
     (MEM-REF PTR :POINTER)))
 (EXPORT 'AIO-ALLOC))

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

(def (alloc) size)
;;(def (bus0-open :kind check) socket)
;;(def (bus0-open-raw :kind check) socket)
(def (close) socket)

(def (ctx-close :kind check) ctx)
(def (ctx-getopt :kind check) ctx opt val valszp)
(def (ctx-getopt-bool :kind check) ctx opt bvalp)
(def (ctx-getopt-int  :kind check) ctx opt ivalp)
(def (ctx-getopt-ms   :kind check) ctx opt durp)
(def (ctx-getopt-size :kind check) ctx opt zp)
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

(def (device :kind check) s1 s2)
(def (dial :kind check) socket url dp flags)
(def (dialer-close) dialer)
(def (dialer-create :kind alloc) socket url)
(def (dialer-getopt :kind check) dialer opt val valszp)
(def (dialer-getopt-bool :kind check) dialer opt bvalp)
(def (dialer-getopt-int  :kind check) dialer opt ivalp)
(def (dialer-getopt-ms   :kind check) dialer opt durp)
(def (dialer-getopt-size :kind check) dialer opt zp)
;;*** TODO: more
(def (dialer-id) dialer)
(def (dialer-setopt :kind check) dialer opt val valszp)
(def (dialer-setopt-bool :kind check) dialer opt bvalp)
(def (dialer-setopt-int  :kind check) dialer opt ivalp)
(def (dialer-setopt-ms   :kind check) dialer opt durp)
(def (dialer-setopt-size :kind check) dialer opt zp)
;; TODO: implement more
(def (dialer-start :kind check) dialer flags)

(def (free) ptr size)
;; TODO: getopt
;;(def (inproc-register :kind check))
;;(def (ipc-register :kind check))

(def (listener-close :kind check) listener)
(def (listener-create :kind alloc) socket url)
;; TODO: listener-getopt
(def (listener-id :kind check) listener)
(def (listener-start :kind check) listener flags)

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
;; TODO: pipe opts
(def (pipe-id) pipe)
(def (pipe-listener) pipe)
(def (pipe-notify :kind check) socket ev cb arg)
(def (pipe-socket) pipe)
;; TODO: pub0 pull0 push0
(def (recv :kind check) socket data sizep flags)
(def (recv-aio) socket aio)
(def (recvmsg :kind check) socket pmsg flags
     )
;; TODO: rep0 req0 respondent0
(def (send :kind check) socket data size flags)
(def (send-aio) socket aio)

(def (sendmsg :kind check) socket msg flags )
;; TODO: setopt
(def (sleep-aio) msec aio)
(def (socket-id) socket)
(def (strdup) src)
(def (strerror) error)
(def (strfree) str)
;; TODO: sub0 surveyor
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
(def (pair0-open     :kind check) socket)
(def (pair0-open-raw :kind check) socket)

(def (bus0-open     :kind check) socket)
(def (bus0-open-raw :kind check) socket)

(def (pair1-open     :kind check) socket)
(def (pair1-open-raw :kind check) socket)

(def (pull0-open     :kind check) socket)
(def (pull0-open-raw :kind check) socket)

(def (push0-open     :kind check) socket)
(def (push0-open-raw :kind check) socket)

(def (pub0-open     :kind check) socket)
(def (pub0-open-raw :kind check) socket)

(def (sub0-open     :kind check) socket)
(def (sub0-open-raw :kind check) socket)

(def (req0-open     :kind check) socket)
(def (req0-open-raw :kind check) socket)

(def (rep0-open     :kind check) socket)
(def (rep0-open-raw :kind check) socket)

(def (survey0-open     :kind check) socket)
(def (survey0-open-raw :kind check) socket)

(def (respond0-open     :kind check) socket)
(def (respond0-open-raw :kind check) socket)


;;======================================================================
;; Thread create
;;
;; Not particularly safe...  Maybe a special as an intermediate fun ptr?
(let (thread-function)
  (defcallback callback :void ((arg :pointer))
    (funcall thread-function arg))
  (defun thread-create (func arg)
    (setf thread-function func)
    (with-foreign-object (ptr :pointer)
      (check (%thread-create ptr (callback callback) arg))
      (mem-ref ptr :pointer)))
  (export 'thead-create))

(def (thread-destroy) thread)
