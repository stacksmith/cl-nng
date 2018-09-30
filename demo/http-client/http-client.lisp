(in-package :nng)

;;==============================================================================
;;
(defun group (source n)
     "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
     (if (zerop n) (error "zero length"))
     (labels ((rec (source acc)
		(let ((rest (nthcdr n source)))
		  (if (consp rest)
		      (rec rest (cons (subseq source 0 n)
				      acc))
		      (nreverse (cons source acc))))))
       (if source
	   (rec source nil)
	   nil)))

(defun %dump (pointer &optional (size-in-bytes 64))
  (let* ((size (if (oddp size-in-bytes) (1+ size-in-bytes) size-in-bytes))
         (data (loop :for i :below size :collect
                  (cffi:mem-ref pointer :uchar i)))
         (batched (group data 16))
         (batched-chars (mapcar
                         (lambda (x)
                           (mapcar
                            (lambda (c)
                              (if (and (> c 31) (< c 126))
                                  (code-char c)
                                  #\.))
                            x))
                         batched)))
    (loop :for batch :in batched
       :for chars :in batched-chars
       :for i :from 0 :by 16 :do
       (when (= 0 (mod i 256))
         (format t "~%87654321    0011 2233 4455 6677 8899 aabb ccdd eeff    0123456789abcdef~%")
         (format t "-----------------------------------------------------------------------~%"))
       (format t "~8,'0X    ~{~@[~2,'0X~]~@[~2,'0X ~]~}   " i batch)
       (format t "~{~a~}~{~c~}~%"
               (loop :for i :below (max 0 (floor (/ (- 16 (length batch)) 2)))
		  :collect "     ")
	       chars))))

	       
(defparameter *url* (url-parse "http://httpbin.org/ip" ))

(defparameter *client* nil)
(defparameter *req* nil)
(defparameter *res* nil)
(defparameter *aio* nil)
(defparameter *conn* nil)

(defun t1 ()
  (setf *client* (http-client-alloc *url*)
	*req*    (http-req-alloc *url*)
	*res*    (http-res-alloc )
	*aio*    (aio-alloc)))

(defun t2 ()
  (http-client-connect *client* *aio*)
  (aio-wait *aio*)
  (format t "~%1.connect: aio result ~A" (aio-result *aio*))
  ;; get connection
  (setf *conn* (aio-get-output *aio* 0))
  ;; send request and wait
  (http-conn-write-req *conn* *req* *aio*)
  (aio-wait *aio*)
  (format t "~%2.sent req:  ~A" (aio-result *aio*))
  ;; read response
  (http-conn-read-res *conn* *res* *aio*)
  (aio-wait *aio*)
  (format t "~%3.read res:  ~A" (aio-result *aio*))
  ;; status
  (let ((status (http-res-get-status *res*)))
    (format t "~%4.status ~d" status))
  ;; header
  (let ((hdr (http-res-get-header *res* "Content-Length")))
    (format t "~%5.Content-length ~A" hdr))
  ;; 
  )


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

(defun iov-make(bytes &optional buf)
  (let ((iov (foreign-alloc '(:struct iov))))
    (with-foreign-slots ((iov-buf  iov-len) iov
			 (:struct iov))
      (setf iov-buf (or buf (foreign-alloc :char :count bytes))
	    iov-len bytes))
    iov))


(defparameter *iov* ;;(cffi::foreign-alloc '(:struct iov))
  (iov-make 33))

(defun t3 (len)
  (with-foreign-slots ((iov-buf
			iov-len) *iov* (:struct iov))
    (setf iov-buf (foreign-alloc :char :count (1+ len) )
	  iov-len len)
    (aio-set-iov *aio* 1 *iov*)
    (http-conn-read-all *conn* *aio*)
    (aio-wait *aio*)
    (format t "~%10.read  ~A" (aio-result *aio*))
    (%dump iov-buf)    ))

(defun t3 ()
  (aio-set-iov *aio* 1 *iov*)
  (http-conn-read-all *conn* *aio*)
  (aio-wait *aio*)
  (format t "~%10.read  ~A" (aio-result *aio*))
  (%dump (iov-buf *iov*))    )


(defun t3 ()
  (with-iov (iov 32 :extra 1)
    (setf iov-len 32)
    (aio-set-iov *aio* 1 iov)
    (http-conn-read-all *conn* *aio*)
    (aio-wait *aio*)
    (format t "~%10.read  ~A" (aio-result *aio*))
    (%dump (iov-buf iov)))    )

(defun test ()
  (t1)
  (t2)
  (t3 32))


(defun qqq ()
  (with-iov (x 32)))
