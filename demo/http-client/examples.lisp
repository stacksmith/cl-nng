(in-package :nng)

;;==============================================================================
;; push-pull
;;
;;
;; push:
(defparameter sock-s nil)
(defparameter sock-r nil)

(defparameter *url* "ipc:///tmp/pipeline.ipc1" )

#||
(defun pusher (msg)
  (let ((msg-length (1+ (length msg))))
    (setf sock-s (push0-open))
    (dial sock-s *url*)
    (format t "~%Sending ~A" msg)
    (with-foreign-string (buf msg)
      (send sock-s buf msg-length))
    (sleep 1)
    (close sock-s))
  )

(defun puller ()
  (setf sock-r (pull0-open))
  (listen sock-r *url*)
  (multiple-value-bind (result buf size) (recv sock-r)
    (format t "~%RECEIVED: ~A ~A ~A" result buf size)
    (format t "~%~A" (foreign-string-to-lisp buf :max-chars 64 ))
    (free buf size))
  (close sock-r)
  )
||#
(defun pusher (msg)
  (with-socket (sock-s push0)
    (dial sock-s *url*)
    (send-string sock-s msg)
    (sleep 0.001))
  )



(defun puller ()
  (with-socket (sock-r pull0)
    (listen sock-r *url*)
    (recv-on (sock-r)
      (format t "~%RECEIVED: ~A ~A ~A" result buf bufsize)
      (format t "~%~A" (foreign-string-to-lisp buf :max-chars 64 )))))

;;=====================
#||(defun replyer ()
  (setf sock-s (rep0-open))
  (listen sock-s *url*)
  (multiple-value-bind (result buf size) (recv sock-s)
    (format t "~%~A" (foreign-string-to-lisp buf ))
    (free buf size))
  (with-foreign-string (buf "asshole")
    (send sock-s buf 8))
  (close sock-s))


(defun replyer ()
  (setf sock-s (rep0-open))
  (listen sock-s *url*)
  (recv-on (sock-s)
    (format t "~%~A" (foreign-string-to-lisp buf )))
  (send-string sock-s "assholes")
  (close sock-s))


(defun requestor ()
  (setf sock-r (req0-open))
  (dial sock-r *url*)
  (with-foreign-string (buf "req,")
    (send sock-r buf 5)
    )
  (multiple-value-bind (result buf size) (recv sock-r)
    (format t "%Received ~A" (foreign-string-to-lisp buf :max-chars 64) )
    (free buf size))
  (close sock-r))

(defun requestor ()
  (setf sock-r (req0-open))
  (dial sock-r *url*)
  (send-string sock-r "req..")
  (recv-on (sock-r)
    (format t "%Received ~A" (foreign-string-to-lisp buf :max-chars 64) ))
  (close sock-r))
||#





(defun replyer ()
  (with-socket (sock-s rep0)
    (listen sock-s *url*)
    (recv-on (sock-s)
      (format t "~%~A" (foreign-string-to-lisp buf )))
    (send-string sock-s "assholes")))

(defun requestor ()
  (with-socket (sock-r req0)
    (dial sock-r *url*)
    (send-string sock-r "req..")
    (recv-on (sock-r)
      (format t "%Received ~A" (foreign-string-to-lisp buf :max-chars 64) ))))

