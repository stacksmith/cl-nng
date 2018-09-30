
(in-package #:nng)

;;==============================================================================
;; HTTP
;;
(def (http-client-alloc :kind alloc) url)
(def (http-client-connect) client aio)
(def (http-client-free)    client)
(def (http-client-get-tls :kind check) client cfgp)
(def (http-client-set-tls :kind check) client cfg)

;;==============================================================================
;;  CONN
(def   (http-conn-close) conn)
(def   (http-conn-read)      conn aio)
(def   (http-conn-read-all)  conn aio)
(def   (http-conn-read-req)  conn req aio)
(def   (http-conn-read-res)  conn res aio)
(def   (http-conn-write)     conn aio)
(def   (http-conn-write-all) conn aio)
(def   (http-conn-write-req) conn req aio)
(def   (http-conn-write-res) conn res aio)
;;==============================================================================
;;  HANDLERn
(def   (http-handler-alloc :kind alloc)          path func) ;;TODO fix callback
(def   (http-handler-alloc-directory :kind alloc) path dirname)
(def   (http-handler-alloc-file      :kind alloc) path filename)
(def   (http-handler-alloc-static    :kind alloc) path data size content-type) 
(def   (http-handler-free)     handler)
(def   (http-handler-get-data :kind check) handler)
(def   (http-handler-set-data :kind check) handler data dtor) ;;***
(def   (http-handler-set-host :kind check) handler host) ;;***
(def   (http-handler-set-tree :kind check) handler ) ;;***

(def   (http-hijack) conn)

;;==============================================================================
;;  REQ

(def   (http-req-add-header :kind check) req key val)
(def   (http-req-alloc  :kind alloc)           url)
(def   (http-req-copy-data :kind check)  req body size)
(def   (http-req-del-header :kind check) req key)
(def   (http-req-free)        req)
(def   (http-req-get-header)  req key)
(def   (http-req-get-method)  req )
(def   (http-req-get-uri)     req )
(def   (http-req-get-version) req )
(def   (http-req-set-data :kind check)   req body size)
(def   (http-req-set-header :kind check) req key val)
(def   (http-req-set-method :kind check) req method)
(def   (http-req-set-uri :kind check)    req uri)
(def   (http-req-set-version :kind check) req version)
;;==============================================================================
;;  RES

(def   (http-res-add-header :kind check) res key val)
(def   (http-res-alloc  :kind alloc) )
(def   (http-res-alloc-error  :kind alloc) status)
(def   (http-res-copy-data :kind check)  res body size)
(def   (http-res-del-header :kind check) res key)
(def   (http-res-free)       res)
(def   (http-res-get-header) res key)
(def   (http-res-get-reason) res )
(def   (http-res-get-status) res )
(def   (http-res-get-version)res )
(def   (http-res-set-data :kind check)   res body size)
(def   (http-res-set-header :kind check) res key val)
(def   (http-res-set-reason :kind check) res reason)
(def   (http-res-set-status :kind check) res status)
(def   (http-res-set-version :kind check)res version)
;;==============================================================================
;;  SERVER

(def   (http-server-add-handler :kind check) server handler)
(def   (http-server-del-handler :kind check) server handler)
(def   (http-server-get-tls :kind check) server cfgp)

(def   (http-server-hold   :kind alloc) cfgp)
(def   (http-server-release) server)
(def   (http-server-set-tls :kind check) server cfg)

(def   (http-server-start :kind check)   server)
(def   (http-server-stop :kind check)   server)
