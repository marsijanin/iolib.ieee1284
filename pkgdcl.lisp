;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; libieee1284 wrapers -  package definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :iolib.ieee1284
  (:nicknames #:coffe)
  (:use :common-lisp :cffi)
  (:import-from :iolib.syscalls :defcfun* :defentrypoint :size-t)
  (:import-from :alexandria :with-gensyms)
  (:export
   ;; C functions wrappers 
   #:%%read-status-lines 
   #:%write-data-lines 
   #:%close 
   #:%ecp-reverse-to-forward 
   #:%read-data-lines 
   #:%wait-status-lines 
   #:%free-ports 
   #:%%read-control-lines 
   #:%get-irq-id 
   #:%ecp-forward-to-reverse 
   #:%do-nack-handshake 
   #:%release 
   #:%set-timeout 
   #:%clear-irq-id 
   #:%read-status-lines 
   #:%claim 
   #:%open 
   #:%wait-data-lines 
   #:%data-lines-direction 
   #:%write-control-lines 
   #:%terminate 
   #:%frob-control-lines 
   #:%negotiate 
   #:%get-device-id 
   #:%find-ports 
   #:%read-control-lines
   ;; clos
   #:parallel-port
   #:parallel-port-base-address
   #:parallel-port-hibase-address
   #:parallel-port-name
   #:parallel-port-filename
   #:*parallel-ports*
   ;; paralle ports finder
   #:find-parallel-ports
   ;; open/close
   #:open-parallel-port
   #:close-parallel-port
   ;; lines status accessors
   #:read-data-lines
   #:read-status-lines
   #:read-control-lines
   #:read-lines
   ;; `with` macro:
   #:with-parallel-ports
   #:with-parallel-port))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
