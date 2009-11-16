;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; libieee1284 wrapers -  package definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :iolib.ieee1284
  (:nicknames #:coffe)
  (:use :common-lisp :cffi)
  (:import-from :iolib.syscalls :defcfun* :size-t)
  (:export
   #:parallel-port
   #:parallel-port-base-address
   #:parallel-port-hibase-address
   #:parallel-port-name
   #:parallel-port-filename
   #:*parallel-ports*
   #:find-parallel-ports))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
