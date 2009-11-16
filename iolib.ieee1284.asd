;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(asdf:defsystem :iolib.ieee1284
  :description "libieee1284 wrapers"
  :maintainer "Razbegaev N.V. <marsijanin@gmail.com>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.streams)
  :serial t
  :components
  ((:file "pkgdcl")
   (cffi-grovel:grovel-file "ffi-types")
   (:file "ffi-functions")
   (:file "wrappers")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
