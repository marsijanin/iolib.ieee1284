;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; Low level C functions definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.ieee1284)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libieee1284
    (:unix (:or "libieee1284.so" "libieee1284.so.3"))
    (t (:default "libieee1284")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-foreign-library libieee1284))    ;</ (eval-when
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%find-ports "ieee1284_find_ports") e1284
  (parports :pointer)                   ;struct parports *list
  (flags    :int))                      ;shoul be zero 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%free-ports "ieee1284_free_ports") :void
  (parports :pointer))                  ;struct parports *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%open "ieee1284_open") e1284
  (parport      :pointer)               ;struct parport *
  (flags        ieee1284_open_flags)    
  (capabilities :pointer))              ;int *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%close "ieee1284_close") e1284
  (parport :pointer))                   ;struct parport *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%claim "ieee1284_claim") e1284
  (parport :pointer))                   ;struct parport *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%release "ieee1284_release") :void
  (parport :pointer))                   ;struct parport *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%get-device-id "ieee1284_get_deviceid") e1284
  (daisy  :int)
  (flags  :int)
  (buffer :string)
  (len    size-t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%read-data-lines "ieee1284_read_data") :int
  (parport :pointer))                   ;struct parport *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%write-data-lines "ieee1284_write_data") :void
  (parport :pointer)                    ;struct parport *
  (data    :uchar))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%data-lines-direction "ieee1284_wait_data") e1284
  (parport :pointer)                    ;struct parport *
  (reverse :int))                       ;zero means turn on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%wait-data-lines "ieee1284_wait_data") e1284
  (parport :pointer)                    ;struct parport *
  (mask    :uchar)
  (val     :uchar)
  (timeout :pointer))                   ;struct timeval *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ieee1284_status/control_bits do not cover all 255 values and
;; `foreign-enum-keyword' can signal an error in raw mode.
(defcfun* (%%read-status-lines "ieee1284_read_status") :int
  (parport :pointer))                   ;struct parport *
(defentrypoint %read-status-lines (pointer)
  (let ((status (%%read-status-lines pointer)))
    (or (foreign-enum-keyword 'ieee1284_status_bits status :errorp nil)
        status)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%wait-status-lines "ieee1284_wait_status") e1284
  (parport :pointer)                    ;struct parport *
  (mask    :uchar)
  (val     :uchar)
  (timeout :pointer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%%read-control-lines "ieee1284_read_control") :int
  (parport :pointer))                   ;struct parport *
(defentrypoint %read-control-lines (pointer)
  (let ((control (%%read-control-lines pointer)))
    (or (foreign-enum-keyword 'ieee1284_control_bits control :errorp nil)
        control)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%write-control-lines "ieee1284_write_control") :void
  (parport  :pointer)                   ;struct parport *
  (controll :uchar))                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%frob-control-lines "ieee1284_frob_control") :void
  (parport :pointer)                    ;struct parport *
  (mask    :uchar)
  (val     :uchar))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%do-nack-handshake "ieee1284_do_nack_handshake") e1284
  (parport         :pointer)            ;struct parport *
  (controll-before :uchar)
  (controll-after  :uchar)
  (timeout         :pointer))           ;struct timeval *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%negotiate "ieee1284_negotiate") e1284
  (parport :pointer)                    ;struct parport *
  (mode    ieee1284_modes))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%terminate "ieee1284_terminate") :void
  (parport :pointer))                   ;struct parport *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%ecp-forward-to-reverse "ieee1284_ecp_fwd_to_rev") e1284
  (parport :pointer))                   ;struct parport *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%ecp-reverse-to-forward "ieee1284_ecp_rev_to_fwd") e1284
  (parport :pointer))                   ;struct parport *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%get-irq-id "ieee1284_get_irq_fd") :int
  (parport :pointer))                   ;struct parport *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%clear-irq-id "ieee1284_clear_irq") e1284
  (parport :pointer)                    ;struct parport *
  (count :pointer))                     ;unsigned int *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcfun* (%set-timeout "ieee1284_set_timeout") :pointer
  (parport :pointer)                    ;struct parport *
  (timeout :pointer))                   ;struct timeval *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
