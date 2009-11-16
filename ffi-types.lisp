;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; Grovel definitions for libieee1284.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(include "ieee1284.h")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cc-flags "-lieee1284")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.ieee1284)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cstruct pointers-to-parports "struct parport_list"
	 (portc "portc" :type :int)
	 (portv "portv" :type :pointer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cstruct parport "struct parport"
	 (name           "name"        :type :string)
	 (base-address   "base_addr"   :type :unsigned-long)
	 (hibase-address "hibase_addr" :type :unsigned-long )
	 (filename       "filename"    :type :string))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cenum (e1284 :define-constants t)
       ((:ok          "E1284_OK"))
       ((:notimpl     "E1284_NOTIMPL"))     
       ((:notavail    "E1284_NOTAVAIL"))    
       ((:timedout    "E1284_TIMEDOUT"))    
       ((:rejected    "E1284_REJECTED"))    
       ((:negfailed   "E1284_NEGFAILED"))   
       ((:nomem       "E1284_NOMEM"))       
       ((:init        "E1284_INIT"))        
       ((:sys         "E1284_SYS"))         
       ((:noid        "E1284_NOID"))        
       ((:invalidport "E1284_INVALIDPORT")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cenum (ieee1284_open_flags :define-constants t)
       ((:excl "F1284_EXCL")
        :documentation
        "This device cannot share the port with any other device."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cenum (ieee1284_capabilities :define-constants t)
       ((:raw    "CAP1284_RAW")
        :documentation
        "Pin-level access.")
       ((:nibble "CAP1284_NIBBLE")
        :documentation
        "There is an implementation of nibble mode for this port.")
       ((:byte   "CAP1284_BYTE")
        :documentation
        "There is an implementation of byte mode for this port.")
       ((:compat "CAP1284_COMPAT")
        :documentation
        "There is an implementation of compatibility mode for this port.")
       ((:ecp    "CAP1284_ECP")
        :documentation
        "There is a hardware implementation of ECP mode for this port.")
       ((:ecprle "CAP1284_ECPRLE")
        :documentation
        "There is an RLE-aware implementation of ECP mode for this port
         (the F1284_RLE flag is recognised by the ECP transfer functions).")
       ((:ecpswe "CAP1284_ECPSWE")
       :documentation
        "There is a software implementation of ECP mode for this port.")
       ((:becp   "CAP1284_BECP")
       :documentation
        "There is an implementation of bounded ECP mode for this port.")
       ((:epp    "CAP1284_EPP")
       :documentation
        "There is a hardware implementation of EPP mode for this port.")
       ((:eppswe "CAP1284_EPPSWE")
       :documentation
        "There is a software implementation of EPP mode for this port.")
       ((:irq    "CAP1284_IRQ")
       :documentation
        "An interrupt line is configured for this port and interrupt
         notifications can be received using ieee1284_get_irq_fd(3).")
       ((:dma    "CAP1284_DMA")
        :documentation "A DMA channel is configured for this port.)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
