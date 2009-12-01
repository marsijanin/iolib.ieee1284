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
(cenum (e1284)
       ((:ok          "E1284_OK")
        :documentation "Everything went fine")
       ((:notimpl     "E1284_NOTIMPL")
        :documentation "Not implemented in libieee1284")     
       ((:notavail    "E1284_NOTAVAIL")
        :documentation "Not available on this system")    
       ((:timedout    "E1284_TIMEDOUT")
        :documentation "Operation timed out")    
       ((:rejected    "E1284_REJECTED")
        :documentation "IEEE 1284 negotiation rejected")    
       ((:negfailed   "E1284_NEGFAILED")
        :documentation "Negotiation went wrong")   
       ((:nomem       "E1284_NOMEM")
        :documentation "No memory left")       
       ((:init        "E1284_INIT")
        :documentation "Error initialising port")        
       ((:sys         "E1284_SYS")
        :documentation "Error interfacing system")         
       ((:noid        "E1284_NOID")
        :documentation "No IEEE 1284 ID available")        
       ((:invalidport "E1284_INVALIDPORT")
        :documentation "Invalid port"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cenum (ieee1284_open_flags)
       ((:excl "F1284_EXCL")
        :documentation
        "This device cannot share the port with any other device."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cenum (ieee1284_capabilities)
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
(cenum (ieee1284_status_bits)
       ((:nfault   "S1284_NFAULT"))
       ((:select   "S1284_SELECT"))
       ((:perror   "S1284_PERROR"))
       ((:nack     "S1284_NACK"))
       ((:busy     "S1284_BUSY"))
       ((:inverted "S1284_INVERTED")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cenum (ieee1284_control_bits)
       ((:nstrobe   "C1284_NSTROBE"))
       ((:nautofd   "C1284_NAUTOFD"))
       ((:ninit     "C1284_NINIT"))
       ((:nselectin "C1284_NSELECTIN"))
       ((:inverted  "C1284_INVERTED")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cenum (ieee1284_modes)
      ((:nibble           "M1284_NIBBLE")
       :documentation
       "Nibble mode. This peripheral-to-host mode uses the status lines
        to read data from the peripheral four bits at a time.")
      ((:byte             "M1284_BYTE")
       :documentation
       "Byte mode. This peripheral-to-host mode uses the data lines in reverse
         mode to read data from the peripheral a byte at a time.")
      ((:compat           "M1284_COMPAT")
       :documentation
       "Compatibility mode. Normal printer protocol. This is not a
        negotiated mode, but is the default mode in absence of
        negotiation.  ieee1284_negotiate(port, M1284_COMPAT) is
        equivalent to ieee1284_terminate(port). This
        host-to-peripheral mode is used for sending data to printers,
        and is historically the mode that has been used for that
        before IEEE 1284.")
      ((:becp             "M1284_BECP")
       :documentation
       "Bounded ECP is a modification to ECP that makes it more robust
        at the point that the direction is changed. (Unfortunately it is not
        yet implemented in the Linux kernel driver.)")
      ((:ecp              "M1284_ECP")
       :documentation
       "ECP mode. On entry to ECP mode it is a host-to-peripheral
        (i.e. forward) mode, but it may be set to reverse mode using
        ieee1284_ecp_fwd_to_rev(3). It is common for PC hardware to provide
        assistance with this mode by the use of a FIFO which the host (or, in
        reverse mode, the peripheral) may fill, so that the hardware can do
        the handshaking itself.")
      ((:ecprle           "M1284_ECPRLE")
       :documentation
       "ECP with run length encoding. In this mode, consecutive data bytes
        of the same value may be transferred in only a few cycles.")
      ((:ecpswe           "M1284_ECPSWE"))
      ((:epp              "M1284_EPP")
       :documentation
       "EPP mode. In this bi-directional mode the direction of data transfer
        is signalled at each byte.")
      ((:eppsl            "M1284_EPPSL"))
      ((:eppswe           "M1284_EPPSWE"))
      ((:flag-device-id   "M1284_FLAG_DEVICEID")
       :documentation
       "Device ID retrieval. This flag may be combined with a nibble, byte,
        or ECP mode to notify the device that it should send its IEEE 1284
        Device ID when asked for data.")
      ((:flage-ext-link "M1284_FLAG_EXT_LINK")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
