;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; Condiotions wrappers for e1284 cenum error statuses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.ieee1284)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *error-stats-docs*
    '(:notimpl     "Not implemented in libieee1284"
      :notavail    "Not available on this system"
      :timedout    "Operation timed out"
      :rejected    "IEEE 1284 negotiation rejected"
      :negfailed   "Negotiation went wrong"  
      :nomem       "No memory left"
      :init        "Error initialising port"
      :sys         "Error interfacing system"
      :noid        "No IEEE 1284 ID available"
      :invalidport "Invalid port"))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *error-condirions-names*
    (mapcar #'(lambda (x)
		(intern (string-upcase (format nil "ieee1284-~a-error" x))))
	    (remove-if #'stringp *error-stats-docs*)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-foreign-type ieee1284-status ()
    ()
    (:actual-type e1284)
    (:simple-parser e1284-st))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-condition ieee1284-error (simple-error)
    ((parport :reader ieee1284-error-parport :initarg :parport)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (macrolet
      ((define-ieee1284-conditions ()
	 (with-gensyms (stream condition p)
	   `(progn ,@(mapcar #'(lambda (name doc)
				 `(define-condition ,name (ieee1284-error)
				    ()
				    (:report
                                     (lambda (,condition ,stream)
                                       (with-accessors 
                                             ((,p ieee1284-error-parport))
                                           ,condition
                                         (format ,stream "~a ~:[~;~a~]"
                                                 ,doc ,p ,p))))))
			     *error-condirions-names*
			     (remove-if #'keywordp *error-stats-docs*))))))
    (define-ieee1284-conditions))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defmethod expand-from-foreign (value (type ieee1284-status))
    (with-gensyms (retval)
      `(let ((,retval (foreign-enum-keyword 'e1284 ,value)))
	 (case ,retval
	   ,@(mapcar #'(lambda (status condition)
			 `(,status (error ',condition)))
		     (remove-if #'stringp *error-stats-docs*)
		     *error-condirions-names*)
	   (:ok ,retval))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
