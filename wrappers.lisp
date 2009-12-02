;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;; C functions wrapers for libieee1284.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :iolib.ieee1284)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *parallel-ports* nil "List of all available paralel ports")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-c-struct-wrapper (parallel-port parport) ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object ((parport parallel-port) stream)
  (print-unreadable-object (parport stream)
    (format stream "Parallel port \"~a\" of base address #X~x"
            (parallel-port-name parport)
            (parallel-port-base-address parport))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass existing-parallel-port (parallel-port)
  ((index                 :reader parallel-port-index
                          :type (integer 0 #.array-rank-limit)
                          :initform 0 :initarg :index)
   (capabilities-pointer  :accessor parallel-port-capabilities-pointer)
   (parport-pointer       :reader   parallel-port-parport-pointer
                          :initarg  :parport-pointer)
   (pointers-to-parports  :reader   parallel-port-pointers-to-parports
                          :allocation :class :initarg :pointers-to-parports)
   (open-and-claim-p      :accessor parallel-port-open-and-claim-p
                          :initform nil :type boolean)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shared-initialize :after ((parport existing-parallel-port) slot-names
                                     &rest initargs &key pointer)
  (declare (ignorable initargs pointer slot-names))
  (let ((ptr (foreign-alloc :int)))
    (setf (parallel-port-capabilities-pointer parport) ptr)
    (trivial-garbage:finalize parport #'(lambda () (foreign-free ptr)))
    (when (slot-boundp parport 'pointers-to-parports)
      (with-slots (pointers-to-parports) parport
        (trivial-garbage:finalize parport
                                  #'(lambda ()
                                      (%free-ports pointers-to-parports)
                                      (foreign-free pointers-to-parports)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *parallel-ports* nil "List of all avaliable parallel ports")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ieee1284_open need struct parport from struct parport_list
;; i.e. if we create struct parport instance, setup base_addr etc.
;; and pass pointer to ieee1284_open it will case segfault or another error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-parallel-ports ()
  "Setf `*parallel-ports*' to the list of avaliable parallel ports
   if cat find some and return it."
  (labels
      ((avaliable-parports-list (pointers-to-parports)
         (if pointers-to-parports
             ;; not first call - ned to clean up
             (%free-ports pointers-to-parports)
             ;; first call - need to allocate memory first
             (setf pointers-to-parports
                   (foreign-alloc 'pointers-to-parports)))
         (when (eql (%find-ports pointers-to-parports 0) :ok)
           (with-foreign-slots ((portc portv)
                                pointers-to-parports
                                pointers-to-parports)
             (loop
                :for i :below portc 
                ;; `:pointer' in mem-aref is `type' parameter value
                :for ptr := (mem-aref portv :pointer i)
                :collect (make-instance 'existing-parallel-port
                                        :pointer ptr
                                        :parport-pointer ptr
                                        :pointers-to-parports pointers-to-parports
                                        :index i) :into parports
                :finally (when parports
                          (return parports)))))))
    (let* ((ptr (when *parallel-ports*
                  (parallel-port-pointers-to-parports (car *parallel-ports*))))
           (avaliable-parports (avaliable-parports-list ptr)))
      (when avaliable-parports
        (setf *parallel-ports* avaliable-parports)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun name-or-base->parallel-port (name-or-base)
  (labels
      ((name-pos (name)
         (find-if #'(lambda (x)
                          (string= name (parallel-port-name x)))
                      *parallel-ports*))
       (filename-pos (name)
         (find-if #'(lambda (x)
                          (string= name (parallel-port-filename x)))
                      *parallel-ports*))
       (base-pos (base)
         (find-if #'(lambda (x)
                          (= base (parallel-port-base-address x)))
                      *parallel-ports*)))
    (funcall
     (typecase name-or-base
       (string (if (char= #\/ (aref name-or-base 0))
                   #'filename-pos
                   #'name-pos))
       (integer #'base-pos)
       (t
        (error
         "You should specify parallel port (file)name or base address")))
     name-or-base)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parport (thing)
  "Return corresponding `parallel-port' instance from `*parallel-ports*'
   and pointers to corresponding struct parport nil.
   `thing' can be parallel port (file)name (i.e. \"parport0\" or
    \"/dev/parport0\"), or base address (i.e. #x378) "
  (let ((pp (if (typep thing 'existing-parallel-port)
                thing
                (name-or-base->parallel-port thing))))
    (when pp
      (values pp (parallel-port-parport-pointer pp)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-parallel-port (parallel-port &key (flags :excl) capabilities)
  "Lispish ieee1284_open & ieee1284_claim wrapper.
   `parallel-port' can be: parallel port (file)name, base address or
   `*parallel-ports* element. `flags' can be `:excl' or 0 (unless whre will be
   more open flags in libieee1284). `capabilities' can be ieee1284_capabilities
   keywords lists or nil.
  
   Example:
   (open-parallel-port (car *parallel-ports*) :capabilities '(:raw))"
  (multiple-value-bind  (parport ptr) (parport parallel-port)
    (when parport
      (with-accessors ((idx parallel-port-index)
                       (cap parallel-port-capabilities-pointer)
                       (oncp parallel-port-open-and-claim-p)) parport
        (when capabilities
          (setf (mem-aref cap :int)
                (apply #'logior
                       (mapcar #'(lambda (x)
                                   (foreign-enum-value 'ieee1284_capabilities
                                                       x))
                               capabilities))))
        (%open ptr flags (if capabilities cap (null-pointer)))
        (%claim ptr)
        (setf oncp t)
        parport))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun close-parallel-port (parallel-port)
  "Release and close `parallel-port' (`*parallel-ports*' element, (file)name
   or base address) opened by `open-parallel-port'"
  (multiple-value-bind (pp ptr) (parport-ptr parallel-port)
    (when ptr
      (%release ptr)
      (%close ptr)
      (setf (parallel-port-open-and-claim-p pp) nil)
      pp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype parallel-port-line () `(array bit (8)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype parallel-port-lines () `(array bit (3 8)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bit-vector->ub8 (bit-vector)
  (loop
     :with int := #b00000000
     :for bit :across bit-vector
     :for i :below (min 8 (length bit-vector))
     :do (setf (ldb (byte 1 i) int) bit)
     :finally (return int)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ub8->bit-vector (ub8)
  (loop
     :with bit-vector := (make-array 8 :element-type 'bit)
     :for  i :below (min 8 (integer-length ub8)) 
     :do (setf (aref bit-vector i) (ldb (byte 1 i) ub8))
     :finally (return bit-vector)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-data-lines (parallel-port &optional numberp)
  "Return data parallel port lines status as bit-vector,
   or as integer if `numberp' is T"
  (let ((data (%read-data-lines (parport-ptr parallel-port))))
    (if numberp data (ub8->bit-vector data))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-control-lines (parallel-port &optional numberp)
  "As `read-data-lines', but for control lines."
  (let ((control (%%read-control-lines (parport-ptr parallel-port))))
    (if numberp control (ub8->bit-vector control))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-status-lines (parallel-port &optional numberp)
  "As `read-data-lines', but for status lines."
  (let ((status (%read-status-lines (parport-ptr parallel-port))))
    (if numberp status (ub8->bit-vector status))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-lines (parallel-port)
  (make-array (list 3 8)
              :element-type 'bit
              :initial-contents 
              (list (read-data-lines parallel-port)
                    (read-status-lines parallel-port)
                    (read-control-lines parallel-port))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun any->ub8 (any)
  (if (integerp any)
      any
      (bit-vector->ub8 any)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-data-lines (parallel-port new-state)
  (%write-data-lines (parport-ptr parallel-port) (any->ub8 new-state)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-control-lines (parallel-port new-state)
  (%write-control-lines (parport-ptr parallel-port) (any->ub8 new-state)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
