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
(defclass existing-paralel-port (parallel-port)
  ((index                :reader parallel-port-index :type fixnum
                         :initform 0 :initarg :index)
   (capabilities-pointer :accessor parallel-port-capabilities-pointer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod shared-initialize :after ((parport existing-paralel-port) slot-names
                                     &rest initargs &key pointer)
  (declare (ignorable initargs pointer slot-names))
  (let ((ptr (foreign-alloc :int)))
    (setf (parallel-port-capabilities-pointer parport) ptr)
    (trivial-garbage:finalize parport #'(lambda () (foreign-free ptr)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *parallel-ports* nil "List of all avaliable parallel ports")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ieee1284_open need struct parport from struct parport_list
;; i.e. if we create struct parport instance, setup base_addr etc.
;; and pass pointer to ieee1284_open it will case segfault or another error
(defparameter *pointers-to-parports* nil "struct parport_list instance.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-parallel-ports ()
  "Setf `*parallel-ports*' to the list of avaliable parallel ports
   if cat find some and return it."
  (labels
      ((avaliable-parports-list ()
         (if *pointers-to-parports*
             ;; not first call - ned to clean up
             (%free-ports *pointers-to-parports*)
             ;; first call - need to allocate memory first
             (setf *pointers-to-parports*
                   (foreign-alloc 'pointers-to-parports)))
         (when (eql (%find-ports *pointers-to-parports* 0) :ok)
           (with-foreign-slots ((portc portv)
                                *pointers-to-parports*
                                pointers-to-parports)
             (loop
                :for i :below portc :collect
                ;; `:pointer' in mem-aref is `type' parameter value
                (make-instance 'existing-paralel-port
                               :pointer (mem-aref portv :pointer i)
                               :index i)
                :into parports
                :finally (when parports
                          (return parports)))))))
    (let ((avaliable-parports (avaliable-parports-list)))
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
(defun open-parallel-port (parallel-port &key (flags :excl) capabilities)
  "Lispish ieee1284_open & ieee1284_claim wrapper.
   `parallel-port' can be: parallel port (file)name, base address or
   `*parallel-ports* element. `flags' can be `:excl' or 0 (unless whre will be
   more open flags in libieee1284). `capabilities' can be ieee1284_capabilities
   keywords lists or nil.
  
   Example:
   (open-parallel-port (car *parallel-ports*) :capabilities '(:raw))"
  (let ((parport (if (typep parallel-port 'existing-paralel-port)
                     parallel-port
                     (name-or-base->parallel-port parallel-port))))
    (when parport
      (with-accessors ((idx parallel-port-index)
                       (cap parallel-port-capabilities-pointer)) parport
        (when capabilities
          (setf (mem-aref cap :int)
                (apply #'logior
                       (mapcar #'(lambda (x)
                                   (foreign-enum-value 'ieee1284_capabilities
                                                       x))
                               capabilities))))
        (with-foreign-slots ((portv) *pointers-to-parports*
                             pointers-to-parports)
          (let ((ptr (mem-aref portv :pointer idx)))
            (%open ptr flags (if capabilities cap (null-pointer)))
            (%claim ptr)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun close-parallel-port (parallel-port)
  "Release and close `parallel-port' (`*parallel-ports*' element, (file)name
   or base address) opened by `open-parallel-port'"
  (let ((parport (if (typep parallel-port 'existing-paralel-port)
                     parallel-port
                     (name-or-base->parallel-port parallel-port))))
    (when parport
      (with-foreign-slots ((portv) *pointers-to-parports* pointers-to-parports)
        (let ((ptr (mem-aref portv :pointer (parallel-port-index parport))))
          (%release ptr)
          (%close ptr))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;