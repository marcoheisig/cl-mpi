#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI bindings for Common Lisp

Copyright (c) 2008,2009  Alex Fukunaga
Copyright (C) 2014 Marco Heisig <marco.heisig@fau.de>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package #:mpi)

#-openmpi
(progn
  (defctype MPI_Errhandler :int)
  (defctype MPI_Comm :int)
  (defctype MPI_Group :int)
  (defctype MPI_Op :int)
  (defctype MPI_Datatype :int)

  (defmacro define-mpi-var (name)
    (let* ((str (symbol-name name))
           (header-sym (find-symbol str "MPI-HEADER")))
      (if (boundp header-sym)
          `(defconstant ,name ,(symbol-value header-sym))
          `(values)))))

#+openmpi
(progn
  (defctype MPI_Errhandler :pointer)
  (defctype MPI_Comm :pointer)
  (defctype MPI_Group :pointer)
  (defctype MPI_Datatype :pointer)
  (defctype MPI_Op :pointer)

  (defmacro define-mpi-var (name)
    (let* ((str (symbol-name name))
           (openmpi-name (concatenate 'string "ompi_" (string-downcase str))))
      (if (foreign-symbol-pointer openmpi-name)
          `(defvar ,name (foreign-symbol-pointer ,openmpi-name))
          `(values)))))

(defconstant MPI_VERSION mpi-header::MPI_VERSION)
(defconstant MPI_SUBVERSION mpi-header::MPI_VERSION)

(defconstant MPI_MAX_ERROR_STRING mpi-header::MPI_MAX_ERROR_STRING)
(defconstant MPI_MAX_PROCESSOR_NAME mpi-header::MPI_MAX_PROCESSOR_NAME)
(defconstant MPI_ANY_TAG mpi-header::MPI_ANY_TAG)

(defvar MPI_STATUS_IGNORE (make-pointer mpi-header::MPI_STATUS_IGNORE))

(define-mpi-var MPI_ERRORS_RETURN)
(define-mpi-var MPI_ERRORS_ARE_FATAL)
(define-mpi-var MPI_GROUP_EMPTY)
(define-mpi-var MPI_GROUP_NULL)
(define-mpi-var MPI_COMM_WORLD)
(define-mpi-var MPI_COMM_SELF)
(define-mpi-var MPI_COMM_NULL)
(define-mpi-var MPI_DATATYPE_NULL)
(define-mpi-var MPI_LB)
(define-mpi-var MPI_UB)
(define-mpi-var MPI_CHAR)
(define-mpi-var MPI_SIGNED_CHAR)
(define-mpi-var MPI_UNSIGNED_CHAR)
(define-mpi-var MPI_BYTE)
(define-mpi-var MPI_SHORT)
(define-mpi-var MPI_UNSIGNED_SHORT)
(define-mpi-var MPI_INT)
(define-mpi-var MPI_UNSIGNED)
(define-mpi-var MPI_LONG)
(define-mpi-var MPI_UNSIGNED_LONG)
(define-mpi-var MPI_LONG_LONG_INT)
(define-mpi-var MPI_UNSIGNED_LONG_LONG)
(define-mpi-var MPI_FLOAT)
(define-mpi-var MPI_DOUBLE)
(define-mpi-var MPI_LONG_DOUBLE)
(define-mpi-var MPI_WCHAR)
(define-mpi-var MPI_C_BOOL)
(define-mpi-var MPI_INT8_T)
(define-mpi-var MPI_INT16_T)
(define-mpi-var MPI_INT32_T)
(define-mpi-var MPI_INT64_T)
(define-mpi-var MPI_UINT8_T)
(define-mpi-var MPI_UINT16_T)
(define-mpi-var MPI_UINT32_T)
(define-mpi-var MPI_UINT64_T)
(define-mpi-var MPI_PACKED)
(define-mpi-var MPI_NULL)
(define-mpi-var MPI_MIN)
(define-mpi-var MPI_MAX)
(define-mpi-var MPI_SUM)
(define-mpi-var MPI_PROD)
(define-mpi-var MPI_LAND)
(define-mpi-var MPI_BAND)
(define-mpi-var MPI_LOR)
(define-mpi-var MPI_BOR)
(define-mpi-var MPI_LXOR)
(define-mpi-var MPI_BXOR)
(define-mpi-var MPI_MAXLOC)
(define-mpi-var MPI_MINLOC)
(define-mpi-var MPI_REPLACE)
(define-mpi-var MPI_NO_OP)

(define-foreign-type mpi-error-type ()
  ()
  (:actual-type :int)
  (:simple-parser mpi-error-code))

(define-condition mpi-error-condition (error)
  ((error-code :initarg :error-code :reader error-code))
  (:report
   (lambda (c stream)
     (princ (mpi-error-string (error-code c))
            stream)))
  (:documentation "Signalled when a MPI function returns a code other than MPI_SUCCESS."))

(defun signal-mpi-error (value)
  "Raise a MPI-CODE-ERROR if VALUE, a mpi-code, is non-zero."
  (restart-case
      (error 'mpi-error-condition :error-code value)
    (ignore () nil)))

(defmethod expand-from-foreign (value (type mpi-error-type))
  (let ((return-value (gensym)))
    `(let ((,return-value ,value))
       (unless (zerop ,return-value)
         (signal-mpi-error ,return-value)))))

;; all native MPI functions qualify for inlining - they are used only once or
;; twice in their respective wrapper functions and provide potential for
;; optimization

(declaim (inline MPI_Send MPI_Recv))

;; blocking communication
(defcfun ("MPI_Bsend" MPI_Bsend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(defcfun ("MPI_Recv" MPI_Recv) mpi-error-code (buf :pointer)(count :int) (datatype MPI_Datatype)(source :int)(tag :int)(comm MPI_Comm)(status (:pointer (:struct MPI_Status))))
(defcfun ("MPI_Send" MPI_Send) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(defcfun ("MPI_Ssend" MPI_Ssend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(defcfun ("MPI_Rsend" MPI_Rsend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(defcfun ("MPI_Buffer_attach" MPI_Buffer_attach) mpi-error-code (buf :pointer) (count :int))
(defcfun ("MPI_Buffer_detach" MPI_Buffer_detach) mpi-error-code (buf :pointer) (count-ptr :pointer))
(defcfun ("MPI_Get_count" MPI_Get_count) mpi-error-code (status :pointer) (datatype MPI_Datatype) (count :pointer))

(defcfun ("MPI_Sendrecv" MPI_Sendrecv) mpi-error-code
  (send-buf :pointer)(send-count :int) (send-datatype MPI_Datatype)(dest :int) (send-tag :int)
  (recv-buf :pointer)(recv-count :int) (recv-datatype MPI_Datatype)(source :int) (recv-tag :int)
  (comm MPI_Comm)(status :pointer))

(defcfun ("MPI_Probe" MPI_Probe) mpi-error-code (source :int)(tag :int)(communicator MPI_Comm)  (status :pointer))

;; non blocking communication

(defcfun ("MPI_Isend" MPI_Isend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(defcfun ("MPI_Ibsend" MPI_Ibsend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(defcfun ("MPI_Issend" MPI_Issend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(defcfun ("MPI_Irsend" MPI_Irsend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))

(defcfun ("MPI_Wait" MPI_Wait) mpi-error-code (request :pointer) (status :pointer))
(defcfun ("MPI_Waitall" MPI_Waitall) mpi-error-code (count :int)(requests :pointer) (statuses :pointer))
(defcfun ("MPI_Waitany" MPI_Waitany) mpi-error-code (count :int)(requests :pointer)(completed-index :pointer)(status :pointer))
(defcfun ("MPI_Waitsome" MPI_Waitsome) mpi-error-code (count :int)(requests :pointer)(outcount :pointer)(completed-indices :pointer)(statuses :pointer))

(defcfun ("MPI_Test" MPI_Test) mpi-error-code (request :pointer) (flag :pointer) (status :pointer))
(defcfun ("MPI_Testall" MPI_Testall) mpi-error-code (count :int) (requests :pointer) (flag :pointer) (statuses :pointer))
(defcfun ("MPI_Testany" MPI_Testany) mpi-error-code (count :int) (requests :pointer) (index :pointer)(flag :pointer) (statuses :pointer))
(defcfun ("MPI_Testsome" MPI_Testsome) mpi-error-code (count :int)(requests :pointer)(outcount :pointer)(completed-indices :pointer)(statuses :pointer))

;; collective operations

(defcfun ("MPI_Bcast" MPI_Bcast) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (root :int) (comm MPI_Comm))
(defcfun ("MPI_Reduce" MPI_Reduce) :int (sendbuf :pointer) (recvbuf :pointer)(count :int)(datatype MPI_datatype)(op MPI_Op)(root :int)(comm MPI_Comm))
(defcfun ("MPI_Allreduce" MPI_Allreduce) :int (sendbuf :pointer) (recvbuf :pointer)(count :int)(datatype MPI_datatype)(op MPI_Op)(comm MPI_Comm))
(defcfun ("MPI_Scatter" MPI_Scatter) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
         (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(root :int)(comm MPI_Comm))
(defcfun ("MPI_Gather" MPI_Gather) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
         (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(root :int)(comm MPI_Comm))
(defcfun ("MPI_Allgather" MPI_Allgather) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
         (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(comm MPI_Comm))

;; misc

(defcfun ("MPI_Comm_set_errhandler" MPI_Comm_set_errhandler) mpi-error-code (comm MPI_Comm) (errhandler MPI_Errhandler))
(defcfun ("MPI_Init" MPI_Init) mpi-error-code (argc :pointer) (argv :pointer))


