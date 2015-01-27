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

(cffi:define-foreign-type mpi-error-type ()
  ()
  (:actual-type :int)
  (:simple-parser mpi-error-code))

(define-condition mpi-error-condition (error)
  ((error-code :initarg :error-code :reader error-code))
  (:report (lambda (c stream) (princ (mpi-error-string (error-code c)) stream)))
  (:documentation "Signalled when a MPI function returns a code other than MPI_SUCCESS."))

(defmethod cffi:translate-from-foreign (value (type mpi-error-type))
  "Raise a MPI-CODE-ERROR if VALUE, a mpi-code, is non-zero."
  (unless (zerop value)
    (restart-case
        (error 'mpi-error-condition :error-code value)
      (ignore () nil))))

;;; blocking communication
(cffi:defcfun ("MPI_Bsend" MPI_Bsend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Recv" MPI_Recv) mpi-error-code (buf :pointer)(count :int) (datatype MPI_Datatype)(source :int)(tag :int)(comm MPI_Comm)(status :pointer))
(cffi:defcfun ("MPI_Send" MPI_Send) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Ssend" MPI_Ssend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Rsend" MPI_Rsend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Buffer_attach" MPI_Buffer_attach) mpi-error-code (buf :pointer) (count :int))
(cffi:defcfun ("MPI_Buffer_detach" MPI_Buffer_detach) mpi-error-code (buf :pointer) (count-ptr :pointer))
(cffi:defcfun ("MPI_Get_count" MPI_Get_count) mpi-error-code (status :pointer) (datatype MPI_Datatype) (count :pointer))

(cffi:defcfun ("MPI_Sendrecv" MPI_Sendrecv) mpi-error-code
  (send-buf :pointer)(send-count :int) (send-datatype MPI_Datatype)(dest :int) (send-tag :int)
  (recv-buf :pointer)(recv-count :int) (recv-datatype MPI_Datatype)(source :int) (recv-tag :int)
  (comm MPI_Comm)(status :pointer))

(cffi:defcfun ("MPI_Probe" MPI_Probe) mpi-error-code (source :int)(tag :int)(communicator MPI_Comm)  (status :pointer))

;;; non blocking communication
(cffi:defcfun ("MPI_Isend" MPI_Isend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(cffi:defcfun ("MPI_Ibsend" MPI_Ibsend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(cffi:defcfun ("MPI_Issend" MPI_Issend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))
(cffi:defcfun ("MPI_Irsend" MPI_Irsend) mpi-error-code (buf :pointer) (count :int) (datatype MPI_Datatype) (dest :int) (tag :int) (comm MPI_Comm)(request :pointer))

(cffi:defcfun ("MPI_Wait" MPI_Wait) mpi-error-code (request :pointer) (status :pointer))
(cffi:defcfun ("MPI_Waitall" MPI_Waitall) mpi-error-code (count :int)(requests :pointer) (statuses :pointer))
(cffi:defcfun ("MPI_Waitany" MPI_Waitany) mpi-error-code (count :int)(requests :pointer)(completed-index :pointer)(status :pointer))
(cffi:defcfun ("MPI_Waitsome" MPI_Waitsome) mpi-error-code (count :int)(requests :pointer)(outcount :pointer)(completed-indices :pointer)(statuses :pointer))

(cffi:defcfun ("MPI_Test" MPI_Test) mpi-error-code (request :pointer) (flag :pointer) (status :pointer))
(cffi:defcfun ("MPI_Testall" MPI_Testall) mpi-error-code (count :int) (requests :pointer) (flag :pointer) (statuses :pointer))
(cffi:defcfun ("MPI_Testany" MPI_Testany) mpi-error-code (count :int) (requests :pointer) (index :pointer)(flag :pointer) (statuses :pointer))
(cffi:defcfun ("MPI_Testsome" MPI_Testsome) mpi-error-code (count :int)(requests :pointer)(outcount :pointer)(completed-indices :pointer)(statuses :pointer))

;;; collective operations
(cffi:defcfun ("MPI_Bcast" MPI_Bcast) :int (buf :pointer) (count :int) (datatype MPI_Datatype) (root :int) (comm MPI_Comm))
(cffi:defcfun ("MPI_Reduce" MPI_Reduce) :int (sendbuf :pointer) (recvbuf :pointer)(count :int)(datatype MPI_datatype)(op MPI_Op)(root :int)(comm MPI_Comm))
(cffi:defcfun ("MPI_Allreduce" MPI_Allreduce) :int (sendbuf :pointer) (recvbuf :pointer)(count :int)(datatype MPI_datatype)(op MPI_Op)(comm MPI_Comm))
(cffi:defcfun ("MPI_Scatter" MPI_Scatter) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
              (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(root :int)(comm MPI_Comm))
(cffi:defcfun ("MPI_Gather" MPI_Gather) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
              (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(root :int)(comm MPI_Comm))
(cffi:defcfun ("MPI_Allgather" MPI_Allgather) :int (sendbuf :pointer) (sendcount :int)(sendtype MPI_datatype)
              (recvbuf :pointer)(recvcount :int)(recvtype MPI_datatype)(comm MPI_Comm))
