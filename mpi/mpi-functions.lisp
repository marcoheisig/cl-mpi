#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI functions as specified in the MPI standard

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

(defcfun "MPI_Finalize" mpi-error-code
    "This routines cleans up all MPI state. Once this routine is called, no
MPI routine (even MPI-INIT) may be called. The user must ensure that all
pending communications involving a process completes before the process calls
MPI-FINALIZE.")

(defcfun ("MPI_Initialized" %mpi-initialized) mpi-error-code (flag :pointer))
(defcfun ("MPI_Abort" %mpi-abort) mpi-error-code (comm mpi-comm) (errorcode :int))
(defcfun ("MPI_Get_processor_name" %mpi-get-processor-name) mpi-error-code (processor-name :string) (namelen :pointer))
(defcfun ("MPI_Error_string" %mpi-error-string) mpi-error-code (errorcode :int) (string :pointer) (resultlen :pointer))

(defcfun "MPI_Wtime" :double
  "Returns a (double) floating-point number of seconds, representing elapsed
wall-clock time since some time in the past.

The 'time in the past' is guaranteed not to change during the life of the
process.  The user is responsible for converting large numbers of seconds to
other units if they are preferred.  This function is portable (it returns
seconds, not 'ticks'), it allows high-resolution, and carries no unnecessary
baggage.  The times returned are local to the node that called them. There is
no requirement that different nodes return 'the same time.'")

(defcfun "MPI_Wtick" :double
  "Returns the resolution of MPI-WTIME in seconds. That is, it returns, as a
double precision value, the number of seconds between successive clock
ticks. For example, if the clock is implemented by the hardware as a counter
that is incremented every millisecond, the value returned by MPI-WTICK should
be 0.001")

(defcfun ("MPI_Barrier" %mpi-barrier) mpi-error-code (communicator mpi-comm))

(defcfun ("MPI_Comm_group" %mpi-comm-group) mpi-error-code (comm mpi-comm) (group mpi-group))
(defcfun ("MPI_Comm_size" %mpi-comm-size) mpi-error-code (communicator mpi-comm) (numprocs :pointer))
(defcfun ("MPI_Comm_rank" %mpi-comm-rank) mpi-error-code (communicator mpi-comm) (myid :pointer))
(defcfun ("MPI_Comm_create" %mpi-comm-create) mpi-error-code (communicator mpi-comm) (group mpi-group) (newcomm mpi-comm))
(defcfun ("MPI_Group_size" %mpi-group-size) mpi-error-code (group MPI-Group) (size (:pointer :int)))
(defcfun ("MPI-Group-rank" %mpi-group-rank) (group MPI-Group) (rank (:pointer :int)))

;; blocking communication
(defcfun ("MPI_Bsend" %mpi-bsend) mpi-error-code (buf :pointer) (count :int) (datatype mpi-datatype) (dest :int) (tag :int) (comm mpi-comm))
(defcfun ("MPI_Recv" %mpi-recv) mpi-error-code (buf :pointer)(count :int) (datatype mpi-datatype)(source :int)(tag :int)(comm mpi-comm)(status (:pointer (:struct mpi-status))))
(defcfun ("MPI_Send" %mpi-send) mpi-error-code (buf :pointer) (count :int) (datatype mpi-datatype) (dest :int) (tag :int) (comm mpi-comm))
(defcfun ("MPI_Ssend" %mpi-ssend) mpi-error-code (buf :pointer) (count :int) (datatype mpi-datatype) (dest :int) (tag :int) (comm mpi-comm))
(defcfun ("MPI_Rsend" %mpi-rsend) mpi-error-code (buf :pointer) (count :int) (datatype mpi-datatype) (dest :int) (tag :int) (comm mpi-comm))
(defcfun ("MPI_Buffer_attach" %mpi-buffer-attach) mpi-error-code (buf :pointer) (count :int))
(defcfun ("MPI_Buffer_detach" %mpi-buffer-detach) mpi-error-code (buf :pointer) (count-ptr :pointer))
(defcfun ("MPI_Get_count" %mpi-get-count) mpi-error-code (status (:pointer (:struct mpi-status))) (datatype mpi-datatype) (count :pointer))

(defcfun ("MPI_Sendrecv" %mpi-sendrecv) mpi-error-code
  (send-buf :pointer) (send-count :int) (send-datatype mpi-datatype) (dest :int) (send-tag :int)
  (recv-buf :pointer) (recv-count :int) (recv-datatype mpi-datatype) (source :int) (recv-tag :int)
  (comm mpi-comm) (status (:pointer (:struct mpi-status))))

(defcfun ("MPI_Probe" %mpi-probe) mpi-error-code (source :int) (tag :int) (communicator mpi-comm) (status (:pointer (:struct mpi-status))))

;; non blocking communication

(defcfun ("MPI_Isend" %mpi-isend) mpi-error-code (buf :pointer) (count :int) (datatype mpi-datatype) (dest :int) (tag :int) (comm mpi-comm)(request :pointer))
(defcfun ("MPI_Ibsend" %mpi-ibsend) mpi-error-code (buf :pointer) (count :int) (datatype mpi-datatype) (dest :int) (tag :int) (comm mpi-comm)(request :pointer))
(defcfun ("MPI_Issend" %mpi-issend) mpi-error-code (buf :pointer) (count :int) (datatype mpi-datatype) (dest :int) (tag :int) (comm mpi-comm)(request :pointer))
(defcfun ("MPI_Irsend" %mpi-irsend) mpi-error-code (buf :pointer) (count :int) (datatype mpi-datatype) (dest :int) (tag :int) (comm mpi-comm)(request :pointer))

(defcfun ("MPI_Wait" %mpi-wait) mpi-error-code (request :pointer) (status (:pointer (:struct mpi-status))))
(defcfun ("MPI_Waitall" %mpi-waitall) mpi-error-code (count :int)(requests :pointer) (statuses :pointer))
(defcfun ("MPI_Waitany" %mpi-waitany) mpi-error-code (count :int)(requests :pointer)(completed-index :pointer)(status (:pointer (:struct mpi-status))))
(defcfun ("MPI_Waitsome" %mpi-waitsome) mpi-error-code (count :int)(requests :pointer)(outcount :pointer)(completed-indices :pointer)(statuses :pointer))

(defcfun ("MPI_Test" %mpi-test) mpi-error-code (request :pointer) (flag :pointer) (status (:pointer (:struct mpi-status))))
(defcfun ("MPI_Testall" %mpi-testall) mpi-error-code (count :int) (requests :pointer) (flag :pointer) (statuses :pointer))
(defcfun ("MPI_Testany" %mpi-testany) mpi-error-code (count :int) (requests :pointer) (index :pointer)(flag :pointer) (statuses :pointer))
(defcfun ("MPI_Testsome" %mpi-testsome) mpi-error-code (count :int)(requests :pointer)(outcount :pointer)(completed-indices :pointer)(statuses :pointer))

;; collective operations

(defcfun ("MPI_Bcast" %mpi-bcast) :int (buf :pointer) (count :int) (datatype mpi-datatype) (root :int) (comm mpi-comm))
(defcfun ("MPI_Reduce" %mpi-reduce) :int (sendbuf :pointer) (recvbuf :pointer)(count :int)(datatype mpi-datatype)(op mpi-op)(root :int)(comm mpi-comm))
(defcfun ("MPI_Allreduce" %mpi-allreduce) :int (sendbuf :pointer) (recvbuf :pointer)(count :int)(datatype mpi-datatype)(op mpi-op)(comm mpi-comm))
(defcfun ("MPI_Scatter" %mpi-scatter) :int (sendbuf :pointer) (sendcount :int)(sendtype mpi-datatype)
         (recvbuf :pointer)(recvcount :int)(recvtype mpi-datatype)(root :int)(comm mpi-comm))
(defcfun ("MPI_Gather" %mpi-gather) :int (sendbuf :pointer) (sendcount :int)(sendtype mpi-datatype)
         (recvbuf :pointer)(recvcount :int)(recvtype mpi-datatype)(root :int)(comm mpi-comm))
(defcfun ("MPI_Allgather" %mpi-allgather) :int (sendbuf :pointer) (sendcount :int)(sendtype mpi-datatype)
         (recvbuf :pointer)(recvcount :int)(recvtype mpi-datatype)(comm mpi-comm))

;; misc

(defcfun ("MPI_Comm_set_errhandler" %mpi-comm-set-errhandler) mpi-error-code (comm mpi-comm) (errhandler mpi-errhandler))
(defcfun ("MPI_Init" %mpi-init) mpi-error-code (argc :pointer) (argv :pointer))


