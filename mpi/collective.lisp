#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI Collective Communication Functions

Copyright (c) 2008,2009  Alex Fukunaga
Copyright (C) 2014,2015  Marco Heisig <marco.heisig@fau.de>

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

(in-package :cl-mpi)

;;; A.2.3 Collective Communication C Bindings

(defmpifun "MPI_Allgather" (*sendbuf sendcount sendtype *recvbuf recvcount recvtype comm) :introduced "1.0")
(defmpifun "MPI_Allgatherv" (*sendbuf sendcount sendtype *recvbuf recvcounts displs recvtype comm) :introduced "1.0")
(defmpifun "MPI_Allreduce" (*sendbuf *recvbuf count datatype op comm) :introduced "1.0")
(defmpifun "MPI_Alltoall" (*sendbuf *recvbuf count datatype op comm) :introduced "1.0")
(defmpifun "MPI_Alltoallv" (*sendbuf sendcounts sdispls sendtype *recvbuf recvcounts rdispls recvtype comm) :introduced "1.0")
(defmpifun "MPI_Alltoallw" (*sendbuf sendcounts sdispls sendtypes *recvbuf recvcounts rdispls recvtypes comm) :introduced "2.0")
(defmpifun "MPI_Barrier" (comm) :introduced "1.0")
(defmpifun "MPI_Bcast" (*buf count datatype root comm) :introduced "1.0")
(defmpifun "MPI_Exscan" (*sendbuf *recvbuf count datatype op comm) :introduced "2.0")
(defmpifun "MPI_Gather" (*sendbuf sendcount sendtype *recvbuf recvcount recvtype root comm) :introduced "1.0")
(defmpifun "MPI_Gatherv" (*sendbuf sendcount sendtype *recvbuf recvcounts displs recvtype root comm) :introduced "1.0")
(defmpifun "MPI_Iallgather" (*sendbuf sendcount sendtype *recvbuf recvcount recvtype comm *request) :introduced "3.0")
(defmpifun "MPI_Iallgatherv" (*sendbuf sendcount sendtype *recvbuf recvcounts displs recvtype comm *request) :introduced "3.0")
(defmpifun "MPI_Iallreduce" (*sendbuf *recvbuf count datatype op comm *request) :introduced "3.0")
(defmpifun "MPI_Ialltoall" (*sendbuf sendcount sendtype *recvbuf recvcount recvtype comm *request) :introduced "3.0")
(defmpifun "MPI_Ialltoallv" (*sendbuf sendcounts sdispls sendtype *recvbuf recvcounts rdispls recvtype comm *request) :introduced "3.0")
(defmpifun "MPI_Ialltoallw" (*sendbuf sendcounts sdispls sendtypes *recvbuf recvcounts rdispls recvtypes comm *request) :introduced "3.0")
(defmpifun "MPI_Ibarrier" (comm *request) :introduced "3.0")
(defmpifun "MPI_Ibcast" (*buf count datatype root comm *request) :introduced "3.0")
(defmpifun "MPI_Iexscan" (*sendbuf *recvbuf count datatype op comm *request) :introduced "3.0")
(defmpifun "MPI_Igather" (*sendbuf sendcount sendtype *recvbuf recvcount recvtype root comm *request) :introduced "3.0")
(defmpifun "MPI_Igatherv" (*sendbuf sendcount sendtype *recvbuf recvcounts displs recvtype root comm *request) :introduced "3.0")
(defmpifun "MPI_Ireduce" (*sendbuf *recvbuf count datatype op root comm *request) :introduced "3.0")
(defmpifun "MPI_Ireduce_scatter" (*sendbuf *recvbuf recvcounts datatype op comm *request) :introduced "3.0")
(defmpifun "MPI_Ireduce_scatter_block" (*sendbuf *recvbuf recvcount datatype op comm *request) :introduced "3.0")
(defmpifun "MPI_Iscan" (*sendbuf *recvbuf count datatype op comm *request) :introduced "3.0")
(defmpifun "MPI_Iscatter" (*sendbuf sendcount sendtype *recvbuf recvcount recvtype root comm *request) :introduced "3.0")
(defmpifun "MPI_Iscatterv" (*sendbuf sendcounts displs sendtype *recvbuf recvcount recvtype root comm *request) :introduced "3.0")
(defmpifun "MPI_Op_commutative" (op *commute))
(defmpifun "MPI_Op_create" (fun commute *op) :introduced "1.0")
(defmpifun "MPI_Op_free" (*op) :introduced "1.0")
(defmpifun "MPI_Reduce" (*sendbuf *recvbuf count datatype op root comm) :introduced "1.0")
(defmpifun "MPI_Reduce_local" (*inbuf *inoutbuf count datatype op))
(defmpifun "MPI_Reduce_scatter" (*sendbuf *recvbuf recvcounts datatype op comm) :introduced "1.0")
(defmpifun "MPI_Reduce_scatter_block" (*sendbuf *recvbuf recvcount datatype op comm) :introduced "2.2")
(defmpifun "MPI_Scan" (*sendbuf *recvbuf count datatype op comm) :introduced "1.0")
(defmpifun "MPI_Scatter" (*sendbuf sendcount sendtype *recvbuf recvcount recvtype root comm) :introduced "1.0")
(defmpifun "MPI_Scatterv" (*sendbuf sendcounts displs sendtype *recvbuf recvcount recvtype root comm) :introduced "1.0")

(defun mpi-barrier (&optional (comm *standard-communicator*))
  "MPI-BARRIER blocks the caller until all members of COMM have called
  it. The call returns at any process only after all members of COMM have
  entered the call."
  (%mpi-barrier comm))

(defun mpi-broadcast (array root &key (comm *standard-communicator*)
                                   start end)
  "Transfer the contents of ARRAY of the process with rank ROOT to all
members of COMM. The call returns at any process only after all members of
COMM have entered the call. The arguments START and END can be used to
manipulate only a sub-sequence of ARRAY."
  (declare (type simple-array array)
           (type (signed-byte 32) root)
           (type mpi-comm comm)
           (type (or null (integer 0 #.array-total-size-limit)) start end))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    (%mpi-bcast ptr count type root comm)))

(defun mpi-allgather (send-array recv-array &key (comm *standard-communicator*)
                                              send-start send-end
                                              recv-start recv-end)
  (declare (type simple-array send-array recv-array)
           (type mpi-comm comm)
           (type (or null (integer 0 #.array-total-size-limit))
                 send-start send-end recv-start recv-end))
  (multiple-value-bind (sendbuf sendtype sendcount)
      (static-vector-mpi-data send-array send-start send-end)
    (multiple-value-bind (recvbuf recvtype recvcount)
        (static-vector-mpi-data recv-array recv-start recv-end)
      (declare (ignore recvcount))
      (%mpi-allgather sendbuf sendcount sendtype
                      recvbuf sendcount recvtype comm))))

(defun mpi-allreduce (send-array recv-array op &key (comm *standard-communicator*)
                                                 send-start send-end
                                                 recv-start recv-end)
  (declare (type simple-array send-array recv-array)
           (type mpi-op op)
           (type mpi-comm comm)
           (type (or null (integer 0 #.array-total-size-limit))
                 send-start send-end recv-start recv-end))
  (multiple-value-bind (sendbuf sendtype sendcount)
      (static-vector-mpi-data send-array send-start send-end)
    (multiple-value-bind (recvbuf recvtype recvcount)
        (static-vector-mpi-data recv-array recv-start recv-end)
      (assert (= recvcount sendcount))
      (assert (eq recvtype sendtype))
      (%mpi-allreduce sendbuf recvbuf sendcount sendtype op comm))))

(defun mpi-reduce (send-array recv-array op root &key (comm *standard-communicator*)
                                                   send-start send-end
                                                   recv-start recv-end)
  (declare (type simple-array send-array)
           (type mpi-op op)
           (type (signed-byte 32) root)
           (type mpi-comm comm)
           (type (or null (integer 0 #.array-total-size-limit))
                 send-start send-end recv-start recv-end))
  (multiple-value-bind (sendbuf sendtype sendcount)
      (static-vector-mpi-data send-array send-start send-end)
    (if (= (mpi-comm-rank comm) root)
        (multiple-value-bind (recvbuf recvtype recvcount)
            (static-vector-mpi-data recv-array recv-start recv-end)
          (assert (= recvcount sendcount))
          (assert (eq recvtype sendtype))
          (%mpi-reduce sendbuf recvbuf sendcount sendtype op root comm))
        (%mpi-reduce sendbuf (null-pointer) sendcount sendtype op root comm))))
