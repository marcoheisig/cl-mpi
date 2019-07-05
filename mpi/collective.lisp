#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI collective communication functions

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

(defun mpi-allgather (send-array recv-array &key (comm *standard-communicator*)
                                              send-start send-end
                                              recv-start recv-end)
  "After MPI-ALLGATHER returns, RECV-ARRAY will contain the contents of
each processes SEND-ARRAY ordered by increasing mpi rank."
  (declare (type simple-array send-array recv-array)
           (type mpi-comm comm)
           (type index send-start send-end recv-start recv-end))
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
  "Combine the contents of each SEND-ARRAY element wise with the operation
OP and store the result RECV-ARRAY."
  (declare (type simple-array send-array recv-array)
           (type mpi-op op)
           (type mpi-comm comm)
           (type index send-start send-end recv-start recv-end))
  (multiple-value-bind (sendbuf sendtype sendcount)
      (static-vector-mpi-data send-array send-start send-end)
    (multiple-value-bind (recvbuf recvtype recvcount)
        (static-vector-mpi-data recv-array recv-start recv-end)
      (assert (= recvcount sendcount))
      (assert (eq recvtype sendtype))
      (%mpi-allreduce sendbuf recvbuf sendcount sendtype op comm))))

(defun mpi-barrier (&optional (comm *standard-communicator*))
  "MPI-BARRIER blocks the caller until all members of COMM have called
  it. The call returns at any process only after all members of COMM have
  entered the call."
  (%mpi-barrier comm))

(defun mpi-bcast (array root &key (comm *standard-communicator*)
                                   start end)
  "Transfer the contents of ARRAY of the process with rank ROOT to all
members of COMM. The call returns at any process only after all members of
COMM have entered the call. The arguments START and END can be used to
manipulate only a sub-sequence of ARRAY."
  (declare (type simple-array array)
           (type int root)
           (type mpi-comm comm)
           (type index start end))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    (%mpi-bcast ptr count type root comm)))

(defun mpi-reduce (send-array recv-array op root &key (comm *standard-communicator*)
                                                   send-start send-end
                                                   recv-start recv-end)
  "Combine the contents of each SEND-ARRAY element wise with the operation
OP and store the result into the RECV-ARRAY of the process with rank ROOT."
  (declare (type simple-array send-array)
           (type mpi-op op)
           (type int root)
           (type mpi-comm comm)
           (type index send-start send-end recv-start recv-end))
  (multiple-value-bind (sendbuf sendtype sendcount)
      (static-vector-mpi-data send-array send-start send-end)
    (if (= (mpi-comm-rank comm) root)
        (multiple-value-bind (recvbuf recvtype recvcount)
            (static-vector-mpi-data recv-array recv-start recv-end)
          (assert (= recvcount sendcount))
          (assert (eq recvtype sendtype))
          (%mpi-reduce sendbuf recvbuf sendcount sendtype op root comm))
        (%mpi-reduce sendbuf (null-pointer) sendcount sendtype op root comm))))
