#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI Point to Point Communication Functions

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

(defmpifun "MPI_Bsend" (*buf count datatype dest tag comm) :introduced "1.0")
(defmpifun "MPI_Bsend_init" (*buf count datatype dest tag comm *request) :introduced "1.0")
(defmpifun "MPI_Buffer_attach" (*buf size) :introduced "1.0")
(defmpifun "MPI_Buffer_detach" (*buf *size) :introduced "1.0")
(defmpifun "MPI_Cancel" (*request) :introduced "1.0")
(defmpifun "MPI_Get_count" (*status datatype *count) :introduced "1.0")
(defmpifun "MPI_Ibsend" (*buf count datatype dest tag comm *request) :introduced "1.0")
(defmpifun "MPI_Improbe" (source tag comm *flag *message *status) :introduced "3.0")
(defmpifun "MPI_Imrecv" (*buf count datatype *message *request) :introduced "3.0")
(defmpifun "MPI_Iprobe" (source tag comm *flag *status) :introduced "1.0")
(defmpifun "MPI_Irecv" (*buf count datatype source tag comm *request) :introduced "1.0")
(defmpifun "MPI_Irsend" (*buf count datatype dest tag comm *request) :introduced "1.0")
(defmpifun "MPI_Isend" (*buf count datatype dest tag comm *request) :introduced "1.0")
(defmpifun "MPI_Issend" (*buf count datatype dest tag comm *request) :introduced "1.0")
(defmpifun "MPI_Mprobe" (source tag comm *message *status) :introduced "3.0")
(defmpifun "MPI_Mrecv" (*buf count datatype *message *status) :introduced "3.0")
(defmpifun "MPI_Probe" (source tag comm *status) :introduced "1.0")
(defmpifun "MPI_Recv" (*buf count datatype source tag comm *status) :introduced "1.0")
(defmpifun "MPI_Recv_init" (*buf count datatype source tag comm *request) :introduced "1.0")
(defmpifun "MPI_Request_free" (*request) :introduced "1.0")
(defmpifun "MPI_Request_get_status" (request *flag *status) :introduced "2.1")
(defmpifun "MPI_Rsend" (*buf count datatype dest tag comm) :introduced "1.0")
(defmpifun "MPI_Rsend_init" (*buf count datatype dest tag comm *request) :introduced "1.0")
(defmpifun "MPI_Send" (*buf count datatype dest tag comm) :introduced "1.0")
(defmpifun "MPI_Send_init" (*buf count datatype dest tag comm *request) :introduced "1.0")
(defmpifun "MPI_Sendrecv" (*sendbuf sendcount sendtype dest sendtag *recvbuf recvcount recvtype source recvtag comm *status) :introduced "1.0")
(defmpifun "MPI_Sendrecv_replace" (*buf count datatype dest sendtag source recvtag comm *status) :introduced "1.0")
(defmpifun "MPI_Ssend" (*buf count datatype dest tag comm) :introduced "1.0")
(defmpifun "MPI_Ssend_init" (*buf count datatype dest tag comm *request) :introduced "1.0")
(defmpifun "MPI_Start" (*request) :introduced "1.0")
(defmpifun "MPI_Startall" (count requests) :introduced "1.0")
(defmpifun "MPI_Test" (*request *flag *status) :introduced "1.0")
(defmpifun "MPI_Test_cancelled" (*status *flag) :introduced "1.0")
(defmpifun "MPI_Testall" (count requests *flag statuses) :introduced "1.0")
(defmpifun "MPI_Testany" (count requests *index *flag *status):introduced "1.0")
(defmpifun "MPI_Testsome" (incount requests *outcount indices statuses) :introduced "1.0")
(defmpifun "MPI_Wait" (*request *status) :introduced "1.0")
(defmpifun "MPI_Waitall" (count requests statuses) :introduced "1.0")
(defmpifun "MPI_Waitany" (count requests *index *status) :introduced "1.0")
(defmpifun "MPI_Waitsome" (incount requests *outcount indices statuses))

(defun mpi-sendrecv (send-data dest recv-data source
                     &key (comm *standard-communicator*)
                       (send-tag 0)
                       (recv-tag +mpi-any-tag+)
                       send-start send-end
                       recv-start recv-end)
  (declare (type simple-array send-data recv-data)
           (type (signed-byte 32)
                 dest send-tag source recv-tag)
           (type mpi-comm comm)
           (type (or null (integer 0 #.array-total-size-limit))
                 send-start send-end recv-start recv-end))
  (multiple-value-bind (send-buf send-type send-count)
      (static-vector-mpi-data send-data send-start send-end)
    (multiple-value-bind (recv-buf recv-type recv-count)
        (static-vector-mpi-data recv-data recv-start recv-end)
      (%mpi-sendrecv
       send-buf send-count send-type dest send-tag
       recv-buf recv-count recv-type source recv-tag
       comm +mpi-status-ignore+))))

(defun mpi-send (array dest &key (comm *standard-communicator*)
                              start end
                              (tag 0)
                              (mode :basic))
  "Send a given ARRAY to a corresponding MPI-RECV. The arrays passed to
MPI-SEND and MPI-RECV must be of type SIMPLE-ARRAY and have the same
element-type and dimensions. Undefined behaviour occurs if the arrays at
sender and receiver side do not match."
  (declare (type simple-array array)
           (type (signed-byte 32) dest tag)
           (type mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode)
           (type (or null (integer 0 #.array-total-size-limit)) start end))
  (let ((send-function
          (ecase mode
            (:basic #'%mpi-send)
            (:buffered #'%mpi-bsend)
            (:synchronous #'%mpi-ssend)
            (:ready #'%mpi-rsend))))
    (multiple-value-bind (ptr type count)
        (static-vector-mpi-data array start end)
      (funcall send-function ptr count type dest tag comm))))

(defun mpi-isend (array dest &key (comm *standard-communicator*)
                               start
                               end
                               (tag 0)
                               (mode :basic))
  "A non-blocking variant of MPI-SEND. It returns immediately. To check
  whether the send operation is complete, use MPI-WAIT or MPI-TEST.

WARNING: The caller of MPI-ISEND is responsible that the given array is not
relocated or garbage-collected until the send operation is complete. This can
be achieved by using STATIC-VECTORS or some implementation dependent
mechanism such as sb-sys:with-pinned-objects."
  (declare (type simple-array array)
           (type (signed-byte 32) dest tag)
           (type mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode)
           (type (or null (integer 0 #.array-total-size-limit)) start end))
  (let ((send-function
          (ecase mode
            (:basic #'%mpi-isend)
            (:buffered #'%mpi-ibsend)
            (:synchronous #'%mpi-issend)
            (:ready #'%mpi-irsend))))
    (multiple-value-bind (ptr type count)
        (static-vector-mpi-data array start end)
      (with-foreign-results ((request 'mpi-request))
        (funcall send-function ptr count type dest tag comm request)))))

(defun mpi-recv (array source &key (comm *standard-communicator*)
                                start end
                                (tag +mpi-any-tag+))
  (declare (type simple-array array)
           (type (signed-byte 32) source tag)
           (type mpi-comm comm)
           (type (or null (integer 0 #.array-total-size-limit)) start end))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    ;; TODO check the mpi-status
    (%mpi-recv ptr count type source tag comm +mpi-status-ignore+)))

(defun mpi-irecv (array source &key (comm *standard-communicator*)
                                 start end
                                 (tag +mpi-any-tag+))
  (declare (type simple-array array)
           (type (signed-byte 32) source tag)
           (type mpi-comm comm)
           (type (or null (integer 0 #.array-total-size-limit)) start end))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    (with-foreign-results ((request 'mpi-request))
      (%mpi-irecv ptr count type source tag comm request))))

(defun mpi-probe (source &key
                           (tag +mpi-any-tag+)
                           (comm *standard-communicator*))
  "Block until a message with matching TAG and SOURCE has been sent on the
communicator COMM. Return three values: The size of the incoming message in
bytes, and the ID and TAG of the sender."
  (declare (type (signed-byte 32) source tag)
           (type mpi-comm comm))
  (with-foreign-object (status '(:struct mpi-status))
    (%mpi-probe source tag comm status)
    (with-foreign-slots ((mpi-tag mpi-source mpi-error) status (:struct mpi-status))
      (values
       (with-foreign-results ((count :int))
         (%mpi-get-count status +mpi-byte+ count))
       mpi-source
       mpi-tag))))

(defun mpi-iprobe (source &key
                            (tag +mpi-any-tag+)
                            (comm *standard-communicator*))
  "MPI-IPROBEL checks whether a message with matching TAG and SOURCE has
been sent on the communicator COMM. If so, it returns three values: The
size of the incoming message in bytes, and the ID and TAG of the
sender. Otherwise, it returns NIL."
  (declare (type (signed-byte 32) source tag)
           (type mpi-comm comm))
  (with-foreign-objects ((status '(:struct mpi-status))
                         (flag :int))
    (%mpi-iprobe source tag comm flag status)
    (unless (zerop (mem-ref flag :int))
      (with-foreign-slots ((mpi-tag mpi-source mpi-error) status (:struct mpi-status))
        (values
         (with-foreign-results ((count :int))
           (%mpi-get-count status +mpi-byte+ count))
         mpi-source
         mpi-tag)))))

(defun mpi-wait (request)
  (declare (type mpi-request request))
  (with-foreign-objects ((status* '(:struct mpi-status))
                         (request* 'mpi-request))
    (setf (mem-ref request* 'mpi-request) request)
    (%mpi-wait request* status*)
    (setf (mpi-object-handle request)
          (mem-ref request* #.+mpi-object-handle-cffi-type+))
    request))

(defun mpi-waitall (&rest requests)
  (let ((n-requests (length requests)))
    (with-foreign-objects ((requests* 'mpi-request n-requests)
                           (statuses* '(:struct mpi-status) n-requests))
      (loop for request in requests
            and i below n-requests do
              (setf (mem-aref requests* 'mpi-request i) request))
      (%mpi-waitall n-requests requests* statuses*)
      (loop for request in requests
            and i below n-requests do
              (setf (mpi-object-handle request)
                    (mem-aref requests* #.+mpi-object-handle-cffi-type+ i)))
      requests)))
