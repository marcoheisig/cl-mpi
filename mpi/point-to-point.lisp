#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI point to point communication functions

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

(defmpifun "MPI_Bsend" (*buf count datatype dest tag comm))
(defmpifun "MPI_Bsend_init" (*buf count datatype dest tag comm *request))
(defmpifun "MPI_Buffer_attach" (*buf size))
(defmpifun "MPI_Buffer_detach" (*buf *size))
(defmpifun "MPI_Cancel" (*request))
(defmpifun "MPI_Get_count" (*status datatype *count))
(defmpifun "MPI_Ibsend" (*buf count datatype dest tag comm *request))
(defmpifun "MPI_Improbe" (source tag comm *flag *message *status) :introduced "3.0")
(defmpifun "MPI_Imrecv" (*buf count datatype *message *request) :introduced "3.0")
(defmpifun "MPI_Iprobe" (source tag comm *flag *status))
(defmpifun "MPI_Irecv" (*buf count datatype source tag comm *request))
(defmpifun "MPI_Irsend" (*buf count datatype dest tag comm *request))
(defmpifun "MPI_Isend" (*buf count datatype dest tag comm *request))
(defmpifun "MPI_Issend" (*buf count datatype dest tag comm *request))
(defmpifun "MPI_Mprobe" (source tag comm *message *status) :introduced "3.0")
(defmpifun "MPI_Mrecv" (*buf count datatype *message *status) :introduced "3.0")
(defmpifun "MPI_Probe" (source tag comm *status))
(defmpifun "MPI_Recv" (*buf count datatype source tag comm *status))
(defmpifun "MPI_Recv_init" (*buf count datatype source tag comm *request))
(defmpifun "MPI_Request_free" (*request))
(defmpifun "MPI_Request_get_status" (request *flag *status) :introduced "2.1")
(defmpifun "MPI_Rsend" (*buf count datatype dest tag comm))
(defmpifun "MPI_Rsend_init" (*buf count datatype dest tag comm *request))
(defmpifun "MPI_Send" (*buf count datatype dest tag comm))
(defmpifun "MPI_Send_init" (*buf count datatype dest tag comm *request))
(defmpifun "MPI_Sendrecv" (*sendbuf sendcount sendtype dest sendtag *recvbuf recvcount recvtype source recvtag comm *status))
(defmpifun "MPI_Sendrecv_replace" (*buf count datatype dest sendtag source recvtag comm *status))
(defmpifun "MPI_Ssend" (*buf count datatype dest tag comm))
(defmpifun "MPI_Ssend_init" (*buf count datatype dest tag comm *request))
(defmpifun "MPI_Start" (*request))
(defmpifun "MPI_Startall" (count requests))
(defmpifun "MPI_Test" (*request *flag *status))
(defmpifun "MPI_Test_cancelled" (*status *flag))
(defmpifun "MPI_Testall" (count requests *flag statuses))
(defmpifun "MPI_Testany" (count requests *index *flag *status))
(defmpifun "MPI_Testsome" (incount requests *outcount indices statuses))
(defmpifun "MPI_Wait" (*request *status))
(defmpifun "MPI_Waitall" (count requests statuses))
(defmpifun "MPI_Waitany" (count requests *index *status))
(defmpifun "MPI_Waitsome" (incount requests *outcount indices statuses))

(defvar *current-buffer* nil)

(defun mpi-demand-buffering (size)
  "Ensure that the MPI buffer that is used for blocking commands has a size
of at least SIZE bytes."
  (declare (type int size))
  (when (> size (length *current-buffer*))
    (mpi-buffer-detach)
    (let ((buffer (make-static-vector size)))
      (%mpi-buffer-attach (static-vector-pointer buffer) size)
      (setf *current-buffer* buffer)
      (values))))

(defun mpi-buffer-detach ()
  "Release the resources that MPI uses for messages with :MODE :BUFFERING."
  (when *current-buffer*
    (let ((buffer *current-buffer*))
      (with-foreign-objects ((pointer :pointer)
                             (size :int))
        (%mpi-buffer-detach pointer size))
      (setf *current-buffer* nil)
      (free-static-vector buffer)
      (values))))

(defun mpi-sendrecv (send-data dest recv-data source
                     &key (comm *standard-communicator*)
                       (send-tag 0)
                       (recv-tag +mpi-any-tag+)
                       send-start send-end
                       recv-start recv-end)
  "Simultaneously send and receive a message.  Behaves as if calls to
MPI-SEND and MPI-RECV were issued in separate threads that were then
joined.

Returns three values:

1. The size of the incoming message in bytes.

2. The rank of the sender of the received message.  This value is
   particularly useful if the SOURCE was specified as +MPI-ANY-SOURCE+.

3. The tag of the sender of the received message.  This value is
   particularly useful if the TAG was specified as +MPI-ANY-TAG+.
"
  (declare (type simple-array send-data recv-data)
           (type int dest send-tag source recv-tag)
           (type mpi-comm comm)
           (type index
                 send-start send-end recv-start recv-end))
  (multiple-value-bind (send-buf send-type send-count)
      (static-vector-mpi-data send-data send-start send-end)
    (multiple-value-bind (recv-buf recv-type recv-count)
        (static-vector-mpi-data recv-data recv-start recv-end)
      (with-foreign-object (status '(:struct mpi-status))
        (%mpi-sendrecv
         send-buf send-count send-type dest send-tag
         recv-buf recv-count recv-type source recv-tag
         comm status)
        (with-foreign-slots ((mpi-tag mpi-source mpi-error) status (:struct mpi-status))
          (values
           (with-foreign-results ((count :int))
             (%mpi-get-count status +mpi-byte+ count))
           mpi-source
           mpi-tag))))))

(defun mpi-send (array dest &key (comm *standard-communicator*)
                              start end
                              (tag 0)
                              (mode :basic))
  "Send a given ARRAY to a corresponding MPI-RECV. The arrays passed to
MPI-SEND and MPI-RECV must be of type SIMPLE-ARRAY and have the same
element-type and dimensions. Undefined behaviour occurs if the arrays at
sender and receiver side do not match."
  (declare (type simple-array array)
           (type int dest tag)
           (type mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode)
           (type index start end))
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
  "A non-blocking variant of MPI-SEND. Returns a MPI-REQUEST that can be
passed to MPI-TEST, MPI-WAIT and MPI-WAITALL.

WARNING: The caller of MPI-ISEND is responsible that the given array is not
relocated or garbage-collected until the send operation is complete. This can
be achieved by using STATIC-VECTORS or some implementation dependent
mechanism such as sb-sys:with-pinned-objects."
  (declare (type simple-array array)
           (type int dest tag)
           (type mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode)
           (type index start end))
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
  "Blocks until a message from a process with rank SOURCE and tag TAG has
been received.

Returns three values:

1. The size of the incoming message in bytes.

2. The rank of the sender of the message.  This value is particularly
   useful if the SOURCE was specified as +MPI-ANY-SOURCE+.

3. The tag of the sender of the message.  This value is particularly
   useful if the TAG was specified as +MPI-ANY-TAG+.
"
  (declare (type simple-array array)
           (type int source tag)
           (type mpi-comm comm)
           (type index start end))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    (with-foreign-object (status '(:struct mpi-status))
      (%mpi-recv ptr count type source tag comm status)
      (with-foreign-slots ((mpi-tag mpi-source mpi-error) status (:struct mpi-status))
        (values
         (with-foreign-results ((count :int))
           (%mpi-get-count status +mpi-byte+ count))
         mpi-source
         mpi-tag)))))

(defun mpi-irecv (array source &key (comm *standard-communicator*)
                                 start end
                                 (tag +mpi-any-tag+))
  (declare (type simple-array array)
           (type int source tag)
           (type mpi-comm comm)
           (type index start end))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    (with-foreign-results ((request 'mpi-request))
      (%mpi-irecv ptr count type source tag comm request))))

(defun mpi-probe (source &key
                           (tag +mpi-any-tag+)
                           (comm *standard-communicator*))
  "Block until a message with matching TAG and SOURCE has been sent on the
communicator COMM.

Returns three values:

1. The size of the incoming message in bytes.

2. The rank of the sender of the message.  This value is particularly
   useful if the SOURCE was specified as +MPI-ANY-SOURCE+.

3. The tag of the sender of the message.  This value is particularly
   useful if the TAG was specified as +MPI-ANY-TAG+.
"
  (declare (type int source tag)
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
  "Checks whether a message with matching TAG and SOURCE has been sent on
the communicator COMM. If so, it returns three values: The size of the
incoming message in bytes, and the rank and tag of the sender. Otherwise,
it returns false.

MPI makes a progress guarantee, such that repeated calls to MPI-IPROBE to
a message that has been sent will eventually succeed."
  (declare (type int source tag)
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

(defun mpi-test (request)
  "Returns whether REQUEST has been completed.

MPI makes a progress guarantee, such that repeated calls to MPI-TEST to a
request whose matching operation has been issued will eventually succeed."
  (declare (type mpi-request request))
  (with-foreign-objects ((status* '(:struct mpi-status))
                         (flag* :int)
                         (request* 'mpi-request))
    (setf (mem-ref request* 'mpi-request) request)
    (%mpi-test request* flag* status*)
    (setf (mpi-object-handle request)
          (mem-ref request* #.foreign-mpi-object-type))
    (values (not (zerop (mem-ref flag* :int))) request)))

(defun mpi-wait (request)
  "Blocks until REQUEST has been completed."
  (declare (type mpi-request request))
  (with-foreign-objects ((status* '(:struct mpi-status))
                         (request* 'mpi-request))
    (setf (mem-ref request* 'mpi-request) request)
    (%mpi-wait request* status*)
    (setf (mpi-object-handle request)
          (mem-ref request* #.foreign-mpi-object-type))
    request))

(defun mpi-waitall (&rest requests)
  "MPI-WAITALL blocks until all given requests have been completed. It
returns REQUESTS."
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
                    (mem-aref requests* #.foreign-mpi-object-type)))
      requests)))
