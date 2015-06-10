#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI functions according to the C Bindings in the MPI standard 3.0

Copyright (C) 2014,2015 Marco Heisig <marco.heisig@fau.de>

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

(in-package :mpi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *naming-conventions*
    '((*buf        :pointer)
      (*sendbuf    :pointer)
      (*recvbuf    :pointer)
      (*inbuf      :pointer)
      (*outbuf     :pointer)
      (*inoutbuf   :pointer)
      (*result    (:pointer :int))
      (*count     (:pointer :int))
      (*position  (:pointer :int))
      (*size      (:pointer :int))
      (*rank      (:pointer :int))
      (*flag      (:pointer :boolean))
      (*index     (:pointer :int))
      (*outcount  (:pointer :int))
      (*commute   (:pointer :int))
      (*keyval    (:pointer :int))
      (string      :string)
      (count       :int)
      (incount     :int)
      (outcount    :int)
      (insize      :int)
      (outsize     :int)
      (sendcount   :int)
      (recvcount   :int)
      (source      :int)
      (dest        :int)
      (tag         :int)
      (sendtag     :int)
      (recvtag     :int)
      (size        :int)
      (flag        :boolean)
      (root        :int)
      (commute     :int)
      (errorcode   :int)
      (errhandler  mpi-errhandler)
      (comm        mpi-comm)
      (oldcomm     mpi-comm)
      (comm1       mpi-comm)
      (comm2       mpi-comm)
      (group       mpi-group)
      (group1      mpi-group)
      (group2      mpi-group)
      (datatype    mpi-datatype)
      (sendtype    mpi-datatype)
      (recvtype    mpi-datatype)
      (oldtype     mpi-datatype)
      (op          mpi-op)
      (request     mpi-request)
      (*status    (:pointer (:struct mpi-status)))
      (*op        (:pointer mpi-op))
      (*message   (:pointer mpi-message))
      (*request   (:pointer mpi-request))
      (*newcomm   (:pointer mpi-comm))
      (*newgroup  (:pointer mpi-comm))
      (*group     (:pointer mpi-comm))
      (*newtype   (:pointer mpi-datatype))
      (statuses   (:array (:struct mpi-status) *))
      (sendtypes  (:array mpi-datatype *))
      (recvtypes  (:array mpi-datatype *))
      (requests   (:array mpi-request *))
      (indices    (:array :int *))
      (ranges     (:pointer (:array :int *)))
      (ranks      (:array :int *))
      (ranks1     (:array :int *))
      (ranks2     (:array :int *))
      (sendcounts (:array :int *))
      (recvcounts (:array :int *))
      (displs     (:array :int *))
      (sdispls    (:array :int *))
      (rdispls    (:array :int *))
      (function    :pointer)
      (argc        :pointer)
      (argv        :pointer))))

(defmacro defmpifun (foreign-name (&rest mpi-identifiers)
                     &key
                       documentation
                       introduced
                       deprecated
                       removed)
  (check-type foreign-name string)
  (let ((lisp-name
          (intern
           (concatenate 'string "%" (substitute #\- #\_ (string-upcase foreign-name)))
           :mpi))
        (args
          (loop for i in mpi-identifiers
                collect (find i *naming-conventions* :test #'eq :key #'car)))
        (introducedp (version<= introduced +mpi-version+))
        (removedp (and removed (version<= removed +mpi-version+)))
        (deprecatedp (version<= deprecated +mpi-version+)))
    (declare (ignorable deprecatedp)) ;; currently I do not handle deprecation
    (if documentation (push documentation args))
    (if (and introducedp (not removedp))
        `(progn
           (defcfun (,foreign-name ,lisp-name) mpi-error-code ,@args))
        `(progn
           (defun ,lisp-name ,mpi-identifiers
             (declare (ignore ,@mpi-identifiers))
             (error "This function was ~:[introduced~;removed~] in MPI version
                 ~:[~A~*~;~*~A~], while your MPI library is version ~A.~%"
                    ,removedp ,removedp ,introduced ,removed +mpi-version+))))))

(declaim (inline %mpi-send %mpi-receive %mpi-barrier))

;;;; The following list of functions should be identical to the C bindings in
;;;; Annex A of the MPI standard 3.0
;;;;
;;;; I commented out many functions because Lisp already provides their
;;;; functionality

;;; A.2.1 Point-to-Point Communication C Bindings

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

;;; A.2.2 Datatypes C Bindings

;; (defmpifun "MPI_Get_address")
;; (defmpifun "MPI_Get_elements")
;; (defmpifun "MPI_Get_elements_x")
(defmpifun "MPI_Pack" (*inbuf incount datatype *outbuf outsize *position comm))
;; (defmpifun "MPI_Pack_external")
;; (defmpifun "MPI_Pack_external_size")
(defmpifun "MPI_Pack_size" (incount datatype comm *size))
;; (defmpifun "MPI_Type_commit" (*datatype))
;; (defmpifun "MPI_Type_contiguous" (count oldtype *newtype))
;; (defmpifun "MPI_Type_create_darray")
;; (defmpifun "MPI_Type_create_hindexed")
;; (defmpifun "MPI_Type_create_hindexed_block")
;; (defmpifun "MPI_Type_create_hvector")
;; (defmpifun "MPI_Type_create_indexed_block")
;; (defmpifun "MPI_Type_create_resized")
;; (defmpifun "MPI_Type_create_struct")
;; (defmpifun "MPI_Type_create_subarray")
;; (defmpifun "MPI_Type_dup" (oldtype *newtype))
;; (defmpifun "MPI_Type_free" (*datatype))
;; (defmpifun "MPI_Type_get_contents")
;; (defmpifun "MPI_Type_get_envelope")
;; (defmpifun "MPI_Txpe_get_extent")
;; (defmpifun "MPI_Type_get_extent_x")
;; (defmpifun "MPI_Type_get_true_extent")
;; (defmpifun "MPI_Type_get_true_extent_x")
;; (defmpifun "MPI_Type_indexed")
(defmpifun "MPI_Type_size" (datatype *size))
;; (defmpifun "MPI_Type_size_x" (datatype *size))
;; (defmpifun "MPI_Type_vector")
(defmpifun "MPI_Unpack" (*inbuf insize *position *outbuf outcount datatype comm))
;; (defmpifun "MPI_Unpack_external")

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
(defmpifun "MPI_Op_create" (function commute *op) :introduced "1.0")
(defmpifun "MPI_Op_free" (*op) :introduced "1.0")
(defmpifun "MPI_Reduce" (*sendbuf *recvbuf count datatype op root comm) :introduced "1.0")
(defmpifun "MPI_Reduce_local" (*inbuf *inoutbuf count datatype op))
(defmpifun "MPI_Reduce_scatter" (*sendbuf *recvbuf recvcounts datatype op comm) :introduced "1.0")
(defmpifun "MPI_Reduce_scatter_block" (*sendbuf *recvbuf recvcount datatype op comm))
(defmpifun "MPI_Scan" (*sendbuf *recvbuf count datatype op comm) :introduced "1.0")
(defmpifun "MPI_Scatter" (*sendbuf sendcount sendtype *recvbuf recvcount recvtype root comm) :introduced "1.0")
(defmpifun "MPI_Scatterv" (*sendbuf sendcounts displs sendtype *recvbuf recvcount recvtype root comm) :introduced "1.0")

;;; A.2.4 Groups, Contexts, Communicators, and Caching C Bindings

;; (defmpifun "MPI_COMM_DUP_FN")
;; (defmpifun "MPI_COMM_NULL_COPY_FN")
;; (defmpifun "MPI_COMM_NULL_DELETE_FN")
(defmpifun "MPI_Comm_compare" (comm1 comm2 *result) :introduced "1.0")
(defmpifun "MPI_Comm_create" (comm group *newcomm) :introduced "1.0")
(defmpifun "MPI_Comm_create_group" (comm group tag *newcomm) :introduced "3.0")
;; (defmpifun "MPI_Comm_create_keyval")
;; (defmpifun "MPI_Comm_delete_attr")
(defmpifun "MPI_Comm_dup" (comm *newcomm) :introduced "1.0")
;; (defmpifun "MPI_Comm_dup_with_info")
(defmpifun "MPI_Comm_free" (comm) :introduced "1.0")
;; (defmpifun "MPI_Comm_free_keyval")
;; (defmpifun "MPI_Comm_get_attr")
;; (defmpifun "MPI_Comm_get_info")
;; (defmpifun "MPI_Comm_get_name")
(defmpifun "MPI_Comm_group" (comm *group) :introduced "1.0")
(defmpifun "MPI_Comm_idup" (comm *newcomm *request) :introduced "3.0")
(defmpifun "MPI_Comm_rank" (comm *rank) :introduced "1.0")
(defmpifun "MPI_Comm_remote_group" (comm *group) :introduced "1.0")
(defmpifun "MPI_Comm_remote_size" (comm *size) :introduced "1.0")
;; (defmpifun "MPI_Comm_set_attr")
;; (defmpifun "MPI_Comm_set_info")
;; (defmpifun "MPI_Comm_set_name")
(defmpifun "MPI_Comm_size" (comm *size) :introduced "1.0")
;; (defmpifun "MPI_Comm_split" :introduced "1.0")
;; (defmpifun "MPI_Comm_split_type")
(defmpifun "MPI_Comm_test_inter" (comm *flag) :introduced "1.0")
(defmpifun "MPI_Group_compare" (group1 group2 *result) :introduced "1.0")
(defmpifun "MPI_Group_difference" (group1 group2 *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_excl" (group count ranges *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_free" (*group) :introduced "1.0")
(defmpifun "MPI_Group_incl" (group count ranges *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_intersection" (group1 group2 *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_range_excl" (group count ranges *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_range_incl" (group count ranges *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_rank" (group *rank) :introduced "1.0")
(defmpifun "MPI_Group_size" (group *size) :introduced "1.0")
(defmpifun "MPI_Group_translate_ranks" (group1 count ranks1 group2 ranks2) :introduced "1.0")
(defmpifun "MPI_Group_union" (group1 group2 *newgroup) :introduced "1.0")
;; (defmpifun "MPI_Intercomm_create" :introduced "1.0")
;; (defmpifun "MPI_Intercomm_merge" :introduced "1.0")
;; (defmpifun "MPI_TYPE_DUP_FN")
;; (defmpifun "MPI_TYPE_NULL_COPY_FN")
;; (defmpifun "MPI_TYPE_NULL_DELETE_FN")
;; (defmpifun "MPI_Type_create_keyval")
;; (defmpifun "MPI_Type_free_keyval")
;; (defmpifun "MPI_Type_get_attr")
;; (defmpifun "MPI_Type_get_name")
;; (defmpifun "MPI_Type_set_attr")
;; (defmpifun "MPI_Type_set_name")
;; (defmpifun "MPI_WIN_DUP_FN")
;; (defmpifun "MPI_WIN_NULL_COPY_FN")
;; (defmpifun "MPI_WIN_NULL_DELETE_FN")
;; (defmpifun "MPI_Win_create_keyval")
;; (defmpifun "MPI_Win_delete_attr")
;; (defmpifun "MPI_Win_free_attr")
;; (defmpifun "MPI_Win_get_attr")
;; (defmpifun "MPI_Win_get_name")
;; (defmpifun "MPI_Win_set_attr")
;; (defmpifun "MPI_Win_set_name")

;;; A.2.5 Process Topologies C Bindings

;;; A.2.6 MPI Environmental Management C Bindings

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

(defmpifun "MPI_Abort" (comm errorcode))
;; (defmpifun "MPI_Add_error_class")
;; (defmpifun "MPI_Add_error_code")
;; (defmpifun "MPI_Add_error_string")
;; (defmpifun "MPI_Alloc_mem")
(defmpifun "MPI_Comm_call_errhandler" (comm errorcode))
;; (defmpifun "MPI_Comm_create_errhandler")
;; (defmpifun "MPI_Comm_get_errhandler")
(defmpifun "MPI_Comm_set_errhandler" (comm errhandler))
;; (defmpifun "MPI_Errhandler_free")
;; (defmpifun "MPI_Error_class")
(defmpifun "MPI_Error_string" (errorcode string *size))
;; (defmpifun "MPI_File_call_errhandler")
;; (defmpifun "MPI_File_create_errhandler")
;; (defmpifun "MPI_File_get_errhandler")
;; (defmpifun "MPI_File_set_errhandler")

(defcfun "MPI_Finalize" mpi-error-code
  "This routines cleans up all MPI state. Once this routine is called, no
MPI routine (even MPI-INIT) may be called. The user must ensure that all
pending communications involving a process complete before the process calls
MPI-FINALIZE.")

(defmpifun "MPI_Finalized" (*flag))
;; (defmpifun "MPI_Free_mem")
;; (defmpifun "MPI_Get_library_version")
(defmpifun "MPI_Get_processor_name" (string *size))
;; (defmpifun "MPI_Get_version")
(defmpifun "MPI_Init" (argc argv))
(defmpifun "MPI_Initialized" (*flag))
;; (defmpifun "MPI_Win_call_errhandler")
;; (defmpifun "MPI_Win_create_errhandler")
;; (defmpifun "MPI_Win_get_errhandler")
;; (defmpifun "MPI_Win_set_errhandler")

;;; A.2.7 The Info Object C Bindings

;;; A.2.8 Process Creation and Management C Bindings

;;; A.2.9 One-Sided Communications C Bindings

;;; A.2.10 External Interfaces C Bindings

;;; A.2.11 I/O C Bindings

;;; A.2.12 Language Bindings C Bindings

;;; A.2.13 Tools / Profiling Interface C Bindings

;;; A.2.14 Tools / MPI Toool Information Interface C Bindings

;;; A.2.15 Deprecated C Bindings


;;; rest

