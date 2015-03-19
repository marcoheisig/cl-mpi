#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI bindings for Common Lisp

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

(in-package :mpi)

(defmacro defmpifun (c-name &rest rest)
  (let ((name (intern (string-upcase c-name))))
    `(defcfun (,c-name ,name) mpi-error-code ,@rest)))

(defmpifun "MPI_Finalize")
(defun mpi-finalize ()
  "This routines cleans up all MPI state. Once this routine is called, no MPI
routine (even MPI-INIT) may be called. The user must ensure that all pending
communications involving a process completes before the process calls
MPI-FINALIZE."
  (MPI_Finalize))

(defun mpi-init ()
  "This routine must be called before any other MPI routine. It must be called
at most once; subsequent calls are erroneous (see MPI-INITIALIZED).

All MPI programs must contain a call to MPI-INIT; this routine must be called
before any other MPI routine (apart from MPI-INITIALIZED) is called."
  (unless (mpi-initialized)
    (MPI_Init (null-pointer) (null-pointer))
    ;; by default MPI reacts to each failure by crashing the process. This is
    ;; not the Lisp way of doing things. The following call makes error
    ;; non-fatal in most cases.
    (MPI_Comm_set_errhandler MPI_COMM_WORLD MPI_ERRORS_RETURN)
    t))

(defmpifun "MPI_Initialized" (flag :pointer))
(defun mpi-initialized ()
  "Returns true if MPI_INIT has been called and nil otherwise.
   This routine may be used to determine whether MPI-INIT has been called. It
   is the only routine that may be called before MPI-INIT is called."
  (with-foreign-object (flag :int)
    (MPI_Initialized flag)
    (= 1 (mem-ref flag :int))))

;;; after careful consideration I considered it is the right thing to call
;;; MPI-INIT as soon as cl-mpi is loaded. Otherwise all MPI calls exhibit
;;; unspecified behaviour.
(eval-when (:load-toplevel) (mpi-init))

(defmpifun "MPI_Abort" (comm MPI_Comm) (errorcode :int))
(defun mpi-abort(&key (comm MPI_COMM_WORLD) (errcode -1))
  "This routine makes a 'best attempt' to abort all tasks in the group of
comm. This function does not require that the invoking environment take any
action with the error code. However, a Unix or POSIX environment should handle
this as a return errorcode from the main program or an abort(errorcode)."
  (with-foreign-object (c-errcode :int)
    (setf (mem-aref c-errcode :int) errcode)
    (MPI_Abort comm (mem-aref c-errcode :int))
    c-errcode))

(defmpifun "MPI_Get_processor_name" (processor_name :string) (namelen :pointer))
(defun mpi-get-processor-name ()
  "This routine returns the name of the processor on which it was called at
the moment of the call.  The name is a character string for maximum
flexibility. From this value it must be possible to identify a specific piece
of hardware; possible values include 'processor 9 in rack 4 of mpp.cs.org' and
'231' (where 231 is the actual processor number in the running homogeneous
system)."
  (with-foreign-object (namelen :int)
    (with-foreign-pointer (processor-name MPI_MAX_PROCESSOR_NAME)
      (MPI_Get_processor_name processor-name namelen)
      (values (foreign-string-to-lisp
               processor-name
               :count (mem-aref namelen :int))))))

(defmpifun "MPI_Error_string" (errorcode :int) (string :pointer) (resultlen :pointer))
(defun mpi-error-string (errorcode)
  "Convert the given errorcode to a human readable error message"
  (with-foreign-object (strlen :int)
    (with-foreign-pointer (error-string MPI_MAX_ERROR_STRING)
      (MPI_Error_String errorcode error-string strlen)
      (values (foreign-string-to-lisp
               error-string
               :count (mem-aref strlen :int))))))

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

(defmpifun "MPI_Barrier" (communicator MPI_Comm))
(defun mpi-barrier (&optional (comm MPI_COMM_WORLD))
  "MPI_BARRIER blocks the caller until all group members have called it. The
  call returns at any process only after all group members have entered the
  call."
  (MPI_Barrier comm))

(declaim (inline cffi-type-to-mpi-type))
(defun cffi-type-to-mpi-type (cffi-type)
  "Convert :int to MPI_INT and so on"
  (declare (type keyword cffi-type))
  (ecase cffi-type
    ((:char) MPI_CHAR)
    ((:uchar :unsigned-char) MPI_UNSIGNED_CHAR)
    ((:short) MPI_SHORT)
    ((:ushort :unsigned-short) MPI_UNSIGNED_SHORT)
    ((:int) MPI_INT)
    ((:uint :unsigned-int) MPI_UNSIGNED)
    ((:long) MPI_LONG)
    ((:ulong :unsigned-long) MPI_UNSIGNED_LONG)
    ((:llong :long-long) MPI_LONG_LONG_INT)
    ((:ullong :unsigned-long-long) MPI_UNSIGNED_LONG_LONG)
    ((:float) MPI_FLOAT)
    ((:double) MPI_DOUBLE)
    ((:long-double) MPI_LONG_DOUBLE)))

(defun mpi-send-foreign (dest foreign-pointer size cffi-type
                         &key (tag 0) (comm MPI_COMM_WORLD))
  "Send data to the MPI process specified by the integers DEST and
  TAG. FOREIGN-POINTER must point to a foreign-array with SIZE elements of
  type CFFI-TYPE."
  (declare (type (signed-byte 32) dest size tag)
           (type keyword cffi-type))
  (MPI_Send foreign-pointer size (cffi-type-to-mpi-type cffi-type) dest tag comm))

(defun mpi-receive-foreign (source foreign-pointer size cffi-type
                            &key (tag MPI_ANY_TAG) (comm MPI_COMM_WORLD))
  "Receive data from the MPI process specified by the integers SOURCE and
  TAG. FOREIGN-POINTER must point to a foreign-array with SIZE elements of
  type CFFI-TYPE."
  (declare (type (signed-byte 32) source size tag)
           (type keyword cffi-type))
  (with-foreign-object (mpi-status '(:struct MPI_Status))
    (MPI_Recv foreign-pointer size (cffi-type-to-mpi-type cffi-type) source tag comm mpi-status)
    (with-foreign-slots ((MPI_SOURCE MPI_TAG MPI_ERROR) mpi-status (:struct MPI_Status))
      (unless (zerop MPI_ERROR) (signal-mpi-error MPI_ERROR))
      (values MPI_SOURCE MPI_TAG))))

(defun mpi-sendreceive-foreign
    (dest   send-buf send-count send-type
     source recv-buf recv-count recv-type
     &key (send-tag 0) (recv-tag MPI_ANY_TAG) (comm MPI_COMM_WORLD))
  (declare (type (signed-byte 32)
                 dest   send-count send-tag
                 source recv-count recv-tag))
  (MPI_Sendrecv
     send-buf send-count (cffi-type-to-mpi-type send-type) dest send-tag
     recv-buf recv-count (cffi-type-to-mpi-type recv-type) source recv-tag
     comm MPI_STATUS_IGNORE))

(defun mpi-send (dest object &key (tag 0) (comm MPI_COMM_WORLD))
  (declare (type (signed-byte 32) dest tag))
  (let* ((data (conspack:encode object :stream :static))
         (count (length data)))
    (with-foreign-object (sendbuf :uchar count)
      (loop for i from 0 below count do
        (setf (mem-ref sendbuf :uchar i) (aref data i)))
      (mpi-send-foreign dest sendbuf count :uchar :tag tag :comm comm))))

(defun mpi-receive (source &key (tag MPI_ANY_TAG) (comm MPI_COMM_WORLD))
  (declare (type (signed-byte 32) source tag))
  (with-foreign-objects ((status '(:struct MPI_Status))
                         (count-mem :int))
    (MPI_Probe source tag comm status)
    (MPI_Get_count status MPI_UNSIGNED_CHAR count-mem)
    (let* ((count (mem-ref count-mem :int))
           (data (make-array count :element-type '(unsigned-byte 8))))
      (with-foreign-object (buf :uchar count)
        (mpi-receive-foreign source buf count :uchar :tag tag :comm comm)
        (loop for i from 0 below count do
          (setf (aref data i) (mem-ref buf :uchar i)))
        (values (conspack:decode data) ;TODO
                )))))

(defmpifun "MPI_Comm_group" (comm MPI_Comm) (group MPI_Group))
(defun mpi-comm-group (comm)
  (with-foreign-object (newgroup 'MPI_Group)
    (MPI_comm_group comm newgroup)
    (mem-ref newgroup 'MPI_Group)))

(defmpifun "MPI_Group_size" (group MPI_Group) (size (:pointer :int)))
(defun mpi-group-size (group)
  (with-foreign-object (size :int)
    (MPI_Group_size group size)
    (mem-ref size :int)))

(defmpifun "MPI_Group_rank" (group MPI_Group) (rank (:pointer :int)))
(defun mpi-group-rank (group)
  (with-foreign-object (rank :int)
    (MPI_Group_rank group rank)
    (mem-ref rank :int)))

(defmpifun "MPI_Group_union" (group1 MPI_Group) (group2 MPI_Group) (newgroup :pointer))
(defun mpi-group-union (group1 group2)
    (with-foreign-object (newgroup 'MPI_Group)
    (MPI_Group_union group1 group2 newgroup)
    (mem-ref newgroup 'MPI_Group)))

(defmpifun "MPI_Group_intersection" (group1 MPI_Group) (group2 MPI_Group) (newgroup :pointer))
;; TODO

(defmpifun "MPI_Group_difference" (group1 :pointer) (group2 :pointer) (newgroup :pointer))
;; TODO

(defmpifun "MPI_Group_range_incl" (group MPI_Group) (n :int) (ranges :pointer) (newgroup :pointer))
(defun mpi-group-select-from (group &rest ranges)
  "Create a new MPI group consisting of a subset of the ranks of the original
 group. A valid range can be
  - an integer
  - a list of the form (first-rank last-rank &optional step-size)"
  (let ((n (length ranges)))
    (with-foreign-objects ((mem :int (* 3 n)) (newgroup 'MPI_Group))
      (loop for range-spec in ranges and i from 0 by 3
         with step-size = 1 and last-rank and first-rank do
           (cond
             ((integerp range-spec)
              (setf first-rank range-spec)
              (setf last-rank range-spec))
             ((listp range-spec)
              (setf first-rank (car range-spec))
              (setf last-rank (cadr range-spec))
              (setf step-size (if (cddr range-spec) (caddr range-spec) 1)))
             (t (error "invalid range spec")))
           (setf (mem-aref mem :int (+ i 0)) first-rank)
           (setf (mem-aref mem :int (+ i 1)) last-rank)
           (setf (mem-aref mem :int (+ i 2)) step-size))
      (MPI_Group_range_incl group n mem newgroup)
      (mem-ref newgroup 'MPI_Group))))

(defmpifun "MPI_Group_excl" (group :pointer) (n :int) (ranks :pointer) (newgroup :pointer))
(defmpifun "MPI_Group_free" (group MPI_Group))
(defun mpi-group-free (group) (MPI_Group_free group))

(defmpifun "MPI_Comm_size" (communicator MPI_Comm)(numprocs :pointer))
(defun mpi-comm-size (&optional (comm MPI_COMM_WORLD))
  "Indicates the number of processes involved in a communicator. For
MPI_COMM_WORLD, it indicates the total number of processes available."
  (with-foreign-object (size-mem :int)
    (MPI_comm_size comm size-mem)
    (let ((size (mem-ref size-mem :int)))
      (assert (> size 0))
      size)))

(defmpifun "MPI_Comm_rank" (communicator MPI_Comm)(myid :pointer))
(defun mpi-comm-rank (&optional (comm MPI_COMM_WORLD))
  "Returns the rank of the process in a given communicator."
  (with-foreign-object (rank-mem :int)
    (MPI_Comm_rank comm rank-mem)
    (let ((rank (mem-ref rank-mem :int)))
      (assert (>= rank 0))
      rank)))

(defmpifun "MPI_Comm_create" (communicator MPI_Comm) (group MPI_Group) (newcomm :pointer))
(defun mpi-comm-create (group &key (comm MPI_COMM_WORLD))
  (with-foreign-object (newcomm 'MPI_Comm)
    (MPI_Comm_create comm group newcomm)
    (mem-ref newcomm 'MPI_Comm)))
