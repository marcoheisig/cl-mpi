#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI bindings for Common Lisp

Copyright (c) 2008,2009  Alex Fukunaga
Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

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

(defmacro defmpifun (c-name &rest rest)
  (let ((name (intern (string-upcase c-name))))
    `(cffi:defcfun (,c-name ,name) mpi-error-code ,@rest)))

(defmpifun "MPI_Comm_set_errhandler" (comm MPI_Comm) (errhandler MPI_Errhandler))
(defmpifun "MPI_Init" (argc :pointer) (argv :pointer))
(defun mpi-init ()
  "This routine must be called before any other MPI routine. It must be called
at most once; subsequent calls are erroneous (see MPI-INITIALIZED).

All MPI programs must contain a call to MPI-INIT; this routine must be called
before any other MPI routine (apart from MPI-INITIALIZED) is called."
  (unless (mpi-initialized)
    (MPI_Init (cffi:null-pointer) (cffi:null-pointer))
    ;; by default MPI reacts to each failure by crashing the process. This is
    ;; not the Lisp way of doing things. The following call makes error
    ;; non-fatal in most cases.
    (MPI_COMM_SET_ERRHANDLER MPI_COMM_WORLD MPI_ERRORS_RETURN)))

(defmpifun "MPI_Initialized" (flag :pointer))
(defun mpi-initialized ()
  "Returns true if MPI_INIT has been called and nil otherwise.
   This routine may be used to determine whether MPI-INIT has been called. It
   is the only routine that may be called before MPI-INIT is called."
  (cffi:with-foreign-object (flag :int)
    (MPI_Initialized flag)
    (= 1 (cffi:mem-ref flag :int))))

(defmpifun "MPI_Finalize")
(defun mpi-finalize ()
  "This routines cleans up all MPI state. Once this routine is called, no MPI
routine (even MPI-INIT) may be called. The user must ensure that all pending
communications involving a process completes before the process calls
MPI-FINALIZE."
  (MPI_Finalize))

(defmpifun "MPI_Abort" (comm MPI_Comm) (errorcode :int))
(defun mpi-abort(&key (comm MPI_COMM_WORLD) (errcode -1))
  "This routine makes a 'best attempt' to abort all tasks in the group of
comm. This function does not require that the invoking environment take any
action with the error code. However, a Unix or POSIX environment should handle
this as a return errorcode from the main program or an abort(errorcode)."
  (cffi:with-foreign-object (c-errcode :int)
    (setf (cffi:mem-aref c-errcode :int) errcode)
    (MPI_Abort comm (cffi:mem-aref c-errcode :int))
    c-errcode))

(defmpifun "MPI_Get_processor_name" (processor_name :string) (namelen :pointer))
(defun mpi-get-processor-name ()
  "This routine returns the name of the processor on which it was called at
the moment of the call.  The name is a character string for maximum
flexibility. From this value it must be possible to identify a specific piece
of hardware; possible values include 'processor 9 in rack 4 of mpp.cs.org' and
'231' (where 231 is the actual processor number in the running homogeneous
system)."
  (cffi:with-foreign-object (namelen :int)
    (cffi:with-foreign-pointer (processor-name MPI_MAX_PROCESSOR_NAME)
      (MPI_Get_processor_name processor-name namelen)
      (values (cffi:foreign-string-to-lisp processor-name
                                           :count (cffi:mem-aref namelen :int))))))

(defmpifun "MPI_Error_string" (errorcode :int) (string :pointer) (resultlen :pointer))
(defun mpi-error-string (errorcode)
  (cffi:with-foreign-object (strlen :int)
    (cffi:with-foreign-pointer (error-string MPI_MAX_ERROR_STRING)
      (MPI_Error_String errorcode error-string strlen)
      (values (cffi:foreign-string-to-lisp error-string
                                           :count (cffi:mem-aref strlen :int))))))

(cffi:defcfun "MPI_Wtime" :double
  "Returns a (double) floating-point number of seconds, representing elapsed
wall-clock time since some time in the past.

The 'time in the past' is guaranteed not to change during the life of the
process.  The user is responsible for converting large numbers of seconds to
other units if they are preferred.  This function is portable (it returns
seconds, not 'ticks'), it allows high-resolution, and carries no unnecessary
baggage.  The times returned are local to the node that called them. There is
no requirement that different nodes return 'the same time.'")

(cffi:defcfun "MPI_Wtick" :double
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

;;; MPI sends only raw bytes or arrays of numeric types. I use the conspack
;;; library to convert any given lisp object to raw bytes and decode it again
;;; at the receiver side. Luckily conspack uses fast-io which can use
;;; static-vector to directly write the data to static memory for transport
;;; via MPI.

(defun mpi-send (dest object &key (tag 0) (comm MPI_COMM_WORLD))
  (let* ((data (conspack:encode object :stream :static))
         (size (length data)))
    ;; first message: number of bytes being transmitted
    (cffi:with-foreign-objects (count-buf :int)
      (setf (cffi:mem-ref count-buf :int) size)
      (MPI_Send count-buf 1 MPI_INT dest 0 comm))
    ;; second message: actual data
    (MPI_Send (static-vectors:static-vector-pointer data) size MPI_BYTE dest tag comm)
    (static-vectors:free-static-vector data)))

(defun mpi-receive (source &key (tag MPI_ANY_TAG) (comm MPI_COMM_WORLD))
  ;; first message: number of bytes being transmitted
  (cffi:with-foreign-objects ((count-buf :int)
                              (status MPI_Status))
    (MPI_Recv count-buf 1 MPI_INT source 0 comm (cffi:inc-pointer (cffi:null-pointer) 1))
    ;; second message: actual data
    (let* ((count (cffi:mem-ref count-buf :int))
           (data (static-vectors:make-static-vector count :element-type '(unsigned-byte 8))))
      (MPI_Recv (static-vectors:static-vector-pointer data) count MPI_BYTE source tag comm MPI_Status)
      (prog1 (conspack:decode data)
        (static-vectors:free-static-vector data)))))

;; TODO (defun mpi-sendrecv (source dest object &key (sendtag 0) (recvtag MPI_ANY_TAG) (comm MPI_COMM_WORLD)) )

;; TODO use CLOS classes
(defvar MPI_Group #+openmpi :pointer #-openmpi :int)
(defvar MPI_Comm  #+openmpi :pointer #-openmpi :int)

(defmpifun "MPI_Comm_group" (comm MPI_Comm) (group :pointer))
(defun mpi-comm-group (comm)
  (cffi:with-foreign-object (newgroup MPI_Group)
    (MPI_comm_group comm newgroup)
    (cffi:mem-ref newgroup MPI_Group)))

(defmpifun "MPI_Group_size" (group MPI_Group) (size :pointer))
(defun mpi-group-size (group)
  (cffi:with-foreign-object (size :int)
    (MPI_Group_size group size)
    (cffi:mem-ref size :int)))

(defmpifun "MPI_Group_rank" (group MPI_Group) (rank :pointer))
(defun mpi-group-rank (group)
  (cffi:with-foreign-object (rank :int)
    (MPI_Group_rank group rank)
    (cffi:mem-ref rank :int)))

(defmpifun "MPI_Group_union" (group1 MPI_Group) (group2 MPI_Group) (newgroup :pointer))
(defun mpi-group-union (group1 group2)
    (cffi:with-foreign-object (newgroup MPI_Group)
    (MPI_Group_union group1 group2 newgroup)
    (cffi:mem-ref newgroup MPI_Group)))

(defmpifun "MPI_Group_intersection" (group1 MPI_Group) (group2 MPI_Group) (newgroup :pointer))
;; TODO

(defmpifun "MPI_Group_difference" (group1 :pointer) (group2 :pointer) (newgroup :pointer))
;; TODO

(defmpifun "MPI_Group_incl" (group MPI_Group) (n :int) (ranks :pointer) (newgroup :pointer))
(defun mpi-group-incl (group ranks)
  (let ((n (length ranks)))
    (cffi:with-foreign-objects ((array :int n) (newgroup MPI_Group))
      (dotimes (i n)
        (setf (cffi:mem-aref array :int i) (aref ranks i)))
      (MPI_Group_incl group n array newgroup)
      (cffi:mem-ref newgroup MPI_Group))))

(defmpifun "MPI_Group_range_incl" (group MPI_Group) (n :int) (ranges :pointer) (newgroup :pointer))
(defun mpi-group-select-from (group &rest ranges)
  "Create a new MPI group consisting of a subset of the ranks of the original
 group. A valid range can be
  - an integer
  - a list of the form (first-rank last-rank &optional step-size)"
  (let ((n (length ranges)))
    (cffi:with-foreign-objects ((mem :int (* 3 n)) (newgroup MPI_Group))
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
           (setf (cffi:mem-aref mem :int (+ i 0)) first-rank)
           (setf (cffi:mem-aref mem :int (+ i 1)) last-rank)
           (setf (cffi:mem-aref mem :int (+ i 2)) step-size))
      (MPI_Group_range_incl group n mem newgroup)
      (cffi:mem-ref newgroup MPI_Group))))

(defmpifun "MPI_Group_excl" (group :pointer) (n :int) (ranks :pointer) (newgroup :pointer))
(defmpifun "MPI_Group_free" (group MPI_Group))
(defun mpi-group-free (group) (MPI_Group_free group))

(defmpifun "MPI_Comm_size" (communicator MPI_Comm)(numprocs :pointer))
(defun mpi-comm-size (&optional (comm MPI_COMM_WORLD))
  "Indicates the number of processes involved in a communicator. For
MPI_COMM_WORLD, it indicates the total number of processes available."
  (cffi:with-foreign-object (numprocs :int)
    (MPI_comm_size comm numprocs)
    (cffi:mem-ref numprocs :int)))

(defmpifun "MPI_Comm_rank" (communicator MPI_Comm)(myid :pointer))
(defun mpi-comm-rank (&optional (comm MPI_COMM_WORLD))
  "Returns the rank of the process in a given communicator."
  (cffi:with-foreign-object (rank :int)
    (MPI_Comm_rank comm rank)
    (cffi:mem-ref rank :int)))

(defmpifun "MPI_Comm_create" (communicator MPI_Comm) (group MPI_Group) (newcomm :pointer))
(defun mpi-comm-create (group &key (comm MPI_COMM_WORLD))
  (cffi:with-foreign-object (newcomm MPI_Comm)
    (MPI_Comm_create comm group newcomm)
    (cffi:mem-ref newcomm MPI_Comm)))
