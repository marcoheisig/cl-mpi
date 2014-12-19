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

(defun mpi-init ()
  "This routine must be called before any other MPI routine. It must be called
at most once; subsequent calls are erroneous (see MPI-INITIALIZED).

All MPI programs must contain a call to MPI-INIT; this routine must be called
before any other MPI routine (apart from MPI-INITIALIZED) is called."
  (unless (mpi-initialized)
    (MPI_Init (cffi:null-pointer) (cffi:null-pointer))))

(defun mpi-initialized ()
  "Returns true if MPI_INIT has been called and nil otherwise.
   This routine may be used to determine whether MPI-INIT has been called. It
   is the only routine that may be called before MPI-INIT is called."
  (cffi:with-foreign-object (flag :int)
    (MPI_Initialized flag)
    (= 1 (cffi:mem-ref flag :int))))

(defun mpi-comm-rank (&optional (comm MPI_COMM_WORLD))
  "Returns the rank of the process in a given communicator."
  (cffi:with-foreign-object (rank :int)
    (MPI_Comm_rank comm rank)
    (cffi:mem-ref rank :int)))

(defun mpi-comm-size (&optional (comm MPI_COMM_WORLD))
  "Indicates the number of processes involved in a communicator. For
MPI_COMM_WORLD, it indicates the total number of processes available."
  (cffi:with-foreign-object (numprocs :int)
    (MPI_comm_size comm numprocs)
    (cffi:mem-aref numprocs :int)))

(defun mpi-finalize ()
  "This routines cleans up all MPI state. Once this routine is called, no MPI
routine (even MPI-INIT) may be called. The user must ensure that all pending
communications involving a process completes before the process calls
MPI-FINALIZE."
  (MPI_Finalize))

(defun mpi-abort(&key (comm MPI_COMM_WORLD)(errcode -1))
  "This routine makes a 'best attempt' to abort all tasks in the group of
comm. This function does not require that the invoking environment take any
action with the error code. However, a Unix or POSIX environment should handle
this as a return errorcode from the main program or an abort(errorcode)."
  (cffi:with-foreign-object (c-errcode :int)
    (setf (cffi:mem-aref c-errcode :int) errcode)
    (MPI_Abort comm (cffi:mem-aref c-errcode :int))
    c-errcode))

(defmacro with-mpi (&body body)
  "executes body in an MPI environment (initializes and finalizes MPI
before/after body)"
  `(progn
     (mpi-init)
     (unwind-protect (progn ,@body)
       (mpi-finalize))))

(defun mpi-get-processor-name ()
  "This routine returns the name of the processor on which it was called at
the moment of the call.  The name is a character string for maximum
flexibility. From this value it must be possible to identify a specific piece
of hardware; possible values include 'processor 9 in rack 4 of mpp.cs.org' and
'231' (where 231 is the actual processor number in the running homogeneous
system)."
  (cffi:with-foreign-object (namelen :int) ;length of name returned by MPI call
    (cffi:with-foreign-pointer (processor-name MPI_MAX_PROCESSOR_NAME)
      (MPI_Get_processor_name processor-name namelen)
      (values (cffi:foreign-string-to-lisp processor-name
                                           :count (cffi:mem-aref namelen :int))))))

(defun mpi-wtime ()
  "Returns a (double) floating-point number of seconds, representing elapsed
wall-clock time since some time in the past.

The 'time in the past' is guaranteed not to change during the life of the
process.  The user is responsible for converting large numbers of seconds to
other units if they are preferred.  This function is portable (it returns
seconds, not 'ticks'), it allows high-resolution, and carries no unnecessary
baggage.  The times returned are local to the node that called them. There is
no requirement that different nodes return 'the same time.'"
  (MPI_Wtime))

(defun mpi-wtick ()
  "Returns the resolution of MPI-WTIME in seconds. That is, it returns, as a
double precision value, the number of seconds between successive clock
ticks. For example, if the clock is implemented by the hardware as a counter
that is incremented every millisecond, the value returned by MPI-WTICK should
be 0.001"
  (MPI_Wtick))

(defun mpi-barrier (&optional (comm MPI_COMM_WORLD))
  "MPI_BARRIER blocks the caller until all group members have called it. The
  call returns at any process only after all group members have entered the
  call."
  (MPI_Barrier comm))

(defun mpi-send (object dest &key (tag 0) (comm MPI_COMM_WORLD))
  (let* ((data (conspack:encode object))
         (count (array-total-size data)))

    (cffi:with-foreign-object (count-buf :int)
      (setf (cffi:mem-ref count-buf :int) count)
      (MPI_Send count-buf 1 MPI_INT dest 42 comm)

      #+sbcl
      (sb-sys:with-pinned-objects (data)
        (let ((buf (sb-sys:vector-sap (sb-ext:array-storage-vector data))))
          (MPI_Send buf count MPI_BYTE dest tag comm)))
      ;; TOOD default case
      )))

(defgeneric mpi-send-vector (vec dest tag comm))
(defmethod mpi-send-vector ((vec (vector ))))

(defun mpi-receive (&key (source MPI_ANY_SOURCE)
                      (tag MPI_ANY_TAG)
                      (comm MPI_COMM_WORLD))
  (cffi:with-foreign-object (count-buf :int)
    (MPI_Recv count-buf 1 MPI_INT source 42 comm (cffi:null-pointer))
    (let* ((count (cffi:mem-ref count-buf :int))
           (data (make-array count :element-type '(unsigned-byte 8))))

      #+sbcl
      (sb-sys:with-pinned-objects (data)
        (let ((buf (sb-sys:vector-sap (sb-ext:array-storage-vector data))))
          (MPI_Recv buf count MPI_BYTE source tag comm (cffi:null-pointer))))

      ;; TOOD default case
      (conspack:decode data))))
