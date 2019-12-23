#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI environmental management

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

(progn
  (setf (fdefinition 'mpi-wtime) (function %mpi-wtime))
  (setf (documentation 'mpi-wtime 'function)
        "Returns a (double) floating-point number of seconds, representing elapsed
wall-clock time since some time in the past.

The 'time in the past' is guaranteed not to change during the life of the
process.  The user is responsible for converting large numbers of seconds to
other units if they are preferred.  This function is portable (it returns
seconds, not 'ticks'), it allows high-resolution, and carries no unnecessary
baggage.  The times returned are local to the node that called them. There is
no requirement that different nodes return 'the same time.'"))

(progn
  (setf (fdefinition 'mpi-wtick) (function %mpi-wtick))
  (setf (documentation 'mpi-wtick 'function)
        "Returns the resolution of MPI-WTIME in seconds. That is, it returns, as a
double precision value, the number of seconds between successive clock
ticks. For example, if the clock is implemented by the hardware as a counter
that is incremented every millisecond, the value returned by MPI-WTICK should
be 0.001"))

(defun mpi-init (&key (thread-support nil thread-support-p))
  "Initialize MPI. If supplied, the keyword parameter THREAD-SUPPORT
denotes the required level of thread support in MPI. It must be one of the
following keywords:

:MPI-THREAD-SINGLE - Only one thread will ever execute.

:MPI-THREAD-FUNNELED - The process may be multi-threaded, but the
  application must ensure that only the main thread makes MPI calls.

:MPI-THREAD-SERIALIZED - The process may be multi-threaded, and multiple
 threads may make MPI calls, but not concurrently from two distinct
 threads.

:MPI-THREAD-MULTIPLE - Multiple threads may call MPI, with no restrictions.

An error is signaled when the MPI implementation fails to provide the
required level of thread support."
  (unless (mpi-initialized)
    ;; Initialize cl-mpi constants like +MPI-COMM-WORLD+.
    (initialize-mpi-constants)
    (if (not thread-support-p)
        (%mpi-init (null-pointer) (null-pointer))
        (let* ((required
                 (cffi:foreign-enum-value 'mpi-thread-options thread-support))
               (provided
                 (with-foreign-results ((provided :int))
                   (%mpi-init-thread (null-pointer) (null-pointer)
                                     required provided))))
          (when (> required provided)
            (error "The required level of thread support is ~W,~@
                    but this MPI implementation can only provide ~W."
                   thread-support
                   (cffi:foreign-enum-keyword 'mpi-thread-options provided)))))
    ;; by default MPI reacts to each failure by crashing the process. This is
    ;; not the Lisp way of doing things. The following call makes errors
    ;; non-fatal in most cases.
    (%mpi-comm-set-errhandler +mpi-comm-world+ +mpi-errors-return+)))

(defun mpi-finalize ()
   "This routines cleans up all MPI state. Once this routine is called, no MPI
routine (even MPI-INIT) may be called. The user must ensure that all pending
communications involving a process complete before the process calls
MPI-FINALIZE."
  (when (mpi-initialized)
    (unless (mpi-finalized)
      (mpi-buffer-detach)
      (%mpi-finalize))))

(defun mpi-initialized ()
  "Returns true if MPI_INIT has been called and nil otherwise.
   This routine may be used to determine whether MPI-INIT has been called. It
   is the only routine that may be called before MPI-INIT is called."
  (with-foreign-results ((flag :boolean))
    (%mpi-initialized flag)))

(defun mpi-finalized ()
  "Returns true if MPI_FINALIZE has been called and nil otherwise."
  (with-foreign-results ((flag :boolean))
    (%mpi-finalized flag)))

(defun mpi-abort (&key (comm *standard-communicator*) (errcode -1))
  "This routine makes a 'best attempt' to abort all tasks in the group of
comm. This function does not require that the invoking environment take any
action with the error code. However, a Unix or POSIX environment should handle
this as a return errorcode from the main program or an abort(errorcode)."
  (%mpi-abort comm errcode))

(defun mpi-get-processor-name ()
  "This routine returns the name of the processor on which it was called at
the moment of the call.  The name is a character string for maximum
flexibility. From this value it must be possible to identify a specific piece
of hardware; possible values include 'processor 9 in rack 4 of mpp.cs.org' and
'231' (where 231 is the actual processor number in the running homogeneous
system)."
  (with-foreign-object (namelen :int)
    (with-foreign-pointer (processor-name +mpi-max-processor-name+)
      (%mpi-get-processor-name processor-name namelen)
      (values (foreign-string-to-lisp
               processor-name
               :count (mem-aref namelen :int))))))

(defun mpi-error-string (errorcode)
  "Convert the given errorcode to a human readable error message"
  (declare (type int errorcode))
  (with-foreign-object (strlen :int)
    (with-foreign-pointer (error-string +mpi-max-error-string+)
      (%mpi-error-string errorcode error-string strlen)
      (values (foreign-string-to-lisp
               error-string
               :count (mem-aref strlen :int))))))
