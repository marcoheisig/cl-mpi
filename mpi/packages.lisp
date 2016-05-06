(defpackage :mpi
  (:nicknames :cl-mpi)
  (:documentation
   "CL-MPI: Common Lisp bindings for the Message Passing Interface MPI")
  (:use :cl :cffi :static-vectors :alexandria)
  (:import-from :uiop #:version<=)
  (:export
   ;; configure.lisp
   #:+mpi-version+
   #:+mpi-library+
   #:+mpi-implementation+
   #:+mpi-implementation-version+
   #:+mpi-max-error-string+
   #:+mpi-any-tag+
   #:+mpi-any-source+
   #:+mpi-proc-null+
   #:+mpi-root+
   #:+mpi-undefined+
   #:+mpi-status-ignore+

   ;; wrapper types
   #:mpi-error-condition
   #:mpi-object
   #:mpi-errhandler
   #:mpi-comm
   #:mpi-group
   #:mpi-datatype
   #:mpi-op
   #:mpi-info
   #:mpi-request
   #:mpi-object=

   ;; constants.lisp
   #:+mpi-errors-return+
   #:+mpi-errors-are-fatal+
   #:+mpi-group-empty+
   #:+mpi-group-null+
   #:+mpi-comm-world+
   #:+mpi-comm-self+
   #:+mpi-comm-null+
   #:+mpi-datatype-null+
   #:+mpi-lb+
   #:+mpi-ub+
   #:+mpi-char+
   #:+mpi-signed-char+
   #:+mpi-unsigned-char+
   #:+mpi-byte+
   #:+mpi-short+
   #:+mpi-unsigned-short+
   #:+mpi-int+
   #:+mpi-unsigned+
   #:+mpi-long+
   #:+mpi-unsigned-long+
   #:+mpi-long-long-int+
   #:+mpi-unsigned-long-long+
   #:+mpi-float+
   #:+mpi-double+
   #:+mpi-long-double+
   #:+mpi-wchar+
   #:+mpi-c-bool+
   #:+mpi-int8-t+
   #:+mpi-int16-t+
   #:+mpi-int32-t+
   #:+mpi-int64-t+
   #:+mpi-uint8-t+
   #:+mpi-uint16-t+
   #:+mpi-uint32-t+
   #:+mpi-uint64-t+
   #:+mpi-packed+
   #:+mpi-null+
   #:+mpi-min+
   #:+mpi-max+
   #:+mpi-sum+
   #:+mpi-prod+
   #:+mpi-land+
   #:+mpi-band+
   #:+mpi-lor+
   #:+mpi-bor+
   #:+mpi-lxor+
   #:+mpi-bxor+
   #:+mpi-maxloc+
   #:+mpi-minloc+
   #:+mpi-replace+
   #:+mpi-request-null+
   #:*standard-communicator*
   #:mpi-null

   ;; utilities.lisp
   #:with-static-vectors
   #:with-fresh-mpi-context

   ;; datatypes.lisp
   #:mpi-type-size

   ;; collective.lisp
   #:mpi-barrier
   #:mpi-bcast
   #:mpi-allgather
   #:mpi-allreduce
   #:mpi-reduce

   ;; contexts.lisp
   #:mpi-comm-group
   #:mpi-group-size
   #:mpi-group-rank
   #:mpi-group-union
   #:mpi-group-intersection
   #:mpi-group-difference
   #:mpi-group-incl
   #:mpi-group-excl
   #:mpi-group-free
   #:mpi-comm-size
   #:mpi-comm-rank
   #:mpi-comm-create
   #:mpi-comm-dup
   #:mpi-comm-free

   ;; environment.lisp
   #:mpi-wtime
   #:mpi-wtick
   #:mpi-finalize
   #:mpi-init
   #:mpi-initialized
   #:mpi-finalized
   #:mpi-abort
   #:mpi-get-processor-name
   #:mpi-error-string

   ;; point-to-point.lisp
   #:mpi-sendrecv
   #:mpi-send
   #:mpi-isend
   #:mpi-recv
   #:mpi-irecv
   #:mpi-probe
   #:mpi-iprobe
   #:mpi-wait
   #:mpi-waitall

   ;; one-sided.lisp

   ;; extensions.lisp
   #:mpi-send-anything
   #:mpi-receive-anything
   #:mpi-broadcast-anything))
