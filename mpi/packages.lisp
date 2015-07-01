(defpackage #:mpi
  (:nicknames #:cl-mpi)
  (:documentation
   "CL-MPI: Common Lisp bindings for the Message Passing Interface MPI")
  (:use #:cl #:cffi #:static-vectors #:alexandria)
  (:import-from #:uiop #:version<=)
  (:export

   ;; constants and handles
   #:+mpi-any-source+
   #:+mpi-any-tag+
   #:+mpi-status-ignore+
   #:+mpi-proc-null+
   #:+mpi-root+
   #:+mpi-undefined+
   #:+mpi-comm-world+ ; communicators
   #:+mpi-comm-self+
   #:+mpi-comm-null+
   #:+mpi-datatype-null+ ; data types
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
   #:+mpi-packed+
   #:+mpi-null+ ; operators
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
   #:+mpi-no-op+
   #:*standard-communicator*

    ;; lisp objects
   #:mpi_status
   #:mpi-errhandler
   #:mpi-comm
   #:mpi-group
   #:mpi-op
   #:mpi-datatype
   #:mpi-object=

   ;; functions
   #:mpi-init
   #:mpi-initialized
   #:mpi-finalize
   #:mpi-comm-rank
   #:mpi-comm-size
   #:mpi-comm-group
   #:mpi-comm-dup
   #:mpi-group-size
   #:mpi-group-incl
   #:mpi-group-excl
   #:mpi-group-free
   #:mpi-get-processor-name
   #:mpi-barrier
   #:mpi-wtime
   #:mpi-wtick
   #:mpi-abort
   #:mpi-send
   #:mpi-receive
   #:mpi-sendreceive
   #:mpi-broadcast
   #:mpi-allgather

   ;; extensions
   #:mpi-send-anything
   #:mpi-receive-anything
   #:mpi-broadcast-anything))

(defpackage #:mpi-header
  (:documentation "All constants extracted from mpi.h"))
