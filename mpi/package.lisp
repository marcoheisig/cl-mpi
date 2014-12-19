(defpackage #:mpi
  (:nicknames #:cl-mpi)
  (:documentation "CL-MPI: Common Lisp bindings for the Message Passing Interface MPI")
  (:use #:cl)
  (:export
   ;; error handling
   mpi-error
   report-error
   ;; constants and handles
   MPI_COMM_WORLD ;; communicators
   MPI_COMM_SELF
   MPI_COMM_NULL
   MPI_DATATYPE_NULL ;; data types
   MPI_LB
   MPI_UB
   MPI_CHAR
   MPI_SIGNED_CHAR
   MPI_UNSIGNED_CHAR
   MPI_BYTE
   MPI_SHORT
   MPI_UNSIGNED_SHORT
   MPI_INT
   MPI_UNSIGNED
   MPI_LONG
   MPI_UNSIGNED_LONG
   MPI_LONG_LONG_INT
   MPI_UNSIGNED_LONG_LONG
   MPI_FLOAT
   MPI_DOUBLE
   MPI_LONG_DOUBLE
   MPI_WCHAR
   MPI_PACKED
   MPI_NULL ;; operators
   MPI_MIN
   MPI_MAX
   MPI_SUM
   MPI_PROD
   MPI_LAND
   MPI_BAND
   MPI_LOR
   MPI_BOR
   MPI_LXOR
   MPI_BXOR
   MPI_MAXLOC
   MPI_MINLOC
   MPI_REPLACE
   MPI_NO_OP
   ;; functions
   mpi-init ;; environment
   mpi-initialized
   mpi-finalize
   mpi-comm-rank
   mpi-comm-size
   mpi-get-processor-name
   mpi-barrier
   mpi-wtime
   mpi-wtick
   mpi-abort
   mpi-send
   mpi-receive
   ))
