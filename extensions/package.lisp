(defpackage :cl-mpi-extensions
  (:nicknames :mpi-extensions)
  (:use :cl :cl-mpi :static-vectors :alexandria)
  (:export
   #:mpi-send-anything
   #:mpi-recv-anything
   #:mpi-waitall-anything
   #:mpi-isend-anything
   #:mpi-irecv-anything
   #:mpi-broadcast-anything))
