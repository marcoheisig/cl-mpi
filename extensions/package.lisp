(defpackage :cl-mpi-extensions
  (:nicknames :mpi-extensions)
  (:use :cl :cl-mpi :static-vectors :alexandria)
  (:export
   #:mpi-send-anything
   #:mpi-receive-anything
   #:mpi-broadcast-anything))
