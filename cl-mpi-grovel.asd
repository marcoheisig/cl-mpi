(cl:in-package #:asdf-user)

(defsystem :cl-mpi-grovel
  :description "CFFI and ASDF integration for CL-MPI."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"
  :depends-on (:cffi-grovel)
  :components
  ((:file "cl-mpi-grovel")))
