(cl:in-package #:asdf-user)

(defsystem :cl-mpi-asdf-integration
  :description "CFFI and ASDF integration for CL-MPI."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"
  :depends-on (:cffi-grovel :cffi-toolchain)
  :components
  ((:file "cl-mpi-asdf-integration")))
