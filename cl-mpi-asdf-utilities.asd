(in-package :asdf-user)

(defsystem :cl-mpi-asdf-utilities
  :description "ASDF support for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"
  :depends-on (:cffi-grovel)
  :components
  ((:module "mpi"
    :components
    ((:file "asdf-utilities")))))
