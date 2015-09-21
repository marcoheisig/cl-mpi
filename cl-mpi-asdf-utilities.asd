#+asdf3 (in-package #:asdf-user)

(defsystem #:cl-mpi-asdf-utilities
  :description "ASDF support for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "1.0"
  :license "MIT"
  :depends-on (#:cffi-grovel)
  :components
  ((:file "cl-mpi-asdf-utilities")))
