(cl:in-package #:asdf-user)

(defsystem :cl-mpi
  :description "Common Lisp bindings for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "1.0"
  :license "MIT"
  :long-description
  "cl-mpi provides convenient CFFI bindings for the Message Passing
Interface (MPI). MPI is typically used in High Performance Computing to
utilize big parallel computers with thousands of cores. It features minimal
communication overhead with a latency in the range of microseconds. In
comparison to the C or FORTRAN interface of MPI, cl-mpi relieves the
programmer from working with raw pointers to memory and a plethora of
mandatory function arguments."
  :defsystem-depends-on (:cl-mpi-asdf-integration)
  :depends-on (:alexandria :uiop :cffi :static-vectors)
  :in-order-to ((test-op (test-op "cl-mpi-test-suite")))
  :components
  ((:module "mpi"
    :serial t
    :components
    ((:file "packages")

     ;; Extract all constants from "mpi.h".
     (:mpi-grovel-file "grovel")

     ;; Derive MPI implementation dependent constants.
     (:file "setup")

     ;; Create a small library to portably access the MPI runtime.
     (:mpi-wrapper-file "wrap")

     ;; Low-level bindings.
     (:file "low-level")

     ;; MPI related utilities.
     (:file "utilities")

     ;; One file per chapter of the MPI specification.
     (:file "datatypes")
     (:file "collective")
     (:file "contexts")
     (:file "environment")
     (:file "point-to-point")
     (:file "one-sided")
     (:file "file")))))
