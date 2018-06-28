(cl:in-package #:asdf-user)

(defsystem :cl-mpi-examples
  :depends-on (:uiop :cl-mpi)
  :defsystem-depends-on (:cl-mpi-asdf-integration)
  :class :mpi-program

  :build-operation :static-program-op
  :build-pathname "cl-mpi-examples"
  :entry-point "cl-mpi-examples:entry-point"

  :serial t
  :components
  ((:file "ring")
   (:file "cl-mpi-examples")))
