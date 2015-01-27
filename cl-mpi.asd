(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system 'cffi-grovel)
  )

(asdf:defsystem :cl-mpi
  :description "Common Lisp bindings for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.2.0"
  :license "MIT"
  :depends-on (:cffi :cl-ppcre :cl-conspack :static-vectors)
  :in-order-to ((test-op (test-op "cl-mpi-testsuite")))
  :components
  ((:module "mpi"
            :serial t
            :components
            ((:file "package")
             (:file "mpi-configure")
             (cffi-grovel:grovel-file "mpi-grovel")
             (:file "mpi-bindings")
             (:file "mpi")))))
