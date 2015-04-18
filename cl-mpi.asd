(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system 'cffi-grovel))

;; use "mpicc" as compiler for all mpi related cffi-grovel files
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod asdf:perform :around
      ((op cffi-grovel::process-op) (c cffi-grovel:grovel-file))
    (let ((cffi-grovel::*cc* "mpicc"))
      (call-next-method))))

(asdf:defsystem :cl-mpi
  :description "Common Lisp bindings for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.2.0"
  :license "MIT"
  :depends-on (:alexandria :cffi :cl-ppcre :static-vectors :cl-conspack)
  :in-order-to ((test-op (test-op "cl-mpi-testsuite")))
  :components
  ((:module "mpi"
    :serial t
    :components
    ((:file "packages")
     (cffi-grovel:grovel-file "mpi-grovel")
     (:file "mpi-configure")
     (:file "mpi-types")
     (:file "mpi-variables")
     (:file "mpi-bindings")
     (:file "mpi")
     (:file "mpi-extensions")))))
