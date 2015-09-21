#+asdf3 (in-package #:asdf-user)

;; this snippet prevents several dozen UIOP redefinition warnings.
(eval-when (:compile-toplevel :load-toplevel :execute)
 (locally
    (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
  (handler-bind
      (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
    (asdf:load-system 'cffi-grovel))))

(defsystem #:cl-mpi
  :description "Common Lisp bindings for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.5"
  :license "MIT"
  :defsystem-depends-on (#:cl-mpi-asdf-utilities)
  :depends-on (#:alexandria #:cffi #:static-vectors #:cl-conspack)
  :in-order-to ((test-op (test-op "cl-mpi-testsuite")))
  :components
  ((:module "mpi"
    :serial t
    ;; Let me explain this long chain of serial dependencies: After the
    ;; package declaration, "grovel.lisp" extracts all constants from mpi.h
    ;; and the system MPI library is loaded via "cl-mpi-stub.c". The constants
    ;; are then used in "configure.lisp" to set up MPI implementation
    ;; dependent reader conditionals. "wrapper-types.lisp" integrates the MPI
    ;; handles and error codes into Lisp objects with their CFFI
    ;; counterparts. After the object definition, MPI related constants and
    ;; variables can be declared in "variables.lisp". Those variables are used
    ;; to define several helper functions in "utilities.lisp". All remaining
    ;; files are independent of each other and correspond to the individual
    ;; chapters of the MPI specification.
    :components
    ((:file "packages")
     ("cffi-grovel:grovel-file" "grovel") ; extract all constants from "mpi.h"
     ("cl-mpi-asdf-utilities:mpi-stub" "cl-mpi-stub") ; load system MPI implementation
     (:file "configure") ; MPI implementation dependent *features*
     (:file "wrapper-types") ; CLOS wrappers for MPI handles
     (:file "variables") ; Lisp-accessible variables from mpi.h
     (:file "utilities")
     (:file "datatypes")
     (:file "collective")
     (:file "contexts")
     (:file "environment")
     (:file "point-to-point")
     (:file "one-sided")
     (:file "extensions")))))
