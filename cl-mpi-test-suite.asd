(cl:in-package #:asdf-user)

(defsystem :cl-mpi-test-suite
  :description "The cl-mpi test suite"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"
  :depends-on (:cl-mpi :fiveam :cffi)
  :perform (test-op (o s)
                    (uiop:symbol-call
                     '#:cl-mpi-test-suite
                     '#:run-cl-mpi-test-suite))
  :serial t
  :components
  ((:module "test-suite"
    :components
    ((:file "packages")
     (:file "serial-tests")
     (:file "parallel-tests")
     (:file "stress-tests")
     (:file "test-suite")))))
