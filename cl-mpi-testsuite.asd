(defsystem #:cl-mpi-testsuite
  :description "The cl-mpi testsuite"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.9"
  :license "MIT"
  :depends-on (#:cl-mpi #:fiveam #:cffi)
  :perform (test-op (o s)
                    (uiop:symbol-call
                     '#:cl-mpi-testsuite '#:run-cl-mpi-testsuite))
  :serial t
  :components
  ((:module "testsuite"
    :components
            ((:file "packages")
             (:file "serial-tests")
             (:file "parallel-tests")
             (:file "testsuite")))))
