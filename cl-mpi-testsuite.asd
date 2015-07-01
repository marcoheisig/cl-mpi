(in-package #:asdf-user)

(defsystem #:cl-mpi-testsuite
  :depends-on (#:cl-mpi #:fiveam #:cffi)
  :perform (test-op (o s)
                    (uiop:symbol-call
                     '#:fiveam  '#:run!
                     (uiop:find-symbol* '#:mpi-testsuite '#:mpi-testsuite)))
  :serial t
  :components
  ((:module "mpi-testsuite"
    :components
            ((:file "packages")
             (:file "serial-tests")
             (:file "parallel-tests")))))
