(defsystem cl-mpi-testsuite
  :depends-on (:cl-mpi :fiveam :cffi)
  :perform (test-op (o s)
                    (uiop:symbol-call
                     :fiveam  '#:run!
                     (uiop:find-symbol* '#:mpi-testsuite :mpi-testsuite)))
  :components
  ((:module "mpi-testsuite"
            :components
            ((:file "mpi-testsuite")))))
