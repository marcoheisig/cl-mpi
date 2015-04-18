(defsystem cl-mpi-benchmarks
  :depends-on (:cl-mpi :cffi)
  :perform (test-op (o s)
                    (uiop:symbol-call
                     :mpi-benchmarks  '#:run-benchmarks))
  :components
  ((:module "mpi-benchmarks"
            :components
            ((:file "mpi-benchmarks")
             (:file "mpi-pingpong")))))
