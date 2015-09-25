(defsystem #:cl-mpi-benchmarks
  :depends-on (#:cl-mpi #:cffi)
  :perform (test-op (o s)
                    (uiop:symbol-call
                     :cl-mpi-benchmarks  '#:run-benchmarks))
  :components
  ((:module "benchmarks"
            :components
            ((:file "package")
             (:file "mpi-pingpong" :depends-on ("package"))
             (:file "mpi-benchmarks" :depends-on ("mpi-pingpong"))))))
