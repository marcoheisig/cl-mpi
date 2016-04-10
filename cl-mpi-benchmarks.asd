(defsystem #:cl-mpi-benchmarks
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.9"
  :license "MIT"
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
