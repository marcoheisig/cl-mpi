(defpackage :cl-mpi-benchmarks
  (:nicknames :mpi-benchmarks)
  (:documentation "benchmark suite for MPI under Lisp")
  (:use :cl :cl-mpi :uiop :cffi :static-vectors)
  (:export #:run-benchmarks))
