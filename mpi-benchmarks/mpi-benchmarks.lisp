(defpackage :mpi-benchmarks
  (:documentation "benchmark suite for MPI under Lisp")
  (:nicknames #:cl-mpi-benchmarks)
  (:use #:cl #:mpi #:uiop #:cffi)
  (:import-from #:cl-mpi
                mpi-comm-rank mpi-comm-size mpi-comm-create
                mpi-comm-group mpi-group-select-from
                mpi-send mpi-receive mpi-wtime)
  (:import-from #:static-vectors
                make-static-vector free-static-vector static-vector-pointer)
  (:export run-benchmarks))
