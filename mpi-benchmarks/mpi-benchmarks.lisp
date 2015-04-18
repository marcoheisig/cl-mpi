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

(in-package :mpi-benchmarks)

(defun run-benchmarks ()
  (fast-pingpong      1  1000000 +mpi-comm-world+)
  (fast-pingpong      2  1000000 +mpi-comm-world+)
  (fast-pingpong      4  1000000 +mpi-comm-world+)
  (fast-pingpong      8   500000 +mpi-comm-world+)
  (fast-pingpong     16   100000 +mpi-comm-world+)
  (fast-pingpong     32   100000 +mpi-comm-world+)
  (fast-pingpong     64   100000 +mpi-comm-world+)
  (fast-pingpong    128    10000 +mpi-comm-world+)
  (fast-pingpong    256    10000 +mpi-comm-world+)
  (fast-pingpong    512    10000 +mpi-comm-world+)
  (fast-pingpong   1024    10000 +mpi-comm-world+)
  (fast-pingpong   2048    10000 +mpi-comm-world+)
  (fast-pingpong   4096    10000 +mpi-comm-world+)
  (fast-pingpong   8192    10000 +mpi-comm-world+)
  (fast-pingpong  16384    10000 +mpi-comm-world+)
  (fast-pingpong  32768    10000 +mpi-comm-world+)
  (fast-pingpong  65536     6400 +mpi-comm-world+)
  (fast-pingpong 131072     3200 +mpi-comm-world+)
  (fast-pingpong 262144     1600 +mpi-comm-world+)
  (fast-pingpong 524288      800 +mpi-comm-world+))
