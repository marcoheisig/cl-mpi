(defpackage #:cl-mpi-test-suite
  (:nicknames #:mpi-test-suite)
  (:use #:cl #:cl-mpi #:5am #:uiop #:cffi #:static-vectors)
  (:export #:run-cl-mpi-test-suite))

(in-package :cl-mpi-test-suite)

(def-suite mpi-test-suite
  :description "All MPI related tests.")

(def-suite mpi-serial-tests
  :in mpi-test-suite
  :description "CL-MPI tests that can be run on a single process.")

(def-suite mpi-parallel-tests
  :in mpi-test-suite
  :description "CL-MPI tests that need more than one participating process.")

(def-suite mpi-stress-tests
  :in mpi-test-suite
  :description "CL-MPI tests that require a long time to run.")
