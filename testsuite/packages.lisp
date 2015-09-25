(defpackage #:cl-mpi-testsuite
  (:nicknames #:mpi-testsuite)
  (:use #:cl #:cl-mpi #:5am #:uiop #:cffi #:static-vectors)
  (:export #:run-cl-mpi-testsuite))

(in-package :cl-mpi-testsuite)

(def-suite mpi-testsuite
  :description "All MPI related tests.")

(def-suite mpi-serial-tests
  :in mpi-testsuite
  :description "CL-MPI tests that can be run on a single process.")

(def-suite mpi-parallel-tests
  :in mpi-testsuite
  :description "CL-MPI tests that need more than one participating process.")
