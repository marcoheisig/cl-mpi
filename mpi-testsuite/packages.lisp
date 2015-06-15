(defpackage :mpi-testsuite
  (:use :cl :mpi :5am :uiop :cffi :static-vectors))

(in-package :mpi-testsuite)

(def-suite mpi-testsuite :description "All MPI related tests.")
