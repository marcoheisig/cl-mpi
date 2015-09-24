
(in-package :cl-user)

(defun main (argv)
  (unwind-protect
       (cond
         ((string-equal (first argv) "benchmark")
          (mpi-benchmarks:run-benchmarks))
         (t
          (mpi-testsuite:run-cl-mpi-testsuite)))
    (ignore-errors
     (uiop:symbol-call "CL-MPI" "MPI-FINALIZE")
     (uiop:quit 0))))
