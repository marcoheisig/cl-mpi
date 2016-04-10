
(in-package :cl-user)

(defun main (argv)
  (unwind-protect
       (cond
         ((string-equal (first argv) "benchmark")
          (cl-mpi-benchmarks:run-benchmarks))
         (t
          (cl-mpi-test-suite:run-cl-mpi-test-suite)))
    (ignore-errors
     (uiop:symbol-call "CL-MPI" "MPI-FINALIZE")
     (uiop:quit 0))))
