(in-package :cl-user)

(defun main (args)
  (unwind-protect
       (cl-mpi-test-suite:run-cl-mpi-test-suite)
    (mpi:mpi-finalize)
    (uiop:quit 0)))
