(in-package :cl-user)

(defun main (args)
  (unwind-protect
       (uiop:symbol-call
        "CL-MPI-TEST-SUITE"
        "RUN-CL-MPI-TEST-SUITE")
    (ignore-errors
     (uiop:symbol-call "CL-MPI" "MPI-FINALIZE")
     (uiop:quit 0))))
