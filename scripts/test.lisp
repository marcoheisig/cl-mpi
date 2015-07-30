(unwind-protect
     (asdf:test-system :cl-mpi)
  (ignore-errors
   (uiop:symbol-call "CL-MPI" "MPI-FINALIZE"))
  (uiop:quit 0))
