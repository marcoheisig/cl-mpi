(unwind-protect
     (asdf:test-system :mpi-benchmarks)
  (ignore-errors
   (uiop:symbol-call "CL-MPI" "MPI-FINALIZE"))
  (uiop:quit 0))
