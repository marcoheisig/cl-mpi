(unwind-protect
     (let ((*standard-output* (make-broadcast-stream)))
       (asdf:load-system :cl-mpi)
       (asdf:load-system :cl-mpi-testsuite)
       (asdf:load-system :mpi-benchmarks))
  (ignore-errors
   (uiop:symbol-call "CL-MPI" "MPI-FINALIZE"))
  (uiop:quit 0))
