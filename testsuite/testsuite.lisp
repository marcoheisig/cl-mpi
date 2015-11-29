(in-package :cl-mpi-testsuite)

(defun run-cl-mpi-testsuite ()
  (mpi-init)
  (assert (mpi-initialized))
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (assert (> size 0))
    (assert (> size rank -1))
    ;; discard the output of all but one MPI process
    (let ((*test-dribble*
            (if (zerop rank)
                *test-dribble*
                (make-broadcast-stream))))
      (let ((machine
              (format nil "~A ~A"
                      (machine-type)
                      (machine-version)))
            (lisp
              (format nil "~A ~A"
                      (lisp-implementation-type)
                      (lisp-implementation-version)))
            (cl-mpi
              (asdf:component-version
               (asdf:find-system :cl-mpi)))
            (mpi
              (format nil "~A ~A"
                      +mpi-implementation+
                      +mpi-implementation-version+)))
        (format *test-dribble*
                "
Machine: ~A
Lisp:    ~A
MPI:     ~A
cl-mpi:  cl-mpi ~A~%"
                machine lisp mpi cl-mpi))
      (run! 'mpi-serial-tests)
      (if (> size 1) ; check whether we run in parallel
          (run! 'mpi-parallel-tests)
          (format *test-dribble* "
Note: You tested cl-mpi with only one process. Many testcases require a
      parallel run and have been skipped. Rerun the program with
      $ mpiexec -np 2 YOUR_PROGRAM
      to perform all tests.
")))))
