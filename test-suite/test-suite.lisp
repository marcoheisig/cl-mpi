(in-package :cl-mpi-test-suite)

(defun print-test-suite-banner (destination)
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
          (format nil "~A" +mpi-library+)))
    (format destination
            "== Testing CL-MPI ==
Machine: ~A
Lisp:    ~A
MPI:     ~A
cl-mpi:  cl-mpi ~A~%"
            machine lisp mpi cl-mpi)))

(defmacro with-single-output (&body body)
  "Disable output on all processes but the one with MPI rank zero."
  `(let ((*test-dribble*
           (if (zerop (mpi-comm-rank))
               *test-dribble*
               (make-broadcast-stream))))
     ,@body))

(defun run-cl-mpi-test-suite ()
  (mpi-init)
  (assert (mpi-initialized))
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (assert (> size 0))
    (assert (> size rank -1))
    ;; discard the output of all but one MPI process
    (with-single-output
        (print-test-suite-banner *test-dribble*)
      (run! 'mpi-serial-tests)
      (if (> size 1) ; check whether we run in parallel
          (run! 'mpi-parallel-tests)
          (format *test-dribble* "
Note: You tested cl-mpi with only one process. Some test cases require a
    parallel run and have been skipped. Rerun the program with `$ mpiexec
    -np 2 YOUR_PROGRAM' to perform all tests.
")))))

(defun run-cl-mpi-stress-tests ()
  (mpi-init)
  (with-single-output
    (print-test-suite-banner *test-dribble*)
    (run! 'mpi-stress-tests)))
