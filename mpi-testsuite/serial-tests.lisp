(in-package :mpi-testsuite)
(in-suite mpi-testsuite)

(test (mpi-init)
  "MPI Initialization."
  (mpi-init)
  (is (mpi-initialized) "failed to initialize MPI (or cl-mpi is broken)"))

(test (size-and-rank :depends-on mpi-init)
  "Check whether it is possible to determine size and rank."
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (is (> size 0) "Invalid size of +mpi-comm-world+")
    (is (> size rank -1) "Invalid MPI rank")))

(test (processor-name :depends-on mpi-init)
  "The function mpi-get-processor-name should return a string describing the
  current processor in use."
  (let ((processor-name (mpi-get-processor-name)))
    (is (stringp processor-name))
    (is (plusp (length processor-name)))))

(test (mpi-barrier :depends-on mpi-init)
  "synchronize all processes with multiple MPI barriers."
  (loop for i from 0 below 10 do (mpi-barrier)))

(test (serial-groups :depends-on size-and-rank)
  "MPI group tests that can be run on a single process."
  (let* ((size (mpi-comm-size))
         (all-procs (mpi-comm-group +mpi-comm-world+))
         (first (mpi-group-incl all-procs 0))
         (all-but-first (mpi-group-excl all-procs 0))
         (evens (mpi-group-incl all-procs `(0 ,(- size 1) 2)))
         (odds  (if (> size 1)
                    (mpi-group-excl all-procs `(1 ,(- size 1) 2))
                    (mpi-group-incl all-procs))))
    (is (= size (mpi-group-size all-procs)))
    (is (= 1 (mpi-group-size first)))
    (is (= (- size 1) (mpi-group-size all-but-first)))
    (is (= (ceiling size 2) (mpi-group-size evens)))
    (is (= (floor size 2) (mpi-group-size odds)))
    (mpi-group-free all-procs first all-but-first odds evens)))

(test (parallel :depends-on size-and-rank)
  "Check whether there is more than one MPI process."
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (is (> size 1) "More than one MPI process is required for most MPI tests")
    ;; discard the output of all but one MPI process
    (unless (zerop rank)
      (setf *test-dribble* (make-broadcast-stream)))))
