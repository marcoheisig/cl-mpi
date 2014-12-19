(defpackage :mpi-testsuite
  (:use #:cl #:mpi #:5am #:uiop))

(in-package :mpi-testsuite)

(5am:def-suite mpi-testsuite :description "All MPI related tests.")

(5am:in-suite mpi-testsuite)

(5am:test (mpi-init)
  "MPI Initialization"
  (mpi-init)
  (is (mpi-initialized) "failed to initialize MPI (or mpi-initialized is broken)"))

(5am:test (error-handling :depends-on mpi-init)
  "Test whether MPI errors are properly handled"
  (5am:is (= 0 (cffi:foreign-enum-value 'MPI::mpi_error :MPI_SUCCESS))))

(5am:test (size-and-rank :depends-on mpi-init)
  "Checking whether it is possible to determine size and rank"
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (is (> size 0) "Invalid size of MPI_COMM_WORLD")
    (is (> size rank -1) "Invalid MPI rank")))

(5am:test (parallel :depends-on size-and-rank)
  "Is there more than one MPI process"
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (is (> size 1) "More than one process is required for most MPI tests")
    (unless (zerop rank) ;; discard the output of all but one MPI process
      (setf *test-dribble* (make-broadcast-stream)))))

(5am:test (mpi-ring :depends-on parallel)
  "Send on message through all nodes"
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size)))
    (let ((left-neighbor  (mod (- rank 1) size))
          (right-neighbor (mod (+ rank 1) size)))
      (cond ((= 0 rank)
             (mpi:mpi-send '(3 different "elements") right-neighbor)
             (is (equalp '(3 different "elements") (mpi:mpi-receive :source left-neighbor))))
            (t
             (mpi:mpi-send
              (mpi:mpi-receive :source left-neighbor)
              right-neighbor))))))
