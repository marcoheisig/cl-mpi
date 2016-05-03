(in-package :cl-mpi-benchmarks)

(defun main (&optional args)
  (run-benchmarks))

(defun run-benchmarks ()
  (mpi-init)
  (when (= 1 (mpi-comm-size))
    (format *error-output* "The MPI Benchmarks can only be run in parallel.")
    (return-from run-benchmarks nil))
  (let ((buf (make-static-vector
              (expt 2 25)
              :element-type '(unsigned-byte 8)
              :initial-element 0)))
    (dolist (msg-size (loop for x from 0 to 25 collect (expt 2 x)))
      (let ((iterations (min (/ (expt 2 29) msg-size) 100000)))
        (pingpong msg-size buf iterations)))
    (free-static-vector buf)))
