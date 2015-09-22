(defpackage :mpi-benchmarks
  (:documentation "benchmark suite for MPI under Lisp")
  (:nicknames #:cl-mpi-benchmarks)
  (:use #:cl #:mpi #:uiop #:cffi :static-vectors)
  (:export run-benchmarks))

(in-package :mpi-benchmarks)

(defun run-benchmarks ()
  (mpi-init)
  (let ((buf (make-static-vector
              (expt 2 25)
              :element-type '(unsigned-byte 8)
              :initial-element 0)))
    (dolist (msg-size (loop for x from 0 to 25 collect (expt 2 x)))
      (let ((iterations (min (/ (expt 2 29) msg-size) 100000)))
        (pingpong msg-size buf iterations)))
    (free-static-vector buf)))
