(cl:defpackage #:cl-mpi-examples
  (:use :cl)
  (:export #:entry-point))

(cl:in-package #:cl-mpi-examples)

(defun entry-point ()
  (cl-mpi/examples/ring::main))
