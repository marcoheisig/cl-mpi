#!/bin/sh
":" ; exec cl-launch -Q -s cl-mpi-extensions -E main -X -- "$0" "$@" || exit || echo " -*- mode: Lisp -*- "

;;; A demonstration of the transmit-anything extension of cl-mpi
;;;
;;; Every rank greets his left and right neighbor by sending a list of a
;;; string and an object. In the end, each process displays the result of
;;; MPI-WAITALL-ANYTHING.

(defpackage :cl-mpi/examples/transmit-anything
  (:use :cl :alexandria :cl-mpi :static-vectors :cl-mpi-extensions))
(in-package :cl-mpi/examples/transmit-anything)

(defun printf (fmt &rest args)
  (format t "rank ~d: " (mpi-comm-rank))
  (apply #'format t fmt args)
  (finish-output))

(defun cl-user::main (&optional arg)
  (declare (ignorable arg))
  (mpi-init)
  (let* ((*random-state* (make-random-state))
         (rank (mpi-comm-rank))
         (size (mpi-comm-size))
         (left-neighbor (mod (- rank 1) size))
         (right-neighbor (mod (+ rank 1) size))
         (object (random-elt '(:banana :potato :apple))))
    (printf
     "~s~%"
     (mpi-waitall-anything
      (mpi-irecv-anything right-neighbor :tag 1)
      (mpi-irecv-anything left-neighbor :tag 2)
      (mpi-isend-anything
       `(,(format nil "Greetings left neighbor! Have a ~a." object) ,object)
       left-neighbor :tag 2)
      (mpi-isend-anything
       `(,(format nil "Greetings right neighbor! Have a ~a." object) ,object)
       right-neighbor :tag 1))))
  (mpi-finalize)
  (uiop:quit))
