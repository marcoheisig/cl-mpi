#!/bin/sh
":" ; exec cl-launch -Q -s cl-mpi-extensions -E main -X -- "$0" "$@" || exit || echo " -*- mode: Lisp -*- "

;;; A demonstration of the transmit-anything extension of cl-mpi

(defpackage :cl-mpi/examples/send-anything
  (:use :cl :alexandria :cl-mpi :static-vectors :cl-mpi-extensions))
(in-package :cl-mpi/examples/send-anything)

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
         (neighbors (list (mod (- rank 1) size)
                          (mod (+ rank 1) size)))
         (objects '(:banana :potato :apple)))
    (loop for neighbor in neighbors
          and send-tag from 1
          and recv-tag downfrom (length neighbors)
          collect
          (let ((object (random-elt objects)))
            (mpi-isend-anything
             (list
              (format nil "Greetings from rank ~d, have a ~a." rank object)
              object)
             neighbor
             :tag send-tag))
            into send-requests
          collect
          (mpi-irecv-anything neighbor :tag recv-tag)
            into recv-requests
          finally
             (let ((results (apply #'mpi-waitall-anything
                                   (append send-requests recv-requests))))
               (loop for (source tag (message object)) in results do
                 (printf "from ~d: ~a~%(attachements: ~s)~%" source message object)))))
  (mpi-finalize)
  (uiop:quit))
