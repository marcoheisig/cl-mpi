;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

(in-package :mpi)

;;;; The MPI "standard" makes no specification of how to compile, link and run
;;;; a MPI application. This file contains magic to provide the MPI
;;;; functionality regardless of the implementation being used. Feel free to
;;;; contact me if your favourite MPI implementation (or the one you have to
;;;; use) does not work out of the box.

(defun detect-mpi-libraries ()
  "guess all libraries that must be loaded in order to use mpi"
  (restart-case
      (multiple-value-bind (flags err-msg exit-code)
          (uiop:run-program "mpicc -show" :output :string)
        (unless (zerop exit-code) (error err-msg))
        (let ((libraries ()))
          (ppcre:do-scans (match-start match-end reg-starts reg-ends "\\s-l(\\S+)" flags)
            (setf libraries
                  (cons (format nil "lib~A.so"
                                (subseq flags (aref reg-starts 0) (aref reg-ends 0)))
                        libraries)))
          (unless libraries (error "no suitable MPI libraries found"))
          libraries))
    (specify-libraries ()
      :report "Specify the MPI libraries manually, something like (\"libmpi.so\")"
      :interactive read)))

(defvar *mpi-libraries* nil)

(declaim (type (member :openmpi :mpich :mpich2 :unknown) *mpi-implementation*))
(defvar *mpi-implementation* :unknown)

(defun configure-mpi ()
  (setf *mpi-libraries* (detect-mpi-libraries))
  (mapcar #'load-foreign-library *mpi-libraries*)
  (cond
    ;; openmpi
    ((or (boundp 'mpi-header::OPEN_MPI)
         (boundp 'mpi-header::OMPI_MAJOR_VERSION))
     (pushnew :openmpi *features*)
     (setf *mpi-implementation* :openmpi))
    ;; mpich
    ((boundp 'mpi-header::MPICH)
     (pushnew :mpich *features*)
     (setf *mpi-implementation* :mpich))
    ;; mpich2
    ((boundp 'mpi-header::MPICH2)
     (pushnew :mpich2 *features*)
     (setf *mpi-implementation* :mpich2))
    )
  (pushnew :mpi *features*))

(configure-mpi)
