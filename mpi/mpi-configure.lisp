;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

(in-package :mpi)

;;;; The MPI "standard" makes no specification of how to compile, link and run
;;;; a MPI application. This file contains magic to provide the MPI
;;;; functionality regardless of the implementation being used. Feel free to
;;;; contact me if your favourite MPI implementation (or the one you have to
;;;; use) does not work out of the box.

(declaim (type (member :openmpi :mpich :mpich2 :unknown) *mpi-implementation*))
(defvar *mpi-implementation* :unknown)

(defun configure-mpi ()
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
