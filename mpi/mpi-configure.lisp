;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

(in-package :mpi)

(defun detect-mpi-implementation ()
  (cond
    ((boundp 'mpi-header::OPEN_MPI) :openmpi)
    ((boundp 'mpi-header::MPICH) :mpich)
    ((boundp 'mpi-header::MPICH2) :mpich2)
    (t :unknown)))

(pushnew (detect-mpi-implementation) *features*)
