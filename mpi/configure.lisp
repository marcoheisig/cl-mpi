;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

(in-package #:mpi)

(defconstant +mpi-implementation+
  (if (boundp '+mpi-implementation+)
      +mpi-implementation+
      (cond
        ((boundp 'mpi-header::|OPEN_MPI|) :openmpi)
        ((boundp 'mpi-header::|MPICH|) :mpich)
        ((boundp 'mpi-header::|MPICH2|) :mpich2)
        (t :unknown))))

(defconstant +mpi-version+
  (if (boundp '+mpi-version+)
      +mpi-version+
      (format nil "~D.~D"
              mpi-header::|MPI_VERSION|
              mpi-header::|MPI_SUBVERSION|)))

(defconstant +mpi-implementation-version+ :unknown)

(defconstant +mpi-max-error-string+ mpi-header::|MPI_MAX_ERROR_STRING|)
(defconstant +mpi-max-processor-name+ mpi-header::|MPI_MAX_PROCESSOR_NAME|)
(defconstant +mpi-any-tag+ mpi-header::|MPI_ANY_TAG|)
(defconstant +mpi-any-source+ mpi-header::|MPI_ANY_SOURCE|)
(defconstant +mpi-proc-null+ mpi-header::|MPI_PROC_NULL|)
(defconstant +mpi-root+ mpi-header::|MPI_ROOT|)
(defconstant +mpi-undefined+ mpi-header::|MPI_UNDEFINED|)

(defconstant +mpi-status-ignore+
  (if (boundp '+mpi-status-ignore+)
      (symbol-value '+mpi-status-ignore+)
      (make-pointer mpi-header::|MPI_STATUS_IGNORE|)))
