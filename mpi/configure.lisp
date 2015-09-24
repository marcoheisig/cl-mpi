;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

(in-package #:mpi)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun numversion-to-version (numversion)
    "MPICH and its derivatives use the following scheme to express their
version number:
digit 1: major version
digit 2-3: minor version
digit 4-5: revision
digit 6: ext
digit 7: ext number

This function converts an integer from MPICH scheme to an ASDF conforming
version string."
    (declare (type integer numversion))
    (let* ((numstr (format nil "~D" numversion))
           (maj (read-from-string (subseq numstr 0 1)))
           (min (read-from-string (subseq numstr 1 3)))
           (rev (read-from-string (subseq numstr 3 5)))
           (ext (read-from-string (subseq numstr 5 6)))
           (enr (read-from-string (subseq numstr 6 7))))
      (format nil "~D.~D.~D.~D.~D" maj min rev ext enr)))

  (defun determine-openmpi-version ()
    (let* ((maj (symbol-value (find-symbol "OMPI_MAJOR_VERSION" "MPI-HEADER")))
           (min (symbol-value (find-symbol "OMPI_MINOR_VERSION" "MPI-HEADER"))))
      (if (and maj min)
          (format nil "~D.~D" maj min)
          :unknown)))

  (defun determine-mpich-version ()
    (let ((numversion (symbol-value (find-symbol "MPICH_NUMVERSION" "MPI-HEADER"))))
      (if numversion
          (numversion-to-version numversion)
          :unknown)))

  (defun determine-mpich2-version ()
    (let* ((numversion
             (or (symbol-value (find-symbol "MPICH_NUMVERSION" "MPI-HEADER"))
                 (symbol-value (find-symbol "MPICH2_NUMVERSION" "MPI-HEADER")))))
      (if numversion
          (numversion-to-version numversion)
          :unknown))))

(defconstant +mpi-implementation+
  (if (boundp '+mpi-implementation+)
      +mpi-implementation+
      (cond
        ((boundp 'mpi-header::|OPEN_MPI|)
         (defconstant +mpi-implementation-version+
           (determine-openmpi-version))
         :openmpi)
        ((boundp 'mpi-header::|MPICH|)
         (defconstant +mpi-implementation-version+
           (determine-mpich-version))
         :mpich)
        ((boundp 'mpi-header::|MPICH2|)
         (defconstant +mpi-implementation-version+
           (determine-mpich2-version))
         :mpich2)
        (t
         (defconstant +mpi-implementation-version+ :unknown)
         :unknown))))

(defconstant +mpi-version+
  (if (boundp '+mpi-version+)
      +mpi-version+
      (format nil "~D.~D"
              mpi-header::|MPI_VERSION|
              mpi-header::|MPI_SUBVERSION|)))

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
