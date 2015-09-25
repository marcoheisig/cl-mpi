;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

(in-package #:cl-mpi)

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
    (let* ((maj (symbol-value (find-symbol "OMPI_MAJOR_VERSION" '#:cl-mpi-header)))
           (min (symbol-value (find-symbol "OMPI_MINOR_VERSION" '#:cl-mpi-header))))
      (if (and maj min)
          (format nil "~D.~D" maj min)
          :unknown)))

  (defun determine-mpich-version ()
    (let ((numversion (symbol-value (find-symbol "MPICH_NUMVERSION" '#:cl-mpi-header))))
      (if numversion
          (numversion-to-version numversion)
          :unknown)))

  (defun determine-mpich2-version ()
    (let* ((numversion
             (or (symbol-value (find-symbol "MPICH_NUMVERSION" '#:cl-mpi-header))
                 (symbol-value (find-symbol "MPICH2_NUMVERSION" '#:cl-mpi-header)))))
      (if numversion
          (numversion-to-version numversion)
          :unknown)))

  (defun configure ()
    (macrolet ((def (name value)
                 `(defconstant ,name
                    (if (boundp ',name)
                        (symbol-value ',name)
                        ,value))))
      (cond
        ((boundp 'cl-mpi-header::|OPEN_MPI|)
         (defconstant +mpi-implementation+ :openmpi)
         (def +mpi-implementation-version+ (determine-openmpi-version))
         (def +mpi-object-handle-type+ 'foreign-pointer)
         (def +mpi-object-handle-cffi-type+ :pointer))
        ((boundp 'cl-mpi-header::|MPICH|)
         (defconstant +mpi-implementation+ :mpich)
         (def +mpi-implementation-version+ (determine-mpich-version))
         (def +mpi-object-handle-type+ '(signed-byte 32))
         (def +mpi-object-handle-cffi-type+ :int))
        ((boundp 'cl-mpi-header::|MPICH2|)
         (defconstant +mpi-implementation+ :mpich2)
         (def +mpi-implementation-version+ (determine-mpich2-version))
         (def +mpi-object-handle-type+ '(signed-byte 32))
         (def +mpi-object-handle-cffi-type+ :int))
        (t
         (defconstant +mpi-implementation+ :unknown)
         (def +mpi-implementation-version+ :unknown)
         (def +mpi-object-handle-type+ '(signed-byte 32))
         (def +mpi-object-handle-cffi-type+ :int)))))

  (configure))

(defconstant +mpi-version+
  (if (boundp '+mpi-version+)
      +mpi-version+
      (format nil "~D.~D"
              cl-mpi-header::|MPI_VERSION|
              cl-mpi-header::|MPI_SUBVERSION|)))

(defconstant +mpi-max-error-string+ cl-mpi-header::|MPI_MAX_ERROR_STRING|)
(defconstant +mpi-max-processor-name+ cl-mpi-header::|MPI_MAX_PROCESSOR_NAME|)
(defconstant +mpi-any-tag+ cl-mpi-header::|MPI_ANY_TAG|)
(defconstant +mpi-any-source+ cl-mpi-header::|MPI_ANY_SOURCE|)
(defconstant +mpi-proc-null+ cl-mpi-header::|MPI_PROC_NULL|)
(defconstant +mpi-root+ cl-mpi-header::|MPI_ROOT|)
(defconstant +mpi-undefined+ cl-mpi-header::|MPI_UNDEFINED|)

(defconstant +mpi-status-ignore+
  (if (boundp '+mpi-status-ignore+)
      (symbol-value '+mpi-status-ignore+)
      (make-pointer cl-mpi-header::|MPI_STATUS_IGNORE|)))
