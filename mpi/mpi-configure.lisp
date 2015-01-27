;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

(in-package :mpi)

;;;; The MPI "standard" makes no specification of how to compile, link and run
;;;; a MPI application. This file contains magic to provide the MPI
;;;; functionality regardless of the implementation being used. Feel free to
;;;; contact me if your favourite MPI implementation (or the one you have to
;;;; use) does not work out of the box.

(defvar *mpi-header* "mpi.h")

(defvar *mpi-libraries* '())

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
      :report "Specify the MPI libraries manually, something like '(\"libmpi.so\")"
      :interactive read)))

(defun configure-mpi ()
  (setf cffi-grovel::*cc* "mpicc")
  (setf *mpi-libraries* (detect-mpi-libraries))
  (mapcar #'cffi:load-foreign-library *mpi-libraries*)
  (if (cffi:foreign-symbol-pointer "ompi_mpi_comm_world")
      (configure-mpi-openmpi)))

(defun configure-mpi-openmpi ()
  (setf cffi-grovel::*cc* "mpicc")
  (mapcar #'cffi:load-foreign-library *mpi-libraries*)
  (macrolet ((defptr (name c_name)
               `(defvar ,name (cffi:foreign-symbol-pointer ,c_name))))

    (cffi:defctype MPI_Errhandler :pointer)
    (defptr MPI_ERRORS_RETURN "ompi_mpi_errors_return")
    (defptr MPI_ERRORS_ARE_FATAL "ompi_mpi_errors_are_fatal")

    (cffi:defctype MPI_Comm :pointer)
    (defptr MPI_COMM_WORLD "ompi_mpi_comm_world")
    (defptr MPI_COMM_SELF  "ompi_mpi_comm_self")
    (defptr MPI_COMM_NULL  "ompi_mpi_comm_null")

    (cffi:defctype MPI_Group :pointer)
    (defptr MPI_GROUP_EMPTY "ompi_mpi_group_empty")
    (defptr MPI_GROUP_NULL  "ompi_mpi_group_null")

    (cffi:defctype MPI_Datatype :pointer)
    (defptr MPI_DATATYPE_NULL "ompi_mpi_datatype_null")
    (defptr MPI_LB "ompi_mpi_lb")
    (defptr MPI_UB "ompi_mpi_ub")
    (defptr MPI_CHAR "ompi_mpi_char")
    (defptr MPI_SIGNED_CHAR "ompi_mpi_signed_char")
    (defptr MPI_UNSIGNED_CHAR "ompi_mpi_unsigned_char")
    (defptr MPI_BYTE "ompi_mpi_byte")
    (defptr MPI_SHORT "ompi_mpi_short")
    (defptr MPI_UNSIGNED_SHORT "ompi_mpi_unsigned_short")
    (defptr MPI_INT "ompi_mpi_int")
    (defptr MPI_UNSIGNED "ompi_mpi_unsigned")
    (defptr MPI_LONG "ompi_mpi_long")
    (defptr MPI_UNSIGNED_LONG "ompi_mpi_unsigned_long")
    (defptr MPI_LONG_LONG_INT "ompi_mpi_long_long_int")
    (defptr MPI_UNSIGNED_LONG_LONG "ompi_mpi_unsigned_long_long")
    (defptr MPI_FLOAT "ompi_mpi_float")
    (defptr MPI_DOUBLE "ompi_mpi_double")
    (defptr MPI_LONG_DOUBLE "ompi_mpi_long_double")
    (defptr MPI_WCHAR "ompi_mpi_wchar")
    (defptr MPI_PACKED "ompi_mpi_packed")

    (cffi:defctype MPI_Op :pointer)
    (defptr MPI_NULL "ompi_mpi_op_null")
    (defptr MPI_MIN "ompi_mpi_op_min")
    (defptr MPI_MAX "ompi_mpi_op_max")
    (defptr MPI_SUM "ompi_mpi_op_sum")
    (defptr MPI_PROD "ompi_mpi_op_prod")
    (defptr MPI_LAND "ompi_mpi_op_land")
    (defptr MPI_BAND "ompi_mpi_op_band")
    (defptr MPI_LOR "ompi_mpi_op_lor")
    (defptr MPI_BOR "ompi_mpi_op_bor")
    (defptr MPI_LXOR "ompi_mpi_op_lxor")
    (defptr MPI_BXOR "ompi_mpi_op_bxor")
    (defptr MPI_MAXLOC "ompi_mpi_op_maxloc")
    (defptr MPI_MINLOC "ompi_mpi_op_minloc")
    (defptr MPI_REPLACE "ompi_mpi_op_replace")
    (defptr MPI_NO_OP "ompi_mpi_op_no_op")))

(configure-mpi)
