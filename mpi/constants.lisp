#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

Lisp-accessible constants from mpi.h

Copyright (c) 2008,2009  Alex Fukunaga
Copyright (C) 2014,2015  Marco Heisig <marco.heisig@fau.de>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package #:cl-mpi)

(defvar *mpi-constants* ())

;;; Despite the naming scheme (e.g. +MPI-COMM-WORLD+), MPI constants are not
;;; Lisp constants, because the underlying handles might be pointers to the
;;; heap and need to be updated after dynamic linkage and each restart of a
;;; lisp image.
(defmacro define-mpi-constant (type mpi-name)
  (let ((lispname
          (intern
           (format nil "+~A+" (substitute #\- #\_ mpi-name)))))
    `(progn
       (defvar ,lispname
         (make-instance ',type :name ,mpi-name))
       (push ,lispname *mpi-constants*))))

(define-mpi-constant mpi-errhandler "MPI_ERRORS_RETURN")
(define-mpi-constant mpi-errhandler "MPI_ERRORS_ARE_FATAL")
(define-mpi-constant mpi-group "MPI_GROUP_EMPTY")
(define-mpi-constant mpi-group "MPI_GROUP_NULL")
(define-mpi-constant mpi-comm "MPI_COMM_WORLD")
(define-mpi-constant mpi-comm "MPI_COMM_SELF")
(define-mpi-constant mpi-comm "MPI_COMM_NULL")
(define-mpi-constant mpi-datatype "MPI_DATATYPE_NULL")
(define-mpi-constant mpi-datatype "MPI_LB")
(define-mpi-constant mpi-datatype "MPI_UB")
(define-mpi-constant mpi-datatype "MPI_CHAR")
(define-mpi-constant mpi-datatype "MPI_SIGNED_CHAR")
(define-mpi-constant mpi-datatype "MPI_UNSIGNED_CHAR")
(define-mpi-constant mpi-datatype "MPI_BYTE")
(define-mpi-constant mpi-datatype "MPI_SHORT")
(define-mpi-constant mpi-datatype "MPI_UNSIGNED_SHORT")
(define-mpi-constant mpi-datatype "MPI_INT")
(define-mpi-constant mpi-datatype "MPI_UNSIGNED")
(define-mpi-constant mpi-datatype "MPI_LONG")
(define-mpi-constant mpi-datatype "MPI_UNSIGNED_LONG")
(define-mpi-constant mpi-datatype "MPI_LONG_LONG_INT")
(define-mpi-constant mpi-datatype "MPI_UNSIGNED_LONG_LONG")
(define-mpi-constant mpi-datatype "MPI_FLOAT")
(define-mpi-constant mpi-datatype "MPI_DOUBLE")
(define-mpi-constant mpi-datatype "MPI_LONG_DOUBLE")
(define-mpi-constant mpi-datatype "MPI_WCHAR")
(define-mpi-constant mpi-datatype "MPI_C_BOOL")
(define-mpi-constant mpi-datatype "MPI_INT8_T")
(define-mpi-constant mpi-datatype "MPI_INT16_T")
(define-mpi-constant mpi-datatype "MPI_INT32_T")
(define-mpi-constant mpi-datatype "MPI_INT64_T")
(define-mpi-constant mpi-datatype "MPI_UINT8_T")
(define-mpi-constant mpi-datatype "MPI_UINT16_T")
(define-mpi-constant mpi-datatype "MPI_UINT32_T")
(define-mpi-constant mpi-datatype "MPI_UINT64_T")
(define-mpi-constant mpi-datatype "MPI_PACKED")
(define-mpi-constant mpi-op "MPI_MIN")
(define-mpi-constant mpi-op "MPI_MAX")
(define-mpi-constant mpi-op "MPI_SUM")
(define-mpi-constant mpi-op "MPI_PROD")
(define-mpi-constant mpi-op "MPI_LAND")
(define-mpi-constant mpi-op "MPI_BAND")
(define-mpi-constant mpi-op "MPI_LOR")
(define-mpi-constant mpi-op "MPI_BOR")
(define-mpi-constant mpi-op "MPI_LXOR")
(define-mpi-constant mpi-op "MPI_BXOR")
(define-mpi-constant mpi-op "MPI_MAXLOC")
(define-mpi-constant mpi-op "MPI_MINLOC")
(define-mpi-constant mpi-op "MPI_REPLACE")
;; (define-mpi-constant mpi-op "MPI_NO_OP") ;; causing trouble in openmpi-1.6
(define-mpi-constant mpi-request "MPI_REQUEST_NULL")

(declaim (type mpi-comm *standard-communicator*))
(defvar *standard-communicator* +mpi-comm-world+)

(defun mpi-null-p (object)
  (mpi-object=
   object
   (typecase object
     (mpi-comm +mpi-comm-null+)
     (mpi-group +mpi-group-null+)
     (mpi-datatype +mpi-datatype-null+)
     (mpi-request +mpi-request-null+)
     (t nil))))
