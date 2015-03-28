#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI variables

Copyright (C) 2015  Marco Heisig <marco.heisig@fau.de>

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

(in-package :mpi)

(defconstant +mpi-version+
  (if (boundp '+mpi-version+)
      +mpi-version+
      (format nil "~D.~D"
              mpi-header::MPI_VERSION
              mpi-header::MPI_SUBVERSION)))

(defconstant +mpi-max-error-string+ mpi-header::MPI_MAX_ERROR_STRING)
(defconstant +mpi-max-processor-name+ mpi-header::MPI_MAX_PROCESSOR_NAME)
(defconstant +mpi-any-tag+ mpi-header::MPI_ANY_TAG)
(defconstant +mpi-any-source+ mpi-header::MPI_ANY_SOURCE)

(defvar *mpi-status-ignore* (make-pointer mpi-header::MPI_STATUS_IGNORE))

(defmacro define-mpi-object (type mpi-name)
  (let ((basename (subseq (symbol-name mpi-name) 4))
        (lispname (translate-name-from-foreign
                   (symbol-name mpi-name)
                   (find-package :mpi-header) t)))
    `(progn
       (declaim (type ,type ,lispname))
       (defvar ,lispname (make-instance ',type :name ,basename)))))

(define-mpi-object mpi-errhandler MPI_ERRORS_RETURN)
(define-mpi-object mpi-errhandler MPI_ERRORS_ARE_FATAL)
(define-mpi-object mpi-group MPI_GROUP_EMPTY)
(define-mpi-object mpi-group MPI_GROUP_NULL)
(define-mpi-object mpi-comm MPI_COMM_WORLD)
(define-mpi-object mpi-comm MPI_COMM_SELF)
(define-mpi-object mpi-comm MPI_COMM_NULL)
(define-mpi-object mpi-datatype MPI_DATATYPE_NULL)
(define-mpi-object mpi-datatype MPI_LB)
(define-mpi-object mpi-datatype MPI_UB)
(define-mpi-object mpi-datatype MPI_CHAR)
(define-mpi-object mpi-datatype MPI_SIGNED_CHAR)
(define-mpi-object mpi-datatype MPI_UNSIGNED_CHAR)
(define-mpi-object mpi-datatype MPI_BYTE)
(define-mpi-object mpi-datatype MPI_SHORT)
(define-mpi-object mpi-datatype MPI_UNSIGNED_SHORT)
(define-mpi-object mpi-datatype MPI_INT)
(define-mpi-object mpi-datatype MPI_UNSIGNED)
(define-mpi-object mpi-datatype MPI_LONG)
(define-mpi-object mpi-datatype MPI_UNSIGNED_LONG)
(define-mpi-object mpi-datatype MPI_LONG_LONG_INT)
(define-mpi-object mpi-datatype MPI_UNSIGNED_LONG_LONG)
(define-mpi-object mpi-datatype MPI_FLOAT)
(define-mpi-object mpi-datatype MPI_DOUBLE)
(define-mpi-object mpi-datatype MPI_LONG_DOUBLE)
(define-mpi-object mpi-datatype MPI_WCHAR)
(define-mpi-object mpi-datatype MPI_C_BOOL)
(define-mpi-object mpi-datatype MPI_INT8_T)
(define-mpi-object mpi-datatype MPI_INT16_T)
(define-mpi-object mpi-datatype MPI_INT32_T)
(define-mpi-object mpi-datatype MPI_INT64_T)
(define-mpi-object mpi-datatype MPI_UINT8_T)
(define-mpi-object mpi-datatype MPI_UINT16_T)
(define-mpi-object mpi-datatype MPI_UINT32_T)
(define-mpi-object mpi-datatype MPI_UINT64_T)
(define-mpi-object mpi-datatype MPI_PACKED)
(define-mpi-object mpi-op MPI_NULL)
(define-mpi-object mpi-op MPI_MIN)
(define-mpi-object mpi-op MPI_MAX)
(define-mpi-object mpi-op MPI_SUM)
(define-mpi-object mpi-op MPI_PROD)
(define-mpi-object mpi-op MPI_LAND)
(define-mpi-object mpi-op MPI_BAND)
(define-mpi-object mpi-op MPI_LOR)
(define-mpi-object mpi-op MPI_BOR)
(define-mpi-object mpi-op MPI_LXOR)
(define-mpi-object mpi-op MPI_BXOR)
(define-mpi-object mpi-op MPI_MAXLOC)
(define-mpi-object mpi-op MPI_MINLOC)
(define-mpi-object mpi-op MPI_REPLACE)
(define-mpi-object mpi-op MPI_NO_OP)

(declaim (inline cffi-type-to-mpi-type))
(defun cffi-type-to-mpi-type (cffi-type)
  "Convert :int to MPI_INT and so on"
  (declare (type keyword cffi-type))
  (ecase cffi-type
    ((:char) *mpi-char*)
    ((:uchar :unsigned-char) *mpi-unsigned-char*)
    ((:short) *mpi-short*)
    ((:ushort :unsigned-short) *mpi-unsigned-short*)
    ((:int) *mpi-int*)
    ((:uint :unsigned-int) *mpi-unsigned*)
    ((:long) *mpi-long*)
    ((:ulong :unsigned-long) *mpi-unsigned-long*)
    ((:llong :long-long) *mpi-long-long-int*)
    ((:ullong :unsigned-long-long) *mpi-unsigned-long-long*)
    ((:float) *mpi-float*)
    ((:double) *mpi-double*)
    ((:long-double) *mpi-long-double*)))
