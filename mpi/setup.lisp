#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI setup - definition of CFFI and CLOS wrapper types and methods

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

(in-package :cl-mpi)

(defvar +mpi-version+
  (format nil "~d.~d" |MPI_VERSION| |MPI_SUBVERSION|))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((boundp '|OPEN_MPI|)
     (defconstant +mpi-object-handle-cffi-type+ :pointer))
    (t
     (defconstant +mpi-object-handle-cffi-type+ :int))))

(defun lisp-constant-accessor-name (symbol)
  "Translate the symbol SYMBOL to a string denoting its C language accessor function.

Example: +mpi-comm-world+ -> cl_mpi_get_MPI-COMM-WORLD"
  (let ((name (symbol-name symbol)))
    (concatenate
     'string "cl_mpi_get_"
     (string-upcase
      (substitute
       #\_ #\-
       (subseq name 1 (- (length name) 1)))))))

;;; (Almost) all MPI functions return a status code as an integer. In cl-mpi,
;;; the user never sees the status code at all. It is automatically checked
;;; and converted to a condition.
(define-condition mpi-error-condition (error)
  ((error-code :initarg :error-code :reader error-code))
  (:report
   (lambda (c stream)
     (princ (mpi-error-string (error-code c))
            stream)))
  (:documentation "Signalled when a MPI function returns a code other than MPI_SUCCESS."))

(defun signal-mpi-error (value)
  (restart-case
      (error 'mpi-error-condition :error-code value)
    (ignore () nil)))

;;; There is a plethora of types in MPI. We represent them as a subclass of
;;; MPI-OBJECT and provide appropriate CFFI wrapper types of the form
;;; <CLASSNAME>-type.

(defclass mpi-object ()
  ((%handle
    :accessor mpi-object-handle
    :initarg :handle)))

(defclass mpi-errhandler (mpi-object) ())

(defclass mpi-comm (mpi-object) ())

(defclass mpi-group (mpi-object) ())

(defclass mpi-datatype (mpi-object) ())

(defclass mpi-op (mpi-object) ())

(defclass mpi-info (mpi-object) ())

(defclass mpi-request (mpi-object) ())

(define-foreign-type mpi-object-type ()
  () (:actual-type
      #.+mpi-object-handle-cffi-type+))

(define-foreign-type mpi-errhandler-type (mpi-object-type)
  () (:simple-parser mpi-errhandler))

(define-foreign-type mpi-comm-type (mpi-object-type)
  () (:simple-parser mpi-comm))

(define-foreign-type mpi-group-type (mpi-object-type)
  () (:simple-parser mpi-group))

(define-foreign-type mpi-datatype-type (mpi-object-type)
  () (:simple-parser mpi-datatype))

(define-foreign-type mpi-op-type (mpi-object-type)
  () (:simple-parser mpi-op))

(define-foreign-type mpi-info-type (mpi-object-type)
  () (:simple-parser mpi-info))

(define-foreign-type mpi-message-type (mpi-object-type)
  () (:simple-parser mpi-message))

(define-foreign-type mpi-request-type (mpi-object-type)
  () (:simple-parser mpi-request))

(define-foreign-type mpi-error-type ()
  ()
  (:actual-type :int)
  (:simple-parser mpi-error-code))

(defmethod expand-to-foreign (value (type mpi-object-type))
  `(mpi-object-handle ,value))

(defmethod expand-from-foreign (value (type mpi-object-type))
  (make-instance 'mpi-object :handle value))

(defmethod expand-from-foreign (value (type mpi-error-type))
  (let ((return-value (gensym)))
    `(let ((,return-value ,value))
       (unless (zerop ,return-value)
         (signal-mpi-error ,return-value)))))

(defmethod expand-from-foreign (value (type mpi-object-type))
  (let ((instance-type
          (etypecase type
            (mpi-errhandler-type 'mpi-errhandler)
            (mpi-comm-type 'mpi-comm)
            (mpi-group-type 'mpi-group)
            (mpi-datatype-type 'mpi-datatype)
            (mpi-op-type 'mpi-op)
            (mpi-info-type 'mpi-info)
            (mpi-request-type 'mpi-request))))
    `(make-instance
      ',instance-type
      :handle ,value)))

(defvar *mpi-constants* ())

(defmacro mpi-symbol-value (symbol &optional cffi-type)
  (let ((type (or cffi-type +mpi-object-handle-cffi-type+)))
    `(foreign-funcall-pointer
      (foreign-symbol-pointer
       (lisp-constant-accessor-name ,symbol))
      () ,type)))

(defvar +mpi-status-ignore+ nil)

(defvar +mpi-library+ ""
  "A string describing the MPI library that CL-MPI uses to send its
  messages. Something like \"Open MPI 1.6.2\" ")

(defun initialize-mpi-constants ()
  "Initialize (or reinitialize) all objects denoted by the symbols in
  *MPI-CONSTANTS* to their values in the underlying MPI C library."
  (mapc
   (lambda (sym)
     (setf (mpi-object-handle (symbol-value sym))
           (mpi-symbol-value sym)))
   *mpi-constants*)
  ;; These 'constants' are different from the ones above, because they are
  ;; not of type MPI-OBJECT
  (setf +mpi-status-ignore+
        (mpi-symbol-value '+mpi-status-ignore+ :pointer))
  (setf +mpi-library+
        (mpi-symbol-value '+mpi-library+ :string)))

(defmacro define-mpi-constant (class-sym name-sym)
  `(progn
     (defvar ,name-sym
       (make-instance ',class-sym))
     (pushnew ',name-sym *mpi-constants*)))

;;; Despite the naming scheme (e.g. +MPI-COMM-WORLD+), MPI constants are not
;;; Lisp constants, because the underlying handles might be pointers to the
;;; heap and need to be updated after dynamic linkage and each restart of a
;;; lisp image.
(define-mpi-constant mpi-errhandler +mpi-errors-return+)
(define-mpi-constant mpi-errhandler +mpi-errors-are-fatal+)
(define-mpi-constant mpi-group +mpi-group-empty+)
(define-mpi-constant mpi-group +mpi-group-null+)
(define-mpi-constant mpi-comm +mpi-comm-world+)
(define-mpi-constant mpi-comm +mpi-comm-self+)
(define-mpi-constant mpi-comm +mpi-comm-null+)
(define-mpi-constant mpi-datatype +mpi-datatype-null+)
(define-mpi-constant mpi-datatype +mpi-lb+)
(define-mpi-constant mpi-datatype +mpi-ub+)
(define-mpi-constant mpi-datatype +mpi-char+)
(define-mpi-constant mpi-datatype +mpi-signed-char+)
(define-mpi-constant mpi-datatype +mpi-unsigned-char+)
(define-mpi-constant mpi-datatype +mpi-byte+)
(define-mpi-constant mpi-datatype +mpi-short+)
(define-mpi-constant mpi-datatype +mpi-unsigned-short+)
(define-mpi-constant mpi-datatype +mpi-int+)
(define-mpi-constant mpi-datatype +mpi-unsigned+)
(define-mpi-constant mpi-datatype +mpi-long+)
(define-mpi-constant mpi-datatype +mpi-unsigned-long+)
(define-mpi-constant mpi-datatype +mpi-long-long-int+)
(define-mpi-constant mpi-datatype +mpi-unsigned-long-long+)
(define-mpi-constant mpi-datatype +mpi-float+)
(define-mpi-constant mpi-datatype +mpi-double+)
(define-mpi-constant mpi-datatype +mpi-long-double+)
(define-mpi-constant mpi-datatype +mpi-wchar+)
(define-mpi-constant mpi-datatype +mpi-c-bool+)
(define-mpi-constant mpi-datatype +mpi-int8-t+)
(define-mpi-constant mpi-datatype +mpi-int16-t+)
(define-mpi-constant mpi-datatype +mpi-int32-t+)
(define-mpi-constant mpi-datatype +mpi-int64-t+)
(define-mpi-constant mpi-datatype +mpi-uint8-t+)
(define-mpi-constant mpi-datatype +mpi-uint16-t+)
(define-mpi-constant mpi-datatype +mpi-uint32-t+)
(define-mpi-constant mpi-datatype +mpi-uint64-t+)
(define-mpi-constant mpi-datatype +mpi-packed+)
(define-mpi-constant mpi-op +mpi-min+)
(define-mpi-constant mpi-op +mpi-max+)
(define-mpi-constant mpi-op +mpi-sum+)
(define-mpi-constant mpi-op +mpi-prod+)
(define-mpi-constant mpi-op +mpi-land+)
(define-mpi-constant mpi-op +mpi-band+)
(define-mpi-constant mpi-op +mpi-lor+)
(define-mpi-constant mpi-op +mpi-bor+)
(define-mpi-constant mpi-op +mpi-lxor+)
(define-mpi-constant mpi-op +mpi-bxor+)
(define-mpi-constant mpi-op +mpi-maxloc+)
(define-mpi-constant mpi-op +mpi-minloc+)
(define-mpi-constant mpi-op +mpi-replace+)
(define-mpi-constant mpi-request +mpi-request-null+)

(declaim (type mpi-comm *standard-communicator*))
(defvar *standard-communicator* +mpi-comm-world+)
