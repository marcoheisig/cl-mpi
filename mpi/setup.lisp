#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

Definition of fundamental MPI constants and types.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error Handling
;;;
;;; (Almost) all MPI functions return a status code as an integer. In
;;; cl-mpi, the user never sees the status code at all. It is automatically
;;; checked and converted to a condition.

(define-condition mpi-error-condition (error)
  ((%error-code :initarg :error-code :reader error-code))
  (:report
   (lambda (c stream)
     (princ (mpi-error-string (error-code c))
            stream)))
  (:documentation
   "Signaled when a MPI function returns a status code other than MPI_SUCCESS."))

(defun signal-mpi-error (value)
  (cerror "Ignore the error."
          'mpi-error-condition :error-code value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version Information

(define-constant +mpi-version+
    (format nil "~d.~d" |MPI_VERSION| |MPI_SUBVERSION|)
  :test #'string-equal
  :documentation
  "The version of the MPI standard supported by the underlying implementation.")

(define-constant +mpi-library+
    (cond
      ((boundp '|MPICH|)
       (format nil "MPICH ~D" (symbol-value '|MPICH_VERSION|)))
      ((boundp '|MPICH2|)
       (format nil "MPICH2 ~D" (symbol-value '|MPICH2_VERSION|)))
      ((boundp '|OPEN_MPI|)
       (format nil "Open MPI ~D.~D.~D"
               (symbol-value '|OPEN_MPI_MAJOR_VERSION|)
               (symbol-value '|OPEN_MPI_MINOR_VERSION|)
               (symbol-value '|OPEN_MPI_RELEASE_VERSION|)))
      (t "Unkown"))
  :test #'string-equal
  :documentation
  "A string describing the MPI library that CL-MPI uses to send its
  messages. Something like \"Open MPI 1.6.2\".")

(defmacro since-mpi-version (version &body body)
  (when (version<= version +mpi-version+)
    `(progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Handling of Foreign Types
;;;
;;; There is a plethora of types in MPI. We represent them as a subclass of
;;; MPI-OBJECT and provide appropriate CFFI wrapper types of the form
;;; <CLASSNAME>-type.

(defclass mpi-object ()
  ((%handle :accessor mpi-object-handle :initarg :handle)))

(defclass mpi-errhandler (mpi-object) ())

(defclass mpi-comm (mpi-object) ())

(defclass mpi-group (mpi-object) ())

(defclass mpi-datatype (mpi-object) ())

(defclass mpi-op (mpi-object) ())

(defclass mpi-info (mpi-object) ())

(defclass mpi-request (mpi-object) ())

;;; some deftypes for the most common types handled by MPI
(deftype int () '(signed-byte 32))

(deftype index () '(or null (integer 0 #.array-total-size-limit)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant foreign-mpi-object-type
      (if (boundp '|OPEN_MPI|) :pointer :int)))

(define-foreign-type mpi-object-type ()
  () (:actual-type
      ;; The MPI standard does not prescribe the C type of several of its
      ;; objects.  In practice, this means that OpenMPI represents its
      ;; objects as pointers to structs, while MPICH and its derivatives
      ;; represent them with ints.
      #.foreign-mpi-object-type))

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

(defmethod expand-from-foreign (value (type mpi-error-type))
  (let ((return-value (gensym)))
    `(let ((,return-value ,value))
       (unless (zerop ,return-value)
         (signal-mpi-error ,return-value))
       (values))))

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
    `(make-instance ',instance-type :handle ,value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessing MPI constants.

;; Each entry in this list is of the form (object c-name reader)
(defvar *mpi-constant-table* '())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((reader-symbol (c-name)
           (intern
            (concatenate 'string "cl_mpi_get_" c-name)))
         (lisp-symbol (c-name)
           (intern
            (format nil "+~A+" (substitute #\- #\_ (string-upcase c-name))))))
    (defmacro define-mpi-constant (class c-name)
      `(defvar ,(lisp-symbol c-name)
         (let ((object (make-instance ',class)))
           (pushnew (list object ',c-name ',(reader-symbol c-name))
                    *mpi-constant-table*
                    :test #'string= :key #'second)
           object)))))

(define-mpi-constant mpi-errhandler "MPI_ERRORS_RETURN")
(define-mpi-constant mpi-errhandler "MPI_ERRORS_ARE_FATAL")
(define-mpi-constant mpi-errhandler "MPI_ERRHANDLER_NULL")
(define-mpi-constant mpi-group "MPI_GROUP_EMPTY")
(define-mpi-constant mpi-group "MPI_GROUP_NULL")
(define-mpi-constant mpi-comm "MPI_COMM_WORLD")
(define-mpi-constant mpi-comm "MPI_COMM_SELF")
(define-mpi-constant mpi-comm "MPI_COMM_NULL")
(define-mpi-constant mpi-datatype "MPI_DATATYPE_NULL")
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
(since-mpi-version "2.2"
  (define-mpi-constant mpi-datatype "MPI_INT8_T")
  (define-mpi-constant mpi-datatype "MPI_INT16_T")
  (define-mpi-constant mpi-datatype "MPI_INT32_T")
  (define-mpi-constant mpi-datatype "MPI_INT64_T")
  (define-mpi-constant mpi-datatype "MPI_UINT8_T")
  (define-mpi-constant mpi-datatype "MPI_UINT16_T")
  (define-mpi-constant mpi-datatype "MPI_UINT32_T")
  (define-mpi-constant mpi-datatype "MPI_UINT64_T"))
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
(define-mpi-constant mpi-op "MPI_OP_NULL")
(define-mpi-constant mpi-request "MPI_REQUEST_NULL")

(declaim (type mpi-comm *standard-communicator*))
(defvar *standard-communicator* +mpi-comm-world+)

(defun initialize-mpi-constants ()
  (loop for (object nil reader-name) in *mpi-constant-table*
        do (setf (mpi-object-handle object)
                 (funcall reader-name))))
