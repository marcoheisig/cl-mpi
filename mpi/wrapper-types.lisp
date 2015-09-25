#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

CLOS wrappers for all MPI handles

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


(in-package #:cl-mpi)

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

(defclass mpi-object ()
  ((name :type string
         :reader name
         :initarg :name
         :initform "")
   (%handle
    :accessor mpi-object-handle
    :initarg :handle
    :type #.+mpi-object-handle-type+)))

(defclass mpi-errhandler (mpi-object) ())

(defclass mpi-comm (mpi-object) ())

(defclass mpi-group (mpi-object) ())

(defclass mpi-datatype (mpi-object) ())

(defclass mpi-op (mpi-object) ())

(defclass mpi-info (mpi-object) ())

(defclass mpi-request (mpi-object) ())

(defun foreign-mpi-value (name class)
  "Given the name of a constant as in the MPI standard and a symbol
designating the desired class of that symbol, return the implementation
dependent handle to that MPI constant."
  (declare (ignorable class))
  #.(case +mpi-implementation+
      (:openmpi
       `(let* ((basename (subseq name 4)) ; drop the "MPI_" prefix
               (prefix
                 (case class
                   ((mpi-request) "ompi_")
                   ((mpi-op) "ompi_mpi_op_")
                   (otherwise "ompi_mpi_")))
               (openmpi-name
                 (concatenate 'string prefix (string-downcase basename)))
               (found
                 (foreign-symbol-pointer openmpi-name)))
          (unless found
            (error "MPI symbol ~A could not be found" openmpi-name))
          found))
      (t
       `(symbol-value
         (find-symbol name '#:cl-mpi-header)))))

(defmethod make-load-form ((object mpi-object) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-of object)
                  :name ,(name object)
                  :handle (foreign-mpi-value ,(name object)
                                             ',(class-name (class-of object)))))

(defmethod expand-to-foreign (value (type mpi-object-type))
  `(mpi-object-handle ,value))

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

(defun mpi-object= (a b)
  #.(case +mpi-implementation+
      (:openmpi
       `(pointer-eq (mpi-object-handle a)
                    (mpi-object-handle b)))
      (t
        `(eql (mpi-object-handle a)
              (mpi-object-handle b)))))


