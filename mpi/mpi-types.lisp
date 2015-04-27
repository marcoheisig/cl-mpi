#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI types for Common Lisp

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

(define-foreign-type mpi-object-type ()
  ()
  (:actual-type
   #+openmpi :pointer #-openmpi :int))

(defmacro define-mpi-type (name)
  (let ((typename
          (intern
           (concatenate 'string (string-upcase (symbol-name name)) "-TYPE")
           :mpi)))
    `(define-foreign-type ,typename (mpi-object-type)
       ()
       (:simple-parser ,name))))

(define-mpi-type mpi-errhandler)
(define-mpi-type mpi-comm)
(define-mpi-type mpi-group)
(define-mpi-type mpi-datatype)
(define-mpi-type mpi-op)
(define-mpi-type mpi-info)
(define-mpi-type mpi-message)
(define-mpi-type mpi-request)

(defclass mpi-object ()
  ((name :type string
         :reader name
         :initarg :name
         :initform "")
   (foreign-object
    :reader foreign-object
    :initarg :foreign-object
    :type
    #+openmpi foreign-pointer
    #-openmpi (unsigned-byte 32))))

(defclass mpi-errhandler (mpi-object) ())
(defclass mpi-comm (mpi-object) ())
(defclass mpi-group (mpi-object) ())
(defclass mpi-datatype (mpi-object) ())
(defclass mpi-op (mpi-object) ())
(defclass mpi-info (mpi-object) ())
(defclass mpi-request (mpi-object) ())

(defmethod make-load-form ((object mpi-object) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-of object)
                  :name ,(name object)))

(defmacro define-mpi-object-expander (type foreign-type)
  `(defmethod expand-to-foreign (value (type ,foreign-type))
     `(progn
        (check-type ,value ,',type)
        (foreign-object ,value))))

(define-mpi-object-expander mpi-errhandler mpi-errhandler-type)
(define-mpi-object-expander mpi-comm mpi-comm-type)
(define-mpi-object-expander mpi-group mpi-group-type)
(define-mpi-object-expander mpi-datatype mpi-datatype-type)
(define-mpi-object-expander mpi-op mpi-op-type)
(define-mpi-object-expander mpi-info mpi-info-type)
(define-mpi-object-expander mpi-request mpi-request-type)

;;; if foreign-object is not given it is derived from the given name. An
;;; error is signalled if such a symbol is not found
(defmethod initialize-instance :after ((object mpi-object) &rest args)
  (declare (ignore args))
  (unless (slot-boundp object 'foreign-object)
    #+openmpi
    (let* ((openmpi-name
             (concatenate
              'string
              (if (typep object 'mpi-op)
                  "ompi_mpi_op_"
                  "ompi_mpi_")
              (string-downcase (name object))))
           (handle (foreign-symbol-pointer openmpi-name)))
      (if handle
          (setf (slot-value object 'foreign-object) handle)
          (error "MPI symbol ~A could not be found" openmpi-name)))
    #-openmpi
    (setf (slot-value object 'foreign-object)
          (let ((symbol-name
                  (concatenate 'string "MPI_" (name object))))
            (symbol-value (find-symbol symbol-name :mpi-header))))))

(define-foreign-type mpi-error-type ()
  ()
  (:actual-type :int)
  (:simple-parser mpi-error-code))

(define-condition mpi-error-condition (error)
  ((error-code :initarg :error-code :reader error-code))
  (:report
   (lambda (c stream)
     (princ (mpi-error-string (error-code c))
            stream)))
  (:documentation "Signalled when a MPI function returns a code other than MPI_SUCCESS."))

(defun signal-mpi-error (value)
  "Raise a MPI-CODE-ERROR if VALUE, a mpi-code, is non-zero."
  (restart-case
      (error 'mpi-error-condition :error-code value)
    (ignore () nil)))

;;; have each function that returns objects of mpi-error-type signal an
;;; appropriate error condition if the error-code is not zero.
(defmethod expand-from-foreign (value (type mpi-error-type))
  (let ((return-value (gensym)))
    `(let ((,return-value ,value))
       (unless (zerop ,return-value)
         (signal-mpi-error ,return-value)))))

(defun %array-element-size (type)
    "Compute the number of bytes reserved per element of a simple-array. May
return fractions of a byte, e.g. for bitvectors.

Examples (sbcl on x86-64):
> (%ARRAY-ELEMENT-SIZE 'character)         -> 4
> (%ARRAY-ELEMENT-SIZE 'base-char)         -> 1
> (%ARRAY-ELEMENT-SIZE '(unsigned-byte 1)) -> 1/8
> (%ARRAY-ELEMENT-SIZE '(unsigned-byte 2)) -> 1/4
> (%ARRAY-ELEMENT-SIZE '(unsigned-byte 9)) -> 2
> (%ARRAY-ELEMENT-SIZE 'double-float)      -> 8"
    (let* ((initial-element
             (cond
               ((subtypep type 'character) #\B)
               ((subtypep type 'float) (coerce 0 type))
               (t 0)))
           (test-array
             (make-static-vector 2 :element-type type
                                   :initial-element initial-element))
           (ptr (static-vector-pointer test-array)))
      ;; flip more and more bits until the second value of the static array
      ;; changes. The upper bound of 128 should never be reached and is only a
      ;; safeguard against overwriting the whole heap in case of something odd.
      (loop for bit from 0 to 128 do
        (setf (mem-ref ptr :uint8 (floor bit 8))
              (expt 2 (mod bit 8)))
            when (not
                  (eql (aref test-array 1)
                       initial-element))
              do (free-static-vector test-array)
                 (return (/ bit 8))
            finally
               (error "Unknown array memory layout. Possible memory corruption!"))))
