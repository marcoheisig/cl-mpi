#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI related utility functions

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

(in-package :mpi)

;;; A CFFI:DEFCFUN invocation looks something like
;;; (CFFI:DEFCFUN NAMESPEC RETURN-VALUE (ARG1 TYPE1) (ARG2 TYPE2) ...)
;;;
;;; A typical MPI routine takes sometimes more than eight arguments and always
;;; returns an MPI-ERROR-CODE. To improve readablility, I provide a function
;;; DEFMPIFUN, which looks up the type of a variable in the table
;;; *MPI-NAMING-CONVENTIONS* and automatically returns MPI-ERROR-CODE. For
;;; example the type of an argument variable COUNT is always :INTEGER.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mpi-naming-conventions*
    (let ((table (make-hash-table)))
      (flet ((introduce-conventions (type &rest symbols)
               (loop for symbol in symbols do
                 (setf (gethash symbol table) type))))
        (mapc
         (lambda (x) (apply #'introduce-conventions x))
         '((:pointer
            *buf *sendbuf *recvbuf *inbuf *outbuf *inoutbuf fun argc argv ptr)
           ((:pointer :int)
            *result *count *position *size *rank *index *outcount *commute *keyval)
           (:string string)
           ((:pointer :boolean) *flag)
           (:int
            count incount outcount insize outsize sendcount recvcount source dest
            tag sendtag recvtag size root commute errorcode)
           (:boolean flag)
           (mpi-errhandler errhandler)
           (mpi-comm comm oldcomm comm1 comm2)
           (mpi-group group group1 group2)
           (mpi-datatype datatype sendtype recvtype oldtype)
           (mpi-op op)
           (mpi-request request)
           ((:pointer (:struct mpi-status)) *status)
           ((:pointer mpi-op) *op)
           ((:pointer mpi-message) *message)
           ((:pointer mpi-request) *request)
           ((:pointer mpi-comm) *newcomm *comm)
           ((:pointer mpi-group) *newgroup *group)
           ((:pointer mpi-datatype) *newtype)
           ((:pointer (:struct mpi-status)) statuses)
           ((:pointer mpi-datatype) sendtypes recvtypes)
           ((:pointer mpi-request) requests)
           ((:array :int *) indices)
           ((:pointer (:array :int *)) ranges)
           ((:array :int *)
            ranks ranks1 ranks2 sendcounts recvcounts displs sdispls rdispls))))
      table)))

(defmacro defmpifun (foreign-name (&rest mpi-identifiers)
                     &key documentation introduced deprecated removed)
  (check-type foreign-name string)
  (let ((lisp-name
          (intern
           (concatenate 'string "%" (substitute #\- #\_ (string-upcase foreign-name)))
           '#:cl-mpi))
        (args
          (loop for symbol in mpi-identifiers
                collect `(,symbol ,(gethash symbol *mpi-naming-conventions*))))
        (introducedp (version<= introduced +mpi-version+))
        (removedp (and removed (version<= removed +mpi-version+)))
        (deprecatedp (version<= deprecated +mpi-version+)))
    ;; Currently I do not handle deprecation - this is ok because as of June
    ;; 2015 the MPI Committee also has no way to handle deprecation.
    (declare (ignorable deprecatedp))
    (if documentation (push documentation args))
    (cond
      ((not introducedp)
       `(defun ,lisp-name ,mpi-identifiers
          (declare (ignore ,@mpi-identifiers))
          (error
           "This function is only available from MPI versions above ~A.~%"
           ,introduced)))
      (removedp
       `(defun ,lisp-name ,mpi-identifiers
          (declare (ignore ,@mpi-identifiers))
          (error
           "This function was removed in MPI version ~A.~%"
           ,removed)))
      (t
       `(defcfun (,foreign-name ,lisp-name) mpi-error-code ,@args)))))

(defmacro %bits-per-element (array-element-type)
  "Compute the number of bits reserved per element of a simple-array."
  (let ((initial-element
          (cond
            ((subtypep array-element-type 'character) #\B)
            ((subtypep array-element-type 'float) (coerce 0 array-element-type))
            (t 0))))
    (with-static-vector (test-array 2 :element-type array-element-type
                                      :initial-element initial-element)
      (let ((ptr (static-vector-pointer test-array)))
        ;; flip more and more bits until the second value of the static array
        ;; changes. The upper bound of 128 should never be reached and is only a
        ;; safeguard against overwriting the whole heap in case of something odd.
        (loop for bit from 0 to 128 do
          (setf (mem-ref ptr :uint8 (floor bit 8))
                (expt 2 (mod bit 8)))
              when (not (eql (aref test-array 1)
                             initial-element))
                do (return bit)
              finally
                 (error "Unknown array memory layout. Possible memory corruption!"))))))

(declaim (inline bits-per-element))
(defun bits-per-element (array)
  (etypecase array
    ((simple-array single-float (*))       (%bits-per-element single-float))
    ((simple-array double-float (*))       (%bits-per-element double-float))
    #-ccl
    ((simple-array (unsigned-byte 1) (*))  (%bits-per-element (unsigned-byte 1)))
    #-ccl
    ((simple-array (unsigned-byte 2) (*))  (%bits-per-element (unsigned-byte 2)))
    #-ccl
    ((simple-array (unsigned-byte 4) (*))  (%bits-per-element (unsigned-byte 4)))
    ((simple-array (signed-byte 8) (*))    (%bits-per-element (signed-byte 8)))
    ((simple-array (unsigned-byte 8) (*))  (%bits-per-element (unsigned-byte 8)))
    ((simple-array (signed-byte 16) (*))   (%bits-per-element (signed-byte 16)))
    ((simple-array (unsigned-byte 16) (*)) (%bits-per-element (unsigned-byte 16)))
    ((simple-array (signed-byte 32) (*))   (%bits-per-element (signed-byte 32)))
    ((simple-array (unsigned-byte 32) (*)) (%bits-per-element (unsigned-byte 32)))
    ((simple-array (signed-byte 64) (*))   (%bits-per-element (signed-byte 64)))
    ((simple-array (unsigned-byte 64) (*)) (%bits-per-element (unsigned-byte 64)))
    ((simple-array base-char (*))          (%bits-per-element base-char))
    #-ccl
    ((simple-array character (*))          (%bits-per-element character))))

(declaim (inline static-vector-mpi-data)
         (ftype (function * (values foreign-pointer mpi-datatype (signed-byte 32)))))
(defun static-vector-mpi-data (vector start end)
  "Return a pointer to the raw memory of the given array, as well as the
corresponding MPI-DATATYPE and the number of elements to transmit.

WARNING: If ARRAY is somehow moved in memory (e.g. by the garbage collector),
your code is broken. So better have a look at the STATIC-VECTORS package."
  (declare (type (simple-array * (*)) vector)
           (type (or null (integer 0 #.array-total-size-limit)) start end)
           (optimize (safety 0) (debug 0)))
  (let* ((n-bits (bits-per-element vector))
         (mpi-datatype
           (ecase n-bits
             ((1 2 4 8) +mpi-uint8-t+)
             (16 +mpi-uint16-t+)
             (32 +mpi-uint32-t+)
             (64 +mpi-uint64-t+)))
         ;; MPI can only send with byte granularity. It is an error to send
         ;; arrays with element size less than 8 where start and end are not
         ;; aligned on byte boundaries
         (bit-alignment
           (case n-bits
             (1 8)
             (2 4)
             (4 2)
             (otherwise 1))))
    (let* ((len (length vector))
           (start (if start start 0))
           (end (if end end len)))
      (assert (<= 0 start end len))
      (assert (zerop (rem start bit-alignment)))
      (assert (zerop (rem end bit-alignment)))
      (let* ((offset (ceiling (* start n-bits) 8))
             (count (- end start))
             (ptr (static-vector-pointer vector :offset offset)))
        (values ptr mpi-datatype count)))))

(defmacro with-foreign-results (bindings &body body)
  "Evaluate body as with WITH-FOREIGN-OBJECTS, but afterwards convert them to
  lisp objects and return them via VALUES."
  `(with-foreign-objects ,bindings
     ,@body
     (values
      ,@(loop for binding in bindings
              collect `(mem-ref ,@binding)))))

(defmacro with-static-vectors (specs &body body)
  "Wrap BODY into multiple invocations of WITH-STATIC-VECTOR."
  (if (null specs)
      `(progn ,@body)
      `(with-static-vector ,(car specs)
         (with-static-vectors ,(cdr specs)
           ,@body))))

(defun reload-mpi-libraries ()
  "Load all MPI related libraries again. This might be necessary after a
session is resumed from a Lisp image"

  ;; the following code can actually be triggered on ECL, when quicklisp is
  ;; available at image creation time, but not when the image is executed.
  #+quicklisp
  (unless (find :quicklisp *features*)
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init))))
  (load-foreign-library
   (asdf:output-file
    'asdf:compile-op
    (asdf:find-component "cl-mpi" '("mpi" . "cl-mpi-stub")))))

(defmacro with-fresh-mpi-context (&body body)
  "Execute body with *STANDARD-COMMUNICATOR* bound to a new unique
communicator. This prevents errors within BODY to affect other parts of the
program."
  (let ((*standard-communicator* (mpi-comm-dup)))
    (unwind-protect
         `(progn ,@body)
      (mpi-comm-free *standard-communicator*))))

(defun mpi-equal (a b)
  (when (and (typep a 'mpi-object)
             (typep b 'mpi-object))
    (let ((a (mpi-object-handle a))
          (b (mpi-object-handle b)))
      (if (and (integerp a) (integerp b))
          (eql a b)
          (pointer-eq a b)))))

(defun mpi-null (object)
  (mpi-equal
   object
   (typecase object
     (mpi-comm +mpi-comm-null+)
     (mpi-group +mpi-group-null+)
     (mpi-datatype +mpi-datatype-null+)
     (mpi-request +mpi-request-null+)
     (t nil))))
