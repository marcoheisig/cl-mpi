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

(in-package #:mpi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mpi-naming-conventions*
    (flet ((genpairs (args)
             (destructuring-bind (type &rest symbols) args
               (loop for symbol in symbols
                     collect (list symbol type)))))
      (apply
       #'append
       (mapcar
        #'genpairs
        '((:pointer
           *buf *sendbuf *recvbuf *inbuf *outbuf *inoutbuf fun argc argv)
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
          ((:pointer mpi-comm) *newcomm)
          ((:pointer mpi-group) *newgroup *group)
          ((:pointer mpi-datatype) *newtype)
          ((:pointer (:struct mpi-status)) statuses)
          ((:pointer mpi-datatype) sendtypes recvtypes)
          ((:pointer mpi-request) requests)
          ((:array :int *) indices)
          ((:pointer (:array :int *)) ranges)
          ((:array :int *)
           ranks ranks1 ranks2 sendcounts recvcounts displs sdispls rdispls)))))))

(defmacro defmpifun (foreign-name (&rest mpi-identifiers)
                     &key documentation introduced deprecated removed)
  (check-type foreign-name string)
  (let ((lisp-name
          (intern
           (concatenate 'string "%" (substitute #\- #\_ (string-upcase foreign-name)))
           '#:cl-mpi))
        (args
          (loop for i in mpi-identifiers
                collect (find i *mpi-naming-conventions* :test #'eq :key #'car)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %bits-per-element (array-element-type)
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
                   (error "Unknown array memory layout. Possible memory corruption!")))))))

(declaim (inline bits-per-element))
(defun bits-per-element (array)
  (etypecase array
    ((simple-array single-float (*))       #.(%bits-per-element 'single-float))
    ((simple-array double-float (*))       #.(%bits-per-element 'double-float))
    #+sbcl
    ((simple-array (unsigned-byte 1) (*))  #.(%bits-per-element '(unsigned-byte 1)))
    #+sbcl
    ((simple-array (unsigned-byte 2) (*))  #.(%bits-per-element '(unsigned-byte 2)))
    #+sbcl
    ((simple-array (unsigned-byte 4) (*))  #.(%bits-per-element '(unsigned-byte 4)))
    ((simple-array (signed-byte 8) (*))    #.(%bits-per-element '(signed-byte 8)))
    ((simple-array (unsigned-byte 8) (*))  #.(%bits-per-element '(unsigned-byte 8)))
    ((simple-array (signed-byte 16) (*))   #.(%bits-per-element '(signed-byte 16)))
    ((simple-array (unsigned-byte 16) (*)) #.(%bits-per-element '(unsigned-byte 16)))
    ((simple-array (signed-byte 32) (*))   #.(%bits-per-element '(signed-byte 32)))
    ((simple-array (unsigned-byte 32) (*)) #.(%bits-per-element '(unsigned-byte 32)))
    ((simple-array (signed-byte 64) (*))   #.(%bits-per-element '(signed-byte 64)))
    ((simple-array (unsigned-byte 64) (*)) #.(%bits-per-element '(unsigned-byte 64)))
    ((simple-array base-char (*))          #.(%bits-per-element 'base-char))
    #+sbcl
    ((simple-array character (*))          #.(%bits-per-element 'character))))

(declaim (inline static-vector-mpi-data))
(defun static-vector-mpi-data (vector start end)
  "Return a pointer to the raw memory of the given array, as well as the
corresponding mpi-type and length.

WARNING: If ARRAY is somehow moved in memory (e.g. by the garbage collector),
your code is broken. So better have a look at the STATIC-VECTORS package."
  (declare (type (simple-array * (*)) vector))
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


