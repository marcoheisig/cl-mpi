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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFMPIFUN
;;;
;;; A CFFI:DEFCFUN invocation looks something like
;;; (CFFI:DEFCFUN NAMESPEC RETURN-VALUE (ARG1 TYPE1) (ARG2 TYPE2) ...)
;;;
;;; A typical MPI routine takes sometimes more than eight arguments and
;;; always returns an MPI-ERROR-CODE. To improve readablility, I provide a
;;; function DEFMPIFUN, which expands into defcfun, but attempts to look up
;;; the type of a variable in the table *MPI-NAMING-CONVENTIONS*.
;;;
;;; example: the type of an argument variable COUNT is always :INTEGER.

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
          tag sendtag recvtag size root commute errorcode color key)
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
    table))

(defmacro defmpifun (foreign-name (&rest args) &key (introduced "1.0"))
  (check-type foreign-name string)
  (let ((lisp-name
          (intern
           (concatenate 'string "%" (substitute #\- #\_ (string-upcase foreign-name)))
           '#:cl-mpi))
        (expanded-args
          (loop for arg in args
                collect
                (if (symbolp arg)
                    `(,arg ,(gethash arg *mpi-naming-conventions*))
                    arg))))
    ;; Currently I do not handle deprecation - this is ok because as of June
    ;; 2015 the MPI Committee also has no way to handle deprecation.
    `(since-mpi-version ,introduced
       (defcfun (,foreign-name ,lisp-name) mpi-error-code ,@expanded-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; converting Lisp vectors to raw pointers
;;;
;;; The next section is not for the weak of heart and demonstrates how one
;;; can obtain the address and length of subsequences of Lisp vectors.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mpi-datatype-table*
    '((+mpi-char+ . :char)
      (+mpi-signed-char+ . :char)
      (+mpi-unsigned-char+ . :unsigned-char)
      (+mpi-byte+ . :char)
      (+mpi-short+ . :short)
      (+mpi-unsigned-short+ . :unsigned-short)
      (+mpi-int+ . :int)
      (+mpi-unsigned+ . :unsigned-int)
      (+mpi-long+ . :long)
      (+mpi-unsigned-long+ . :unsigned-long)
      (+mpi-long-long-int+ . :long-long)
      (+mpi-unsigned-long-long+ . :unsigned-long-long)
      (+mpi-float+ . :float)
      (+mpi-double+ . :double)
      (+mpi-long-double+ . :long-double))
    "An alist of MPI datatypes and corresponding CFFI types."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bits-per-element (array-element-type)
    "Bits reserved per array element of type ARRAY-ELEMENT-TYPE."
    (when (eq (upgraded-array-element-type array-element-type) t)
      (error "Cannot determine bits per element for elements of type T."))
    (let ((initial-element
            (cond
              ((subtypep array-element-type 'character) #\B)
              ((subtypep array-element-type 'float) (coerce 0 array-element-type))
              (t 0))))
      (with-static-vector (test-array 8 :element-type array-element-type
                                         :initial-element initial-element)
        (let ((ptr (static-vector-pointer test-array)))
          ;; flip more and more bytes until the outer values of the static
          ;; array changes. The upper bound of 128 should never be reached
          ;; and is only a safeguard against overwriting the whole heap in
          ;; case of something odd.
          (loop for i below 128 and flipped-bytes from 1 do
            (setf (mem-ref ptr :int8 i) (lognot (mem-ref ptr :int8 i)))
                when (not (eql (aref test-array 7) initial-element))
                  do (return (if (< flipped-bytes 8) flipped-bytes
                                 (* 8 (floor flipped-bytes 7))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *bits-per-element-table*
    (let* ((upgraded-array-element-types
             (remove-duplicates
              (mapcar #'upgraded-array-element-type
                      `(single-float
                        double-float
                        base-char
                        character
                        ,@(loop for n from 1 to 64
                                append `((signed-byte ,n)
                                         (unsigned-byte ,n)))))
              :test #'equal))
           (alist (loop for uaet in (remove 't upgraded-array-element-types)
                        collect
                        (let ((bits (bits-per-element uaet)))
                          `(,uaet . ,bits)))))
      ;; now remove all unnecessary clauses, that is where the element type
      ;; is a true subtype of another one, while having the same number of
      ;; bits per element.
      (loop for (uaet . bits) in (copy-seq alist) do
        (setf alist
              (delete-if
               (lambda (x)
                 (let ((x-uaet (car x))
                       (x-bits (cdr x)))
                   (and (subtypep x-uaet uaet)
                        (not (equal x-uaet uaet))
                        (= x-bits bits))))
               alist)))
      alist)))

(defmacro mpi-datatype-of-size (size)
  `',(car (find size *mpi-datatype-table*
                :key (lambda (x) (foreign-type-size (cdr x))))))

;;; This macro is essentially a specialized compiler for vector memory
;;; access and is only useful in the body of STATIC-VECTOR-MPI-DATA
(defmacro static-vector-mpi-data-dispatch (vector start end)
  (let ((blocks
          (loop for bits in (remove-duplicates (mapcar #'cdr *bits-per-element-table*))
                collect
                (let ((mpi-datatype (ecase bits
                                      ((1 2 4 8) (mpi-datatype-of-size 1))
                                      (16 (mpi-datatype-of-size 2))
                                      (32 (mpi-datatype-of-size 4))
                                      (64 (mpi-datatype-of-size 8))))
                      (bytes-per-element (/ bits 8))
                      (elements-per-mpi-datatype (ceiling 8 bits)))
                  `(,bits
                    (setf offset (* ,start ,bytes-per-element))
                    (setf mpi-datatype ,mpi-datatype)
                    (setf count (ceiling (- ,end ,start) ,elements-per-mpi-datatype))
                    (go end)))))
        (clauses
          (loop for (uaet . bits) in *bits-per-element-table* collect
                `((simple-array ,uaet (*)) (go ,bits)))))
    `(let ((offset 0) mpi-datatype (count 0))
       (declare (type fixnum offset)
                (type (or mpi-datatype null) mpi-datatype)
                (type int count))
       (tagbody
          (etypecase ,vector ,@clauses)
          ,@(apply #'append blocks)
        end)
       (values (static-vector-pointer ,vector :offset offset) mpi-datatype count))))

(defun static-vector-mpi-data (vector &optional start end)
  "Return a pointer to the raw memory of the given vector, as well as the
corresponding MPI-DATATYPE and the number of elements to transmit.

WARNING: If ARRAY is somehow moved in memory (e.g. by the garbage collector),
your code is broken, so better have a look at the STATIC-VECTORS package."
  (declare (type (simple-array * (*)) vector)
           (type index start end)
           (optimize (safety 0) (debug 0)))
  (let* ((dim (length vector))
         (start (or start 0))
         (end (or end dim)))
    (assert (<= 0 start end dim))
    ;; The macroexpansion of the following line is quite beautiful
    (static-vector-mpi-data-dispatch vector start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finally some miscellaneous utilites for cl-mpi

(defmacro with-foreign-results (bindings &body body)
  "Evaluate body as with WITH-FOREIGN-OBJECTS, but afterwards convert them to
  lisp objects and return them via VALUES."
  `(with-foreign-objects ,bindings
     ,@body
     (values
      ,@(loop for binding in bindings
              collect `(mem-ref ,@binding)))))

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
     (mpi-group +mpi-group-null+)
     (mpi-comm +mpi-comm-null+)
     (mpi-request +mpi-request-null+)
     (mpi-op +mpi-op-null+)
     (mpi-errhandler +mpi-errhandler-null+)
     (t nil))))
