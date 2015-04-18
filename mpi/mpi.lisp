#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

Convenient wrappers for most MPI functions

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

;;; helper functions

(defun cffi-type-to-mpi-type (cffi-type)
  "Convert :int to MPI_INT and so on"
  (declare (type keyword cffi-type))
  (ecase cffi-type
    ((:char) +mpi-char+)
    ((:uchar :unsigned-char) +mpi-unsigned-char+)
    ((:short) +mpi-short+)
    ((:ushort :unsigned-short) +mpi-unsigned-short+)
    ((:int) +mpi-int+)
    ((:uint :unsigned-int) +mpi-unsigned+)
    ((:long) +mpi-long+)
    ((:ulong :unsigned-long) +mpi-unsigned-long+)
    ((:llong :long-long) +mpi-long-long-int+)
    ((:ullong :unsigned-long-long) +mpi-unsigned-long-long+)
    ((:float) +mpi-float+)
    ((:double) +mpi-double+)))

(defun mpi-type-to-cffi-type (mpi-type)
  "Convert MPI_INT to :int and so on"
  (declare (type mpi-datatype mpi-type))
  (cond
    ((eq mpi-type +mpi-char+) :char)
    ((eq mpi-type +mpi-unsigned-char+) :unsigned-char)
    ((eq mpi-type +mpi-short+) :short)
    ((eq mpi-type +mpi-unsigned-short+) :unsigned-short)
    ((eq mpi-type +mpi-int+) :int)
    ((eq mpi-type +mpi-unsigned+) :unsigned-int)
    ((eq mpi-type +mpi-long+) :long)
    ((eq mpi-type +mpi-unsigned-long+) :unsigned-long)
    ((eq mpi-type +mpi-long-long-int+) :long-long)
    ((eq mpi-type +mpi-unsigned-long-long+) :unsigned-long-long)
    ((eq mpi-type +mpi-float+) :float)
    ((eq mpi-type +mpi-double+) :double)))


(defun simple-array-data (array)
  "Return a pointer to the raw memory of the given array, as well as the
corresponding mpi-type and length. This is highly implementation dependent.

WARNING: If ARRAY is somehow moved in memory (e.g. by the garbage collector),
your code is broken. So better have a look at the STATIC-VECTORS package."
  (declare (type simple-array object))
  #+sbcl
  (let* ((vector (sb-ext:array-storage-vector array))
         (mpi-type
           (etypecase vector
             ((simple-array single-float (*)) +mpi-float+)
             ((simple-array double-float (*)) +mpi-double+)
             ((simple-array (signed-byte 8) (*)) +mpi-int8-t+)
             ((simple-array (unsigned-byte 8) (*)) +mpi-uint8-t+)
             ((simple-array (signed-byte 16) (*)) +mpi-int16-t+)
             ((simple-array (unsigned-byte 16) (*)) +mpi-uint16-t+)
             ((simple-array character (*)) #+sb-unicode +mpi-uint32-t+
                                           #-sb-unicode +mpi-uint8-t+)
             ((simple-array (signed-byte 32) (*)) +mpi-int32-t+)
             ((simple-array (unsigned-byte 32) (*)) +mpi-uint32-t+)
             ((simple-array (signed-byte 64) (*)) +mpi-int64-t+)
             ((simple-array (unsigned-byte 64) (*)) +mpi-uint64-t+)))
         (count (length vector))
         (pointer (sb-sys:vector-sap vector)))
    (values pointer mpi-type count))
  #-sbcl
  (error "currently there is no support for this lisp, sorry"))

(defmacro with-foreign-results (bindings &body body)
  "Evaluate body as with WITH-FOREIGN-OBJECTS, but afterwards convert them to
  lisp objects and return them via VALUES."
   ;; TOOD bindings are currently evaluated multiple times
  (let ((results
          (loop for binding in bindings
                collect
                (if (cddr binding)
                    `(mem-ref ,(car binding) (:array ,(cadr binding) ,(caddr binding)))
                    `(mem-ref ,@binding)))))
    `(with-foreign-objects ,bindings
       ,@body
       (values ,@results))))

(defmacro with-pinned-arrays ((&rest objects) &body body)
  "Ensure or assert that the given arrays will not be moved during the execution
of BODY."
  #+sbcl
  `(sb-sys:with-pinned-objects ,objects
     ,@body)
  #-sbcl
  `,body)

(defun mpi-init ()
  "This routine must be called before any other MPI routine. It must be called
at most once; subsequent calls are erroneous (see MPI-INITIALIZED).

All MPI programs must contain a call to MPI-INIT; this routine must be called
before any other MPI routine (apart from MPI-INITIALIZED) is called."
  (unless (mpi-initialized)
    (%mpi-init (null-pointer) (null-pointer))
    ;; by default MPI reacts to each failure by crashing the process. This is
    ;; not the Lisp way of doing things. The following call makes error
    ;; non-fatal in most cases.
    (%mpi-comm-set-errhandler +mpi-comm-world+ +mpi-errors-return+)))

(defun mpi-initialized ()
  "Returns true if MPI_INIT has been called and nil otherwise.
   This routine may be used to determine whether MPI-INIT has been called. It
   is the only routine that may be called before MPI-INIT is called."
  (with-foreign-results ((flag :boolean))
    (%mpi-initialized flag)))

(defun mpi-abort(&key (comm *standard-communicator*) (errcode -1))
  "This routine makes a 'best attempt' to abort all tasks in the group of
comm. This function does not require that the invoking environment take any
action with the error code. However, a Unix or POSIX environment should handle
this as a return errorcode from the main program or an abort(errorcode)."
  (%mpi-abort comm errcode))

(defun mpi-get-processor-name ()
  "This routine returns the name of the processor on which it was called at
the moment of the call.  The name is a character string for maximum
flexibility. From this value it must be possible to identify a specific piece
of hardware; possible values include 'processor 9 in rack 4 of mpp.cs.org' and
'231' (where 231 is the actual processor number in the running homogeneous
system)."
  (with-foreign-object (namelen :int)
    (with-foreign-pointer (processor-name +mpi-max-processor-name+)
      (%mpi-get-processor-name processor-name namelen)
      (values (foreign-string-to-lisp
               processor-name
               :count (mem-aref namelen :int))))))

(defun mpi-error-string (errorcode)
  "Convert the given errorcode to a human readable error message"
  (with-foreign-object (strlen :int)
    (with-foreign-pointer (error-string +mpi-max-error-string+)
      (%mpi-error-string errorcode error-string strlen)
      (values (foreign-string-to-lisp
               error-string
               :count (mem-aref strlen :int))))))

(defun mpi-barrier (&optional (comm *standard-communicator*))
  "MPI-BARRIER blocks the caller until all group members have called it. The
  call returns at any process only after all group members have entered the
  call."
  (%mpi-barrier comm))

(defun mpi-sendreceive (out dest in source
                        &key
                          (send-tag 0)
                          (recv-tag +mpi-any-tag+)
                          (comm *standard-communicator*))
  (declare (type simple-array out in)
           (type (signed-byte 32)
                 dest send-tag source  recv-tag))
  (with-pinned-arrays (out in)
    (multiple-value-bind (send-buf send-type send-count)
        (simple-array-data out)
      (multiple-value-bind (recv-buf recv-type recv-count)
        (simple-array-data in)
        (%mpi-sendrecv
         send-buf send-count send-type dest send-tag
         recv-buf recv-count recv-type source recv-tag
         comm +mpi-status-ignore+)))))

(defun mpi-send (array dest &key
                              (tag 0)
                              (comm *standard-communicator*)
                              (mode :basic))
  "Send a given ARRAY to a corresponding MPI-RECEIVE. The arrays passed to
MPI-SEND and MPI-RECEIVE must be of type SIMPLE-ARRAY and have the same
element-type and dimensions. Undefined behaviour occurs if the arrays at
sender and receiver side do not match."
  (declare (type simple-array array)
           (type (signed-byte 32) dest tag)
           (type mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode))
  (let ((send-function
          (ecase mode
            (:basic #'%mpi-send)
            (:buffered #'%mpi-bsend)
            (:synchronous #'%mpi-ssend)
            (:ready #'%mpi-rsend))))
    (with-pinned-arrays (array)
      (multiple-value-bind (ptr type count)
          (simple-array-data array)
        (funcall send-function ptr count type dest tag comm)))))

(defun mpi-isend (array dest &key
                               (tag 0)
                               (comm *standard-communicator*)
                               (mode :basic))
  "A non-blocking variant of MPI-SEND. It returns immediately. To check
  whether the send operation is complete, use MPI-WAIT or MPI-TEST.

WARNING: The caller of MPI-ISEND is responsible that the given array is not
relocated or garbage-collected until the send operation is complete. This can
be achieved by using STATIC-VECTORS or some implementation dependent
mechanism such as sb-sys:with-pinned-objects."
  (declare (type simple-array array)
           (type (signed-byte 32) dest tag)
           (type mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode)
           (values mpi-request))
  (let ((send-function
          (ecase mode
            (:basic #'%mpi-isend)
            (:buffered #'%mpi-ibsend)
            (:synchronous #'%mpi-issend)
            (:ready #'%mpi-irsend))))
    (multiple-value-bind (ptr type count)
        (simple-array-data array)
      (with-foreign-results ((request 'mpi-request))
        (funcall send-function ptr count type dest tag comm request)))))

(defun mpi-receive (array source &key
                                   (tag +mpi-any-tag+)
                                   (comm *standard-communicator*))
  (declare (type simple-array array)
           (type (signed-byte 32) source tag)
           (type mpi-comm comm))
  (with-pinned-arrays (array)
    (multiple-value-bind (ptr type count)
        (simple-array-data array) ;; TODO check the mpi-status
      (%mpi-recv ptr count type source tag comm +mpi-status-ignore+))))

(defun mpi-comm-group (&optional (comm *standard-communicator*))
  (make-instance
   'mpi-group
   :foreign-object
   (with-foreign-results ((newgroup 'mpi-group))
     (%mpi-comm-group comm newgroup))))

(defun mpi-group-size (group)
  (with-foreign-results ((size :int))
    (%mpi-group-size group size)))

(defun mpi-group-rank (group)
  (with-foreign-results ((rank :int))
    (%mpi-group-rank group rank)))

(defun mpi-group-union (group1 group2)
  (with-foreign-results ((newgroup 'mpi-group))
    (%mpi-group-union group1 group2 newgroup)))

(defun mpi-group-intersection (group1 group2)
  (with-foreign-results ((newgroup 'mpi-group))
    (%mpi-group-intersection group1 group2 newgroup)))

(defun mpi-group-difference (group1 group2)
  (with-foreign-results ((newgroup 'mpi-group))
    (%mpi-group-difference group1 group2 newgroup)))

;; (defun mpi-group-select-from (group &rest ranges)
;;   "Create a new MPI group consisting of a subset of the ranks of the original
;;  group. A valid range can be
;;   - an integer
;;   - a list of the form (first-rank last-rank &optional step-size)"
;;   (let ((n (length ranges)))
;;     (with-foreign-objects ((mem :int (* 3 n)) (newgroup 'mpi-group))
;;       (loop for range-spec in ranges and i from 0 by 3
;;          with step-size = 1 and last-rank and first-rank do
;;            (cond
;;              ((integerp range-spec)
;;               (setf first-rank range-spec)
;;               (setf last-rank range-spec))
;;              ((listp range-spec)
;;               (setf first-rank (car range-spec))
;;               (setf last-rank (cadr range-spec))
;;               (setf step-size (if (cddr range-spec) (caddr range-spec) 1)))
;;              (t (error "invalid range spec")))
;;            (setf (mem-aref mem :int (+ i 0)) first-rank)
;;            (setf (mem-aref mem :int (+ i 1)) last-rank)
;;            (setf (mem-aref mem :int (+ i 2)) step-size))
;;       (MPI-Group-range-incl group n mem newgroup)
;;       (mem-ref newgroup 'MPI-Group))))

;; (defmpifun "MPI-Group-excl" (group :pointer) (n :int) (ranks :pointer) (newgroup :pointer))
;; (defmpifun "MPI-Group-free" (group mpi-group))
;;(defun mpi-group-free (group) (MPI-Group-free group))

(defun mpi-comm-size (&optional (comm *standard-communicator*))
  "Indicates the number of processes involved in a communicator. For
+mpi-comm-world+, it indicates the total number of processes available."
  (with-foreign-results ((size :int))
    (%mpi-comm-size comm size)))

(defun mpi-comm-rank (&optional (comm *standard-communicator*))
  "Returns the rank of the process in a given communicator."
  (with-foreign-results ((rank :int))
    (%mpi-comm-rank comm rank)))

(defun mpi-comm-create (group &key (comm *standard-communicator*))
  (make-instance
   'mpi-comm
   :foreign-object
   (with-foreign-results ((newcomm 'mpi-comm))
     (%mpi-comm-create comm group newcomm))))

(defun mpi-type-size (datatype)
  (with-foreign-results ((size :int))
    (%mpi-type-size datatype size)))

;;; after some consideration I decided it is the right thing to call MPI-INIT
;;; as soon as cl-mpi is loaded. Otherwise all MPI calls exhibit unspecified
;;; behavior.
(eval-when (:load-toplevel :execute)
  (mpi-init))
